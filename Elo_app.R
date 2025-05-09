############################################## app.R

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)


# 1) Read in your expanded standings exactly as before:
stand_raw <- read.csv(text = standings_text, stringsAsFactors = FALSE, check.names = FALSE)

# 2) Fix the first two column names:
#    Column 1 is "Rk", column 2 is currently "" → rename to "Team"
colnames(stand_raw)[1:2] <- c("Rk", "Team")

# 3) Drop the ranking column (you don’t need it for modeling):
stand_clean <- stand_raw %>% select(-Rk)

# 4) Define your helper to parse "W‑L‑OTL" strings:
parse_rec <- function(x) {
  parts <- as.integer(str_split(x, "-", simplify=TRUE))
  W <- parts[1]; L <- parts[2]; OTL <- parts[3]
  pct <- (W + 0.5*OTL) / (W + L + OTL)
  list(W=W, L=L, OTL=OTL, pct=pct)
}



# ———————————————— 3) Parse head2head into win% + N ————————————————

library(dplyr)
library(tidyr)

# 1) Read it in exactly as before:
h2h_raw <- read.csv(text = head2head_text,
                    stringsAsFactors = FALSE,
                    check.names = FALSE)

# 2) Fix the column names:
#   - Column 1 is "Rk"   
#   - Column 2 is ""  → rename to "Team"
colnames(h2h_raw)[1:2] <- c("Rk", "Team")

# 3) Drop the Rk column, pivot the rest:
h2h_long <- h2h_raw %>%
  select(-Rk) %>%                # remove the ranking column
  pivot_longer(
    cols      = -Team,           # everything except Team
    names_to  = "Team2",         # opponent code
    values_to = "rec"            # record string
  ) %>%
  filter(rec != "" & !is.na(rec)) %>%
  # split "W-L-OTL" into numeric
  separate(rec, into = c("W","L","OTL"), sep = "-", convert = TRUE) %>%
  mutate(
    games = W + L + OTL,
    h2h_pct = (W + 0.5*OTL)/games
  ) %>%
  rename(Team1 = Team)

# Quick sanity check:
head(h2h_long, 5)



# ———————————————— 4) Parse expanded standings → home adv + momentum ————————————————

parse_rec <- function(x){
  # x like "30-7-4"
  parts <- as.integer(str_split(x, "-", simplify=TRUE))
  W <- parts[1]; L<-parts[2]; OTL<-parts[3]
  list(W=W, L=L, OTL=OTL, pct=(W+0.5*OTL)/(W+L+OTL))
}

# 5) Now safely rowwise‐mutate:
std <- stand_clean %>%
  rowwise() %>%
  mutate(
    home    = parse_rec(Home)$pct,
    road    = parse_rec(Road)$pct,
    overall = parse_rec(Overall)$pct,
    recent  = {
      m <- parse_rec(Mar)$pct
      a <- parse_rec(Apr)$pct
      (m + a) / 2
    }
  ) %>%
  ungroup() %>%
  select(Team, home, road, overall, recent)


# ———————————————— 5) Assemble master “ratings” table ————————————————

# 1) compute pim_per_game in your master_df if you want it:
master_df <- master_df %>%
  mutate(
    pim_per_game = PIM / GP.x    # or whichever GP column is correct
  )
colnames(master_df)
# 2) assemble ratings, keeping both Home_Adv and Road_Adv
ratings <- master_df %>%
  # bring in home/road/momentum
  left_join(std, by="Team") %>%
  # rename the columns you need
  rename(
    Team_Elo = Current_Elo,
    Home_Adv = home,
    Road_Adv = road,
    Momentum = recent
    # PP_pct, PK_pct, SV_pct already exist with those names
  ) %>%
  # now select exactly the ones you actually have
  select(
    Team,
    Team_Elo,
    Goalie_Elo,    # now present because you joined goalie_ratings
    Home_Adv,
    Road_Adv,
    PP_pct,
    PK_pct,
    SV_pct,
    Momentum,
    pim_per_game   # now present because you just mutat‑ed it
  )



ratings <- ratings %>%
  left_join(pim_data %>% select(Team, PIM, GP), by="Team") %>%
  mutate(
    pim_per_game = PIM / GP
  )%>%
  mutate(
    PP = PP_pct / 100,
    PK = PK_pct / 100,
    SV = SV_pct / 100
  )%>%
  mutate(across(c(Goalie_Elo, PP, PK, SV, Momentum, Home_Adv, Road_Adv), as.numeric))



# ———————————————— 7) Simulate a best‑of‑7 series ————————————————

simulate_series <- function(t1, t2) {
  # home schedule pattern for 2‑2‑1‑1‑1
  home <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  w1 <- w2 <- 0
  g   <- 1L
  while(w1 < 4L && w2 < 4L && g <= 7L) {
    p1 <- multi_win_prob(t1, t2, is_home1 = home[g])
    if(runif(1) < p1) w1 <- w1 + 1L else w2 <- w2 + 1L
    g <- g + 1L
  }
  winner <- if(w1==4L) t1 else t2
  length <- g - 1L
  list(winner=winner, length=length)
}

# — now drop simulate_series() into your bracket simulator and you’re
#   accounting for head‑to‑head, goalie, special teams, home ice, momentum!

# --- 1) Multi‑factor win probability function ---
# before you call multi_win_prob(), somewhere near the top of your script:
league_avg_pim <- mean(ratings$pim_per_game, na.rm=TRUE)

# --- 1) Multi‑factor win probability function ---
multi_win_prob <- function(team1, team2, is_home1 = FALSE) {
  #d_inj <- ratings$InjuryScore[ratings$Team==team1]-ratings$InjuryScore[ratings$Team==team2]
  # look up features
  r1 <- ratings[ratings$Team == team1, ]
  r2 <- ratings[ratings$Team == team2, ]
  if (nrow(r1)==0 || nrow(r2)==0) {
    stop("Team not found in ratings: ", team1, " or ", team2)
  }
  
  # 1) Elo & goalie differences (scale ~ ±1)
  d_team   <- (r1$Team_Elo   - r2$Team_Elo)   / 400
  d_goalie <- (r1$Goalie_Elo - r2$Goalie_Elo) / 400
  
  # 2) home‑ice difference: home win pct minus away win pct
  d_home <- if (is_home1) {
    r1$Home_Adv - r2$Road_Adv
  } else {
    r1$Road_Adv - r2$Home_Adv
  }
  
  # 3) special teams static differences
  d_pp_static <- (r1$PP - r2$PP)  
  d_pk_static <- (r1$PK - r2$PK)  
  
  # 4) adjust by actual penalties
  opp_penalty_rate <- r2$pim_per_game / league_avg_pim
  own_penalty_rate <- r1$pim_per_game / league_avg_pim
  
  d_pp <- d_pp_static #* opp_penalty_rate
  d_pk <- d_pk_static #* own_penalty_rate
  
  # 5) saving/shot suppression
  d_sv  <- (r1$SV - r2$SV)
  
  # 6) weights (you should re‑fit these via logistic regression)
  beta0       <-  0.0
  beta_team   <-  1.2 *.5
  beta_goalie <-  0.8 *.0
  beta_home   <-  0.5 *.0
  beta_pp     <-  0.6 *.0
  beta_pk     <-  0.4 *.0
  beta_sv     <-  1.0 *.0
  #beta_inj    <-  0.5* .0
  
  # linear predictor
  eta <- beta0 +
    beta_team   * d_team   +
    beta_goalie * d_goalie +
    beta_home   * d_home   +
    beta_pp     * d_pp     +
    beta_pk     * d_pk     +
    beta_sv     * d_sv     #+
  #beta_inj    * d_inj
  
  # logistic
  return(1 / (1 + exp(-eta)))
}


# --- 2) Simulate a best‑of‑7 series with that new probability ---
simulate_series <- function(t1, t2) {
  # precompute static features
  p_home <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  
  w1 <- w2 <- 0
  game <- 1
  
  while(w1 < 4 && w2 < 4 && game <= 7) {
    # win probability for game 'game'
    prob1 <- multi_win_prob(t1, t2, is_home1 = p_home[game])
    
    # simulate
    if(runif(1) < prob1) {
      w1 <- w1 + 1
    } else {
      w2 <- w2 + 1
    }
    game <- game + 1
  }
  
  # return winner + series length
  winner <- if(w1 == 4) t1 else t2
  length <- game - 1
  list(winner = winner, length = length)
}


simulate_bracket_once <- function(){
  # R1 West
  r1w <- list(
    simulate_series("Winnipeg Jets","St. Louis Blues"),
    simulate_series("Dallas Stars","Colorado Avalanche"),
    simulate_series("Vegas Golden Knights","Minnesota Wild"),
    simulate_series("Los Angeles Kings","Edmonton Oilers")
  )
  # R1 East
  r1e <- list(
    simulate_series("Toronto Maple Leafs","Ottawa Senators"),
    simulate_series("Tampa Bay Lightning","Florida Panthers"),
    simulate_series("Washington Capitals","Montreal Canadiens"),
    simulate_series("Carolina Hurricanes","New Jersey Devils")
  )
  # R2 West
  r2w <- list(
    simulate_series(r1w[[1]]$winner, r1w[[2]]$winner),
    simulate_series(r1w[[3]]$winner, r1w[[4]]$winner)
  )
  # R2 East
  r2e <- list(
    simulate_series(r1e[[1]]$winner, r1e[[2]]$winner),
    simulate_series(r1e[[3]]$winner, r1e[[4]]$winner)
  )
  # Conf Finals
  cf <- list(
    simulate_series(r2w[[1]]$winner, r2w[[2]]$winner),
    simulate_series(r2e[[1]]$winner, r2e[[2]]$winner)
  )
  # Final
  fin <- simulate_series(cf[[1]]$winner, cf[[2]]$winner)
  
  rounds <- list(
    round1 = data.frame(
      Series = c(paste0("W-R1-",1:4), paste0("E-R1-",1:4)),
      Winner = c(sapply(r1w,`[[`,"winner"), sapply(r1e,`[[`,"winner")),
      stringsAsFactors = FALSE
    ),
    round2 = data.frame(
      Series = c("W-R2-1","W-R2-2","E-R2-1","E-R2-2"),
      Winner = c(sapply(r2w,`[[`,"winner"), sapply(r2e,`[[`,"winner")),
      stringsAsFactors = FALSE
    ),
    round3 = data.frame(
      Series = c("W-CF","E-CF"),
      Winner = c(cf[[1]]$winner, cf[[2]]$winner),
      stringsAsFactors = FALSE
    ),
    finals = data.frame(
      Series = "Stanley Cup Final",
      Winner = fin$winner,
      stringsAsFactors = FALSE
    )
  )
  lengths <- c(
    setNames(sapply(r1w,`[[`,"length"), paste0("W-R1-",1:4)),
    setNames(sapply(r1e,`[[`,"length"), paste0("E-R1-",1:4)),
    setNames(sapply(r2w,`[[`,"length"), paste0("W-R2-",1:2)),
    setNames(sapply(r2e,`[[`,"length"), paste0("E-R2-",1:2)),
    `W-CF`=cf[[1]]$length, `E-CF`=cf[[2]]$length,
    `Stanley Cup Final`=fin$length
  )
  list(rounds=rounds, lengths=lengths)
}

simulate_n_times <- function(n=2000){
  keys <- c(
    paste0("W-R1-",1:4), paste0("E-R1-",1:4),
    paste0("W-R2-",1:2), paste0("E-R2-",1:2),
    "W-CF","E-CF","Stanley Cup Final"
  )
  winL <- setNames(vector("list",length(keys)), keys)
  lenL <- setNames(vector("list",length(keys)), keys)
  for(i in seq_len(n)){
    sim <- simulate_bracket_once()
    for(k in keys){
      df <- do.call(rbind, sim$rounds)
      w  <- df$Winner[df$Series==k]
      winL[[k]] <- c(winL[[k]], w)
      lenL[[k]] <- c(lenL[[k]], sim$lengths[k])
    }
  }
  cond <- lapply(keys, function(k){
    teams <- unique(winL[[k]])
    df <- data.frame(Team=teams, stringsAsFactors=FALSE)
    df$Prob     <- vapply(teams, function(t) mean(winL[[k]]==t), 0)
    df$ExpGames <- vapply(teams, function(t) mean(lenL[[k]][winL[[k]]==t]),0)
    df[order(-df$Prob),]
  })
  names(cond) <- keys
  cup <- cond[["Stanley Cup Final"]]
  cup$Lower <- vapply(cup$Prob, function(p) qbeta(0.025,p*n+1,(1-p)*n+1),0)
  cup$Upper <- vapply(cup$Prob, function(p) qbeta(0.975,p*n+1,(1-p)*n+1),0)
  list(cond=cond, cup=cup)
}



###################
# app.R

# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
# later is brought in by shiny; if you need to load it explicitly:
# library(later)

# —————————————————————————————————————————————————————
# GLOBAL: load your data & helper functions here, e.g.:
#   ratings <- ...
#   simulate_bracket_once <- function(...) { ... }
#   (you already have these from your previous code)
# —————————————————————————————————————————————————————

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .series-logo {
        height: 50px !important;
        width: 50px !important;
        object-fit: contain;
        margin-bottom: 4px;
      }
    "))
  ),
  titlePanel("NHL Playoff Simulator"),
  sidebarLayout(
    sidebarPanel(
      numericInput("nsims","Monte Carlo sims:",2000,min=100,step=100),
      checkboxInput("liveOdds","Show live Stanley Cup odds (slower)", TRUE),
      # inside sidebarPanel() in ui:
      #numericInput("wins1", "Current wins for Team 1:", 0, min=0, max=3),
      #numericInput("wins2", "Current wins for Team 2:", 0, min=0, max=3),
      actionButton("runSim","Run Monte Carlo"),
      br(), br(),
      actionButton("runOne","Run Single Bracket")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Live Odds",
                 h3("Stanley Cup Odds"),
                 conditionalPanel(
                   condition = "!output.finished && input.liveOdds",
                   plotOutput("progressPlot", height="350px")
                 ),
                 conditionalPanel(
                   condition = "output.finished || !input.liveOdds",
                   DT::dataTableOutput("cupTable"),
                   plotOutput("cupPlot", height="350px")
                 )
        ),
        tabPanel("Single Bracket",
                 h3("Single Bracket Results"),
                 fluidRow(
                   column(3, h4("Round 1"), uiOutput("r1")),
                   column(3, h4("Round 2"), uiOutput("r2")),
                   column(3, h4("Conf Finals"), uiOutput("r3")),
                   column(3, h4("Final"),      uiOutput("r4"))
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive storage
  partialCup <- reactiveVal(NULL)       # incremental Stanley Cup odds
  condRV     <- reactiveVal(NULL)       # full series-by-series cond list
  mcDone     <- reactiveVal(FALSE)      # has MC finished?
  simCount   <- reactiveVal(0)          # how many sims done
  
  # LOCAL helper to render a series box (uses condRV for the probabilities)
  render_box <- function(df, cond_list) {
    lapply(seq_len(nrow(df)), function(i) {
      s <- df$Series[i]
      w <- df$Winner[i]
      stats <- cond_list[[s]]
      prob <- round(100 * stats$Prob[stats$Team==w], 1)
      eg   <- round(  stats$ExpGames[stats$Team==w],  2)
      key  <- w %>%
        tolower() %>%
        gsub("[^a-z0-9 ]","",.) %>%
        gsub(" +","_",.)
      div(class="series-box",
          img(src=paste0("logos/", key, ".png"), class="series-logo"),
          strong(w), br(),
          paste0("Win%: ",prob,"%"), br(),
          paste0("E[G]: ",eg)
      )
    })
  }
  
  # —————————————— Single‑Bracket Tab ——————————————
  one <- eventReactive(input$runOne, simulate_bracket_once())
  output$r1 <- renderUI({
    req(one(), condRV())
    render_box(one()$rounds$round1, condRV())
  })
  output$r2 <- renderUI({
    req(one(), condRV())
    render_box(one()$rounds$round2, condRV())
  })
  output$r3 <- renderUI({
    req(one(), condRV())
    render_box(one()$rounds$round3, condRV())
  })
  output$r4 <- renderUI({
    req(one(), condRV())
    render_box(one()$rounds$finals, condRV())
  })
  # —————————————— Monte‐Carlo Live Odds ——————————————
  observeEvent(input$runSim, {
    n    <- input$nsims
    live <- isTRUE(input$liveOdds)
    keys <- c(
      paste0("W-R1-",1:4), paste0("E-R1-",1:4),
      paste0("W-R2-",1:2), paste0("E-R2-",1:2),
      "W-CF","E-CF","Stanley Cup Final"
    )
    
    # init accumulators
    winL <- setNames(vector("list", length(keys)), keys)
    lenL <- setNames(vector("list", length(keys)), keys)
    partialCup(NULL); condRV(NULL); mcDone(FALSE); simCount(0)
    
    progress <- Progress$new(session, min=0, max=n)
    progress$set(message="Simulating brackets…", value=0)
    
    if (live) {
      # live = TRUE → schedule one‐by‐one with later()
      stepSim <- function(idx) {
        sim <- simulate_bracket_once()
        for(k in keys) {
          df  <- do.call(rbind, sim$rounds)
          w   <- df$Winner[df$Series==k]
          winL[[k]] <<- c(winL[[k]], w)
          lenL[[k]] <<- c(lenL[[k]], sim$lengths[k])
        }
        # update final odds every 10 sims
        if (idx %% 10 == 0 || idx == n) {
          finalWins <- winL[["Stanley Cup Final"]]
          teams     <- unique(finalWins)
          df0       <- data.frame(Team=teams, stringsAsFactors=FALSE)
          df0$Prob  <- vapply(teams,
                              function(t) mean(finalWins==t),
                              numeric(1))
          df0$Lower <- vapply(df0$Prob,
                              function(p) qbeta(0.025, p*idx+1, (1-p)*idx+1),
                              numeric(1))
          df0$Upper <- vapply(df0$Prob,
                              function(p) qbeta(0.975, p*idx+1, (1-p)*idx+1),
                              numeric(1))
          partialCup(df0)
        }
        simCount(idx)
        progress$set(value=idx)
        if (idx < n) {
          later::later(~stepSim(idx+1), 0)
        } else {
          mcDone(TRUE)
          progress$close()
          # build the full series‐by‐series cond list at the end
          condList <- lapply(keys, function(k){
            teams <- unique(winL[[k]])
            df <- data.frame(Team=teams, stringsAsFactors=FALSE)
            df$Prob     <- vapply(teams, function(t) mean(winL[[k]]==t), 0)
            df$ExpGames <- vapply(teams,
                                  function(t) mean(lenL[[k]][winL[[k]]==t]),
                                  0)
            df[order(-df$Prob),]
          })
          names(condList) <- keys
          condRV(condList)
        }
      }
      stepSim(1)
    } else {
      # live = FALSE → plain for‐loop
      for (i in seq_len(n)) {
        sim <- simulate_bracket_once()
        for(k in keys) {
          df  <- do.call(rbind, sim$rounds)
          w   <- df$Winner[df$Series==k]
          winL[[k]] <- c(winL[[k]], w)
          lenL[[k]] <- c(lenL[[k]], sim$lengths[k])
        }
        simCount(i)
        progress$inc(1)
      }
      progress$close()
      
      # final Stanley Cup odds
      finalWins <- winL[["Stanley Cup Final"]]
      teams     <- unique(finalWins)
      df0       <- data.frame(Team=teams, stringsAsFactors=FALSE)
      df0$Prob  <- vapply(teams,
                          function(t) mean(finalWins==t),
                          numeric(1))
      df0$Lower <- vapply(df0$Prob,
                          function(p) qbeta(0.025, p*n+1, (1-p)*n+1),
                          numeric(1))
      df0$Upper <- vapply(df0$Prob,
                          function(p) qbeta(0.975, p*n+1, (1-p)*n+1),
                          numeric(1))
      partialCup(df0)
      simCount(n)
      mcDone(TRUE)
      
      # build full series‐by‐series cond list
      condList <- lapply(keys, function(k){
        teams <- unique(winL[[k]])
        df <- data.frame(Team=teams, stringsAsFactors=FALSE)
        df$Prob     <- vapply(teams, function(t) mean(winL[[k]]==t), 0)
        df$ExpGames <- vapply(teams,
                              function(t) mean(lenL[[k]][winL[[k]]==t]),
                              0)
        df[order(-df$Prob),]
      })
      names(condList) <- keys
      condRV(condList)
    }
  })
  
  # Expose the finished flag to JS for conditionalPanel()
  output$finished <- reactive(mcDone())
  outputOptions(output, "finished", suspendWhenHidden=FALSE)
  
  # ————————————— Live Odds Outputs —————————————
  output$progressPlot <- renderPlot({
    req(partialCup(), input$liveOdds)
    df <- partialCup()
    ggplot(df, aes(x = reorder(Team, Prob), y = Prob, fill = Team)) +
      geom_col() +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
      scale_y_continuous(labels = percent) +
      coord_flip() +
      scale_fill_manual(values = team_colors) +
      labs(
        x = "", y = "Win Probability",
        title = sprintf("Live Stanley Cup Odds (%d / %d sims)", simCount(), input$nsims)
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })
  
  output$cupTable <- DT::renderDataTable({
    req(partialCup())
    
    tryCatch({
      df <- partialCup()
      
      # Ensure required columns exist and are numeric
      stopifnot(all(c("Team", "Prob", "Lower", "Upper") %in% names(df)))
      df <- df[order(-df$Prob), ]  # sort by true numeric Prob descending
      
      DT::datatable(
        df[, c("Team", "Prob", "Lower", "Upper")],
        rownames = FALSE,
        options = list(
          pageLength = 16,
          order = list(list(1, 'desc'))  # default sort by Prob column
        )
      ) %>%
        DT::formatPercentage(c("Prob", "Lower", "Upper"), digits = 1)
    }, error = function(e) {
      cat("Error in cupTable:\n", conditionMessage(e), "\n")
      DT::datatable(data.frame(
        Team = "Error", Prob = NA, Lower = NA, Upper = NA
      ))
    })
  })
  
  
  output$cupPlot <- renderPlot({
    req(partialCup())
    df <- partialCup()
    ggplot(df, aes(x = reorder(Team, Prob), y = Prob, fill = Team)) +
      geom_col() +
      geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
      scale_y_continuous(labels = percent) +
      coord_flip() +
      scale_fill_manual(values = team_colors) +
      labs(x = "", y = "Win Probability", title = "Final Stanley Cup Odds (95% CI)") +
      theme_minimal() +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)
#rsconnect::deployApp()


