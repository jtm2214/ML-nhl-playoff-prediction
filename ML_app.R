############################################## app.R
#setwd("~/Documents/NHLPROJECT")
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)


# --- 0. Data & Elo Setup ---
team_colors <- c(
  "Winnipeg Jets" = "#004C97",
  "St. Louis Blues" = "#002F87",
  "Dallas Stars" = "#006847",
  "Colorado Avalanche" = "#6F263D",
  "Vegas Golden Knights" = "#B4975A",
  "Minnesota Wild" = "#154734",
  "Los Angeles Kings" = "#A2AAAD",
  "Edmonton Oilers" = "#041E42",
  "Toronto Maple Leafs" = "#00205B",
  "Ottawa Senators" = "#C8102E",
  "Tampa Bay Lightning" = "#002868",
  "Florida Panthers" = "#041E42",
  "Washington Capitals" = "#041E42",
  "Montreal Canadiens" = "#AF1E2D",
  "Carolina Hurricanes" = "#CC0000",
  "New Jersey Devils" = "#CE1126"
)

# insert in elo ranking_data 

# 3) Clean up any naming mismatches:

# 4) Merge all three:
# 3) Build master_df from the 16 playoff teams only (so Arizona and any others never show up)
playoff_teams <- c(
  "Winnipeg Jets","St. Louis Blues","Dallas Stars","Colorado Avalanche",
  "Vegas Golden Knights","Minnesota Wild","Los Angeles Kings","Edmonton Oilers",
  "Toronto Maple Leafs","Ottawa Senators","Tampa Bay Lightning","Florida Panthers",
  "Washington Capitals","Montreal Canadiens","Carolina Hurricanes","New Jersey Devils"
)

master_df <- teams_data %>%
  filter(Team %in% playoff_teams) %>%                    # only keep the 16
  left_join(standings_data, by="Team") %>% 
  left_join(goalie_data,   by="Team")

# 4) Compute Goalie_Elo from goalie_data (which now has exactly one row per playoff team)
goalie_ratings <- goalie_data %>%
  filter(Team %in% playoff_teams) %>%
  mutate(
    z_sv    = (SV_pct - mean(SV_pct)) / sd(SV_pct),
    z_gag   = (GA_G  - mean(GA_G )) / sd(GA_G ) * -1,
    z_sopct = (SO_pct- mean(SO_pct)) / sd(SO_pct)
  ) %>%
  mutate(Goalie_Elo = 1500 + 100*(1.2*z_sv + 0.8*z_gag + 0.6*z_sopct)) %>%
  select(Team, Goalie_Elo)

# 5) Merge that single Goalie_Elo column back in
master_df <- master_df %>%
  left_join(goalie_ratings, by="Team")

# --- 0) Preliminaries: make sure you have `master_df` loaded ---
# master_df must contain at least:
#   Team, Current_Elo, Goalie_Elo, Home_Adv, PP_pct, PK_pct, SV_pct


library(dplyr)
library(tidyr)
library(stringr)




# 1) Read in your expanded standings exactly as before:
# stand_raw <- read.csv(text = standings_text, stringsAsFactors = FALSE, check.names = FALSE)

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

# simulate_series <- function(t1, t2) {
#   # home schedule pattern for 2‑2‑1‑1‑1
#   home <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
#   w1 <- w2 <- 0
#   g   <- 1L
#   while(w1 < 4L && w2 < 4L && g <= 7L) {
#     p1 <- multi_win_prob(t1, t2, is_home1 = home[g])
#     if(runif(1) < p1) w1 <- w1 + 1L else w2 <- w2 + 1L
#     g <- g + 1L
#   }
#   winner <- if(w1==4L) t1 else t2
#   length <- g - 1L
#   list(winner=winner, length=length)
# }
simulate_series <- function(t1, t2) {
  home_schedule <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  w1 <- w2 <- 0L
  g  <- 1L
  while (w1 < 4L && w2 < 4L) {
    p1 <- predict_prob(t1, t2, home_schedule[g], input$model)
    if (runif(1) < p1) w1 <- w1 + 1L else w2 <- w2 + 1L
    g <- g + 1L
    if (g > 7) break
  }
  winner <- if (w1==4L) t1 else t2
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

simulate_n_times <- function(n=100){
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
# ──────────────────────────────────────────────────────────────────────────────
# 0) PACKAGES & GLOBAL SETUP
# ──────────────────────────────────────────────────────────────────────────────
# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(h2o)

# ──────────────── GLOBAL SETUP ──────────────────

# 1) Read in your 16-team CSV (3-letter codes)
teams16_raw <- read.csv("teams (16).csv",
  stringsAsFactors = FALSE
)

# 2) Keep only the “Team Level / all” row per team code
teams16 <- teams16_raw %>%
  filter(position=="Team Level", situation=="all") %>%
  distinct(team, .keep_all=TRUE)

# 3) Map code ↔ full NHL name (update if needed)
team_map <- tribble(
  ~code, ~full,
  "WPG","Winnipeg Jets",
  "STL","St. Louis Blues",
  "DAL","Dallas Stars",
  "COL","Colorado Avalanche",
  "VGK","Vegas Golden Knights",
  "MIN","Minnesota Wild",
  "LAK","Los Angeles Kings",
  "EDM","Edmonton Oilers",
  "TOR","Toronto Maple Leafs",
  "OTT","Ottawa Senators",
  "TBL","Tampa Bay Lightning",
  "FLA","Florida Panthers",
  "WSH","Washington Capitals",
  "MTL","Montreal Canadiens",
  "CAR","Carolina Hurricanes",
  "NJD","New Jersey Devils"
)

teams16 <- teams16 %>%
  left_join(team_map, by = c("team"="code"))

# 4) Which numeric columns did you train on?
feat_cols <- teams16 %>%
  select(where(is.numeric)) %>%
  names()

# 5) Helpers to look up codes or full names interchangeably
lookup_code <- function(x) {
  if (x %in% teams16$team) return(x)
  if (x %in% team_map$full)
    return(team_map$code[match(x,team_map$full)])
  stop("Unknown team: ", x)
}

# 6) Build the one-row feature frame for any matchup
create_game_features <- function(team1, team2, is_home1 = FALSE) {
  c1 <- lookup_code(team1)
  c2 <- lookup_code(team2)
  r1 <- teams16[teams16$team == c1, ]
  r2 <- teams16[teams16$team == c2, ]
  diffs <- r1[feat_cols] - r2[feat_cols]
  colnames(diffs) <- paste0(feat_cols, "_diff")
  df <- as.data.frame(diffs, stringsAsFactors = FALSE)
  # *** add the home‐ice flag that your AutoML run almost certainly used ***
  df$is_home1 <- as.integer(is_home1)
  df
}

# 7) Probability wrapper for any model type
predict_prob <- function(model, team1, team2, is_home1 = FALSE) {
  if (is.null(model)) {
    stop("Tried to predict with a NULL model")
  }
  newx <- create_game_features(team1, team2, is_home1)
  if      (inherits(model,"train")) {
    return(predict(model, newx, type = "prob")[, "Win"])
  } else if (inherits(model, "glm")) {
    return(predict(model, newx, type = "response"))
  } else if (inherits(model, "H2OBinomialModel")) {
    # convert to an H2OFrame with the same column names your model expects
    hf <- as.h2o(newx, destination_frame = "newx_for_scoring")
    ph <- h2o.predict(model, hf)
    # ph has columns: "predict","p0","p1" – we want the probability of class 1
    return(as.vector(ph[ , "p1"]))
  } else if (inherits(model, "svm")) {
    preds <- predict(model, newx, probability = TRUE)
    probs <- attr(preds, "probabilities")
    return(probs[, "Win"])
  }
  else stop("Unsupported model class: ", class(model)[1])
}

# app.R
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(DT)
library(h2o)

# ──────────────────────────────────────────────────────────────────────────────
# 0) GLOBAL SETUP: 16-team features + load models + accuracy map
# ──────────────────────────────────────────────────────────────────────────────

teams16_raw <- read.csv("teams (16).csv", stringsAsFactors=FALSE)
teams16 <- teams16_raw %>%
  filter(position=="Team Level", situation=="all") %>%
  distinct(team, .keep_all=TRUE)

team_map <- tribble(
  ~code, ~full,
  "WPG","Winnipeg Jets",    "STL","St. Louis Blues",
  "DAL","Dallas Stars",     "COL","Colorado Avalanche",
  "VGK","Vegas Golden Knights","MIN","Minnesota Wild",
  "LAK","Los Angeles Kings","EDM","Edmonton Oilers",
  "TOR","Toronto Maple Leafs","OTT","Ottawa Senators",
  "TBL","Tampa Bay Lightning","FLA","Florida Panthers",
  "WSH","Washington Capitals","MTL","Montreal Canadiens",
  "CAR","Carolina Hurricanes","NJD","New Jersey Devils"
)

teams16 <- left_join(teams16, team_map, by=c("team"="code"))
feat_cols <- teams16 %>% select(where(is.numeric)) %>% names()

lookup_code <- function(x){
  if (x %in% teams16$team) return(x)
  if (x %in% team_map$full) return(team_map$code[match(x,team_map$full)])
  stop("Unknown team: ", x)
}
create_game_features <- function(t1,t2,is_home1=FALSE){
  c1 <- lookup_code(t1); c2 <- lookup_code(t2)
  r1 <- teams16[teams16$team==c1, feat_cols]
  r2 <- teams16[teams16$team==c2, feat_cols]
  diffs <- as.data.frame(r1 - r2)
  names(diffs) <- paste0(feat_cols,"_diff")
  # if your models used a home flag, uncomment:
  # diffs$is_home1 <- as.integer(is_home1)
  diffs
}

# 0.2) load your 5 predictive models
logit_model <- readRDS("logit_model.rds")
svm_model   <- readRDS("svm_model.rds")
gbm_model   <- readRDS("gbm_model.rds")
xgb_model   <- readRDS("xgb_model.rds")
h2o.init(max_mem_size="2G"); h2o.no_progress()
best_h2o    <- h2o.loadModel("models/GLM_1_AutoML_8_20250508_173845")

# — updated H2O branch pulls the "Win" column instead of "p1" —
predict_prob <- function(model, t1, t2, is_home1=FALSE){
  newx <- create_game_features(t1,t2,is_home1)
  if      (inherits(model,"train"))           predict(model,newx,type="prob")[, "Win"]
  else if (inherits(model,"glm"))             predict(model,newx,type="response")
  else if (inherits(model,"H2OBinomialModel")){
    ph <- h2o.predict(model, as.h2o(newx))
    # your H2O binomial models return columns: "predict", "Loss", "Win"
    as.vector(ph[,"Win"])
  }
  else if (inherits(model,"svm")) {
    preds <- predict(model,newx, probability=TRUE)
    attr(preds,"probabilities")[,"Win"]
  }
  else stop("Unsupported model class: ", class(model)[1])
}

models <- list(
  "Logistic Regression" = logit_model,
  "SVM"                 = svm_model,
  "GBM"                 = gbm_model,
  "XGBoost"             = xgb_model,
  "H2O AutoML"          = best_h2o
)

# 0.3) model accuracies (from your summary)
accuracy_map <- c(
  "Logistic Regression" = 0.495,
  "SVM"                 = 0.532,
  "GBM"                 = 0.539,
  "XGBoost"             = 0.502,
  "H2O AutoML"          = 0.522
)

# ──────────────────────────────────────────────────────────────────────────────
# 1) DP‐based bracket (exact) and CI from accuracy
# ──────────────────────────────────────────────────────────────────────────────

round1_pairs <- list(
  "W-R1-1"=c("Winnipeg Jets","St. Louis Blues"),
  "W-R1-2"=c("Dallas Stars","Colorado Avalanche"),
  "W-R1-3"=c("Vegas Golden Knights","Minnesota Wild"),
  "W-R1-4"=c("Los Angeles Kings","Edmonton Oilers"),
  "E-R1-1"=c("Toronto Maple Leafs","Ottawa Senators"),
  "E-R1-2"=c("Tampa Bay Lightning","Florida Panthers"),
  "E-R1-3"=c("Washington Capitals","Montreal Canadiens"),
  "E-R1-4"=c("Carolina Hurricanes","New Jersey Devils")
)
next_map <- list(
  "W-R2-1"=c("W-R1-1","W-R1-2"), "W-R2-2"=c("W-R1-3","W-R1-4"),
  "E-R2-1"=c("E-R1-1","E-R1-2"), "E-R2-2"=c("E-R1-3","E-R1-4"),
  "W-CF"  =c("W-R2-1","W-R2-2"), "E-CF"  =c("E-R2-1","E-R2-2"),
  "Final" =c("W-CF","E-CF")
)
home_pat <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE,TRUE)

series_dp <- function(p1){
  f <- matrix(0,5,5); f[1,1]<-1
  P1<-P2<-EL<-0
  for(g in 1:7){
    newf <- matrix(0,5,5)
    for(a in 0:3) for(b in 0:3){
      pr <- f[a+1,b+1]; if(pr==0) next
      if(a+1==4){ P1<-P1+pr*p1[g]; EL<-EL+pr*p1[g]*g }
      else          newf[a+2,b+1] <- newf[a+2,b+1] + pr*p1[g]
      if(b+1==4){ P2<-P2+pr*(1-p1[g]); EL<-EL+pr*(1-p1[g])*g }
      else          newf[a+1,b+2] <- newf[a+1,b+2] + pr*(1-p1[g])
    }
    f <- newf
  }
  list(P1=P1, P2=P2, E_len=EL/(P1+P2))
}

bracket_dp <- function(model){
  cond <- list()
  # Round 1
  for(code in names(round1_pairs)){
    tm <- round1_pairs[[code]]
    p1 <- sapply(home_pat, function(h)
      predict_prob(model, tm[1], tm[2], h))
    dp <- series_dp(p1)
    cond[[code]] <- tibble(
      Series   = code,
      Team     = tm,
      Prob     = c(dp$P1, dp$P2),
      ExpGames = dp$E_len
    )
  }
  # Rounds 2+, Final
  for(code in names(next_map)){
    src <- next_map[[code]]
    df1 <- cond[[src[1]]]; df2 <- cond[[src[2]]]
    teams <- unique(c(df1$Team, df2$Team))
    Pwin  <- setNames(numeric(length(teams)), teams)
    Eacc  <- Pwin
    for(i in seq_len(nrow(df1))) for(j in seq_len(nrow(df2))){
      t1 <- df1$Team[i]; t2 <- df2$Team[j]
      meet_pr <- df1$Prob[i]*df2$Prob[j]
      p1 <- sapply(home_pat, function(h)
        predict_prob(model, t1, t2, h))
      dp <- series_dp(p1)
      Pwin[t1] <- Pwin[t1] + meet_pr*dp$P1
      Pwin[t2] <- Pwin[t2] + meet_pr*dp$P2
      Eacc[t1] <- Eacc[t1] + meet_pr*dp$P1*dp$E_len
      Eacc[t2] <- Eacc[t2] + meet_pr*dp$P2*dp$E_len
    }
    cond[[code]] <- tibble(
      Series   = code,
      Team     = teams,
      Prob     = as.numeric(Pwin),
      ExpGames = as.numeric(Eacc/Pwin)
    )
  }
  cup <- cond[["Final"]] %>% select(Team, Prob)
  list(cond=cond, cup=cup)
}

# ──────────────────────────────────────────────────────────────────────────────
# 2) Single-Bracket Monte-Carlo (unchanged)
# ──────────────────────────────────────────────────────────────────────────────

simulate_series_mc <- function(model,t1,t2){
  w1<-w2<-0; g<-1L
  while(w1<4 && w2<4){
    p1 <- predict_prob(model, t1,t2, home_pat[g])
    if(runif(1)<p1) w1<-w1+1L else w2<-w2+1L
    g<-g+1L
  }
  list(winner=if(w1==4)t1 else t2, length=g-1L)
}
simulate_bracket_mc <- function(model){
  r1w <- lapply(round1_pairs[1:4],  function(p) simulate_series_mc(model,p[1],p[2]))
  r1e <- lapply(round1_pairs[5:8],  function(p) simulate_series_mc(model,p[1],p[2]))
  r2w <- list(
    simulate_series_mc(model, r1w[[1]]$winner, r1w[[2]]$winner),
    simulate_series_mc(model, r1w[[3]]$winner, r1w[[4]]$winner)
  )
  r2e <- list(
    simulate_series_mc(model, r1e[[1]]$winner, r1e[[2]]$winner),
    simulate_series_mc(model, r1e[[3]]$winner, r1e[[4]]$winner)
  )
  cf  <- list(
    simulate_series_mc(model, r2w[[1]]$winner, r2w[[2]]$winner),
    simulate_series_mc(model, r2e[[1]]$winner, r2e[[2]]$winner)
  )
  fin <- simulate_series_mc(model, cf[[1]]$winner, cf[[2]]$winner)
  list(
    round1 = data.frame(
      Series = names(round1_pairs),
      Winner = c(sapply(r1w,`[[`,"winner"), sapply(r1e,`[[`,"winner"))
    ),
    round2 = data.frame(
      Series = c("W-R2-1","W-R2-2","E-R2-1","E-R2-2"),
      Winner = c(r2w[[1]]$winner, r2w[[2]]$winner,
                 r2e[[1]]$winner, r2e[[2]]$winner)
    ),
    round3 = data.frame(
      Series = c("W-CF","E-CF"),
      Winner = c(cf[[1]]$winner, cf[[2]]$winner)
    ),
    finals = data.frame(
      Series = "Final",
      Winner = fin$winner
    )
  )
}

# ──────────────────────────────────────────────────────────────────────────────
# 3) UI
# ──────────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("NHL Playoff Simulator"),
  sidebarLayout(
    sidebarPanel(
      selectInput("model","Choose model:", names(models)),
      actionButton("calcOdds","Calculate Odds"),
      br(), br(),
      actionButton("runOne","Run Single Bracket")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Odds",
                 DTOutput("cupTable"),
                 plotOutput("cupPlot", height="400px")
        ),
        tabPanel("Single Bracket",
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

# ──────────────────────────────────────────────────────────────────────────────
# 4) SERVER
# ──────────────────────────────────────────────────────────────────────────────

server <- function(input, output, session){
  # DP + CI from accuracy
  odds_dp <- eventReactive(input$calcOdds, {
    dp   <- bracket_dp(models[[input$model]])
    acc  <- accuracy_map[[input$model]]
    half_err <- (1 - acc)/2
    cup <- dp$cup %>%
      mutate(
        Lower = pmax(0, Prob - half_err),
        Upper = pmin(1, Prob + half_err)
      )
    list(cond = dp$cond, cup = cup)
  })
  
  # single bracket
  oneB <- eventReactive(input$runOne, {
    simulate_bracket_mc(models[[input$model]])
  })
  
  # DT + plot
  output$cupTable <- renderDT({
    df <- odds_dp()$cup; req(df)
    datatable(df, rownames=FALSE,
              options=list(pageLength=16, order=list(list(2,"desc")))) %>%
      formatPercentage(c("Prob","Lower","Upper"), digits=1)
  })
  output$cupPlot <- renderPlot({
    df <- odds_dp()$cup; req(df)
    ggplot(df, aes(x=reorder(Team,Prob), y=Prob)) +
      geom_col(fill="steelblue") +
      geom_errorbar(aes(ymin=Lower,ymax=Upper), width=0.2) +
      scale_y_continuous(labels=percent) +
      coord_flip() + theme_minimal()
  })
  
  # single-bracket boxes
  render_box <- function(df){
    cond <- odds_dp()$cond
    lapply(seq_len(nrow(df)), function(i){
      s <- df$Series[i]; w <- df$Winner[i]
      st <- cond[[s]]
      prob <- round(100*st$Prob[st$Team==w],1)
      eg   <- round(    st$ExpGames[st$Team==w],2)
      div(class="series-box",
          tags$b(w), br(),
          paste0("Win%: ",prob,"%"), br(),
          paste0("E[G]: ",eg)
      )
    })
  }
  output$r1 <- renderUI({ req(oneB(), odds_dp()); render_box(oneB()$round1) })
  output$r2 <- renderUI({ req(oneB(), odds_dp()); render_box(oneB()$round2) })
  output$r3 <- renderUI({ req(oneB(), odds_dp()); render_box(oneB()$round3) })
  output$r4 <- renderUI({ req(oneB(), odds_dp()); render_box(oneB()$finals) })
}

shinyApp(ui, server)





