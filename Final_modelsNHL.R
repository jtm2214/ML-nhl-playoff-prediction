### models_training.R (final revised with evaluation and visualization)
#–– 0) Dependencies & Setup ––#
install_if_missing <- function(pkgs) {
  for(pk in pkgs) if(!requireNamespace(pk, quietly=TRUE)) install.packages(pk)
}
install_if_missing(c(
  "dplyr","purrr","lubridate","caret","e1071","gbm","pROC",
  "xgboost","h2o","jsonlite","ggplot2","reshape2","knitr","kableExtra"
))
library(dplyr); library(stringr); library(purrr); library(lubridate); library(tidyverse); library(tidyr)
library(caret); library(e1071); library(gbm); library(pROC)
library(xgboost); library(h2o); library(jsonlite)
library(ggplot2); library(reshape2); library(knitr); library(kableExtra)

set.seed(2025)
#setwd("")


playoff_teams <- c(
  "Winnipeg Jets","St. Louis Blues","Dallas Stars","Colorado Avalanche",
  "Vegas Golden Knights","Minnesota Wild","Los Angeles Kings","Edmonton Oilers",
  "Toronto Maple Leafs","Ottawa Senators","Tampa Bay Lightning","Florida Panthers",
  "Washington Capitals","Montreal Canadiens","Carolina Hurricanes","New Jersey Devils"
)

#games_raw <- games_raw %>%
#  filter(team1 %in% playoff_teams, team2 %in% playoff_teams)

# get all_teams <- read.csv("all_teams.csv", stringsAsFactors = FALSE)
#teams <- read.csv("teams.csv", stringsAsFactors = FALSE)

hist_playoff <- all_teams %>%
  filter(playoffGame == 1, situation == "all") %>%
  mutate(gameDate = as.Date(as.character(gameDate), "%Y%m%d"))

# 2) Auto‐detect the numeric metrics
id_cols     <- c("team","season","name","gameId","playerTeam",
                 "opposingTeam","home_or_away","position",
                 "situation","playoffGame")
metric_cols <- hist_playoff %>%
  select(-all_of(id_cols), -gameDate) %>%
  select(where(is.numeric)) %>%
  names()

# 3a) Build the AWAY side
away_df <- hist_playoff %>%
  filter(home_or_away=="AWAY") %>%
  rename(team1 = playerTeam) %>%
  rename_with(~ paste0(.x, "_1"), all_of(metric_cols)) %>%
  select(gameId, gameDate, team1, ends_with("_1"))

# 3b) Build the HOME side
home_df <- hist_playoff %>%
  filter(home_or_away=="HOME") %>%
  rename(team2 = playerTeam) %>%
  rename_with(~ paste0(.x, "_2"), all_of(metric_cols)) %>%
  select(gameId, team2, ends_with("_2"))

# 4) Join, compute win1, then loop through every “*_1” column and create its “*_diff”
train_df <- away_df %>%
  inner_join(home_df, by="gameId") %>%     # only games with both sides
  mutate(
    win1 = as.integer(goalsFor_1 > goalsFor_2)
  ) %>%
  { 
    df <- .
    one_cols <- grep("_1$", names(df), value=TRUE)
    for(col in one_cols) {
      base <- sub("_1$", "", col)
      df[[paste0(base, "_diff")]] <- df[[col]] - df[[paste0(base, "_2")]]
    }
    df
  } %>%
  select(gameId, gameDate, team1, team2, win1, ends_with("_diff")) %>%
  # drop any games where we still have missing values
  filter(!is.na(win1))


# 3) Train/Test split on your historical playoff-game frame (train_df from above)
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# createDataPartition stratifies on win1
set.seed(2025)
idx       <- createDataPartition(train_df$win1, p = 0.8, list = FALSE)
train_set <- train_df[idx, ]
test_set  <- train_df[-idx, ]

# AFTER your train/test split, do exactly this:
train_set$win1 <- factor(train_set$win1, levels = c(0,1), labels = c("Loss","Win"))
test_set$win1  <- factor(test_set$win1,  levels = c(0,1), labels = c("Loss","Win"))

# drop identifiers
drop_cols  <- c("gameId","gameDate","team1","team2","win1")
predictors <- setdiff(names(train_set), drop_cols)


########### EDA
library(ggplot2)
library(dplyr)

# Ensure win1 is a factor (if not already)
test_set$win1 <- factor(test_set$win1, levels = c("Loss", "Win"))

# 1. Boxplot: xGoalsPercentage_diff by outcome
ggplot(test_set, aes(x = win1, y = xGoalsPercentage_diff, fill = win1)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "xGoals Percentage Differential by Outcome",
       x = "Outcome (Team 1)", y = "xGoalsPercentage_diff") +
  theme_minimal()

# 2. Boxplot: corsiPercentage_diff by outcome
ggplot(test_set, aes(x = win1, y = highDangerGoalsFor_diff, fill = win1)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "High-Danger Goals Differential by Outcome",
    x = "Outcome (Team 1)", y = "highDangerGoalsFor_diff"
  ) +
  theme_minimal()

# 3. Histogram: goalsFor_diff (team1 - team2), colored by outcome
ggplot(test_set, aes(x = goalsFor_diff, fill = win1)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(title = "Histogram of Goal Differential by Outcome",
       x = "goalsFor_diff (Team 1 - Team 2)", y = "Count") +
  theme_minimal()


# Check for missing values
missing_summary <- sapply(train_df, function(x) sum(is.na(x)))
missing_summary[missing_summary > 0]

# Summary of outcome variable
table(train_df$win1)

# Check goal differentials
summary(train_df$goalsFor_1 - train_df$goalsFor_2)

# Sanity check: goalsFor should be non-negative
summary(train_df$goalsFor_1)
summary(train_df$goalsFor_2)

# Distribution of a few advanced stats
library(ggplot2)
ggplot(train_df, aes(x = xGoalsPercentage_diff)) +
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  labs(title = "Distribution of xGoalsPercentage Differential",
       x = "xGoalsPercentage_diff", y = "Count") +
  theme_minimal()

# Win rate breakdown (check balance)
win_rate <- prop.table(table(train_df$win1))
win_rate

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 4) Train Models on all “_diff” predictors
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

# 4a) Logistic Regression
logit_model <- glm(
  formula = as.formula(paste("win1 ~", paste(predictors, collapse = " + "))),
  data    = train_set,
  family  = binomial
)
saveRDS(logit_model, "logit_model.rds")

# 4b) SVM
svm_model <- svm(
  formula     = as.formula(paste("win1 ~", paste(predictors, collapse = " + "))),
  data        = train_set,
  type        = 'C-classification',
  kernel      = 'radial',
  probability = TRUE
)
saveRDS(svm_model, "svm_model.rds")

# 4c) GBM (caret)
ctrl <- trainControl(
  method         = 'repeatedcv',
  number         = 5,
  repeats        = 2,
  classProbs     = TRUE, 
  summaryFunction= twoClassSummary
)

gbm_grid <- expand.grid(
  n.trees           = c(100,300),
  interaction.depth = c(3,5),
  shrinkage         = c(0.01,0.1),
  n.minobsinnode    = 10
)

gbm_model <- train(
  win1 ~ .,
  data      = train_set %>% select(all_of(predictors), win1),
  method    = 'gbm',
  metric    = 'ROC',
  trControl = ctrl,
  tuneGrid  = gbm_grid,
  verbose   = FALSE
)
saveRDS(gbm_model, "gbm_model.rds")

# 4d) XGBoost (caret)
xgb_grid <- expand.grid(
  nrounds            = 100,
  max_depth          = 6,
  eta                = 0.1,
  gamma              = 0,
  colsample_bytree   = 0.8,
  min_child_weight   = 1,
  subsample          = 0.8
)
xgb_model <- train(
  win1 ~ .,
  data      = train_set %>% select(all_of(predictors), win1),
  method    = 'xgbTree',
  metric    = 'ROC',
  trControl = ctrl,
  tuneGrid  = xgb_grid
)
saveRDS(xgb_model, "xgb_model.rds")


###################################

#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 4e) Optional: H2O AutoML
#–––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
# 0) If there’s any old H2O session, shut it down
library(h2o)

# 0) Gracefully shut down any old H2O (if there isn’t one, this will silently fail)
suppressWarnings(
  try(h2o.shutdown(prompt = FALSE), silent = TRUE)
)
Sys.sleep(5)

# 1) Start a brand‐new H2O cluster
h2o.init(
  nthreads             = -1,
  max_mem_size         = "4G",
  strict_version_check = FALSE
)

# 2) Make sure your response is a factor in R
train_set$win1 <- factor(train_set$win1, levels = c("Loss","Win"))

# 3) Push the data into H2O
h2o_train <- as.h2o(train_set)

# 4) Run AutoML
# … after
aml <- h2o.automl(
  x              = predictors,
  y              = "win1",
  training_frame = h2o_train,
  max_models     = 10,
  seed           = 2025
)

# 1) Get the leader model object
best_h2o <- aml@leader
message("AutoML leader model ID: ", best_h2o@model_id)

# 2) Make sure your output folder exists
if (!dir.exists("models")) dir.create("models")

# 3) Save the leader
model_path <- h2o.saveModel(
  object = best_h2o,
  path   = "models",
  force  = TRUE
)

message("Saved AutoML leader to: ", model_path)

# 6) Score on your test set
h2o_test <- as.h2o(test_set %>% select(all_of(predictors), win1))
pred_h2o <- h2o.predict(aml@leader, h2o_test)
probs    <- as.vector(pred_h2o[ , "Win"])
preds    <- factor(ifelse(probs > 0.5, "Win", "Loss"), levels = c("Loss","Win"))

# 7) Compute performance
cm      <- confusionMatrix(preds, test_set$win1)
roc_obj <- roc(as.numeric(test_set$win1) - 1, probs)

tibble(
  Model       = "H2O AutoML",
  Accuracy    = cm$overall["Accuracy"],
  Sensitivity = cm$byClass["Sensitivity"],
  Specificity = cm$byClass["Specificity"],
  AUC         = as.numeric(auc(roc_obj))
)


#–– models list ––#
models <- list(
  Logistic = logit_model,
  SVM      = svm_model,
  GBM      = gbm_model,
  XGBoost  = xgb_model,
  AutoML   = best_h2o
)

#–– evaluation function ––#
evaluate_model <- function(mod, name) {
  # for H2O AutoML
  if (name == "AutoML") {
    # get the H2OFrame predictions
    pred_h2o <- h2o.predict(best_h2o, h2o_test)
    # pull out the "Win" probability
    probs <- as.vector(pred_h2o[ , "Win"])
    # threshold at 0.5 to get class labels
    preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                    levels = c("Loss","Win"))
  }
  # for glm
  else if (inherits(mod, "glm")) {
    probs <- predict(mod, test_set, type = "response")
    preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                    levels = c("Loss","Win"))
  }
  # for SVM
  else if (inherits(mod, "svm")) {
    pr    <- attr(predict(mod, test_set, probability = TRUE), "probabilities")
    probs <- pr[,"Win"]
    preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                    levels = c("Loss","Win"))
  }
  # for caret models (GBM, XGBoost)
  else {
    pro   <- predict(mod, test_set, type = "prob")
    probs <- pro[,"Win"]
    preds <- predict(mod, test_set)  # already a factor
  }
  
  # compute metrics against the correct test_set$win1
  cm      <- confusionMatrix(preds, test_set$win1)
  roc_obj <- roc(as.numeric(test_set$win1) - 1, probs)
  
  tibble(
    Model       = name,
    Accuracy    = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity'],
    AUC         = as.numeric(auc(roc_obj))
  )
}

#–– run evaluation ––#
results <- imap_dfr(models, evaluate_model)

#–– print as before ––#
print(
  kable(results, digits = 3, caption = "Model Performance Comparison") %>%
    kable_styling(full_width = FALSE)
)

#–– ROC curves ––#
roc_list <- imap_dfr(models, function(mod, name) {
  # AutoML (H2O) uses the "Win" probability column
  if (name == "AutoML") {
    pr_h2o <- h2o.predict(best_h2o, h2o_test)
    prs     <- as.vector(pr_h2o[ , "Win"])
  }
  # GLM
  else if (inherits(mod, "glm")) {
    prs <- predict(mod, test_set, type = "response")
  }
  # SVM
  else if (inherits(mod, "svm")) {
    pr   <- attr(predict(mod, test_set, probability = TRUE), "probabilities")
    prs  <- pr[ , "Win"]
  }
  # caret models (GBM, XGBoost)
  else {
    prs <- predict(mod, test_set, type = "prob")[ , "Win"]
  }
  
  roc_obj <- roc(as.numeric(test_set$win1) - 1, prs)
  tibble(
    Model = name,
    FPR   = 1 - roc_obj$specificities,
    TPR   = roc_obj$sensitivities
  )
})

ggplot(roc_list, aes(x = FPR, y = TPR, color = Model)) +
  geom_line(size = 1) +
  geom_abline(linetype = 'dashed') +
  labs(x = 'False Positive Rate', y = 'True Positive Rate',
       title = 'ROC Curves Comparison') +
  theme_minimal()



best_name <- results$Model[which.max(results$AUC)]
best_mod  <- models[[best_name]]

#— get probs & preds on the correct test_set ——
if (inherits(best_mod, "glm")) {
  probs <- predict(best_mod, test_set, type = "response")
  preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                  levels = c("Loss","Win"))
} else if (inherits(best_mod, "svm")) {
  pr    <- attr(predict(best_mod, test_set, probability = TRUE), "probabilities")
  probs <- pr[, "Win"]
  preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                  levels = c("Loss","Win"))
} else if (inherits(best_mod, "H2OBinomialModel")) {
  ph    <- h2o.predict(best_h2o, h2o_test)
  probs <- as.vector(ph[, "Win"])
  preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                  levels = c("Loss","Win"))
} else {
  # caret models (GBM, XGBoost)
  pro   <- predict(best_mod, test_set, type = "prob")
  probs <- pro[, "Win"]
  preds <- predict(best_mod, test_set)  # raw factor
}

#— assemble the misclassified games table —
misclassified <- test_set %>%
  mutate(
    Predicted = preds,
    Actual    = win1
  ) %>%
  filter(Predicted != Actual) %>%
  select(gameDate, team1, team2, Actual, Predicted)

cat("\nExamples of Misclassified Games (", best_name, "):\n")
print(
  kable(head(misclassified, 10), caption = 'Misclassified Games') %>%
    kable_styling(full_width = FALSE)
)





################

### models_training.R (season‑stats–driven playoff prediction)

#–– 0) Dependencies & Setup ––#
install_if_missing <- function(pkgs) {
  for (pk in pkgs) if (!requireNamespace(pk, quietly=TRUE)) install.packages(pk)
}
install_if_missing(c(
  "dplyr","purrr","lubridate","tidyr","stringr",
  "caret","e1071","gbm","pROC","xgboost","h2o",
  "jsonlite","ggplot2","knitr","kableExtra"
))
library(dplyr); library(lubridate); library(tidyr); library(stringr)
library(caret); library(e1071); library(gbm); library(pROC)
library(xgboost); library(h2o); library(ggplot2)
library(purrr); library(jsonlite); library(knitr); library(kableExtra)

set.seed(2025)
setwd("~/Documents/NHLPROJECT")

#–– 1) Load historical playoff games from all_teams.csv ––#
if (!exists("all_teams")) {
  all_teams <- read.csv("all_teams.csv", stringsAsFactors=FALSE)
}
hist_playoff <- all_teams %>%
  filter(playoffGame == 1, situation == "all") %>%
  mutate(
    gameDate = as.Date(as.character(gameDate), "%Y%m%d"),
    season   = season  # already numeric year
  )

# Assemble paired games with outcome
away_df <- hist_playoff %>%
  filter(home_or_away == "AWAY") %>%
  select(gameId, season, team1 = playerTeam, goals1 = goalsFor)
home_df <- hist_playoff %>%
  filter(home_or_away == "HOME") %>%
  select(gameId, season, team2 = playerTeam, goals2 = goalsFor)

games_raw <- away_df %>%
  inner_join(home_df, by = c("gameId","season")) %>%
  mutate(win1 = as.integer(goals1 > goals2)) %>%
  select(gameId, season, team1, team2, win1)

# 1) grab the “true” header from the next-good file
correct_hdr <- names(
  read.csv("teams (15).csv", stringsAsFactors = FALSE, nrows = 0)
)

# 2) a little reader that fixes teams (14).csv on the fly
smart_read <- function(path) {
  if (basename(path) == "teams (14).csv") {
    # skip the bogus first line, then assign the correct header
    df <- read.csv(path, stringsAsFactors = FALSE, header = FALSE, skip = 1)
    names(df) <- correct_hdr
    df
  } else {
    read.csv(path, stringsAsFactors = FALSE, header = TRUE)
  }
}

# 3) list & load all 17 files via smart_read
season_files <- list.files(pattern = "^teams \\([0-9]+\\)\\.csv$")
season_dfs    <- lapply(season_files, smart_read)

# 4) figure out the intersection of their columns
common_cols <- Reduce(intersect, lapply(season_dfs, names))
# drop that stray team.1 if it snuck in
common_cols <- setdiff(common_cols, "team.1")

# 5) bind them all together, filter to situation=="all" and rename
season_stats <- map_dfr(season_files, smart_read)
season_stats_clean <- season_stats %>%
  filter(situation == "all") %>%
  rename(team_code = team.1) %>%
  select(team_code, season, all_of(common_cols))

  

# Identify numeric metrics (exclude 'season' and code cols)
num_cols <- season_stats_clean %>%
  select(-team_code, -season) %>%
  select(where(is.numeric)) %>%
  names()

#–– 3) Merge season stats into each game and compute diffs ––#
model_df <- games_raw %>%
  # team1 metrics
  left_join(
    season_stats_clean %>% select(team_code, season, all_of(num_cols)),
    by = c("team1" = "team_code","season" = "season")
  ) %>%
  rename_with(~ paste0(.x, "_1"), all_of(num_cols)) %>%
  # team2 metrics
  left_join(
    season_stats_clean %>% select(team_code, season, all_of(num_cols)),
    by = c("team2" = "team_code","season" = "season")
  ) %>%
  rename_with(~ paste0(.x, "_2"), all_of(num_cols)) %>%
  # compute difference for every metric present
  { df <- .;
  metrics1 <- grep("_1$", names(df), value=TRUE);
  for (m in metrics1) {
    base <- sub("_1$", "", m)
    df[[paste0(base, "_diff")]] <- df[[m]] - df[[paste0(base, "_2")]]
  }
  df
  } %>%
  #select(team1, team2, win1, ends_with("_diff")) %>%
  filter(!is.na(win1))

# Sanity checks
if (nrow(model_df) < 2) stop("Not enough games to partition")
if (length(unique(model_df$win1)) < 2) stop("Need both wins and losses in data")

#–– 4) Train/Test split ––#
set.seed(2025)
idx <- createDataPartition(model_df$win1, p = 0.8, list = FALSE)
train_set <- model_df[idx, ]; test_set <- model_df[-idx, ]
train_set$win1 <- factor(train_set$win1, levels=c(0,1), labels=c("Loss","Win"))
test_set$win1  <- factor(test_set$win1,  levels=c(0,1), labels=c("Loss","Win"))

# Filter out zero‑variance predictors
predictors <- grep("_diff$", names(train_set), value=TRUE)
zv <- sapply(train_set[predictors], function(x) sd(x, na.rm=TRUE)==0)
if (any(zv)) predictors <- predictors[!zv]
if (length(predictors)==0) stop("No predictive features remain after filtering")

#–– 5) Train Models ––#
## Logistic Regression
logit_model <- glm(
  formula = as.formula(paste("win1 ~", paste(predictors, collapse = "+"))),
  data    = train_set, family = binomial
)
saveRDS(logit_model, "logit_model.rds")

## SVM
svm_model <- svm(
  formula     = as.formula(paste("win1 ~", paste(predictors, collapse = "+"))),
  data        = train_set, type='C-classification', kernel='radial', probability=TRUE
)
saveRDS(svm_model, "svm_model.rds")

## GBM (caret)
ctrl <- trainControl(method='repeatedcv', number=5, repeats=2,
                     classProbs=TRUE, summaryFunction=twoClassSummary)
gbm_grid <- expand.grid(n.trees=c(100,300), interaction.depth=c(3,5),
                        shrinkage=c(0.01,0.1), n.minobsinnode=10)
gbm_model <- train(
  win1 ~ ., data = train_set %>% select(all_of(predictors), win1),
  method = 'gbm', metric = 'ROC', trControl = ctrl, tuneGrid = gbm_grid, verbose = FALSE
)
saveRDS(gbm_model, "gbm_model.rds")

## XGBoost (caret)
xgb_grid <- expand.grid(nrounds=100, max_depth=6, eta=0.1, gamma=0,
                        colsample_bytree=0.8, min_child_weight=1, subsample=0.8)
xgb_model <- train(
  win1 ~ ., data = train_set %>% select(all_of(predictors), win1),
  method = 'xgbTree', metric = 'ROC', trControl = ctrl, tuneGrid = xgb_grid
)
saveRDS(xgb_model, "xgb_model.rds")

## H2O AutoML (optional)
h2o.init(nthreads=-1)
h2o_train <- as.h2o(train_set %>% select(all_of(predictors), win1))
h2o_test  <- as.h2o(test_set  %>% select(all_of(predictors), win1))
aml <- h2o.automl(x = predictors, y = 'win1', training_frame = h2o_train,
                  max_models = 10, seed = 2025)
best_h2o <- aml@leader
h2o.saveModel(best_h2o, path = 'models', force = TRUE)

#–– 6) Evaluate & Visualize ––#
models <- list(Logistic=logit_model, SVM=svm_model,
               GBM=gbm_model, XGBoost=xgb_model, AutoML=best_h2o)
evaluate_model <- function(mod, name) {
  if (name == 'AutoML') {
    ph <- h2o.predict(best_h2o, h2o_test)
    probs <- as.vector(ph[,'Win']); preds <- factor(ifelse(probs>0.5,'Win','Loss'), levels=c('Loss','Win'))
  } else if (inherits(mod, 'glm')) {
    probs <- predict(mod, test_set, type='response'); preds <- factor(ifelse(probs>0.5,'Win','Loss'), levels=c('Loss','Win'))
  } else if (inherits(mod, 'svm')) {
    pr <- attr(predict(mod, test_set, probability=TRUE),'probabilities'); probs <- pr[,'Win']; preds <- factor(ifelse(probs>0.5,'Win','Loss'), levels=c('Loss','Win'))
  } else {
    pro <- predict(mod, test_set, type='prob'); probs <- pro[,'Win']; preds <- predict(mod, test_set)
  }
  cm <- confusionMatrix(preds, test_set$win1)
  roc_obj <- roc(as.numeric(test_set$win1)-1, probs)
  tibble(Model=name,
         Accuracy=cm$overall['Accuracy'],
         Sensitivity=cm$byClass['Sensitivity'],
         Specificity=cm$byClass['Specificity'],
         AUC=as.numeric(auc(roc_obj)))
}
results <- imap_dfr(models, evaluate_model)
print(
  kable(results, digits=3, caption="Model Performance Comparison") %>%
    kable_styling(full_width=FALSE)
)

roc_list <- imap_dfr(models, function(mod,name){
  if(name=='AutoML'){prs<-as.vector(h2o.predict(best_h2o,h2o_test)[,'Win'])}
  else if(inherits(mod,'glm')){prs<-predict(mod,test_set,type='response')}
  else if(inherits(mod,'svm')){prs<-attr(predict(mod,test_set,probability=TRUE),'probabilities')[,'Win']}
  else{prs<-predict(mod,test_set,type='prob')[,'Win']}
  roc_obj<-roc(as.numeric(test_set$win1)-1,prs)
  tibble(Model=name, FPR=1-roc_obj$specificities, TPR=roc_obj$sensitivities)
})

ggplot(roc_list, aes(FPR,TPR,color=Model)) + geom_line(size=1) + geom_abline(linetype='dashed') +
  labs(x='False Positive Rate',y='True Positive Rate', title='ROC Curves Comparison') + theme_minimal()

best_name <- results$Model[which.max(results$AUC)]
best_mod  <- models[[best_name]]

# Misclassified examples
if (inherits(best_mod, "glm")) {
  probs <- predict(best_mod, test_set, type = "response")
  preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                  levels = c("Loss","Win"))
} else if (inherits(best_mod, "svm")) {
  pr    <- attr(predict(best_mod, test_set, probability = TRUE), "probabilities")
  probs <- pr[ , "Win"]
  preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                  levels = c("Loss","Win"))
} else if (inherits(best_mod, "H2OBinomialModel")) {
  ph    <- h2o.predict(best_h2o, h2o_test)
  probs <- as.vector(ph[ , "Win"])
  preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                  levels = c("Loss","Win"))
} else {
  # caret models (GBM, XGBoost)
  pro   <- predict(best_mod, test_set, type = "prob")
  probs <- pro[ , "Win"]
  preds <- predict(best_mod, test_set)  # already a factor Loss/Win
}

misclassified <- test_set %>%
  mutate(
    Predicted = preds,
    Actual    = win1
  ) %>%
  filter(Predicted != Actual) %>%
  select(team1, team2, Actual, Predicted)

cat("\nExamples of Misclassified Games (", best_name, "):\n")
print(
  kable(head(misclassified, 10), caption = "Misclassified Games") %>%
    kable_styling(full_width = FALSE)
)

###### 7. Rerun models that are optimized
library(caret)

# 1) Near-Zero Variance filtering
nzv_metrics <- nearZeroVar(train_set[, predictors], saveMetrics = TRUE)
predictors1  <- predictors[ ! nzv_metrics$nzv ]

# 2) Correlation filtering (drop one of any pair with |r| > .90)
cor_mat   <- cor(train_set[, predictors1], use = "pairwise.complete.obs")
high_cor  <- findCorrelation(cor_mat, cutoff = 0.90)
predictors2 <- predictors1[ - high_cor ]

# sanity-check
length(predictors);     length(predictors1);     length(predictors2)

# 3) Now do RFE on this reduced set:
rfe_ctrl <- rfeControl(
  functions = rfFuncs,
  method    = "repeatedcv",
  number    = 5,
  repeats   = 2
)

set.seed(2025)
rfe_res <- rfe(
  x          = train_set[, predictors2],
  y          = train_set$win1,
  sizes      = c(10, 20, 50, 100),
  rfeControl = rfe_ctrl
)

# What RFE picked:
predictors3 <- predictors(rfe_res)
print(rfe_res)
print(predictors3)

library(glmnet)

# build x/y


# (1) build the feature matrix (drop the “~1” intercept column afterwards)
x <- model.matrix(~ ., data = train_set[, predictors2])[ , -1]

# (2) build the response vector (0/1)
# if you already have train_set$win1 as factor("Loss","Win"):
y <- as.numeric(train_set$win1) - 1

# quick sanity‐check
dim(x)        # should be nrow(train_set) × length(predictors2)
length(y)     # should be nrow(train_set)

# (3) fit a LASSO‐penalized logistic via cross‐validation
set.seed(2025)
cvfit <- cv.glmnet(x, y, family = "binomial", alpha = 1, 
                   type.measure = "auc", nfolds = 5)

# plot the CV‐curve
plot(cvfit)

# best lambda
lambda_min <- cvfit$lambda.min

# grab the nonzero coefficients at lambda.min
best_coefs <- coef(cvfit, s = "lambda.min")
nz <- which(best_coefs != 0)
data.frame(
  feature = rownames(best_coefs)[nz],
  coef    = best_coefs[nz]
)
set.seed(2025)
cv_lasso <- cv.glmnet(x, y, family="binomial", alpha=1)
lasso_coefs <- coef(cv_lasso, s="lambda.min")
keep4 <- rownames(lasso_coefs)[which(lasso_coefs != 0)]
# drop the intercept
predictors4 <- rownames(lasso_coefs)[ which(lasso_coefs != 0) ]
predictors4 <- setdiff(predictors4, "(Intercept)")

# Example: GBM on your RFE‐chosen features
gbm_model2 <- train(
  x          = train_set[, predictors4],
  y          = train_set$win1,
  method     = "gbm",
  metric     = "ROC",
  trControl  = ctrl,        # note: ctrl already has classProbs=TRUE & twoClassSummary
  tuneGrid   = gbm_grid,
  verbose    = FALSE
)
saveRDS(gbm_model2, "gbm_model2.rds")


#–– 6) Evaluate & Visualize ––#
models <- list(Logistic=logit_model, SVM=svm_model,
               GBM=gbm_model, XGBoost=xgb_model, AutoML=best_h2o)
evaluate_model <- function(mod, name) {
  if (name == 'AutoML') {
    ph <- h2o.predict(best_h2o, h2o_test)
    probs <- as.vector(ph[,'Win']); preds <- factor(ifelse(probs>0.5,'Win','Loss'), levels=c('Loss','Win'))
  } else if (inherits(mod, 'glm')) {
    probs <- predict(mod, test_set, type='response'); preds <- factor(ifelse(probs>0.5,'Win','Loss'), levels=c('Loss','Win'))
  } else if (inherits(mod, 'svm')) {
    pr <- attr(predict(mod, test_set, probability=TRUE),'probabilities'); probs <- pr[,'Win']; preds <- factor(ifelse(probs>0.5,'Win','Loss'), levels=c('Loss','Win'))
  } else {
    pro <- predict(mod, test_set, type='prob'); probs <- pro[,'Win']; preds <- predict(mod, test_set)
  }
  cm <- confusionMatrix(preds, test_set$win1)
  roc_obj <- roc(as.numeric(test_set$win1)-1, probs)
  tibble(Model=name,
         Accuracy=cm$overall['Accuracy'],
         Sensitivity=cm$byClass['Sensitivity'],
         Specificity=cm$byClass['Specificity'],
         AUC=as.numeric(auc(roc_obj)))
}
results <- imap_dfr(models, evaluate_model)
print(
  kable(results, digits=3, caption="Model Performance Comparison") %>%
    kable_styling(full_width=FALSE)
)


####### LOGSITIC
# 1) Full‐feature logistic (you already have this)
logit_all <- glm(
  win1 ~ .,
  data   = train_set %>% select(all_of(predictors), win1),
  family = binomial
)

# 2) PCA‐reduced logistic
# --------------------------------
# (a) fit PCA on the train predictors
X_train <- train_set[, predictors]
pca_fit <- prcomp(X_train, center = TRUE, scale. = TRUE)

# (b) choose number of PCs to explain e.g. 90% of variance
var_exp  <- cumsum(pca_fit$sdev^2) / sum(pca_fit$sdev^2)
n_pc     <- which(var_exp >= 0.90)[1]

# (c) construct PC‐scores for train & test
train_pcs <- data.frame(pca_fit$x[, 1:n_pc])
train_pcs$win1 <- train_set$win1

test_pcs <- predict(pca_fit, newdata = test_set[, predictors])
test_pcs <- as.data.frame(test_pcs[, 1:n_pc])
test_pcs$win1 <- test_set$win1

# (d) fit logistic on those PCs
logit_pca <- glm(win1 ~ ., data = train_pcs, family = binomial)

# 3) Simple “few‐vars” logistic
# --------------------------------
# pick the top 5 LASSO‐selected diffs (for example)
simple_vars <- c(
  "blockedShotAttemptsFor_diff",
  "playContinuedOutsideZoneFor_diff",
  "xGoalsFromxReboundsOfShotsFor_diff",
  "scoreAdjustedTotalShotCreditFor_diff",
  "lowDangerGoalsAgainst_diff"
)

logit_simple <- glm(
  as.formula(paste("win1 ~", paste(simple_vars, collapse = " + "))),
  data   = train_set %>% select(all_of(simple_vars), win1),
  family = binomial
)

# pick the top 5 LASSO‐selected diffs (for example)
chosen_vars <- c(
  "xGoalsPercentage_diff",
  "corsiPercentage_diff",
  "fenwickPercentage_diff",
  "highDangerxGoalsFor_diff",
  "highDangerGoalsFor_diff",
  "shotAttemptsFor_diff",
  "unblockedShotAttemptsFor_diff",
  "faceOffsWonFor_diff",
  "takeawaysFor_diff", 
  "penalityMinutesFor_diff"
)

logit_chosen <- glm(
  as.formula(paste("win1 ~", paste(chosen_vars, collapse = " + "))),
  data   = train_set %>% select(all_of(chosen_vars), win1),
  family = binomial
)

# ——— Evaluation helper ———
evaluate_logistic <- function(mod, data, name) {
  # caret::train objects
  if (inherits(mod, "train")) {
    probs <- predict(mod, data, type = "prob")[, "Win"]
    preds <- predict(mod, data, type = "raw")
  }
  # base‐R glm objects
  else if (inherits(mod, "glm")) {
    probs <- predict(mod, data, type = "response")
    preds <- factor(ifelse(probs > 0.5, "Win", "Loss"),
                    levels = c("Loss","Win"))
  } else {
    stop("Unsupported model class: ", class(mod)[1])
  }
  
  cm      <- confusionMatrix(preds, data$win1)
  roc_obj <- roc(as.numeric(data$win1)-1, probs)
  
  tibble(
    Model       = name,
    Accuracy    = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"],
    AUC         = as.numeric(auc(roc_obj))
  )
}


# ——— Compare on test_set ———
results2 <- bind_rows(
  evaluate_logistic(logit_all,    test_set,           "Full Features"),
  evaluate_logistic(logit_pca,    test_pcs,           paste0("PCA (",n_pc," PCs)")),
  evaluate_logistic(logit_simple, test_set,           "Simple 5‐Vars"),
  evaluate_logistic(logit_chosen, test_set,           "Chosen 10‐Vars")
)

print(results2)


### Forward-stepwise feature selection:
# 1) build the null & full formulas
null_mod <- glm(win1 ~ 1, data = train_set, family = binomial)
full_fmla <- as.formula(paste("win1 ~", paste(predictors, collapse = " + ")))
full_mod  <- glm(full_fmla, data = train_set, family = binomial)

library(caret)

ctrl_acc <- trainControl(
  method = "repeatedcv",
  number = 5,       # 5‐fold CV
  repeats = 2,
  classProbs = TRUE # needed for two‐class
  # leave summaryFunction = defaultSummary so that 'Accuracy' is used
)

# trainStep will do the forward/backward AIC search and we’ll pick the model
# that maximizes **Accuracy** under CV
trainStep <- train(
  full_fmla,
  data    = train_set,
  method  = "glmStepAIC",
  trControl = ctrl_acc,
  metric = "Accuracy"
)

# which vars did it settle on?
trainStep$finalModel


### BACKWARDS

ctrl_acc <- trainControl(
  method     = "repeatedcv",
  number     = 5,
  repeats    = 2,
  classProbs = TRUE
)

train_back <- train(
  full_fmla,
  data       = train_set,
  method     = "glmStepAIC",
  trControl  = ctrl_acc,
  metric     = "Accuracy",
  # pass control to step:
  tuneGrid   = NULL,
  # tell step() to go backward only:
  trace      = FALSE,
  direction  = "backward"
)

# See which variables survived:
train_back$finalModel

# ——— Compare on test_set ———
results2 <- bind_rows(
  evaluate_logistic(logit_all,    test_set,           "Full Features"),
  evaluate_logistic(logit_pca,    test_pcs,           paste0("PCA (",n_pc," PCs)")),
  evaluate_logistic(logit_simple, test_set,           "Simple 5-Vars"),
  evaluate_logistic(logit_chosen, test_set,  "Chosen"),
  evaluate_logistic(trainStep,      test_set, "Forward"),
  evaluate_logistic(train_back,     test_set, "Backward")
  )

print(
  kable(results2, digits=3, caption="All Logistic Models") %>%
    kable_styling(full_width=FALSE)
)

########## OTHER OPTIMIZING
# 1) assemble your named list of feature‐sets
feature_sets <- list(
  Simple   = simple_vars,
  Chosen   = chosen_vars,
  PCA      = paste0("PC", seq_len(n_pc)),
  Forward  = setdiff(names(coef(trainStep$finalModel)), "(Intercept)"),
  Backward = setdiff(names(coef(train_back$finalModel)), "(Intercept)")
)

# 2) fit all 4 algorithms on each feature‐set
# ——— 1) Rebuild models_by_set to include a Logistic glm for each feature‐set ———
models_by_set <- imap(feature_sets, function(vars, set_name) {
  if (set_name == "PCA") {
    tr     <- train_pcs
    h2o_tr <- as.h2o(train_pcs)
  } else {
    tr     <- train_set %>% select(all_of(vars), win1)
    h2o_tr <- as.h2o(tr)
  }
  
  # a) Logistic regression on exactly those vars / PCs
  logit_mod <- glm(win1 ~ ., data = tr, family = binomial)
  
  # b) SVM
  svm_mod <- svm(
    win1 ~ ., data = tr,
    type='C-classification', kernel='radial', probability=TRUE
  )
  
  # c) GBM via caret
  gbm_mod <- train(
    win1 ~ ., data     = tr,
    method    = 'gbm', metric='ROC',
    trControl = ctrl,  tuneGrid=gbm_grid,
    verbose   = FALSE
  )
  
  # d) XGBoost via caret
  xgb_mod <- train(
    win1 ~ ., data     = tr,
    method    = 'xgbTree', metric='ROC',
    trControl = ctrl,  tuneGrid=xgb_grid
  )
  
  # e) H2O AutoML
  aml           <- h2o.automl(
    x              = if(set_name=="PCA") colnames(train_pcs)[1:n_pc] else vars,
    y              = "win1",
    training_frame = h2o_tr,
    max_models     = 5,
    seed           = 2025
  )
  automl_leader <- aml@leader
  
  list(
    Logistic = logit_mod,
    SVM      = svm_mod,
    GBM      = gbm_mod,
    XGBoost  = xgb_mod,
    AutoML   = automl_leader
  )
})


# ——— 2) Evaluate them all ———
eval_all <- imap_dfr(models_by_set, function(model_list, set_name) {
  if (set_name == "PCA") {
    te     <- test_pcs
    h2o_te <- as.h2o(test_pcs)
  } else {
    te     <- test_set %>% select(all_of(feature_sets[[set_name]]), win1)
    h2o_te <- as.h2o(te)
  }
  
  imap_dfr(model_list, function(mod, alg) {
    if (alg == "AutoML") {
      ph    <- h2o.predict(mod, h2o_te)
      probs <- as.vector(ph[, "Win"])
      preds <- factor(ifelse(probs > 0.5, "Win","Loss"), c("Loss","Win"))
      
    } else if (alg == "Logistic") {
      probs <- predict(mod, te, type = "response")
      preds <- factor(ifelse(probs > 0.5, "Win","Loss"), c("Loss","Win"))
      
    } else if (inherits(mod, "svm")) {
      pr    <- attr(predict(mod, te, probability=TRUE), "probabilities")
      probs <- pr[,"Win"]
      preds <- factor(ifelse(probs > 0.5, "Win","Loss"), c("Loss","Win"))
      
    } else {
      probs <- predict(mod, te, type="prob")[, "Win"]
      preds <- predict(mod, te, type="raw")
    }
    
    cm      <- confusionMatrix(preds, te$win1)
    roc_obj <- roc(as.numeric(te$win1) - 1, probs)
    
    tibble(
      FeatureSet  = set_name,
      Algorithm   = alg,
      Accuracy    = cm$overall["Accuracy"],
      Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"],
      AUC         = as.numeric(auc(roc_obj))
    )
  })
})

# ——— 3) Show your summary table ———
print(
  kable(eval_all, digits = 3, caption = "All Models × All Feature‐Sets") %>%
    kable_styling(full_width = FALSE)
)


########## FURTHER OPTI
library(caretEnsemble)
library(pROC)
library(DMwR2)       # for SMOTE
library(isotone)    # for isotonic calibration

#–– 4) CLASS–IMBALANCE & THRESHOLD TUNING ––#

# create a trainControl that uses SMOTE to rebalance the two classes:
ctrl_smote <- trainControl(
  method         = "repeatedcv",
  number         = 5,
  repeats        = 2,
  classProbs     = TRUE,
  summaryFunction= twoClassSummary,
  sampling       = "smote",          # automatically up‐samples minority
  savePredictions= "final"           # needed if we want to stack later
)

# re‐train GBM with SMOTE
gbm_smote <- train(
  win1 ~ .,
  data      = train_set %>% select(all_of(predictors), win1),
  method    = "gbm",
  metric    = "ROC",
  trControl = ctrl_smote,
  tuneGrid  = gbm_grid,
  verbose   = FALSE
)

# on test set, get raw probabilities
probs_gbm_smote <- predict(gbm_smote, test_set, type="prob")[, "Win"]

# find the Youden‐optimal threshold on your ROC curve
roc_obj  <- roc(test_set$win1, probs_gbm_smote)
opt_thr  <- coords(roc_obj, "best", ret="threshold", transpose=FALSE)

# compare default vs optimized threshold
pred_def <- factor(ifelse(probs_gbm_smote>0.5, "Win","Loss"), levels=c("Loss","Win"))
pred_opt <- factor(ifelse(probs_gbm_smote>opt_thr,  "Win","Loss"), levels=c("Loss","Win"))

cat("GBM w/ SMOTE — default accuracy:", 
    confusionMatrix(pred_def, test_set$win1)$overall["Accuracy"], "\n")

# 1) get the “Win” probability
probs_opt <- predict(gbm_smote, newdata = test_set, type = "prob")[, "Win"]

# 2) threshold at 0.5
pred_opt  <- factor(ifelse(probs_opt > 0.5, "Win", "Loss"),
                    levels = c("Loss","Win"))

stopifnot(length(pred_opt) == nrow(test_set))

cm_opt <- confusionMatrix(pred_opt, test_set$win1)
cat("GBM w/ SMOTE — tuned   accuracy:", 
    cm_opt$overall["Accuracy"], "\n")


#–– 5) SIMPLE ENSEMBLE & STACKING ––#
# 1) glm probabilities
logit_probs <- predict(logit_model, test_set, type = "response")

# 2) SVM probabilities (e1071)
svm_pr    <- predict(svm_model, test_set, probability = TRUE)
svm_probs <- attr(svm_pr, "probabilities")[, "Win"]

# 3) caret GBM probabilities
gbm_probs <- predict(gbm_model, test_set, type = "prob")[, "Win"]

# 4) caret XGBoost probabilities
xgb_probs <- predict(xgb_model, test_set, type = "prob")[, "Win"]

# 5) average them
ensemble_probs <- (logit_probs + svm_probs + gbm_probs + xgb_probs) / 4

# 6) threshold into class labels
ensemble_preds <- factor(
  ifelse(ensemble_probs > 0.5, "Win", "Loss"),
  levels = c("Loss","Win")
)

# 7) evaluate
cm  <- confusionMatrix(ensemble_preds, test_set$win1)
roc <- roc(as.numeric(test_set$win1) - 1, ensemble_probs)

cat("Ensemble Accuracy:",   cm$overall["Accuracy"], "\n")
cat("Ensemble AUC:",        as.numeric(auc(roc)), "\n")

ensemble_pred  <- factor(ifelse(ensemble_probs>0.5, "Win","Loss"),
                         levels=c("Loss","Win"))
print(confusionMatrix(ensemble_pred, test_set$win1))

#–– 6) PROBABILITY CALIBRATION ––#

# (a) Platt scaling on your validation set
#   fit a tiny glm mapping raw probs → calibrated probs
cal_df <- data.frame(
  raw   = predict(logit_model, train_set, type="response"),
  obs   = as.numeric(train_set$win1) - 1
)
platt <- glm(obs ~ raw, data = cal_df, family = binomial)

# apply to test set
raw_test <- predict(logit_model, test_set, type="response")
cal_test <- predict(platt, newdata = data.frame(raw = raw_test), type="response")

cal_pred <- factor(ifelse(cal_test>0.5, "Win","Loss"), levels=c("Loss","Win"))
cat("Logit w/ Platt scaling accuracy:", 
    confusionMatrix(cal_pred, test_set$win1)$overall["Accuracy"], "\n")

# (b) Isotonic regression
iso_fit <- isoreg(cal_df$raw, cal_df$obs)
iso_test <- pmin(pmax(approx(iso_fit$x, iso_fit$yf, xout=raw_test)$y, 0), 1)
iso_pred <- factor(ifelse(iso_test>0.5, "Win","Loss"), levels=c("Loss","Win"))
cat("Logit w/ isotonic accuracy:", 
    confusionMatrix(iso_pred, test_set$win1)$overall["Accuracy"], "\n")

# Naive model
#–– BASELINE: Predict team with more regular-season points wins ––#
# Create goal differential for team1 and team2
test_set <- test_set %>%
  mutate(
    goal_diff_1 = goalsFor_1 - goalsAgainst_1,
    goal_diff_2 = goalsFor_2 - goalsAgainst_2
  )

# Evaluate baseline: predict team1 wins if they had better goal diff
baseline_eval <- evaluate_points_baseline(test_set, "goal_diff_1", "goal_diff_2")

# Output results
cat("=== Naive Baseline: Pick team with better goal differential ===\n")
cat("Direction: ", baseline_eval$direction, "\n")
cat("Accuracy:  ", baseline_eval$accuracy, "\n")
cat("AUC:       ", baseline_eval$auc, "\n\n")




#–– 7) Print a quick overview of everything ––#

# 7a) Data dimensions & splits
cat("Total playoff games (rows in train_df):", nrow(train_df), "\n")
cat("  → train_set:", nrow(train_set), "rows\n")
cat("  → test_set: ", nrow(test_set),  "rows\n")
cat("Number of predictors used:", length(predictors), "\n\n")

# 7b) Feature‐selection diagnostics
cat("### RFE results ###\n")
print(rfe_res)                    # shows accuracy per subset size
cat("→ RFE chose these predictors:\n")
print(predictors3)                # your final RFE set
cat("\n")

cat("### LASSO results ###\n")
cat("Best λ:", lambda_min, "\n")
print(data.frame(
  feature = rownames(best_coefs)[nz],
  coef    = best_coefs[nz]
))                                # non‐zero LASSO coefficients
cat("\n")

# 7c) All logistic model comparisons
cat("### Logistic-only comparisons ###\n")
print(
  results2 %>%
    arrange(desc(AUC))           # Full, PCA, Simple, Chosen, Forward, Backward
)

# 7d) All models × all feature-sets
cat("### All models × feature-sets ###\n")
print(
  eval_all %>%
    arrange(desc(AUC))          # SVM, GBM, XGBoost, AutoML across Simple/Chosen/PCA/…
)

# 7e) Main 5 models performance
cat("### Main five models (diff-features) ###\n")
print(
  results %>%
    arrange(desc(AUC))
)

# 7g) Ensemble metrics
cat("### Ensemble of logit/SVM/GBM/XGB ###\n")
cat("Ensemble accuracy:", cm$overall["Accuracy"], "\n")
cat("Ensemble AUC:     ", as.numeric(auc(roc)), "\n\n")

# 7h) Mis-classified examples for your best model
cat("### Misclassified games for", best_name, "###\n")
print(head(misclassified, 10))

