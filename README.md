# NHL Playoff Game Prediction

This project applies various machine learning models (Logistic Regression, SVM, GBM, XGBoost, H2O AutoML) to predict the outcome of NHL playoff games using advanced team-level statistics.

## Files
- `Final_modelsNHL.R`: main script for cleaning, modeling, and evaluating ML models
- `Elo_app.R`: main script for RShiny creation of elo app
- `ML_app.R`: main script for RShiny simulation of ML models
- `README.md`: project overview and instructions

## Data Access
All data was sourced from [NHL.com](https://www.nhl.com/stats/). Specifically, game logs and advanced team-level statistics were scraped or downloaded through moneypuck.com. Data is not included in this repository.

## Reproducibility
To replicate this study, download team-level playoff game logs from moneypuck.com, format the data as described in `Final_modelsNHL.R`, and run the script in R (R 4.2+ with required packages).
You can also simply access my RShiny publications with the following links below:
ELO: https://jacob-malter.shinyapps.io/app1/
ML MODEL: https://jacob-malter.shinyapps.io/NHLPROJECT_ML/

Here is the report for my project as well:
https://drive.google.com/file/d/1XYFlafdZqgdQ6s91sVQRfRU9QiNC-k_V/view?usp=sharing
