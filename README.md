# Bayesian multinomial model VAW


This repository contains the data processing, descriptive analysis, and Bayesian multinomial logistic regression modeling developed for the study “Violence Against Women (VAW) in Colombia: A Bayesian Approach to a Wicked Policy Problem.”


 
### Directory structure


```         
├── data/
│   ├── raw/                # Original unmodified data
│   └── processed/           # Cleaned and standardized datasets (e.g., non_fatal_data_model.csv.csv)
│
├── scripts/
│   ├── 01_data_processing.R    # Runs the data cleaning pipeline
│   ├── 02_descriptive.R     # Generates descriptive statistics and visualizations
│   └── 03_modeling.R        # Fits and evaluates the Bayesian model
│
├── results/
│   ├── figures/             # Plots and visual outputs
│   └── models/              # Saved model objects (.rds)
|
├── renv.lock                # Reproducible environment file
└── README.md

```