# BAYESIAN MULTINOMIAL LOGISTIC REGRESSION MODEL
# ==============================================================================
#
# Purpose:
#   Train and evaluate a Bayesian multinomial logistic regression model.
#
# Input:
#   non_fatal_data_model.csv (from 01_data_processing.R)
#
# Output:
#   - Trained model (results/model/brms_total_fullrank.rds)
#   - Odds ratios and ROC curves
#
# ==============================================================================


# Load libraries required 

library(dplyr)
library(tibble)
library(caret)
library(brms)
library(pROC)
library(cmdstanr)

# Load data
non_fatal_data <- read.csv("data/processed/non_fatal_data_model.csv", stringsAsFactors = TRUE)

# Verify target variable distribution
message("\nTarget variable (def_naturaleza) distribution:")
print(table(non_fatal_data$type_of_violence))

## Split data into training and test 

# Create stratified split (70% training, 30% testing)
# Stratification ensures balanced class distribution in both sets
train_index <- createDataPartition(
  non_fatal_data$type_of_violence,
  p = 0.7,
  list = FALSE
)

# Split the data
train_data <- non_fatal_data[train_index, ]
test_data <- non_fatal_data[-train_index, ]

## Design matrix
data <- model.matrix(~ life_cycle_stage +
                       education_level +
                       day_of_event +
                       time_of_the_event +
                       setting_of_event +
                       geographical_area +
                       type_of_perpetrator, data =  train_data)

test_data <- test_data %>%
  mutate(type_of_violence = as.factor(as.numeric(as.factor(type_of_violence))))

train_data <- train_data %>%
  mutate(type_of_violence = as.factor(as.numeric(as.factor(type_of_violence))))

## Train bayesian multinomial logistict regression model 
# # Set cmdstanr as backend
# set_cmdstan_path()
#
# fit <- brm(
#   type_of_perpetrator ~ life_cycle_stage +
#     education_level +
#     day_of_event +
#     time_of_the_event +
#     setting_of_event +
#     geographical_area +
#     type_of_perpetrator,
#   data = train_data,
#   family = categorical(),
#   algorithm ="fullrank",
#   chains = 4,
#   cores = 5,
#   backend = "cmdstanr",
#   threads = threading(8)  # Within-chain parallelization
#  )

# Load previously trained model
fit <- readRDS("results/model/brms_total_fullrank.rds")

## Model summary and dignosis
print(summary(fit))

# Extraer las muestras para diagnóstico con posterior
posterior_samples <- as_draws_df(fit)

# Diagnóstico effective sample size (ESS)

neff_vals <- neff_ratio(fit) * posterior::niterations(fit)  # neff en número absoluto

print(neff_vals)

## Odds ratio
# Extract fixed effects with 95% credible intervals
fixed_effects_summary <- fixef(fit, probs = c(0.025, 0.975))

# Calculate odds ratios (exponentiate coefficients)
odds_ratios <- round(exp(fixed_effects_summary), 2)
odds_ratios

## Multiclass ROC curves

colnames(test_data) <- c("def_naturaleza",
                          "ciclo_vital",
                          "escolaridad",
                          "dia_del_hecho",
                          "rango_de_hora_del_hecho_x_3_horas",
                          "escenario_del_hecho",
                          "zona_del_hecho",
                          "agresor_group"
                          )

test_data <- test_data %>% 
  mutate(
    dia_del_hecho = case_when(
      dia_del_hecho == "Semana" ~ 
        "semana",
      TRUE ~ dia_del_hecho
    )
  )

# Create validation subset (30% of test data)
test_index <- createDataPartition(test_data$def_naturaleza, p = 0.7, list = FALSE)
test_data_roc <- test_data[-test_index, ]

# Get predicted probabilities for each class
pred_probs <- predict(fit, 
                      newdata = test_data_roc[, -1], 
                      prob_method = "softmax_for_cat")

# Prepare data for ROC analysis
test_data_roc$type_of_violence <- as.character(test_data_roc$def_naturaleza)
pred_probs_df <- as.data.frame(pred_probs)

# Calculate ROC curves for each class (one-vs-rest)
roc_1 <- roc(response = test_data_roc$type_of_violence == "1",
             predictor = pred_probs_df[[1]])  # Interpersonal

roc_2 <- roc(response = test_data_roc$type_of_violence == "2",
             predictor = pred_probs_df[[2]])  # Domestic

roc_3 <- roc(response = test_data_roc$type_of_violence == "3",
             predictor = pred_probs_df[[3]])  # Sexual

roc_4 <- roc(response = test_data_roc$type_of_violence == "4",
             predictor = pred_probs_df[[4]])  # Sociopolitical

# Plot all ROC curves
plot(roc_1, col = "blue", lwd = 2, 
     main = "ROC Curves by Category (One-vs-Rest)", 
     xlim = c(1, 0), xaxs = "i", yaxs = "i", asp = NA)

plot(roc_2, col = "red", lwd = 2, add = TRUE, 
     xlim = c(1, 0), xaxs = "i", yaxs = "i", asp = NA)

plot(roc_3, col = "darkgreen", lwd = 2, add = TRUE, 
     xlim = c(1, 0), xaxs = "i", yaxs = "i", asp = NA)

plot(roc_4, col = "purple", lwd = 2, add = TRUE, 
     xlim = c(1, 0), xaxs = "i", yaxs = "i", asp = NA)

# Add legend with AUC values
legend("bottom",
       legend = c(
         paste("Interpersonal Violence (AUC =", round(auc(roc_1), 3), ")"),
         paste("Domestic Violence (AUC =", round(auc(roc_2), 3), ")"),
         paste("Sexual Violence (AUC =", round(auc(roc_3), 3), ")"),
         paste("Sociopolitical Violence (AUC =", round(auc(roc_4), 3), ")")
       ),
       col = c("blue", "red", "darkgreen", "purple"),
       lwd = 2)
