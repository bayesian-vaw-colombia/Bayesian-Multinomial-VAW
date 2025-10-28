# ==============================================================================
# FINAL DATA CLEANING AND DESCRIPTIVE ANALYSIS: NON-FATAL RECORDS
# ==============================================================================
#
# Purpose:
#   Perform descriptive statistical analysis and generate visualizations for
#   Gender-Based Violence (GBV) research publication.
#
# Data Source:
#   non_fatal_data_model.csv (cleaned dataset from 01_data_processing.R)
#
# Output:
#   Visualization plots.
#
# ==============================================================================

## # Load libraries required 
library(dplyr)
library(forcats)
library(stringr)
library(ggplot2)
library(patchwork)

# Suppress scientific notation for better readability
options(scipen = 999)

## Load cleaned data

non_fatal_data <- read.csv("data/processed/non_fatal_data_model.csv", stringsAsFactors = TRUE)

non_fatal_data %>%
  count(type_of_perpetrator) %>%
  mutate(proporcion = n / sum(n) * 100) %>%
  ggplot(aes(x = type_of_perpetrator, y = n)) +
  geom_col(fill = "#AE86D3", alpha = 0.8) +  
  geom_text(aes(label = paste0(round(proporcion, 1), "%")), 
            vjust = -0.2, 
            color = "#4B0082",  
            fontface = "bold",
            size = 3.4) + 
  labs(title = "Violence cases by alleged perpetrator", 
       x = "", 
       y = "Frequency") +
  theme_minimal() +
  scale_x_discrete(labels = c("Actor in conflict", 
                              "Known individual", 
                              "Ex-partner", 
                              "Family member", 
                              "Partner"))
non_fatal_data %>%   
  count(type_of_violence) %>%   
  mutate(proporcion = n / sum(n) * 100) %>%   
  ggplot(aes(x = type_of_violence, y = n)) +   
  geom_col(fill = "#AE86D3", alpha = 0.8) +  # Color de las barras   
  geom_text(aes(label = paste0(round(proporcion, 1), "%")),              
            vjust = -0.2,              
            color = "#4B0082",  # Morado oscuro             
            fontface = "bold",             
            size = 3.4) +  # Texto en negrita   
  labs(title = "Cases of Violence by Type", x = "", y = "Frecuency") +   
  theme_minimal() +   
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = c("Interpersonal violence",
                              "Domestic violence",
                              "Sexual violence",
                              "Sociopolitical violence"))

orden <- c("Niñas y adolescentes", "Jovenes", "Adultez")
a <- non_fatal_data %>%
  count(life_cycle_stage, type_of_violence) %>%
  group_by(type_of_violence) %>%
  mutate(proporcion = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  mutate(
    life_cycle_stage = factor(life_cycle_stage, levels = orden)
  ) %>%
  ggplot(aes(x = proporcion, y = life_cycle_stage, fill = type_of_violence)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = proporcion),
    position = position_dodge(width = 0.9),
    vjust = -0.5, 
    hjust = 0.5,        
    fontface = "bold",   
    size = 2.8
  ) +
  scale_fill_manual(
    values = c(
      "Violencia interpersonal" = "#fc9272",
      "Violencia intrafamiliar" = "#78c679",
      "Violencia sexual"       = "#1f9bcf",
      "Violencia sociopolítica"= "#c994c7"
    ),
    labels = c(
      "Violencia interpersonal" = "Interpersonal violence",
      "Violencia intrafamiliar" = "Domestic violence",
      "Violencia sexual"        = "Sexual violence",
      "Violencia sociopolítica" = "Sociopolitical violence"
    )
  ) +
  scale_y_discrete(labels = c(
    "Niñas y adolescentes" = "Girls and adolescents",
    "Jovenes"              = "Youth",
    "Adultez"              = "Adulthood"
  )) +
  labs(x = "%", y = NULL, fill = "Type of violence", title = "Life cycle") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_flip(clip = "off") +                               
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) 

a + theme(
  plot.title = element_text(hjust = 0)
)

