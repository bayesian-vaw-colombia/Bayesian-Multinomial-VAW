# ==============================================================================
# NON-FATAL INJURIES DATA CLEANING AND PROCESSING
# ==============================================================================
# 
# Purpose: 
#   Clean and standardize non-fatal injury data from INMLCF (2015-2024)
#   focusing on Gender-Based Violence (GBV) cases against women in Colombia.
#
# Data Source:
#   Instituto Nacional de Medicina Legal y Ciencias Forenses (INMLCF)
#   Sexual violence
#   (https://www.datos.gov.co/Justicia-y-Derecho/Ex-menes-m-dico-legales-por-presunto-delito-sexual/hyqu-diue/about_data)
#   Interpersonal_violence
#   (https://www.datos.gov.co/Justicia-y-Derecho/Violencia-interpersonal-Colombia-a-os-2015-a-2023-/e3xi-4zq5/about_data)
#   Domestic violence
#   [Enlace](https://www.datos.gov.co/Justicia-y-Derecho/Violencia-intrafamiliar-Colombia-a-os-2015-a-2023-/ers2-kerr/about_data)
#   Non fatal injuries 2024
#   [(Ver)](https://www.datos.gov.co/Justicia-y-Derecho/Lesiones-no-fatales-de-causa-externa-Informaci-n-p/79dd-d24f/about_data)
#
# Output:
#   Cleaned dataset: non_fatal_data_model.csv
#
# ==============================================================================


# Load libraries required 

library(dplyr)
library(lubridate)
library(stringr)
library(stringi)
library(janitor)

## Load and prepare non-fatal injury datasets

# Load non-fatal injury datasets from INMLCF
# Each dataset corresponds to a different type or year range
non_fatal_2024 <- read.csv("data/raw/non_fatal_injuries_2024.csv")
sexual_2015_2023 <- read.csv("data/raw/sexual_violence_2015_2023.csv")
interpersonal_2015_2023 <- read.csv("data/raw/interpersonal_violence_2015_2023.csv")
domestic_2015_2023 <- read.csv("data/raw/domestic_violence_2015_2023.csv")

# The non-fatal injury datasets are composed as follows:
cat("Sexual violence dataset:", nrow(sexual_2015_2023), "rows and", ncol(sexual_2015_2023), "columns.\n")
cat("Interpersonal violence dataset:", nrow(interpersonal_2015_2023), "rows and", ncol(interpersonal_2015_2023), "columns.\n")
cat("Domestic violence dataset:", nrow(domestic_2015_2023), "rows and", ncol(domestic_2015_2023), "columns.\n")
 
# Regarding the 2024 dataset, it includes records that are not related to the Gender-Based Violence (GBV) indicator.  
# The GBV indicator comprises cases of suspect sexual offenses, interpersonal violence, and domestic violence.  
# Therefore, only records corresponding to these three categories were selected for further analysis.

## Check for inconsistent columns between datasets

# Get column names for all datasets
cols_2024 <- colnames(non_fatal_2024)
cols_sexual <- colnames(sexual_2015_2023)
cols_interpersonal <- colnames(interpersonal_2015_2023)
cols_domestic <- colnames(domestic_2015_2023)


## Data cleaning and filtering for 2024 dataset

# Keep only relevant categories for the VGB indicator
non_fatal_2024 <- non_fatal_2024 %>%
  filter(!violence_context %in% c(
    "7 Lesiones por Eventos de Transporte",
    "8 Lesiones Accidentales"
  ))


## Merge all datasets

# Merge all non-fatal injury datasets into a single dataframe
non_fatal_data <- bind_rows(
  non_fatal_2024,
  sexual_2015_2023,
  interpersonal_2015_2023,
  domestic_2015_2023
)

cat("Final merged dataset:", nrow(non_fatal_data), "rows and", ncol(non_fatal_data), "columns.\n")

## Standardize text information

#Function to replace "Sin Información" with "Sin información" in all text columns

replace_info <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) | is.factor(col)) {
      col <- ifelse(col == "Sin Información", "Sin información", col)
    }
    return(col)
  })
  return(df)
}

non_fatal_data <- replace_info(non_fatal_data)


## Selection of relevant variables

# Remove variables that only apply to accidental deaths or traffic incidents
# Also remove 'id' as it's not relevant for analysis
columns_to_remove <- c(
  "victim_condition",
  "transport_or_displacement_means",
  "vehicle_service_type",
  "accident_class_or_type",
  "collision_object",
  "collision_object_service",
  "id"
)

non_fatal_data <- non_fatal_data %>% 
  select(-all_of(columns_to_remove))

## Filter by gender

# Filter cases where victim is female (based on biological sex, not gender identity)
# Note: Gender identity is not available for all records
non_fatal_data <- non_fatal_data %>% 
  filter(victim_sex == "Mujer")

# Remove victim_sex column as all remaining records are female
non_fatal_data <- non_fatal_data %>% 
  select(-victim_sex)

# Keep only victims born in Colombia
non_fatal_data <- non_fatal_data %>%
  filter(victim_country_of_birth == "Colombia")

## Standardize violence context variable

# Rename violence_context levels to main categories
non_fatal_data <- non_fatal_data %>% 
  mutate(
    violence_context = case_when(
      # Domestic violence
      violence_context %in% c(
        "5 Lesiones no Fatales contra el Adulto Mayor por Violencia Intrafamiliar",
        "4 Lesiones no Fatales por Violencia entre otros Familiares",
        "3 Lesiones no Fatales contra Niños, Niñas y Adolescentes por Violencia Intrafamiliar",
        "Violencia Entre Otros Familiares (VIF)",
        "Violencia Contra el Adulto Mayor (VIF)",
        "Violencia Contra Niños, Niñas y Adolescentes (VIF)",
        "Violencia de Pareja",
        "6 Lesiones no Fatales por Violencia de Pareja"
      ) ~ "Violencia intrafamiliar",
      
      # Interpersonal violence
      violence_context %in% c(
        "1 Lesiones no Fatales por Violencia Interpersonal",
        "Violencia Interpersonal"
      ) ~ "Violencia interpersonal",
      
      # Suspect sexual crime
      violence_context %in% c(
        "2 Exámenes Medicolegales por Presunto Delito Sexual",
        "Presunto Delito Sexual"
      ) ~ "Presunto delito sexual",
      
      TRUE ~ NA_character_
    )
  )


## Standardize temporal variables

# Standardize month names 
non_fatal_data <- non_fatal_data %>% 
  mutate(month_of_event = str_to_title(month_of_event))

# Standardize day names 
non_fatal_data <- non_fatal_data %>% 
  mutate(day_of_event = str_to_title(day_of_event))

# Group into weekday vs. weekend
non_fatal_data <- non_fatal_data %>%
  mutate(
    day_of_event = if_else(
      day_of_event %in% c("Sábado", "Domingo"),
      "Fin de semana",
      "Semana"
    )
  )

# Standardize time range format (replace " a " with " - ")
non_fatal_data <- non_fatal_data %>% 
  mutate(time_range_3h = str_replace(time_range_3h, " a ", " - "))

# Group time ranges into: dawn, day, night
non_fatal_data <- non_fatal_data %>%
  mutate(
    time_of_the_event = case_when(
      time_range_3h == "Sin información" ~ "Sin información",
      
      # Dawn: 00:00 - 05:59
      time_range_3h %in% c("00:00 - 02:59", "03:00 - 05:59") ~ "Madrugada",
      
      # Day: 06:00 - 17:59
      time_range_3h %in% c(
        "06:00 - 08:59", "09:00 - 11:59",
        "12:00 - 14:59", "15:00 - 17:59"
      ) ~ "Dia",
      
      # Night: 18:00 - 23:59
      TRUE ~ "Noche"
    )
  )


# Remove locality variable
non_fatal_data <- non_fatal_data %>% 
  select(-event_locality)

# Standardize geographical area variable 
non_fatal_data <- non_fatal_data %>% 
  mutate(
    geographical_area = case_when(
      geographical_area == "Centro poblado(corregimiento, inspección de policía y caserío)" ~ 
        "Centro poblado (corregimiento, inspección de policía y caserío)",
      TRUE ~ geographical_area
    )
  )

## Standardize setting of the event variable 

non_fatal_data <- non_fatal_data %>% 
  mutate(setting_of_event = str_to_lower(setting_of_event)) %>% 
  mutate(
    setting_of_event = case_when(
      setting_of_event == "vehículo de servicio particular" ~ 
        "vehículo servicio particular",
      setting_of_event == "calle (autopista, avenida, dentro de la ciudad)" ~ 
        "vía pública",
      setting_of_event == "piscina, jacuzzi (establecimientos turísticos)" ~ 
        "piscina y jacuzzi (establecimientos turísticos, recreativos, deportivos)",
      setting_of_event == "medio de transporte masivo" ~ 
        "transporte masivo",
      TRUE ~ setting_of_event
    )
  )

# Simplify event scenario: home vs. social spaces
non_fatal_data <- non_fatal_data %>%
  mutate(
    setting_of_event = if_else(
      setting_of_event == "vivienda",
      "vivienda",
      "Espacios sociales"
    )
  )


## Standardize suspeted aggressor variable

# Define replacement patterns for suspect aggressor standardization
aggressor_replacements <- c(
  "(?i)^ex esposo ?\\(a\\)$" = "Ex-esposo(a)",
  "(?i)^ex compañero ?\\(a\\) permanente$" = "Ex-compañero(a) permanente",
  "(?i)^compañero ?\\(a\\) permanente$" = "Compañero(a) permanente",
  "(?i)^esposo ?\\(a\\)$" = "Esposo(a)",
  "(?i)^novio ?\\(a\\)$" = "Novio(a)",
  "(?i)^ex novio ?\\(a\\)$" = "Ex-novio(a)",
  "(?i)^ex amante$" = "Ex-amante",
  "(?i)^amigo ?\\(a\\)$" = "Amigo(a)",
  "(?i)^hijo ?\\(a\\)$" = "Hijo(a)",
  "(?i)^hermano ?\\(a\\)$" = "Hermano(a)",
  "(?i)^suegro ?\\(a\\)$" = "Suegro(a)",
  "(?i)^cuñado ?\\(a\\)$" = "Cuñado(a)",
  "(?i)^primo ?\\(a\\)$" = "Primo(a)",
  "(?i)^empleado ?\\(a\\)$" = "Empleado(a)",
  "(?i)^tío ?\\(a\\)$" = "Tío(a)",
  "(?i)^conocido sin ningun trato$" = "Conocido sin ningún trato",
  "(?i)^compañero ?\\(a\\) de trabajo$" = "Compañero(a) de trabajo",
  "(?i)^abuelo ?\\(a\\)$" = "Abuelo(a)",
  "(?i)^profesor ?\\(a\\)$" = "Profesor(a)",
  "(?i)^ex compañero ?\\(a\\) sentimental$" = "Ex-compañero(a) sentimental",
  "(?i)^compañero ?\\(a\\) de estudio$" = "Compañero(a) de estudio",
  "(?i)^ejercito$" = "Fuerzas Militares",
  "(?i)^sobrino ?\\(a\\)$" = "Sobrino(a)",
  "(?i)^compañero ?\\(a\\) de celda$" = "Compañero(a) de celda",
  "(?i)^compañero de celda$" = "Compañero(a) de celda",
  "(?i)^nieto ?\\(a\\)$" = "Nieto(a)",
  "(?i)^barra\\(s\\) futbolera\\(s\\)$" = "Barra(s) futbolera(s)",
  "(?i)^barras futboleras$" = "Barra(s) futbolera(s)",
  "(?i)^eln$" = "ELN",
  "(?i)^farc$" = "FARC",
  "(?i)^epl$" = "EPL",
  "(?i)^cti$" = "CTI",
  "(?i)^miembro del inpec$" = "Miembro del INPEC",
  "(?i)^hermanastro ?\\(a\\)$" = "Hermanastro(a)",
  "(?i)^hijastro ?\\(a\\)$" = "Hijastro(a)"
)

non_fatal_data <- non_fatal_data %>% 
  mutate(
    suspect_agressor = str_replace_all(
      str_to_lower(suspect_agressor), 
      aggressor_replacements
    ),
    suspect_agressor = str_replace(
      suspect_agressor, 
      "^(.)", 
      ~str_to_upper(.x)
    )
  )

## Create type of perpetrator variable

# Group aggressors by relationship type 
non_fatal_data <- non_fatal_data %>% 
  mutate(
    type_of_perpetrator = case_when( 
      # Current partner
      suspect_agressor %in% c(
        "Amante", "Novio(a)", "Compañero(a) permanente", 
        "Esposo(a)", "Pareja o expareja"
      ) ~ "Pareja",
      
      # Ex-partner
      suspect_agressor %in% c(
        "Ex-esposo(a)", "Ex-compañero(a) permanente", 
        "Ex-amante", "Ex-novio(a)", "Ex-compañero(a) sentimental"
      ) ~ "Ex-pareja",
      
      # Family member
      suspect_agressor %in% c(
        "Madrastra", "Padre", "Hijo(a)", "Tío(a)", "Primo(a)", 
        "Abuelo(a)", "Cuñado(a)", "Hermanastro(a)", "Hijastro(a)", 
        "Hermano(a)", "Padrastro", "Madre", 
        "Otros familiares civiles o consanguíneos", 
        "Sobrino(a)", "Suegro(a)", "Nuera", "Yerno", "Nieto(a)"
      ) ~ "Familiar",
      
      # Acquaintance
      suspect_agressor %in% c(
        "Compañero(a) de celda", "Compañero(a) de estudio", 
        "Compañero(a) de trabajo", "Conocido sin ningun trato", 
        "Curandero", "Empleador", "Profesor(a)", "Vecino", 
        "Conocido sin ningún trato", "Otros conocidos"
      ) ~ "Conocido",
      
      # Armed forces and police
      suspect_agressor %in% c(
        "Servicios de inteligencia", "Policía", "Fuerzas militares", 
        "Armada", "CTI", "Personal de custodia", "Miembro del INPEC", 
        "Policía judicial", "Fuerzas Militares"
      ) ~ "Miembros de las fuerzas armadas, de policía, policía judicial y servicios de inteligencia",
      
      # Illegal armed groups
      suspect_agressor %in% c(
        "Otras guerrillas", "ELN", "FARC", 
        "Fuerzas irregulares", "EPL", "Paramilitares"
      ) ~ "Miembro de grupos alzados al margen de la ley",
      
      # Organized crime
      suspect_agressor %in% c(
        "Narcotraficantes", "Pandillas", "Bandas criminales"
      ) ~ "Miembro de un grupo de la delincuencia organizada",
      
      # Caregiver
      suspect_agressor %in% c(
        "Encargado del niño, niña o adolescente",
        "Encargado de la persona mayor"
      ) ~ "Encargado del cuidado",
      
      # Other
      suspect_agressor %in% c(
        "Barra(s) futbolera(s)", "Curandero", "Cabezas rapadas", 
        "Metaleros", "Hoppers", "Punks"
      ) ~ "Otro",
      
      TRUE ~ suspect_agressor
    )
  )

# Group conflict actors and delincuency into a single category
non_fatal_data <- non_fatal_data %>%
  mutate(
    type_of_perpetrator = case_when(
      # Conflict actors and organized crime
      type_of_perpetrator %in% c(
        "Delincuencia común",
        "Miembro de grupos alzados al margen de la ley",
        "Miembro de un grupo de la delincuencia organizada",
        "Miembros de las fuerzas armadas, de policía, policía judicial y servicios de inteligencia"
      ) ~ "Actores del conflicto",
      
      # Caregivers and friends grouped as acquaintances
      type_of_perpetrator %in% c("Encargado del cuidado", "Amigo(a)") ~ "Conocido",
      
      # Keep other categories as is
      TRUE ~ as.character(type_of_perpetrator)
    )
  )

# Keep only relevant aggressor categories
relevant_aggressors <- c(
  "Pareja",
  "Familiar",
  "Ex-pareja",
  "Conocido",
  "Actores del conflicto"
)

non_fatal_data <- non_fatal_data %>%
  filter(type_of_perpetrator %in% relevant_aggressors)

## Standardize victim age

# Major/minor age classification
non_fatal_data <- non_fatal_data %>% 
  mutate(
    victim_age_major_minor_group = case_when(
      victim_age_major_minor_group == "a) Menores de Edad (<18 años)" ~ 
        "Menor de edad",
      victim_age_major_minor_group == "b) Mayores de Edad (>18 años)" ~ 
        "Mayor de edad",
      TRUE ~ victim_age_major_minor_group
    )
  )

# Remove records with undetermined age
non_fatal_data <- non_fatal_data %>% 
  filter(victim_age_major_minor_group != "Por determinar")

# Remove judicial_age variable (redundant with victim_age_group)
non_fatal_data <- non_fatal_data %>% 
  select(-judicial_age)

# Standardize age group format
non_fatal_data <- non_fatal_data %>% 
  mutate(
    victim_age_group = if_else(
      grepl("\\(\\d+\\s*(a|y)\\s*\\d+|más\\)", victim_age_group),
      gsub(
        "[()]", "", 
        gsub(
          "\\s*a\\s*", "-", 
          gsub("\\s*y\\s*más", "-más", victim_age_group)
        )
      ),
      victim_age_group
    )
  )

# Create detailed age groups (10-year ranges, except for 0-9 and 10-19)
non_fatal_data <- non_fatal_data %>%
  mutate(
    age_group_detailed = case_when(
      victim_age_group %in% c("00-04", "05-09") ~ "00-09",
      victim_age_group %in% c("10-14", "15-17", "18-19") ~ "10-19",
      victim_age_group %in% c("20-24", "25-29") ~ "20-29",
      victim_age_group %in% c("30-34", "35-39") ~ "30-39",
      victim_age_group %in% c("40-44", "45-49") ~ "40-49",
      victim_age_group %in% c("50-54", "55-59") ~ "50-59",
      TRUE ~ as.character(victim_age_group)
    )
  )

# Create life cycle categories
non_fatal_data <- non_fatal_data %>%
  mutate(
    life_cycle_stage = case_when(
      victim_age_group %in% c(
        "00-04", "05-09", "10-14", "15-17", "18-19"
      ) ~ "Niñas y adolescentes",
      
      victim_age_group %in% c("20-24", "25-29", "30-34") ~ "Jovenes",
      
      victim_age_group %in% c(
        "35-39", "40-44", "45-49", "50-54", "55-59"
      ) ~ "Adultez",
      
      TRUE ~ "Adulto mayor"
    )
  )

# Exclude elderly adults from analysis
non_fatal_data <- non_fatal_data %>%
  filter(life_cycle_stage %in% c(
    "Niñas y adolescentes",
    "Jovenes",
    "Adultez"
  ))

## Standardize sociedemographic variables 

# Education level
non_fatal_data <- non_fatal_data %>% 
  mutate(
    education_level = case_when(
      education_level == "Especialización, Maestría o equivalente" ~ 
        "Especialización, maestría o equivalente",
      TRUE ~ education_level
    )
  )

# Group education levels into: basic education vs. higher education
non_fatal_data <- non_fatal_data %>%
  mutate(
    education_level = case_when(
      # Basic education
      education_level %in% c(
        "Educación básica primaria",
        "Educación básica secundaria o secundaria baja"
      ) ~ "Educación básica primaria y secundaria",
      
      # Higher education
      education_level %in% c(
        "Doctorado o equivalente",
        "Educación técnica profesional y tecnológica",
        "Especialización, maestría o equivalente",
        "Universitario",
        "Educación media o secundaria alta"
      ) ~ "Educación superior",
      
      # Keep other categories unchanged
      TRUE ~ as.character(education_level)
    )
  )


## Standardize event circumstance variable

non_fatal_data <- non_fatal_data %>% 
  mutate(
    event_circumstance = case_when(
      event_circumstance == "Embriaguez (Alcohólica y no alcohólica)" ~ 
        "Embriaguez (alcohólica y no alcohólica)",
      event_circumstance == "Acto sexual violento con persona protegida" ~ 
        "Acceso carnal violento/acto sexual violento con persona protegida",
      event_circumstance == "Hurto" ~ "Atraco callejero o intento de",
      event_circumstance == "Ajuste de cuentas" ~ 
        "Venganza o ajuste de cuentas",
      event_circumstance == "Violencia Económica" ~ "Violencia económica",
      event_circumstance %in% c(
        "Presunta esclavitud sexual o prostitución forzada",
        "Presunta explotación sexual"
      ) ~ "Presunta explotación sexual y comercial",
      event_circumstance == "Violencia Sociopolitica" ~ 
        "Violencia sociopolítica",
      event_circumstance == "Desplazamiento forzado" ~ 
        "Abandono o despojo forzado de tierra",
      event_circumstance %in% c(
        "Retención ilegal", "Secuestro", "secuestro"
      ) ~ "Retención ilegal - secuestro",
      TRUE ~ event_circumstance
    )
  )

# Exclude circumstances not corresponding to GBV

non_gbv_circumstances <- c("Bala perdida", "Riña")

non_fatal_data <- non_fatal_data %>%
  filter(!event_circumstance %in% non_gbv_circumstances)

## Create type of violence classification variable

# Create comprehensive type of violence variable based on event circumstances
non_fatal_data <- non_fatal_data %>% 
  mutate(
    type_of_violence = case_when(
      # Domestic violence
      event_circumstance %in% c(
        "Conflicto de pareja", "Conflicto familiar", 
        "Violencia intrafamiliar", 
        "Violencia a niños, niñas y adolescentes",
        "Violencia al adulto mayor", "Violencia de pareja",
        "Violencia entre otros familiares"
      ) ~ "Violencia intrafamiliar",
      
      # Interpersonal violence
      event_circumstance %in% c(
        "Venganza o ajuste de cuentas", "Ajuste de cuentas", "Celos",
        "Abuso de autoridad", "Amenaza o intimidación", "Intolerancia",
        "Embriaguez (Alcohólica y no alcohólica)",
        "Embriaguez (alcohólica y no alcohólica)",
        "Contacto engañoso vía internet", "Ejercicio de actividades ilícitas",
        "Matoneo", "Retención legal", "Riña", "Tortura",
        "Aglomeración de público", "Bala perdida", "Sicariato",
        "Disturbios civiles", "Intervención Legal", "Linchamiento",
        "Minería ilegal", "Homofobia", "Incendio", "Negligencia"
      ) ~ "Violencia interpersonal",
      
      # Economic violence
      event_circumstance %in% c(
        "Violencia Económica", "Violencia económica", "Económicas",
        "Pillaje", "Hurto", "Atraco callejero o intento de"
      ) ~ "Violencia económica",
      
      # Sociopolitical violence
      event_circumstance %in% c(
        "Abandono o despojo forzado de tierra",
        "Acción bandas criminales",
        "Acción grupos alzados al margen de la ley",
        "Acción militar", "Acto terrorista",
        "Agresión contra grupos marginales o descalificados",
        "Artefacto explosivo", "Artefacto explosivo improvisado",
        "Asesinato político",
        "Ataque a instalación de las fuerzas armadas estatales",
        "Ataque contra misión médica", "Atentado terrorista",
        "Bombardeo", "Combate", "Cultivo ilícito",
        "Desaparición forzada", "Desplazamiento forzado",
        "Emboscada", "Enfrentamiento armado", "Explosión",
        "Hostigamiento", "Marcha o protesta social", "Masacre",
        "Mina Antipersonal - Munición sin Explotar",
        "Reclutamiento de niños, niñas y adolescentes",
        "Retención ilegal", "Retención ilegal - secuestro",
        "Violencia Sociopolítica", "Machismo",
        "Mina Antipersona - Munición sin Explotar",
        "Despojo forzado de tierra", "Violencia Sociopolitica"
      ) ~ "Violencia sociopolítica",
      
      # Sexual violence
      event_circumstance %in% c(
        "Acceso carnal violento",
        "Acceso carnal violento/acto sexual violento con persona protegida",
        "Agresión o ataque sexual", "Explotación sexual y comercial",
        "Abuso sexual", "Acoso sexual", "Acto sexual",
        "Acto sexual violento", "Acto sexual violento con persona protegida",
        "Asalto sexual", "Desnudez forzada",
        "Obligar a presenciar actos sexuales",
        "Obligar a realizar actos sexuales", "Pornografía",
        "Presunta esclavitud sexual o prostitución forzada",
        "Presunta explotación sexual",
        "Presunta trata de personas con fines de explotación sexual",
        "Tráfico o trata de personas",
        "Violencia sexual en el contexto del conflicto armado",
        "Presunta explotación sexual de niños, niñas o adolescentes",
        "Presunta explotación sexual y comercial",
        "Abuso dentro de establecimiento prestador de servicios de salud",
        "Aborto forzado", "Embarazo forzado"
      ) ~ "Violencia sexual",
      
      # Missing information
      event_circumstance == "Sin información" ~ "Sin informacion",
      
      # Other types
      event_circumstance %in% c(
        "Abandono", "Consumo de alcohol y/o sustancias psicoactivas",
        "Enfermedad física o mental", "Infidelidad"
      ) ~ "Otra",
      
      TRUE ~ event_circumstance
    )
  )

# Keep only specific violence types relevant to GBV
violence_types_to_keep <- c(
  "Violencia sexual",
  "Violencia interpersonal",
  "Violencia intrafamiliar",
  "Violencia sociopolítica"
)

non_fatal_data <- non_fatal_data %>%
  filter(type_of_violence %in% violence_types_to_keep)

## Convert all variables to factors

non_fatal_data <- non_fatal_data %>% 
  mutate(across(everything(), as.factor))

# Select only relevant variables for statistical modeling

modeling_data <- non_fatal_data %>%
  select(
    type_of_violence,
    life_cycle_stage,
    education_level,
    day_of_event,
    time_of_the_event,
    setting_of_event,
    geographical_area,
    type_of_perpetrator
  )

write.csv(modeling_data, "data/processed/non_fatal_data_model.csv", row.names = FALSE)

