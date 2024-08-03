## Analyse des donnees manquantes----

## Importation des packages----

pacman::p_load(
  rio,
  here,
  tidyverse,
  naniar, ## Bilan des donnees manquantes 
  mice ## Imputation 
)

## importation de la base de donnees----

linelist <-  import( here("data", "linelist_cleaned.rds"))
View(linelist)

##NA et ses derives----
linelist <- linelist %>% 
  
  # Créer une nouvelle colonne "age_years" à partir de la colonne "age"
  mutate(age_years = case_when(
    age_unit == "years"  ~ age,    # si l'unité est années => garder la valeur originale
    age_unit == "months" ~ age/12, # l'unité est en mois, diviser par 12
    is.na(age_unit)      ~ age,    # si l'unité est manquante, supposer que l'age est en années
    TRUE                 ~ NA_real_)) # sinon, définir age comme valeur manquante


z <- c(1, 22, NA, Inf, NaN, 5)
max(z)                           # retourne NA
max(z, na.rm=T)                  # retourne Inf
max(z[is.finite(z)])             # retourne 22

## supprimer toutes les valeurs manquantes dans linelist----

base <-  na.omit(linelist)
    ## ou
base <- linelist %>% 
  drop_na()

## Supprimer les lignes contenant les valeur manquantes de quelques variables----

base2 <- linelist %>% 
  drop_na(date_onset,hospital)

##Identification des valeurs manquantes dans tout le dataframe----

## pourcentage des donnees manquantes dans toute la base de donnees

pct_miss(linelist)

n_miss(linelist)

## pourcentage des lignes avec au moin une valeur manquante

pct_miss_case(linelist)

## Pourcentage de lignes sans valeur manquantes

pct_complete_case(linelist)

##Visualisation des valeurs manquantes 

gg_miss_var(linelist,
            show_pct = TRUE ## afficher avec les pourcentages
            )

linelist %>% 
  gg_miss_var(show_pct = TRUE,
              facet = outcome ##Visualiser suivant les modalite de la variales Outcome
              )
# Carte thermique de la complétude des données à l'échelle du dataframe

vis_miss(linelist)

##Exploration et Visualisation des relations entre donnees manquantes

ggplot(
  data = linelist,
  mapping = aes(x = age_years, y = temp)) +     
  geom_miss_point() ##Afficher les valeurs manquantes

gg_miss_fct(linelist, age_cat5) ##carte thermique du pourcentage de valeurs 
                            ##manquantes dans le dataframe pour chaque catégorie age_cat5

gg_miss_fct(linelist, date_onset)

##Colonnes “fantômes”----

shadowed_linelist <- linelist %>% 
  bind_shadow()

names(shadowed_linelist)

ggplot(data = shadowed_linelist,   # dataframe augmenté avec les colonnes fantômes
       mapping = aes(x = date_hospitalisation, # colonne numérique ou date
                     colour = age_years_NA)) + # colonne fantôme d'intérêt
  geom_density()                          # trace les courbes de densité

## Imputation des valeurs manquantes ----

#Imputation par la moyenne
linelist <- linelist %>%  ##Cas d'une variable quantitative
  mutate(temp_replace_na_with_mean = replace_na(temp, mean(temp, na.rm = T)))

linelist <- linelist %>% ##cas des variables categorielles
  mutate(outcome_replace_na_with_death = replace_na(outcome, "Death"))

# Imputation par regression

simple_temperature_model_fit <- lm(temp ~ fever + age_years, 
                                   data = linelist)

# Nous utilisons un modèle linéaire simple avec la température comme variable réponse pour prédire les valeurs de température manquantes

predictions_for_missing_temps <- predict(simple_temperature_model_fit,
                                         newdata = linelist %>%
                                           filter(is.na(temp))) 


# Utilisation du package mice

model_dataset <- linelist %>%
  select(temp, fever, age_years)  

temp_imputed <- mice(model_dataset,
                     method = "norm.predict",
                     seed = 1,
                     m = 1,
                     print = FALSE)

temp_imputed_values <- temp_imputed$imp$temp

 ##report a la derniere valeur observeee ( Utile pour les donnees longitudinales)

# Création d'un jeu de données
disease <- tibble::tribble(
  ~quarter, ~year, ~cases,
  "Q1",    2000,    66013,
  "Q2",      NA,    69182,
  "Q3",      NA,    53175,
  "Q4",      NA,    21001,
  "Q1",    2001,    46036,
  "Q2",      NA,    58842,
  "Q3",      NA,    44568,
  "Q4",      NA,    50197)

# Imputation ds données manquantes pour l'année (vers le bas par défaut)
disease %>% fill(year)
