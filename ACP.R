## Premier pas dans l'ACP----

## Importation des packages----

pacman::p_load(tidyverse, ade4, FactoMineR, questionr, naniar, gtsummary, gridExtra)

## Importation de la base  de donnees----

data("iris")

## Analyse de la dimension et des types de variables----

str(iris)

look_for(iris)

## Analyse des valeurs manquantes et visualisation----

 #Pourcentage de valeur manquantes dans toute la base de donnees

pct_miss(iris)

 # visualisation

gg_miss_var(iris, show_pct = TRUE)

## Analyse descriptive de la base----
summary(iris)

# Analyse de dispersion et detection des outliers ( Ideal avec le boxplot)


base1 <- iris %>% 
  pivot_longer( cols = c("Sepal.Length","Sepal.Width", ## restructuration de la base
  "Petal.Width","Petal.Length")
                ,names_to = "Variable", values_to = "valeurs" )

ggplot(base1)+
  geom_boxplot( aes( x = Variable , y = valeurs)) ## Visualisation du boxplot

## Analyse de la normalite des variables d'interets

##Sepal.Length
p1 <- ggplot(iris ,aes_string( x = "Sepal.Length"))+
  geom_histogram( aes(y = ..density..), binwidth = 0.5, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", size = 1)

shapiro_test1 <- shapiro.test(iris$Sepal.Length) ## Valeur du test statistique

p1 <- p1 + annotate("text", x = Inf, y = Inf, label = paste("p-value:", round(shapiro_test1$p.value, 4)), 
             hjust = 1.1, vjust = 2, size = 4, color = "black")

##Sepal.Width
p2 <- ggplot(iris ,aes_string( x = "Sepal.Width"))+
  geom_histogram( aes(y = ..density..), binwidth = 0.5, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", size = 1)

shapiro_test2 <- shapiro.test(iris$Sepal.Width) ## Valeur du test statistique

p2 <- p2 + annotate("text", x = Inf, y = Inf, label = paste("p-value:", round(shapiro_test2$p.value, 4)), 
              hjust = 1.1, vjust = 2, size = 4, color = "black")

##Petal.Length

p3 <- ggplot(iris ,aes_string( x = "Petal.Length"))+
  geom_histogram( aes(y = ..density..), binwidth = 0.5, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", size = 1)

shapiro_test3 <- shapiro.test(iris$Petal.Length) ## Valeur du test statistique

p3 <- p3 + annotate("text", x = Inf, y = Inf, label = paste("p-value:", round(shapiro_test3$p.value, 4)), 
              hjust = 1.1, vjust = 2, size = 4, color = "black")
##Petal.Width

p4 <- ggplot(iris ,aes_string( x = "Petal.Width"))+
  geom_histogram( aes(y = ..density..), binwidth = 0.5, fill = "blue", alpha = 0.6) +
  geom_density(color = "red", size = 1)

shapiro_test4 <- shapiro.test(iris$Petal.Width) ## Valeur du test statistique

p4 <- p4 + annotate("text", x = Inf, y = Inf, label = paste("p-value:", round(shapiro_test4$p.value, 4)), 
              hjust = 1.1, vjust = 2, size = 4, color = "black")

grid.arrange(p1, p2, p3,p4, ncol = 2)
