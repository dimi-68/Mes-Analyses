## Analyse Factorielle----



# Chargement des packages----

library(tidyverse)
library(ade4)
library(factoextra)
pacman:: p_load(explor)

# Anayse en composantes principales----

# Importation de la base de donnees

iris <- iris
# visualisation du nuage de points
ggplot(iris)+
  aes( x = Petal.Length , y = Petal.Width)+
  geom_point()

# selection des variables et creaction des composantes principales
resultat <- iris %>% 
  
  select(starts_with("Petal")) %>%
  
  dudi.pca( nf = 2 ,  scannf = FALSE) # nf = 2 indique qu'il y' aura 2 composantes 
                                      # car 2 variables on ete choisi
# utiliser la fonction explor()  du package **explor** pour visualiser l'ensemble des resultats de l'ACP

#explor(resultat)
res <- explor::prepare_results(resultat)
explor::PCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     
                     ind_lab_min_contrib = 0, col_var = NULL, labels_size = 9, point_opacity = 0.5,
                     
                     opacity_var = NULL, point_size = 64, ellipses = FALSE, transitions = TRUE,
                     
                     labels_positions = NULL, xlim = c(-3.44, 4.65), ylim = c(-4.9, 3.19))


# Analyse des correspondances multiples

library(questionr) # chargement du package

data("hdv2003") # Importation du jeu de donnees

# recodage des variables
hdv2003$grpage <- cut(hdv2003$age, c(16, 25, 45, 65, 93), right = FALSE, include.lowest =
                  TRUE)

hdv2003$etud <- hdv2003$nivetud

levels(hdv2003$etud) <- c(

    "Primaire", "Primaire", "Primaire", "Secondaire", "Secondaire",
  
    "Technique/Professionnel", "Technique/Professionnel", "Supérieur"
)

# selection des variables et creaction des axes factorielles pour l'ACM
acm <- hdv2003 %>% 
  
  select(grpage,sexe,peche.chasse,cinema,cuisine,bricol,sport,lecture.bd, etud)%>%
  
  dudi.acm(scannf = FALSE, nf = Inf)

# explor(acm)
  
screeplot(acm)

fviz_screeplot(acm, choice = "eigenvalue")

fviz_screeplot(acm) # Voir graphiquement le pourcentage de variance expliquee par chaque axe factorielle

summary(acm) # Voir le pourcentage de variance expliquee par chaque axe factorielle

s.corcircle(acm$co, clabel = .7) # faire un cercle de correlation avec le package ade4

fviz_mca_var(acm, repel = TRUE)  #  meme chose mais avec le package factoextra

fviz_contrib( acm, choice = "var" , axes =  1) # contribution des variables au premier axe

fviz_contrib( acm, choice = "var" , axes =  2) # contribution des variables au deuxieme axe
