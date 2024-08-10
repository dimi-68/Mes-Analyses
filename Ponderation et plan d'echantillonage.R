# Données ponderées et plan d'échantillonage----
pacman::p_load( tidyverse, questionr, survey,labelled)

## Importation de la base de donnees-----

data(hdv2003)


## Simuler les strates et les grappes----

hdv2003$grappe <-  rep.int( 1: 25 , times = 80)

hdv2003 <- hdv2003 %>% 
  arrange(grappe)

hdv2003$strate <-  "a"
hdv2003$strate[801 : 1200] <- "b"
hdv2003$strate[1201 : 2000] <- "c"

xtabs( ~grappe + strate, data = hdv2003)

## definitions du plan d'echantillonage

dp <-  svydesign( ids = ~1 , weights =  ~poids, data = hdv2003)

dp_strates <-  svydesign( ids = ~ 1, weights =  ~poids ,  strata =  ~ strate, data = hdv2003)

dp_grappe <-  svydesign( ids = ~ grappe , weights =  ~ poids , data = hdv2003)

dp_strates_grappes <-  svydesign( ids = ~grappe, weight  = ~ poids, data = hdv2003, strata =  ~ strate)

## pour extraire  certaines valeurs specifiques on peut utiliser la fonction subset

dp_femme <- subset(dp, sexe == "Femme") ### Filtrer sur les femmes


# tableau croise et test du Chi2-----

xtabs( ~ sexe + cuisine , data = hdv2003)

xtabs( poids ~ sexe + cuisine , data = hdv2003)

tab <- xtabs( poids ~ sexe + cuisine , data = hdv2003)

rprop(tab)

chisq.test(tab) # mauvaise methode !!!!!

tab <-  svytable(~ sexe + cuisine, design =  dp)

tab

rprop(tab)

svychisq( ~ sexe + cuisine, design =  dp)

# gtsummary : tbl_svysummary----

library(gtsummary)

dp %>% 
  tbl_svysummary( include =  c( "sexe" , "sport", "cuisine"))

dp %>% 
  tbl_svysummary( include =  c( "sexe" , "sport", "cuisine"),
                  by = "sexe", statistic =  all_categorical() ~ "{p}% ( obs : {n_unweighted})"
                  ) %>% 
  modify_header( update =  all_stat_cols() ~ " **{level}**, N = {n_unweighted}") %>% 
  add_overall( last = TRUE, col_label = " **Total** , N = { N_unweighted}") %>% 
  add_p()

# Graphiques avec ggplot2----

library(GGally)

ggplot( hdv2003)+
  aes( x =  sexe , weight = poids, fill = sport)+
  geom_bar( position = "fill")+
  geom_text( stat =  "prop", position =  position_fill(.5) )

questionr:: ggsurvey(dp)+
  aes( x =  sexe , fill = sport)+
  geom_bar( position = "fill")+
  geom_text( stat =  "prop", position =  position_fill(.5) )

