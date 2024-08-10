##graphique uni et bivariee----

library(tidyverse)
pacman::p_load(gtsummary,questionr,labelled, scales)

data("hdv2003")
look_for(hdv2003)
data("trial")
look_for(trial)

## une variable continue 

ggplot(hdv2003)+
  aes( x= age)+
  geom_histogram( binwidth = 5 , ## largeur des barres
                  fill = "orange",  ## couleur des barres
                  color = "black"   # contour des barres
                  )  

ggplot(hdv2003)+
  aes( x= age)+
  geom_histogram( binwidth = 5 , ## largeur des barres
                  fill = "orange",  ## couleur des barres
                  color = "black" ,  # contour des barres
                  breaks = c(18, seq( 20, 95, by = 5), 97)
  )

## une varible continue et une variable categorielle
ggplot(hdv2003)+
  aes( x= age)+
  geom_histogram( binwidth = 5 , ## largeur des barres
                  fill = "orange",  ## couleur des barres
                  color = "black"   # contour des barres
  )+
  facet_grid( rows =  vars(sexe) # presenter les graphiques en fonction du sexe et en ligne
              ) 


ggplot(hdv2003)+
  aes( x= age )+
  geom_histogram( binwidth = 5 , ## largeur des barres
                  fill = "orange",  ## couleur des barres
                  color = "black"   # contour des barres
  )+
  facet_grid( cols =  vars(sexe)  #en colonne
              )

ggplot(hdv2003)+
  aes(x= age, fill = sexe )+
  geom_histogram( color = "black")+
  facet_grid( cols =  vars(sexe)  #en colonne
  )

ggplot(hdv2003)+
  aes(x= age, fill = sexe, y = after_stat(density))+ ## Ajouter la densite
  geom_histogram( color = "black")+
  facet_grid( cols =  vars(sexe))  


ggplot(hdv2003)+
  aes(x= age, color =  sexe)+ 
  geom_density() ## Ajouter la courbe de densite

## Boxplot 

ggplot(hdv2003)+
  aes(x = sexe , y = age)+
  geom_boxplot()

ggplot(hdv2003)+
  aes(x = sexe , y = age, fill = sport)+
  geom_boxplot()



## diagramme de violon

ggplot(hdv2003)+
  aes(x = sexe , y = age, fill = sport)+
  geom_violin()

## Variables categorielles----

look_for(trial)

ggplot(trial)+
  aes( x = stage ,fill = grade )+
  geom_bar()

ggplot(trial)+
  aes( x = stage ,fill = grade )+
  geom_bar( position = "dodge") ## mettre cote a cote

ggplot(trial)+
  aes( x = stage ,fill = grade )+
  geom_bar( position = "fill")+ ## mettre en proportion
  scale_y_continuous( labels = label_percent ( suffix = " %")) # mettre en pourcentage


ggplot(trial)+
  aes( y = stage ,fill = grade )+
  geom_bar( position = "fill")+ ## mettre en proportion
  scale_x_continuous( labels = label_percent ( suffix = " %"))

ggplot(trial)+
  aes( x = stage ,fill = grade )+
  geom_bar()+
  geom_text( 
    mapping =  aes( label = after_stat(count)),
    stat =  "count",
    position =  position_stack(.5))

ggplot(trial)+
  aes( x = stage ,fill = grade , label = after_stat(count))+
  geom_bar( position = "dodge")+ ## mettre cote a cote
  geom_text( stat = "count", position =  position_dodge(.9))

ggplot(trial)+
  aes( x = stage ,fill = grade , label = after_stat(count))+
  geom_bar( position = "dodge")+ ## mettre cote a cote
  geom_text( stat = "count", position =  position_dodge(.9), vjust = 1)


library(GGally)

ggplot(trial)+
  aes( x = stage ,fill = grade ,
       label = percent(after_stat(prop), accuracy = 1),
       by = stage)+
  geom_bar( position = "fill")+
  geom_text( stat = "prop" , position = position_fill(.5))+
  facet_wrap( vars(trt))

