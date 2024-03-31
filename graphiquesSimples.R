#Projet 808

jo <- read.csv('C:/Users/axell/Documents/M1/808-Visualisation/projet/dataset_olympics.csv')

library(ggplot2)
library(dplyr)

#histogramme de la taille
ggplot(jo)+aes(x=Height, fill="lightblue")+geom_bar()

#histogramme du poids
ggplot(jo)+aes(x=Weight, fill="lightblue")+geom_bar(width=0.70)

#Plot du poids en fonction de la taille ? masculin/féminin
ggplot(jo)+aes(x=Height, y=Weight, color=Sex)+geom_point()

#Plot du poids en fonction de la taille, groupée par médailles
medailles <- dplyr::filter(jo, Medal %in% c("Gold","Silver","Bronze"))
ggplot(medailles)+aes(x=Height, y=Weight, color=Medal)+geom_point()

#regarder le nb de femmes par rapport au nb d’hommes dans le dataset en général
jo %>% count(Sex)
ggplot(jo)+aes(x=Sex, fill="lightblue")+geom_bar()

#le nb de pays participant par année/un graphique par saison
pays_par_annee <- jo %>% group_by(Year,Season) %>% summarise(nb_pays = n_distinct(Team))
ggplot(pays_par_annee)+aes(x=Year,y=nb_pays, fill="lightblue")+
  geom_bar(stat = "identity")+facet_wrap(~ Season)

#évolution du nb d’événements par année
event_par_annee <- jo %>% group_by(Year,Season) %>% summarise(nb_event = n_distinct(Event))
ggplot(event_par_annee)+aes(x=Year,y=nb_event, fill="lightblue")+
  geom_bar(stat = "identity")+facet_wrap(~ Season)

#évolution du nb de sport par année
sport_par_annee <- jo %>% group_by(Year,Season) %>% summarise(nb_sport = n_distinct(Sport))
ggplot(sport_par_annee)+aes(x=Year,y=nb_sport, fill="lightblue")+
  geom_bar(stat = "identity")+facet_wrap(~ Season)

#nb participant par sport 
ggplot(jo)+aes(x=Sport, fill="lightblue")+geom_bar()

#nb participant par pays 
ggplot(jo)+aes(x=Team, fill="lightblue")+geom_bar()

#age des participants en fonction des années
library(rAmCharts)
amBoxplot(Age ~ Year, data = jo)

#âge des participants par pays
amBoxplot(Age ~ Team, data = jo)

#âge des participants par sport
amBoxplot(Age ~ Sport, data = jo)

#nb de médailles
ggplot(medailles)+aes(x=Medal,fill="lightblue")+geom_bar()

#nb de médailles par age
ggplot(medailles)+aes(x=Age,fill="lightblue")+geom_bar()+ylab("Nb de médailles")

#nb de médailles par pays
nb_medal_pays <- medailles %>% count(Team)
nb_medal_pays <- nb_medal_pays[order(nb_medal_pays$n, decreasing = TRUE),]
nb_medal_pays[1:10,]
