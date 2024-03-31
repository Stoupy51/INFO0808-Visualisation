#Projet 808

path = "E:/Mes dossiers/3) Professionnel et Important/1) Scolarité/Master/INFO0808 - Visualisation/Projet/dataset_olympics.csv"
if (!file.exists(path)) { path = file.choose() }
jo = read.csv(path, header=TRUE, stringsAsFactors=TRUE, sep = ",")

library(ggplot2)
library(dplyr)
library(rAmCharts)

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
participations = dplyr::select(jo, Team)
nb_participant_pays <- participations %>% count(Team)
nb_participant_pays <- nb_participant_pays[order(nb_participant_pays$n, decreasing = TRUE),]
nb_participant_pays[1:10,]

#age des participants en fonction des années
amBoxplot(Age ~ Year, data = jo)

#âge des participants par pays
age_per_country = function(data, countries) {
  amBoxplot(Age ~ NOC, data = filter(data, NOC %in% countries))
}
age_per_country(jo, unique(jo$NOC)[1:10])

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
