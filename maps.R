
## Libraries
library(ggplot2)
library(dplyr)

## Load data
path = "E:/Mes dossiers/3) Professionnel et Important/1) Scolarité/Master/INFO0808 - Visualisation/Projet/dataset_olympics.csv"
if (!file.exists(path)) { path = file.choose() }
data = read.csv(path, header=TRUE, stringsAsFactors=TRUE, sep = ",")


# Carte des médailles selon une plage d'années
medal_maps = function(data, years = NA) {

  # Restrict data
  restricted = filter(data, Medal %in% c("Gold","Silver","Bronze"))
  if (!is.na(years)) {
    restricted = filter(restricted, Year %in% years)
  }
  
  # Rename countries with "-XXX" like (France-2 to France)
  restricted$Team = gsub("-.*", "", restricted$Team)
  
  # Count medals for each countries
  per_country = data.frame(NOC = unique(restricted$NOC), nb_medals = 0)
  for (i in per_country$NOC) {
    index = per_country$NOC == i
    r_index = restricted$NOC == i
    per_country[index, "nb_medals"] = sum(r_index)
    Team = unique(restricted[r_index, "Team"])[1]
    per_country[index, "Team"] = Team
  }
  
  # Merge data with world map data
  world_map = map_data("world")
  for (i in per_country$Team) {
	  index = per_country$Team == i
	  nb_medals = sum(per_country[index, "nb_medals"])
	  world_indexes = i == world_map$region
	  if (sum(world_indexes) > 0) {
		  world_map[world_indexes, "nb_medals"] = nb_medals
	  }
	  else {
	    NOC = per_country$NOC[index]
		  world_indexes = i == world_map$subregion | NOC == world_map$region
		  world_indexes[is.na(world_indexes)] = FALSE
		  world_map[world_indexes, "nb_medals"] = nb_medals
	  }
  }
  world_map = world_map[order(world_map$order),]
  
  # Prepare the plot
  p = ggplot(world_map) + aes(x=long,y=lat,group=group,fill=nb_medals) +
	  geom_polygon() +
    scale_fill_gradient(low="yellow", high="brown")+
    theme_bw()
  
  # Add legends
  min_year = min(restricted$Year)
  max_year = max(restricted$Year)
  title = paste0("Nombre de médailles par pays (",min_year,")")
  if (min_year != max_year) {
    title = paste0("Nombre de médailles par pays (",min_year,"-",max_year,")")
  }
  p = p + labs(x = "Longitude", y = "Latitude", fill = "Nombre de médailles", title = title)
  
  # Print the plot
  p
}

# Carte de toutes les années puis l'année 2012
medal_maps(data)
medal_maps(data, 2012)
medal_maps(data, min(data$Year))
medal_maps(data, max(data$Year))







# Carte du nombre de participant selon une plage d'années
participations_map = function(data, years = NA) {

  # Restrict data
  restricted = filter(data, Medal %in% c("Gold","Silver","Bronze"))
  if (!is.na(years)) {
    restricted = filter(restricted, Year %in% years)
  }
  
  # Rename countries with "-XXX" like (France-2 to France)
  restricted$Team = gsub("-.*", "", restricted$Team)
  
  # Count for each countries
  per_country = data.frame(NOC = unique(restricted$NOC), nb_participants = 0)
  for (i in per_country$NOC) {
    index = per_country$NOC == i
    r_index = restricted$NOC == i
    per_country[index, "nb_participants"] = sum(r_index)
    Team = unique(restricted[r_index, "Team"])[1]
    per_country[index, "Team"] = Team
  }
  
  # Divide count by the number of participations per country
  min_year = min(restricted$Year)
  max_year = max(restricted$Year)
  for (i in per_country$NOC) {
    index = per_country$NOC == i
    participations = length(unique(restricted$Year[index]))
    per_country[index, "nb_participants"] = per_country[index, "nb_participants"] / participations
  }
  
  # Merge data with world map data
  world_map = map_data("world")
  for (i in per_country$Team) {
    index = per_country$Team == i
    nb_participants = sum(per_country[index, "nb_participants"])
    world_indexes = i == world_map$region
    if (sum(world_indexes) > 0) {
      world_map[world_indexes, "nb_participants"] = nb_participants
    }
    else {
      NOC = per_country$NOC[index]
      world_indexes = i == world_map$subregion | NOC == world_map$region
      world_indexes[is.na(world_indexes)] = FALSE
      world_map[world_indexes, "nb_participants"] = nb_participants
    }
  }
  world_map = world_map[order(world_map$order),]
  
  # Prepare the plot
  p = ggplot(world_map) + aes(x=long,y=lat,group=group,fill=nb_participants) +
    geom_polygon() +
    scale_fill_gradient(low="yellow", high="brown")+
    theme_bw()
  
  # Add legends
  title = paste0("Nombre moyen de participants par pays (",min_year,")")
  if (min_year != max_year) {
    title = paste0("Nombre moyen de participants par pays (",min_year,"-",max_year,")")
  }
  p = p + labs(x = "Longitude", y = "Latitude", fill = "Nombre moyen de participants", title = title)
  
  # Print the plot
  p
}
participations_map(data)
participations_map(data, 2012)
participations_map(data, min(data$Year))
participations_map(data, max(data$Year))

