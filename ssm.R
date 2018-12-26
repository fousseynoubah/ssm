##########################################################################
########################################/// CODE POUR LE TRAITEMENT DES DONNEES SUR LA SITUATION SECURITAIRE AU MALI ///
########################################/// CODES TO PROCESS DATA ON SECURITY INCIDENTS IN MALI ///
##########################################################################

##########################################################################
########################################/// FUSEAU HORAIRE / TIME ZONE  ///
##########################################################################
Sys.setenv(TZ="Africa/Bamako") #Sys.getenv("TZ") # verifier / to check

##########################################################################
########################################/// PACKAGES ///
##########################################################################
library(tidyverse)
library(readxl)
library(downloader)
library(rgdal)
library(sf)


##########################################################################
########################################/// CARTOGRAPHIE : TELECHARGEMENT ET TRANSFORMATION DES SHAPEFILES
########################################/// MAPPING : DOWNLOADING AND TRANSFORMING THE SHAPEFILES
##########################################################################
### Data frame
map_df <- data_frame(name = c("adm1", "adm2", "adm3"),
                     link = c("https://data.humdata.org/dataset/3feaf6d7-8b21-4db1-a097-fa8a2b680a89/resource/1f4755a2-b3d7-4634-9273-430048d40684/download/mli_admbnda_adm1_pop_2017.zip",
                              "https://data.humdata.org/dataset/3feaf6d7-8b21-4db1-a097-fa8a2b680a89/resource/bc251c53-9c78-48b2-ab8f-c92d9269faf2/download/mli_admbnda_adm2_pop_2017.zip",
                              "https://data.humdata.org/dataset/3feaf6d7-8b21-4db1-a097-fa8a2b680a89/resource/303f57d8-965f-4c70-a081-3e5e0fcba2bf/download/mli_admbnda_adm3_pop_2017.zip"))

### Télécharger et décompresser / Download and unzip
for(i in 1:nrow(map_df)){
  download(paste(map_df[i, "link"]), 
           dest = paste(map_df[i, "name"], "zip", sep = "_"), 
           mode="wb") 
  unzip(paste(map_df[i, "name"], "zip", sep = "_"),
        exdir = paste(map_df[i, "name"]))
  file.remove(paste(map_df[i, "name"], "zip", sep = "_"))
  rm(i)
}

### Transformation des shapefiles / Transforming the shapefiles
shp_list <- list()

for(i in 1:nrow(map_df)){
  shp_list[[i]] <- st_read(paste0(map_df[i, "name"],"/","mli_admbnda_adm", i ,"_pop_2017.shp"))
}

map_adm1_sf <- shp_list[[1]]
map_adm2_sf <- shp_list[[2]]
map_adm3_sf <- shp_list[[3]]

### Remove unneeded objects
#Zip files
for(i in 1:nrow(map_df)){
  unlink(paste(map_df[i, "name"]), recursive = TRUE)
  rm(i)
}

# Objects in working environment
rm(map_df, shp_list)

##########################################################################
########################################/// CARTOGRAPHIE : COORDONNEES
########################################/// MAPPING : COORDINATES
##########################################################################
### Chargement des données sur les entités administratives - données originales tirée de : https://data.humdata.org/dataset/mali-settlements
### Loading data on administrative units into the working environment - data from: https://data.humdata.org/dataset/mali-settlements
#coordinates <- rio::import("https://github.com/fousseynoubah/ssm/blob/master/localites.xlsx?raw=true") # remote solution


localites <- read_excel("localites.xlsx") %>% 
  select(id, longitude, latitude, featureNam, featureRef, admin1Pcod, admin1Name, admin2Pcod, admin2Name, admin3Pcod, admin3Name, popPlace_1)  %>%
  rename(nom = featureNam, ref = featureRef, adm1_cod = admin1Pcod, adm1_name = admin1Name, adm2_cod = admin2Pcod, adm2_name = admin2Name, adm3_cod = admin3Pcod, adm3_name = admin3Name, type = popPlace_1) %>%
  mutate(id = as.numeric(id),
         type_fr = case_when(type == "National Capital" ~  "Capitale",
                          type == "Admin1 Capital" ~ "Chef-lieu de région",
                          type == "Admin2 Capital" ~ "Chef-lieu de cercle",
                          type == "Admin3 Capital" ~ "Chef-lieu de commune",
                          is.na(type) ~ "Autre"),
         type = ifelse(!is.na(type), type, "Other"))

##########################################################################
########################################/// LES DONNEES SUR LES INCIDENTS ///
########################################/// DATA ON THE INCIDENTS ///
##########################################################################
### Liste des incidents (version organisée) depuis GitHub / List of incidents (preprocessed version) from GitHub
#incidents <- rio::import("https://github.com/fousseynoubah/ssm/blob/master/liste_incidents.xlsx?raw=true")
incidents <- read_excel("liste_incidents.xlsx", sheet = "INCIDENTS")

incidents <- incidents %>% 
  mutate(date = as.Date(paste(annee, mois, jour, sep = "-"))) %>% 
  filter(!is.na(date), origin == "webpage") # garder comme tel jusqu'à la fin de la consolidation / keep that until end of consolidation

### Incidents ayant eu lieu dans des points connus / Incidents that took place in known places
incidents.point <- 
  incidents %>%
  filter(!is.na(annee)) %>% # exclusion des incidents sans date / excluding incidents with no dates 
  filter(!is.na(point)) %>% # exclusion des incidents dont les points ne sont pas spécifiés / excluding incidents with no coordinates
  select(-c(depart_id, depart, arrivee_id, arrivee) )  %>% # exclusion des incidents de lignes / excluding incidents with unknow coordinates
  rename(id = point_id) %>%
  mutate(id = as.numeric(id)) %>% # s'assurer c'est compatible avec le format de "id" dans "localites" / making sure it matches the "id" from "localites" dataframe
  left_join(localites, by = c("id")) %>%  # ajout des informations sur les localités
  mutate(longitude = ifelse(point == "Frontière Boulkessi", -1.0622, longitude),
         latitude = ifelse(point == "Frontière Boulkessi", 15.1364, latitude),
         nom = ifelse(point == "Frontière Boulkessi", "Frontière Boulkessi", nom),
         longitude = ifelse(point == "Almoustarat", 0.087, longitude),
         latitude = ifelse(point == "Almoustarat", 17.367, latitude),
         nom = ifelse(point == "Almoustarat", "Frontière Boulkessi", nom)
  )

### Incidents ayant eu lieu entre  points connus / Incidents that took place between two known places
incidents.depart <- 
  incidents %>%
  filter(!is.na(annee)) %>%
  filter(!is.na(depart)) %>% 
  select(-c(point_id, point) ) %>% 
  select(-c(arrivee_id, arrivee) ) %>% 
  rename(id = depart_id, point = depart) %>%
  mutate(position = "départ") 

incidents.arrivee <- 
  incidents %>%
  filter(!is.na(annee)) %>%
  filter(!is.na(depart)) %>%
  select(-c(point_id, point) ) %>%
  select(-c(depart_id, depart) ) %>%
  rename(id = arrivee_id, point = arrivee) %>% mutate(id = as.numeric(id)) %>% # investigate source later
  mutate(position = "arrivée") # pas sûr, mais moyen de séparer les deux lignes dans la table longue

incidents.line <- 
  incidents.depart %>% 
  bind_rows(incidents.arrivee) %>%
  left_join(localites, by = c("id")) %>% # ajout des informations sur les localités
  mutate(longitude = ifelse(point == "Frontière Boulkessi", -1.0622, longitude),
         latitude = ifelse(point == "Frontière Boulkessi", 15.1364, latitude),
         nom = ifelse(point == "Frontière Boulkessi", "Frontière Boulkessi", nom),
         longitude = ifelse(point == "Almoustarat", 0.087, longitude),
         latitude = ifelse(point == "Almoustarat", 17.367, latitude),
         nom = ifelse(point == "Almoustarat", "Frontière Boulkessi", nom)
  )

rm(incidents.depart, incidents.arrivee)

## Extraction des informations sur les victimes / Extracting the info on the victims
victimes.stat <- incidents %>%
  select(no_liste, stat_morts_1, count_morts_1, stat_morts_2, count_morts_2, stat_morts_3, count_morts_3, 
         stat_blesses_1, count_blesses_1, stat_blesses_2, count_blesses_2, stat_blesses_3, count_blesses_3) %>%
  gather(key = key, value = value, - no_liste) %>%
  mutate(type = ifelse(grepl(pattern = "stat", x = key), "Statut", "Nombre"),
         categorie = ifelse(grepl(pattern = "morts", x = key), "Morts", "Blessés"),
         groupe = ifelse(grepl(pattern = "1", x = key), 1,
                         ifelse(grepl(pattern = "2", x = key), 2, 3))) %>%
  filter(type == "Statut") %>%
  select(-c(key, type)) %>%
  select(no_liste, categorie, value, groupe)  %>%
  rename(statut = value)

victimes.nbr <- incidents %>%
  select(no_liste, stat_morts_1, count_morts_1, stat_morts_2, count_morts_2, stat_morts_3, count_morts_3, 
         stat_blesses_1, count_blesses_1, stat_blesses_2, count_blesses_2, stat_blesses_3, count_blesses_3) %>%
  gather(key = key, value = value, - no_liste) %>%
  mutate(type = ifelse(grepl(pattern = "stat", x = key), "Statut", "Nombre"),
         categorie = ifelse(grepl(pattern = "morts", x = key), "Morts", "Blessés"),
         groupe = ifelse(grepl(pattern = "1", x = key), 1,
                         ifelse(grepl(pattern = "2", x = key), 2, 3))) %>%
  filter(type == "Nombre") %>%
  select(-c(key, type)) %>%
  select(no_liste, categorie, value, groupe)  %>%
  rename(nombre = value)

victimes <- merge(victimes.stat, victimes.nbr, by = c("no_liste", "categorie", "groupe")) %>%
  select(-c(groupe)) %>%
  filter(!is.na(nombre))  %>% # exclusion des incidents qui n'ont pas fait de victimes / excluding incidents with no victims
  mutate(nombre = as.numeric(nombre), 
         categorie = factor(categorie, levels = c("Morts", "Blessés"), labels = c("Morts", "Blessés"), ordered = TRUE), 
         corps = "ND") %>% # ND jusqu'à spécification / ND until more specifications made
  left_join(incidents, by = c("no_liste")) %>% # importation des dates via "no_liste" / importing the dates through "no_liste"
  select(no_liste, date, categorie, statut, corps, nombre)

rm(victimes.stat, victimes.nbr)

### Création d'une variable "corps" pour agréger les statuts / Creating a variable 'corps' to agregate the status of the victims
# Travail en cours / working in progress
minusma <- c("casque bleu", "MINUSMA") #remplir ce vecteur avec les mots cl?s
fama <- c("soldat", "militaire", "policier", "douanier") # remplir ce vecteur avec les mots cl?s
assaillants <- c("assaillant", "djihadiste", "terroriste", "MNLA", "CMA")
civils <- c("civil", "maire")
list_corps <- list(minusma) # fama, assaillants, civils

for(i in minusma){
  victimes$corps <- ifelse(grepl(pattern = i, x = victimes$statut), "MINUSMA", victimes$corps)
}

for(i in fama){
  victimes$corps <- ifelse(grepl(pattern = i, x = victimes$statut), "FAMA", victimes$corps)
}
for(i in assaillants){
  victimes$corps <- ifelse(grepl(pattern = i, x = victimes$statut), "Assaillants", victimes$corps)
}
for(i in civils){
  victimes$corps <- ifelse(grepl(pattern = i, x = victimes$statut), "Civils", victimes$corps)
}
rm(list_corps, minusma, fama, assaillants, civils)

rm(i)

######################################################################################################
### SAUVEGARDE DES DONNEES
######################################################################################################

save.image("ssm_data.RData")

