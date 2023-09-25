library(httr)
library(xml2)
library(rgdal)
library(dplyr)

source("reiseavstand_funksjon.R")

kommuner = xlsx::read.xlsx2("kommuner_lat_lon.xls",1,header = F,)
names(kommuner) = c("kommune","kommunenr","lat","lon")
kommuner$lat = as.numeric(as.character(kommuner$lat))
kommuner$lon = as.numeric(as.character(kommuner$lon))

sykehus = xlsx::read.xlsx2("hospitals_lat_lon.xlsx",1,header = T,)
sykehus$lat = as.numeric(as.character(sykehus$lat))
sykehus$lon = as.numeric(as.character(sykehus$lon))


sykehus <- sykehus %>% filter(!is.na(lat))

df = data.frame()
for(j in 1:nrow(kommuner)){
  print(paste(j,":",kommuner$kommune[j],kommuner$lat[j],kommuner$lon[j]))
  d  = SVVrouting(kommuner$lon[j],kommuner$lat[j],sykehus$lon,sykehus$lat)
  d  = d %>% select(reisetid_min,distanse_km, Resultat) %>% 
    mutate(sykehusnavn = sykehus$name,
           sykehusnr   = sykehus$hospital_id,
           kommune     = kommuner$kommune[j],
           kommunenr   = kommuner$kommunenr[j])
  df = rbind(df,d)
}
print("Ferdig")

write.csv(df,"Resultat_routing_kommune_sykehus.csv")

