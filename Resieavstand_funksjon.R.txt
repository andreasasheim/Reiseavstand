library(httr)
library(xml2)
library(XML)
library(rgdal)
library(tidyr)


SVVrouting <- function(fraX,fraY,tilX,tilY){
  # Funksjon som spr SVV om avstand og tid fra ett startpunkt til flere sluttpunkter
  #
  # Fra X og Y er N og E - koordinat for startpunktet
  # Til X og Y er lister av N og E - koordinater for sluttpunktene 
  # 
  # Mulige utvidelser: Kan ogsa sjekke reisetid avh. av trafikk og veiutbygging med kall "&startTime=yyyyMMddHHmmss"
  #                    Far ogsa informasjon om ferger osv.
  #
  # Ser ut til at ca 50 destinasjoner er godtatt med denne 
  # kan muligens okes med mindre presise posisjoner, lengden pa kallet er visst begrensningen
  
  n = length(tilX)
  
  # Paramtre fra SVV (UTM-sone, passord osv.)
  utm.prj = "+init=epsg:32633"
  pass    = 
  user    = // Trenger bruker og passord
  url     = "https://www.vegvesen.no/ws/no/vegvesen/ruteplan/routingservice_v2_0/routingservice/"
  suff    = "&format=xml&route_type=best"
  
  # oversett til UTM
  coord_fra <- SpatialPoints(cbind(fraX, fraY), proj4string = CRS("+proj=longlat"))
  coord_til <- SpatialPoints(cbind(tilX, tilY), proj4string = CRS("+proj=longlat"))
  fra_UTM   <- spTransform(coord_fra, CRS(utm.prj))
  til_UTM   <- spTransform(coord_til, CRS(utm.prj))
  
  # Koordinatstrenger som kan gis til SVV
  fra_string = paste(round(fra_UTM$fraX),round(fra_UTM$fraY),sep=",")
  til_string = paste(paste(round(til_UTM$tilX),round(til_UTM$tilY),sep=","),collapse=";")
  
  # Kall til databasen + autentiseringsstreng
  call = paste(url,paste("?from=",fra_string,sep=""),paste("&stops=",til_string,sep=""),suff,sep="")
  auth = paste("Basic", base64enc::base64encode(charToRaw(paste(user,pass,sep=":"))))
  
  result = GET(call,add_headers('Authorization' = auth))
  print(paste("API-kall returnerte kode",result[2]))
  
  root = xmlRoot(xmlParse(result,encoding = "UTF-8"))
  
  if (length(root[[2]])==0){
    # Henter ut resultatet
    D    = xmlToDataFrame(root[[1]])

    # Lag dataframe med resultatene
    d = data.frame("fraX" = rep(fraX,n),
                   "fraY" = rep(fraY,n),
                   "tilX" = tilX,
                   "tilY" = tilY,
                   "reisetid_min" = as.numeric(D$DurationSeconds)/60,
                   "distanse_km"  = as.numeric(D$DistanceMeters)/1000,
                   "Resultat"     = rep(paste("API-res.",result[2]),n))
  }else{
    # Hvis XML-dataen som kom tilbake var p? feil format
    
    print(paste("API-kall:",call))
    print("Fikk ikke fornuftig svar.")
    d = data.frame("fraX" = rep(fraX,n),
                   "fraY" = rep(fraY,n),
                   "tilX" = tilX,
                   "tilY" = tilY,
                   "reisetid_min" = rep(NA,n),
                   "distanse_km"  = rep(NA,n),
                   "Resultat"     = rep("Feil",n))
  }
  return(d)
}



