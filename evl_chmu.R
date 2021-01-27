# Stažení dat ze stanic ČHMÚ----

library("purrr")
library(sf)

# this script is using station ids generated in "poloha_stanic_merge.R" also available in this repo 
setwd("your//directory")

# load output of previous script
stanice<-read.table("https://raw.githubusercontent.com/manmatej/chmu-process/master/stanice_ids.csv",sep = ";",stringsAsFactors = F,header = T) 
# extract station ids
id<-as.character(stanice$id)



## daily mean air temperature
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/T-AVG/" # url base
uri<-paste0(ur,id,"_T_N.csv.zip") # sign of mean temp
nam<-paste0("tmean_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip not existing urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "tmean_day_*") # load downaloded file names
downed<-data.frame(id=substr(files,11,18), # extract id, add flag the data exists 
                   tmean=rep("tmean",length(files)))

stanice1<-merge(stanice,downed,by="id",all.x = T) # merge with staion data

## daily max air temperature
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/TMA-21.00/" # url base
uri<-paste0(ur,id,"_TMA_N.csv.zip") # sign of max temp
nam<-paste0("tmax_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "tmax_day*") # load downaloded file names
downed<-data.frame(id=substr(files,10,17), # extract id, add sign of data exists 
                   tmax=rep("tmax",length(files)))

stanice2<-merge(stanice1,downed,by="id",all.x = T) # merge with sttaion data


## daily min air temperature
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/TMI-21.00/" # url base
uri<-paste0(ur,id,"_TMI_N.csv.zip") # sign of max temp
nam<-paste0("tmin_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "tmin_day*") # load downaloded file names
downed<-data.frame(id=substr(files,10,17), # extract id, add sign of data exists 
                   tmin=rep("tmin",length(files)))

stanice3<-merge(stanice2,downed,by="id",all.x = T) # merge with sttaion data

## daily mean air humidity
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/H-AVG/" # url base
uri<-paste0(ur,id,"_H_N.csv.zip") # sign of max temp
# writeClipboard(uri[10]) # test
nam<-paste0("hmean_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "hmean_day*") # load downaloded file names
downed<-data.frame(id=substr(files,11,18), # extract id, add sign of data exists 
                   humid=rep("humid",length(files)))

stanice4<-merge(stanice3,downed,by="id",all.x = T) # merge with sttaion data

# daily sum precipitation
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/SRA-07.00/" # url base
uri<-paste0(ur,id,"_SRA_N.csv.zip") # sign of max temp
writeClipboard(uri[1]) # test
nam<-paste0("prec_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "prec_day*") # load downaloded file names
downed<-data.frame(id=substr(files,10,17), # extract id, add sign of data exists 
                   prec=rep("prec",length(files)))

stanice5<-merge(stanice4,downed,by="id",all.x = T) # merge with sttaion data

# daily snow height
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/SCE-07.00/" # url base
uri<-paste0(ur,id,"_SCE_N.csv.zip") # sign of max temp
writeClipboard(uri[1]) # test
nam<-paste0("snow_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "snow_day*") # load downaloded file names
downed<-data.frame(id=substr(files,10,17), # extract id, add sign of data exists 
                   snow=rep("snow",length(files)))

stanice6<-merge(stanice5,downed,by="id",all.x = T) # merge with sttaion data

# daily sun duration
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/SSV-00.00/" # url base
uri<-paste0(ur,id,"_SSV_N.csv.zip") # sign of max temp
writeClipboard(uri[1]) # test
nam<-paste0("solr_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "solr_day*") # load downaloded file names
downed<-data.frame(id=substr(files,10,17), # extract id, add sign of data exists 
                   solr=rep("solr",length(files)))

stanice7<-merge(stanice6,downed,by="id",all.x = T) # merge with sttaion data


# daily mean wind speed
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/F-AVG/" # url base
uri<-paste0(ur,id,"_F_N.csv.zip") # sign of max temp
writeClipboard(uri[1]) # test
nam<-paste0("wind_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "wind_day*") # load downaloded file names
downed<-data.frame(id=substr(files,10,17), # extract id, add sign of data exists 
                   wind=rep("wind",length(files)))

stanice8<-merge(stanice7,downed,by="id",all.x = T) # merge with sttaion data

# daily mean air pressure
#############################
ur<-"http://portal.chmi.cz/files/portal/docs/meteo/ok/denni_data/P-AVG/" # url base
uri<-paste0(ur,id,"_P_N.csv.zip") # sign of max temp
writeClipboard(uri[1]) # test
nam<-paste0("pres_day_",id,".zip") # construct dwnload names

safe_download <- purrr::safely(~ download.file(.x , .y, mode = "wb",quiet = T)) # construct envelope to skip broken urls
purrr::walk2(uri, nam, safe_download) # walk through valid urls and save.

files<-list.files(pattern = "pres_day*") # load downaloded file names
downed<-data.frame(id=substr(files,10,17), # extract id, add sign of data exists 
                   pres=rep("pres",length(files)))

stanice9<-merge(stanice8,downed,by="id",all.x = T) # merge with sttaion data
stanice9$params<-apply(stanice9[,16:24], MARGIN = 1, function(x) sum(!is.na(x)))
stanice_sf<-st_as_sf(stanice9,coords = c("X", "Y"), crs = 4326 )

write_sf(stanice_sf,"stanice_data.gpkg")
write.table(stanice9,"stanice_data.csv",sep = ";",col.names = T,row.names = F)


# Unzip data ČHMÚ----
# read output of downloading script

loc<-"E:/_AOPK/_VLIVY"
zips<-list.files(loc,pattern = "*.zip$",full.names = T) # list paths to all zip files in current directory

unzips<-"E:/_AOPK/_VLIVY"
setwd(unzips)

for (i in 1:length(zips)){
  unzip(zips[i],exdir=unzips)  # unzip your file 
}


library(data.table)

## mean air temperature
files<-list.files(pattern = "*_T_N.csv$") # load files based on pattern

years<-lapply(files,fread,skip = "Měsíc",header = T, dec=",",select=1) # find absolute start + end date
min(unlist(lapply(years,min,na.rm=T)))
max(unlist(lapply(years,min,na.rm=T)))

# generate clear time series by day
prvni<-as.POSIXct("1961-01-01",tz="UTC") # start time
posledni<-as.POSIXct("2019-12-31",tz="UTC") # end time
cas<-seq(from=prvni,to=posledni, by=60*60*24) # generate clear time series

## cler data frame for joining data together
airTmean<-data.frame(date=cas)
airTmean$date<-as.character(airTmean$date)

records<-lapply(files,fread,skip = "Měsíc",header = T, dec=",",select=c(1:4)) 
# read all csv files starting with line where string "měsíc" is found
# didn not work with "Rok" because of "Rokytnice nad Jizerou"

for (i in 1:length(files)) {
  tm<-data.frame(records[[i]]) # single data frame
  loc<-substr(files[i],1,8) # staton id
  datum<-paste(tm$Rok,tm$Měsíc,tm$Den,sep="-") # construct posix date
  pos<-as.POSIXct(datum)
  cha<-as.character(pos) # convert to character, merge not works with posix
  tms<-data.frame(date=cha,mer=tm$Hodnota) # single station ready for merge
  names(tms)<-c("date",loc)
  airTmean<-merge(airTmean,tms,by="date",all.x = T) # final merge
}

save(airTmean,file="airTmean_merged.RData")

airTmean$P1JILS01

library(ggplot2)
ggplot(data = airTmean, aes(x = date, y = H1LUCB01)) +
  geom_point()
  
# Nalezení nejbližší stanice pro EVL----

library(leaflet)
library(rgdal)
library(raster)
library(sf)
library(rgeos)
library(tidyverse)

# Seznam EVL s předměty ochrany
sites_subjects <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/sites_subjects_utf.csv", encoding = "UTF-8")

# Mapová vrstva EVL
evl <- readOGR(dsn = ".", layer = "Evropsky_v%C3%BDznamn%C3%A9_lokality", encoding = "UTF-8")
evl_sf <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
evl_sf <- st_transform(evl, CRS("+init=epsg:4326"))

# Vyhledávání SITECODE EVL v nichž je druh předmětem ochrany
find_evl_SITECODE <- function(species){
  return(sites_subjects$SITECODE[which(grepl(species, sites_subjects$SUBJECT))])
}
# Vyhledávání NAZEV EVL v nichž je druh předmětem ochrany
find_evl_NAZEV <- function(species){
  return(sites_subjects$NAZEV[which(grepl(species, sites_subjects$SUBJECT))])
}

find_evl_SITECODE("Lucanus cervus")
target_evl_sf <- evl_sf[evl_sf$SITECODE %in% find_evl_SITECODE("Lucanus cervus"),]


find_STANICE_CHMU <- function(evl_nazev) {
  
  target_evl_stanice <- st_buffer(subset(evl_sf, evl_sf$NAZEV == evl_nazev), dist = 0)
  
  if (sum(st_intersects(stanice_mapa, target_evl_stanice, sparse = FALSE)) == 0){
    
    stanice_mapa[st_nearest_feature(target_evl_stanice, stanice_mapa),]$FNAME
    
  } else {
    
    st_intersection(stanice_mapa, target_evl_stanice)$FNAME
    
  }
  
}

find_STANICE_CHMU("Smědá")

evl_chmu_map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopo) %>%
  addPolygons(data = evl_sf,
              color = "green",
              fill = "green",
              weight = 1,
              opacity = .8,
              label = ~NAZEV) %>%
  addCircleMarkers(data = stanice_mapa,
              color = "blue",
              fill = "blue",
              weight = .1,
              opacity = .8,
              radius = 4,
              label = ~FNAME)
evl_chmu_map

library(htmlwidgets)
saveWidget(evl_chmu_map, file="evl_chmu_map.html")
