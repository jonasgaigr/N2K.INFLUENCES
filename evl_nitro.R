# LOAD PACKAGES ----
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)}

if(!isTRUE(require(sf, quietly = TRUE))) {
  install.packages("sf", dependencies = TRUE); library(sf)
} else {
  require(sf)}

if(!isTRUE(require(sp, quietly = TRUE))) {
  install.packages("sp", dependencies = TRUE); library(sp)
} else {
  require(sp)}

if(!isTRUE(require(proj4, quietly = TRUE))) {
  install.packages("proj4", dependencies = TRUE); library(proj4)
} else {
  require(proj4)}

if(!isTRUE(require(ggradar, quietly = TRUE))) {
  install.packages("ggradar", dependencies = TRUE); library(ggradar)
} else {
  require(ggradar)}

if(!isTRUE(require(leaflet, quietly = TRUE))) {
  install.packages("leaflet", dependencies = TRUE); library(leaflet)
} else {
  require(leaflet)}

# DEPOZICE DUSÍKU ----
results_habitats_read <- read.csv2("S:/Gaigr/hodnoceni_stanovist_grafy/results_habitats_fin.csv")
results_habitats_read[is.na(results_habitats_read)] <- 0

habitat_hodnoceni_vyber <- results_habitats_read %>%
  group_by(HABITAT_CODE, SITECODE) %>%
  mutate(HABITAT_NAZEV = find.habitat.NAME_CZ(unique(HABITAT_CODE)))

write.csv2(habitat_hodnoceni_vyber,
           "S:/Gaigr/hodnoceni_stanovist_grafy/habitat_hodnoceni_vyber.csv",
           row.names = FALSE)
nempir <- raster::raster("Nempir.tif")
ntotal2017 <- read_stars("ntot2017.tif")
ntotal2017 %>% plot 
ndepo2017 <- st_as_sf(ntotal2017)
ndepo_semin <- st_intersection(filter(evl_sjtsk, NAZEV == "Semínský přesyp"), ndepo2017)
ndepo_semin <- ndepo_semin %>% 
  mutate(depo_segment = ntot2017.tif*st_area(geometry)/sum(st_area(geometry)))
ndepo_semin$depo_segment %>% sum

karpaty_6210p_depo <- vmb_shp_sjtsk %>%
  st_intersection(filter(evl_sjtsk, NAZEV == "Bílé Karpaty")) %>%
  filter(HABITAT == "6210p") %>%
  st_intersection(., ndepo2017) %>%
  mutate(AREA_real = units::drop_units(st_area(geometry))) %>%
  mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA) %>%
  mutate(depo_segment = ntot2017.tif*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL))
plot(karpaty_6210p_depo$geometry)

krkonose_6520_depo <- vmb_shp_sjtsk %>%
  st_intersection(filter(evl_sjtsk, NAZEV == "Krkonoše")) %>%
  filter(HABITAT == "6520") %>%
  st_intersection(., ndepo2017) %>%
  mutate(AREA_real = units::drop_units(st_area(geometry))) %>%
  mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA) %>%
  mutate(depo_segment = ntot2017.tif*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL))

krkonose_6520_depo %>%
  mutate(TYP_DRUHY_SEG = case_when(TD == "N" ~ 0, # PŘEPOČET TYPICKÝCH DRUHŮ NA SEGMENTU
                                   TD == "MP" ~ 5,
                                   TD == "P" ~ 10),
         TD_SEG = TYP_DRUHY_SEG*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL),
         QUAL = case_when(KVALITA == 1 ~ 1,
                          KVALITA == 2 ~ 1,
                          KVALITA == 3 ~ 2,
                          KVALITA == 4 ~ 2)) %>%
  group_by(QUAL)

habdepo <- function(hab_code, evl_site) {
  
  vmb_target_sjtsk <- vmb_shp_sjtsk %>%
    st_intersection(filter(evl_sjtsk, NAZEV == evl_site)) %>%
    filter(HABITAT == hab_code) %>%
    st_intersection(., ndepo2017) %>%
    mutate(AREA_real = units::drop_units(st_area(geometry))) %>%
    mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA) %>%
    mutate(depo_segment = ntot2017.tif*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL))
  
  return(as.numeric(sum(vmb_target_sjtsk$depo_segment)))
  
}

habdepo("6210", "Bílé Karpaty")
find.habitat.NAME_CZ(6520)
find.evl.TARGETS("Šumava")

sites_habitats_ndepo <- sites_habitats %>%
  mutate(nitrodepo = habdepo(Kód.fenoménu, Název.lokality))

conpal <- colorNumeric(palette = "Spectral", 
                       domain = krkonose_6520_depo$ntot2017.tif, 
                       na.color = "black")
conpal
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
  addPolygons(data = sf::st_transform(krkonose_6520_depo, CRS("+init=epsg:4326")) %>%
                sf::st_make_valid(),
              color = ~conpal(ntot2017.tif),
              fill = ~conpal(ntot2017.tif),
              weight = 1,
              opacity = 1,
              label = ~format(round(ntot2017.tif, 3), nsmall = 3),
              group = "Potential habitat patches") %>%
  addLayersControl(baseGroups = c("Esri WorldTopoMap", "Esri WorldImagery"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addMeasure(primaryLengthUnit = "meters") %>% 
  addScaleBar(position = "bottomleft")
