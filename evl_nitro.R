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

# LOAD DATA ----
# VRSTVA EVL
evl <- st_read("//bali.nature.cz/du/OchranaPrirody/Natura 2000/EvVyzLok_440_2021.shp")
evl_sjtsk <- st_transform(evl, CRS("+init=epsg:5514"))

# VMB
vmb_shp_sjtsk_22_read <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/20220531_Segment.shp")
vmb_hab_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/HAB_BIOTOP.dbf")
vmb_pb_dbf_22 <- sf::st_read("//bali.nature.cz/du/Mapovani/Biotopy/CR_2022/Biotop/PB_BIOTOP.dbf") %>%
  dplyr::filter(!OBJECTID %in% vmb_hab_dbf_22$OBJECTID)
vmb_hab_pb_dbf_22 <- dplyr::bind_rows(vmb_hab_dbf_22, vmb_pb_dbf_22) %>%
  dplyr::group_by(SEGMENT_ID) %>%
  dplyr::mutate(moz_num = n(),
                FSB_EVAL_prep = dplyr::case_when(sum(STEJ_PR, na.rm = TRUE) < 50 ~ "X",
                                                 sum(STEJ_PR, na.rm = TRUE) >= 50 &
                                                   sum(STEJ_PR, na.rm = TRUE) < 200 ~ "moz.",
                                                 sum(STEJ_PR, na.rm = TRUE) == 200 ~ NA_character_)) %>%
  dplyr::ungroup() %>% 
  dplyr::select(SEGMENT_ID,
                FSB_EVAL_prep) %>%
  distinct()

vmb_shp_sjtsk_22 <- vmb_shp_sjtsk_22_read %>%
  dplyr::left_join(vmb_hab_dbf_22, by = "SEGMENT_ID") %>%
  dplyr::left_join(vmb_hab_pb_dbf_22, by = "SEGMENT_ID") %>%
  dplyr::mutate(FSB_EVAL = dplyr::case_when(FSB_EVAL_prep == "moz." ~ "moz.",
                                            FSB_EVAL_prep == "X" ~ "X",
                                            TRUE ~ FSB),
                HABITAT = dplyr::case_when(BIOTOP %in% c("T3.3C", "3.4A", "T3.4C", "T3.5A") ~ "6210p",
                                           TRUE ~ HABITAT))


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
ntotal2017 <- stars::read_stars("ntot2017.tif")
ndepo2017 <- sf::st_as_sf(ntotal2017)

ndepo_semin <- st_intersection(filter(evl_sjtsk, NAZEV == "Semínský přesyp"), ndepo2017)
ndepo_semin <- ndepo_semin %>% 
  mutate(depo_segment = ntot2017.tif*st_area(geometry)/sum(st_area(geometry)))
ndepo_semin$depo_segment %>% sum()

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

oblik_depo <- vmb_shp_sjtsk_22 %>%
  st_intersection(filter(evl_sjtsk, NAZEV == "Oblík - Srdov - Brník")) %>%
  dplyr::filter(grepl("T3.3", BIOTOP) | grepl("T3.4", BIOTOP)) %>%
  st_intersection(., ndepo2017) %>%
  sf::st_cast(., "POLYGON") %>%
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

biodepo <- function(bio_code, evl_site) {
  
  vmb_target_sjtsk <- vmb_shp_sjtsk_22 %>%
    sf::st_intersection(filter(evl_sjtsk, NAZEV == evl_site)) %>%
    dplyr::filter(BIOTOP == bio_code) %>%
    sf::st_intersection(., ndepo2017) %>%
    dplyr::mutate(AREA_real = units::drop_units(st_area(geometry))) %>%
    dplyr::mutate(PLO_BIO_M2_EVL = PLO_BIO_M2*AREA_real/SHAPE_AREA) %>%
    dplyr::mutate(depo_segment = ntot2017.tif*PLO_BIO_M2_EVL/sum(PLO_BIO_M2_EVL))
  
  return(as.numeric(sum(vmb_target_sjtsk$depo_segment)))
  
}

oblik_depo <- biodepo("T3.3A", "Oblík-Srdov-Brník")

habdepo("6210", "Bílé Karpaty")
find.habitat.NAME_CZ(6520)
find.evl.TARGETS("Šumava")

sites_habitats_ndepo <- sites_habitats %>%
  mutate(nitrodepo = habdepo(Kód.fenoménu, Název.lokality))

conpal <- colorNumeric(palette = "Spectral", 
                       domain = oblik_depo$ntot2017.tif, 
                       na.color = "black")
conpal
oblik_depo[130,]
leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri WorldTopoMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri WorldImagery") %>%
  addPolygons(data = sf::st_transform(oblik_depo, CRS("+init=epsg:4326")) %>%
                slice(c(1:129,131:430)) %>%
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

# ANALYSIS ----
vmb_T <- vmb_shp_sjtsk_22 %>%
  dplyr::filter(grepl("T", BIOTOP)) %>%
  dplyr::mutate(TYP_DRUHY_SEG = dplyr::case_when(DG == "W" ~ 0,
                                                 TD == "N" ~ 0,
                                                 TD == "MP" ~ 5,
                                                 TD == "P" ~ 10)) %>%
  sf::st_intersection(., ndepo2017) %>%
  dplyr::mutate(AREA_real = units::drop_units(st_area(geometry)),
                PLO_BIO_M2_SEG = PLO_BIO_M2*AREA_real/SHAPE_AREA) %>%
  dplyr::group_by(SEGMENT_ID) %>%
  dplyr::mutate(depo_segment = ntot2017.tif*PLO_BIO_M2_SEG/sum(PLO_BIO_M2_SEG)) %>%
  dplyr::ungroup() 

vmb_L <- vmb_shp_sjtsk_22 %>%
  dplyr::filter(grepl("L", BIOTOP)) %>%
  dplyr::mutate(TYP_DRUHY_SEG = dplyr::case_when(DG == "W" ~ 0,
                                                 TD == "N" ~ 0,
                                                 TD == "MP" ~ 5,
                                                 TD == "P" ~ 10)) %>%
  sf::st_intersection(., ndepo2017) %>%
  dplyr::mutate(AREA_real = units::drop_units(st_area(geometry)),
                PLO_BIO_M2_SEG = PLO_BIO_M2*AREA_real/SHAPE_AREA) %>%
  dplyr::group_by(SEGMENT_ID) %>%
  dplyr::mutate(depo_segment = ntot2017.tif*PLO_BIO_M2_SEG/sum(PLO_BIO_M2_SEG)) %>%
  dplyr::ungroup() 
  
plot(vmb_T %>% slice(1:500) %>% dplyr::select(depo_segment, geometry))

ndepo_T <- vmb_T %>%
  sf::st_drop_geometry() %>%
  dplyr::mutate(BIOTOP_CAT = stringr::str_sub(BIOTOP, 1, 4)) %>%
  dplyr::group_by(SEGMENT_ID, BIOTOP_SEZ, BIOTOP) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(TYP_DRUHY_SEG = dplyr::case_when(is.na(TD) ~ 0,
                                                 DG == "W" ~ 0,
                                                 TD == "N" ~ 0,
                                                 TD == "MP" ~ 5,
                                                 TD == "P" ~ 10),
                TD_CAT = dplyr::case_when(TYP_DRUHY_SEG == 0 ~ "N",
                                          TYP_DRUHY_SEG == 5 ~ "MP",
                                          TYP_DRUHY_SEG == 10 ~ "P"),
                TD_TWO = dplyr::case_when(TYP_DRUHY_SEG == 0 ~ "B",
                                          TYP_DRUHY_SEG == 5 ~ "S",
                                          TYP_DRUHY_SEG == 10 ~ "S"),
                DG_CAT = NA,
                QUAL = dplyr::case_when(is.na(KVALITA) ~ 0,
                                        DG == "W" ~ 0,
                                        KVALITA == 1 ~ 1,
                                        KVALITA == 2 ~ 1,
                                        KVALITA == 3 ~ 2,
                                        KVALITA == 4 ~ 2,
                                        TRUE ~ 0),
                REDLIST = dplyr::case_when(BIOTOP == "T3.3A" ~ "NT",
                                           BIOTOP == "T3.3B" ~ "NT",
                                           BIOTOP == "T3.3C" ~ "EN",
                                           BIOTOP == "T3.3D" ~ "NT",
                                           BIOTOP == "T3.4A" ~ "EN",
                                           BIOTOP == "T3.4B" ~ "NT",
                                           BIOTOP == "T3.4C" ~ "VU",
                                           BIOTOP == "T3.4D" ~ "NT",
                                           BIOTOP == "T3.5A" ~ "VU",
                                           BIOTOP == "T3.5B" ~ "VU"),
                REDLIST = as.factor(REDLIST))

write.csv(ndepo_T,
          "S:/Gaigr/hodnoceni_stanovist_grafy/ndepo_T.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
ggplot(data = ndepo_T %>%
         dplyr::filter(grepl("T1.4", BIOTOP)), 
       aes(x = as.factor(TD_CAT), y = ntot2017.tif, fill = as.factor(TD_CAT))) +
  geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
  geom_point(aes(y = ntot2017.tif, color = as.factor(TD_CAT)), 
             position = position_jitter(width = 0.15), size = 1, alpha = 0.5) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
  xlab("typické druhy\n") +
  ylab("\ncelková depozice N (kg/ha)") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  coord_flip() +
  guides(fill=guide_legend(title="New Legend Title"),
         color = "none") +
  theme_classic()

ggplot(data = ndepo_T %>%
         dplyr::filter(grepl("T3.3", BIOTOP)), 
       aes(x = reorder(as.factor(TD_CAT), -desc(TYP_DRUHY_SEG)), 
           y = ntot2017.tif, 
           fill = BIOTOP)) +
  geom_boxplot(alpha = 0.8) +
  xlab("typical species' value\n") +
  ylab("\nnitrogen deposition (kg/ha)") +
  scale_x_discrete(labels = c('poor', 'moderate', 'good')) +
  scale_fill_viridis_d(option = "magma") +
  scale_colour_viridis_d(option = "magma") +
  coord_flip() +
  guides(fill=guide_legend(title="habitat type"),
         color = "none") +
  theme_classic() +
  theme(text = element_text(family = "Arial"),
        axis.title.x = element_text(color = "black", size = 18, face = "bold"),
        axis.title.y = element_text(color = "black", size = 18, face = "bold"),
        axis.text = element_text(color = "black", size = 16, face = "bold"),
        legend.title = element_text(color = "black", size = 18, face = "bold"),
        legend.text = element_text(color = "black", size = 16, face = "bold"),
        legend.text.align = 0.5)

ggplot(data = ndepo_T %>%
         dplyr::filter(grepl("T3.3", BIOTOP)), 
       aes(x = as.factor(TD_CAT), 
           y = ntot2017.tif, 
           fill = BIOTOP)) +
  geom_boxplot(alpha = 0.8) +
  xlab("typical species' presence\n") +
  ylab("\nnitrogen deposition (kg/ha)") +
  scale_fill_viridis_d(option = "magma") +
  scale_colour_viridis_d(option = "magma") +
  coord_flip() +
  guides(fill=guide_legend(title="habitat type"),
         color = "none") +
  theme_classic() +
  theme(text = element_text(family = "Arial"),
        axis.title.x = element_text(color = "black", size = 18, face = "bold"),
        axis.title.y = element_text(color = "black", size = 18, face = "bold"),
        axis.text = element_text(color = "black", size = 16, face = "bold"),
        legend.title = element_text(color = "black", size = 18, face = "bold"),
        legend.text = element_text(color = "black", size = 16, face = "bold"),
        legend.text.align = 0.5)

ggplot(data = ndepo_T %>%
         dplyr::filter(grepl("T3.3", BIOTOP)), 
       aes(x = reorder(as.factor(TD_TWO), -desc(TYP_DRUHY_SEG)), 
           y = ntot2017.tif, 
           fill = BIOTOP)) +
  geom_boxplot(alpha = 0.8) +
  xlab("typické druhy\n") +
  ylab("\ncelková depozice N (kg/ha)") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  coord_flip() +
  theme_classic()

model_n <- glm(data = ndepo_T %>%
                 dplyr::filter(grepl("T3.3", BIOTOP) |
                                 grepl("T3.4", BIOTOP) |
                                 grepl("T3.5", BIOTOP) |
                                 grepl("T3.2", BIOTOP)), 
               TYP_DRUHY_SEG ~ ntot2017.tif * BIOTOP,
               family = "poisson")
summary(model_n)
anova(model_n)


one_way <- aov(data = ndepo_T %>%
                 dplyr::filter(grepl("T3.4", BIOTOP)), 
               TYP_DRUHY_SEG ~ ntot2017.tif)
two_way <- aov(data = ndepo_T %>%
                 dplyr::filter(grepl("T3.4", BIOTOP)), 
               TYP_DRUHY_SEG ~ ntot2017.tif + BIOTOP)
interaction <- aov(data = ndepo_T %>%
                     dplyr::filter(grepl("T3.4", BIOTOP)), 
                   TYP_DRUHY_SEG ~ ntot2017.tif*BIOTOP)

library(AICcmodavg)
model.set <- list(one_way, two_way, interaction)
model.names <- c("one.way", "two.way", "interaction")

aictab(model.set, modnames = model.names)

TukeyHSD(interaction)
