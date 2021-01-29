# Packages ----
library(tidyverse)
library(shiny)
library(shinycssloaders)
library(DT)
library(zoo)
library(lubridate)
library(condformat)
library(ggplot2)
library(rgdal)
library(raster)
library(ggsn)
library(proj4)
library(sf)
library(randomcoloR)
library(leaflet)


sites_subjects <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/sites_subjects_utf.csv", encoding = "UTF-8")
sites_subjects$SUBJECT <- gsub("Maculinea", "Phengaris", sites_subjects$SUBJECT)
sites_subjects$SUBJECT <- gsub("Thersamonolycaena", "Lycaena", sites_subjects$SUBJECT)
sites_subjects$SUBJECT <- gsub("Callimorpha quadripunctaria", "Euplagia quadripunctaria", sites_subjects$SUBJECT)
sites_subjects$SUBJECT <- gsub("Osmoderma eremita", "Osmoderma barnabita", sites_subjects$SUBJECT)
sites_subjects$SUBJECT <- gsub("Triturus montandoni", "Lissotriton montandoni", sites_subjects$SUBJECT)
sites_subjects$SUBJECT <- gsub("Aspius aspius", "Leuciscus aspius", sites_subjects$SUBJECT)
sites_subjects$SUBJECT <- gsub("Cobitis elongatoides", "Cobitis taneia", sites_subjects$SUBJECT)
sites_subjects$SUBJECT <- gsub("Rhodeus sericeus amarus", "Rhodeus amarus", sites_subjects$SUBJECT)

evl_sf <- st_read("Evropsky_v%C3%BDznamn%C3%A9_lokality.shp")
evl_sf <- st_transform(evl_sf, CRS("+init=epsg:4326"))

stanice <- read.csv("https://raw.githubusercontent.com/jonasgaigr/N2K.EFFECTS/master/stanice_ids.csv", encoding = "UTF-8") 
stanice_mapa <- st_as_sf(x= stanice, coords = c("X", "Y"), crs = "+init=epsg:4326")

taxa = read.table(
  text = "Group Species Species_CZ
''  ''  ''
Motýli  '' ''
Brouci  '' ''
Vážky   '' ''
Rovnokřídlí '' ''
Plži  '' ''
Mlži '' ''
Korýši '' ''
Štírci '' ''
Ryby '' ''
Mihule '' ''
Obojživelníci '' ''
Savci '' ''
Motýli	'Phengaris nausithous' 'modrásek bahenní'
Motýli	'Phengaris teleius' 'modrásek očkovaný'
Brouci	'Bolbelasmus unicornis' 'chrobák jednorohý'
Motýli	'Euplagia quadripunctaria' 'přástevník kostivalový'
Brouci	'Carabus hungaricus' 'střevlík uherský'
Brouci	'Carabus menetriesi pacholei' 'střevlík Ménétriésův'
Brouci	'Carabus variolosus' 'střevlík hrbolkatý'
Brouci	'Cerambyx cerdo' 'tesařík velký'
Motýli	'Colias myrmidone' 'žluťásek barvoměnný'
Brouci	'Cucujus cinnaberinus' 'lesák rumělkový'
Motýli	'Euphydryas aurinia' 'hnědásek chrastavcový'
Motýli	'Euphydryas maturna' 'hnědásek osikový'
Brouci	'Graphoderus bilineatus' 'potápník dvoučárý'
Brouci	'Limoniscus violaceus' 'kovařík fialový'
Brouci	'Lucanus cervus' 'roháč obecný'
Motýli	'Lycaena dispar' 'ohniváček černočárný'
Brouci	'Osmoderma barnabita' 'páchník hnědý'
Brouci	'Rhysodes sulcatus' 'rýhovec pralesní'
Brouci	'Rosalia alpina' 'tesařík alpský'
Rovnokřídlí	'Stenobothrus eurasius' 'saranče skalní'
Motýli	'Eriogaster catax' 'bourovec trnkový'
Vážky	'Coenagrion ornatum' 'šidélko ozdobné'
Vážky	'Leucorrhinia pectoralis' 'vážka jasnoskvrnná'
Vážky	'Ophiogomphus cecilia' 'klínatka rohatá'
Vážky	'Cordulegaster heros' 'páskovec velký'
Plži	'Anisus vorticulus' 'svinutec tenký'
Korýši	'Austropotamobius torrentium' 'rak říční'
Mlži	'Margaritifera margaritifera' 'perlorodka říční'
Mlži	'Unio crassus' 'velevrub tupý'
Plži	'Vertigo angustior' 'vrkoč útlý'
Plži	'Vertigo geyeri' 'vrkoč Geyerův'
Plži	'Vertigo moulinsiana' 'vrkoč bažinný'
Štírci	'Anthrenochernes stellae' 'štírek Stella'
Ryby	'Leuciscus aspius' 'bolen dravý'
Ryby	'Cobitis taneia' 'sekavec písečný'
Ryby	'Cottus gobio' 'vranka obecná'
Ryby	'Gobio albipinnatus' 'hrouzek běloploutvý'
Ryby	'Gobio kessleri' 'hrouzek Kesslerův'
Ryby	'Gymnocephalus baloni' 'ježdík dunajský'
Ryby	'Gymnocephalus schraetser' 'ježdík žlutý'
Ryby	'Misgurnus fossilis' 'piskoř pruhovaný'
Ryby	'Pelecus cultratus' 'ostrucha křivočará'
Ryby	'Rhodeus amarus' 'hořavka duhová'
Ryby	'Sabanejewia aurata' 'sekavčík dunajský'
Ryby	'Salmo salar' 'losos obecný'
Ryby	'Zingel streber' 'drsek menší'
Ryby	'Zingel zingel' 'drsek větší'
Mihule	'Eudontomyzon mariae' 'mihule ukrajinská'
Mihule	'Lampetra planeri' 'mihule říční'
Obojživelníci	'Triturus carnifex' 'čolek dravý'
Obojživelníci	'Triturus cristatus' 'čolek velký'
Obojživelníci	'Triturus dobrogicus' 'čolek dunajský'
Obojživelníci	'Lissotriton montandoni' 'čolek karpatský'
Obojživelníci	'Bombina bombina' 'kuňka ohnivá'
Obojživelníci	'Bombina variegata' 'kuňka žlutobřichá'
Savci	'Rhinolophus hipposideros' 'vrápenec malý'
Savci	'Barbastella barbastellus' 'netopýr černý'
Savci	'Myotis bechsteini' 'netopýr velkouchý'
Savci	'Myotis blythii' 'netopýr východní'
Savci	'Myotis dasycneme' 'netopýr pobřežní'
Savci	'Myotis emarginatus' 'netopýr brvitý'
Savci	'Myotis myotis' 'netopýr velký '
Savci	'Spermophilus citellus' 'sysel obecný'
Savci	'Castor fiber' 'bobr evropský'
Savci	'Canis lupus' 'vlk obecný'
Savci	'Ursus arctos' 'medvěd hnědý'
Savci	'Lutra lutra' 'vydra říční'
Savci	'Lynx lynx' 'rys ostrovid'", 
  header = TRUE,
  stringsAsFactors = FALSE,
  encoding = "UTF-8"
)

# UI----
ui <- fluidPage(
  theme = "bootstrap.css",
  
  tags$head(tags$link(rel = "shortcut icon", 
                      href = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/favicon.ico")),
  
  titlePanel(div(HTML("<h1>Vyhledávač stanic ČHMÚ pro EVL")
  ),
  windowTitle = "Vyhledávač stanic ČHMÚ pro EVL"),
  
  br(),
  
  column(2,
         htmlOutput("group_selector")),
  column(3,
         htmlOutput("species_selector")),
  column(2,
         htmlOutput("evl_selector")),
  column(3,
         sliderInput("evl_dist", "Vzdálenost od hranic EVL",
                       min = 0.01, max = 0.3, value = 0, step = 0.005, ticks = FALSE)),
  
  br(),
  br(),
  br(),
  br(),
  
  hr(), 
  
  fluidRow(
    uiOutput("default")
  ),
  
  fluidRow(
           leafletOutput(outputId = "evl_chmi_mapa", height = "700px") %>%
             withSpinner(color = "green")
  ),
  
  hr(),
  
  fluidRow(div(img(
    src = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/LOGO.jpg", 
    height = 70), style = "text-align: center;")),
  
  br(),

  fluidRow(HTML('<center><b>© 2020 <a href="http://www.nature.cz" target="_blank">AOPK ČR</a></b></center>')),
 
  br()
)

server <- function(input, output, session){
  
  # Vyhledávání SITECODE EVL v nichž je druh předmětem ochrany
  find_evl_SITECODE <- function(species){
    return(sites_subjects$SITECODE[which(grepl(species, sites_subjects$SUBJECT))])
  }
  
  # Vyhledávání NAZEV EVL v nichž je druh předmětem ochrany
  find_evl_NAZEV <- function(species){
    return(sites_subjects$NAZEV[which(grepl(species, sites_subjects$SUBJECT))])
  }
  
  find_STANICE_CHMU <- function(evl_nazev) {
    
    target_evl_stanice <- st_buffer(subset(evl_sf, evl_sf$NAZEV == evl_nazev), dist = input$evl_dist)
    
    if (sum(st_intersects(stanice_mapa, target_evl_stanice, sparse = FALSE)) == 0){
      
      stanice_mapa[st_nearest_feature(target_evl_stanice, stanice_mapa),]
      
    } else {
      
      st_intersection(stanice_mapa, target_evl_stanice)
      
    }
    
  }
  
  
  # Mapa EVL s nejbližší stanicí ČHMÚ ----
  output$evl_chmi_mapa <- renderLeaflet({
    
    req(input$evl_site)
    
    result <- find_STANICE_CHMU(input$evl_site)
    
    target_evl <- evl_sf[evl_sf$NAZEV %in% input$evl_site,]
    
    distCol<- colorFactor(topo.colors(2), stanice_mapa$T)
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldTopo) %>%
      addPolygons(data = target_evl,
                  color = "green",
                  fill = "green",
                  weight = 1,
                  opacity = .8,
                  label = ~NAZEV) %>%
      addCircleMarkers(data = result,
                       color = ~distCol(T),
                       fill = ~distCol(T),
                       weight = 1.5,
                       opacity = .95,
                       radius = 6,
                       label = ~FNAME) %>%
      addLegend(data = result, pal = distCol, values = ~T, title = "Měření teploty")
    })
  
  # Výběr taxonu ----
  
  output$group_selector = renderUI({
    selectInput(inputId = "Group",
                label = "Vyberte skupinu",
                choices = as.character(unique(taxa$Group)))
  })
  
  output$species_selector = renderUI({
    data_available = taxa[taxa$Group == input$Group, "Species"]
    selectInput(
      inputId = "species",
      label = "Vyberte druh",
      choices = unique(data_available),
      selected = unique(data_available)[1]
    )
  })
  
  output$evl_selector = renderUI({
    
    evl_available = find_evl_NAZEV(input$species)
    
    selectInput(
      inputId = "evl_site",
      label = "Vyberte EVL",
      choices = c("", as.character(evl_available)),
      selected = NULL
    )
  })
  
  # Default UI ----
  output$default <- renderUI({
    
    if (input$evl_site == "") {
      
      div(img(src = "https://raw.githubusercontent.com/jonasgaigr/N2K.CZ/main/WWW/logo.png",
              width = "50%"), style = "text-align: center;")
      
    } else {
      NULL
    }
    
  })
  
}

shinyApp(ui = ui, server = server)