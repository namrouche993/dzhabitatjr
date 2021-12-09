#setwd("C:/electron-quick-start-win32-ia32/resources/app")
#getwd()


#setwd("C:/kalafat electron/electron-quick-start-win32-ia32/resources/app")


setwd("C:/Users/stat_ch_et/Downloads/DZ HABITAT/resources/app")




############## JOB FOR M BEN ############## 
############## JOB FOR M BEN ############## 
############## JOB FOR M BEN ############## 
############## JOB FOR M BEN ############## 
############## JOB FOR M BEN ############## 


#.libPaths(paste0(getwd(),"/R-Portable-Win/library"))
#.Library=paste0(getwd(),"/R-Portable-Win/library")

library(shiny)
library(fresh)
#library(shinydashboard)
#library(shinydashboardPlus)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
#library(ECharts2Shiny)
#library(rAmCharts)
library(shinyBS)

library(leaflet)
#library(htmltools)
library(leaflet.extras)


library(rgdal)
library(sp)
library(readxl)
library(highcharter)
library(tidyverse)
library(excelR)
library(farver)
library(readxl)


library(reactable)
#library(grDevices)
#library(janitor)

library(shinyjs)
library(rmapshaper)
library(geojsonio)
library(sass)
#library(gt)
#library(flextable)

library(rpivotTable)
library(shinytreeview)
library(sparkline)
library(RColorBrewer)
library(sp)


# 
# options(shiny.host = '192.168.5.242')
# options(shiny.port = 8181)




#conso <- read_excel(paste0(getwd(),"/conso.xlsx"))
donnees_consomation <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="Consommation")
dd0=donnees_consomation
dd0=dd0 %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
dd0d =donnees_consomation %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
for(i in 1:50){
  for(j in 2:11){
    dd0[i,j]=as.numeric(format(round(dd0[i,j]/ 1e9, 1), trim = TRUE))
  }
}
dd16=c("16-ALGER (DEB+BMR+HD)",0,0,0,0,0,0,0,0,0)
dd16
for(i in 2:11){
  dd16[i]=sum(dd0[16:18,i])
}
dd160=data.frame(t(dd16))
colnames(dd160)=c("OPGI","2013","2014","2015","2016","2017","2018","2019","2020","2021","Total")
dd48=rbind(dd0[1:15,],dd160,dd0[19:50,])
dd48=data.frame(dd48)
for(i in 1:48){
  for(j in 2:11){
    dd48[i,j]=as.numeric(dd48[i,j])
  }
}
colnames(dd48)=c("OPGI","2013","2014","2015","2016","2017","2018","2019","2020","2021","Total")
dd=donnees_consomation %>% gather("Annee","consomation",2:10)




######################## consomation mensuelle 2021
#conso_21 <- read_excel(paste0(getwd(),"/conso.xlsx"))
dco21 <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="consomation21")
dd_21=dco21

dco21=dco21 %>% select(-1) %>% mutate(Prevtotal=Prev1+Prev2+Prev3+Prev4+Prev5+Prev6+Prev7+Prev8+Prev9,
                                      Realtotal=Real1+Real2+Real3+Real4+Real5+Real6+Real7+Real8+Real9,
                                      tauxtotal=Realtotal/Prevtotal
)

#dd0_21=dd0_21 %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
#dd0d =donnees_consomation %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)


dco48_21 <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="consomation21M")
#dd_21=donnees_consomation %>% gather("Annee","consomation",2:10)

######################### consomation mensuelle 2021 f



######################## consomation mensuelle 2021
#conso_21 <- read_excel(paste0(getwd(),"/conso.xlsx"))
donnees_consomation_21 <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="consomation_mensuelles_2021")
dd0_21=donnees_consomation_21
#dd0_21=dd0_21 %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
#dd0d =donnees_consomation %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)


donnees_consomation48_21 <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="consomationd48_mensuelles_2021")
#dd_21=donnees_consomation %>% gather("Annee","consomation",2:10)

consommation2021_data_hchart <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="Consommation2021")

consommation2021_data_distplot2 <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="consomationd48_mensuelles_2021")

consommation2021_data_distplot2=
  consommation2021_data_distplot2 %>% 
  mutate(Prevision_m=as.numeric(round(Prevision/1e6, 1)),
         Consommations_m=as.numeric(round(Realisation/1e6, 1)),
         Taux=Taux*100
  )
######################### consomation mensuelle 2021 f






####################################### Livraison #####################
#donnees_consomation <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="Consommation")


donnees_livraison <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="Livraison")
dd0_livraison=donnees_livraison
dd0_livraison=dd0_livraison %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
dd0d_livraison =donnees_livraison %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
for(i in 1:48){
  for(j in 2:11){
    dd0_livraison[i,j]=as.numeric(dd0_livraison[i,j])
  }
}
dd48_livraison=dd0_livraison

colnames(dd48_livraison)=c("Wilayas","2013","2014","2015","2016","2017","2018","2019","2020","2021","Total")
dd_livraison=donnees_livraison %>% gather("Annee","Livraison",2:10)
####################################### Livraison #####################




####################################### Encours #####################
#donnees_consomation <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="En Cours")


donnees_encours <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="En cours")
dd0_encours=donnees_encours
dd0_encours=dd0_encours %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
dd0d_encours =donnees_encours %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
for(i in 1:48){
  for(j in 2:10){
    dd0_encours[i,j]=as.numeric(dd0_encours[i,j])
  }
}
dd48_encours=dd0_encours
colnames(dd48_encours)=c("Wilayas","2013","2014","2015","2016","2017","2018","2019","2020","Total")
dd_encours=donnees_encours %>% gather("Annee","EnCours",2:9)
####################################### Livraison #####################




####################################### Non Lances #####################
#donnees_consomation <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="Non Lances")


donnees_nonlances <- read_excel(paste0(getwd(),"/donnees_stat.xlsx"),sheet="Non lances")
dd0_nonlances=donnees_nonlances
dd0_nonlances=dd0_nonlances %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
dd0d_nonlances =donnees_nonlances %>% mutate(Total=`2013`+`2014`+`2015`+`2016`+`2017`+`2018`+`2019`+`2020`)
for(i in 1:48){
  for(j in 2:10){
    dd0_nonlances[i,j]=as.numeric(dd0_nonlances[i,j])
  }
}
dd48_nonlances=dd0_nonlances
colnames(dd48_nonlances)=c("Wilayas","2013","2014","2015","2016","2017","2018","2019","2020","Total")
dd_nonlances=donnees_nonlances %>% gather("Annee","NonLances",2:9)
####################################### Livraison #####################







#orange_pal <- function(x) rgb(colorRamp(c("#ffe4cc", "#ffb54d"))(x), maxColorValue = 255)

#blues_pal <- function(x) rgb(colorRamp(c("##D3D3D3", "#08306B"))(x), maxColorValue = 255)
blues_pal <-  function(x) rgb(colorRamp(c("#D3D3D3", "#041c40"))(x), maxColorValue = 255)
greens_pal <-  function(x) rgb(colorRamp(c("#ccd9d7", "#00423a"))(x), maxColorValue = 255)

#BuYlRd <- function(x) rgb(colorRamp(c("#cc0000","#faa87a","#7BCD58"))(x), maxColorValue = 255)

BuYlRd <- function(x) rgb(colorRamp(c("#cc0000","#c23e21","#7BCD58"))(x), maxColorValue = 255)
#BuYlRd <- function(x) rgb(colorRamp(c("#cc0000","#c28521","#7BCD58"))(x), maxColorValue = 255)

#EB3223










col_stops <- data.frame(
  q = c(0.33, 0.66, .99),
  c = c('#DF5353','#DDDF0D','#55BF3B'),
  stringsAsFactors = FALSE
)





countries <- geojsonio::geojson_read("polbnda_dza.json", what = "sp")
algeria <- rmapshaper::ms_simplify(countries, keep = 0.05, keep_shapes = TRUE)

#algeria=rgdal::readOGR(paste0(getwd(),"/polbnda_dza.json"))


#class(algeria)
#glimpse(algeria)
#glimpse(algeria@data)
#slotNames(algeria)
#algeria@data$pop[1:48]=rnorm(48,1000000,300000)
#algeria@data$pop[49:96]=algeria@data$pop[1:48]

id_wilaya=c(27,31,29,22,46,13,20,15,6,35,16,42,9,10,2,19,26,44,34,28,38,48,17,14,5,7,21,23,36,18,24,43,25,41,4,12,40,8,32,45,1,3,47,30,39,33,37,11)

algeria@data$id_wilaya=id_wilaya
algeria@data=algeria@data[1:96,]

#algeria@data=algeria@data%>%
#arrange(id_wilaya)

#for(i in 1:96){
#  j=algeria@data$id_wilaya[i]
#  algeria@data$parc_logts[i]=liv$Parc_logement2019[j]
#}

#for(i in 1:96){
# j=algeria@data$id_wilaya[i]
#  algeria@data$pop[i]=liv$Population_2019[j]
#}

algeria@data=algeria@data[1:48,]

#palo <- colorNumeric("YlGnBu",algeria@data$pop)
#algeria@data$couleur=palo(algeria@data$pop)

gps=data.frame(longitude=rep(0,48),latitude=rep(0,48))
gps=data.frame(longitude=rep(0,48),latitude=rep(0,48))
for(i in 1:48){
  gps[i,]=algeria@polygons[[i]]@labpt
}
algeria@data$longitude=gps$longitude
algeria@data$latitude=gps$latitude
algeria@data$wilayas=unique(dco48_21$OPGI)[id_wilaya]

#algeria@data$nam=unique(livraison_wilayas$waw)[round(livraison_wilayas%>%
#                                                      group_by(id_wilaya)%>%
#                                                     summarise(liv=sum(Livraison))%>%
#                                                    arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,30,44,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
#                                                   select(id_wilaya))$id_wilaya]
#providers n:114  n20 n38  43 117
mapdz=leaflet(algeria)%>%
  setView(lng = 3.03333 , lat = 28.6167, zoom = 5)%>%
  #clearBounds()%>%
  
  addProviderTiles("OpenStreetMap.BZH",
                   options = leafletOptions(minZoom = 5, maxZoom = 10,dragging = TRUE))%>%   #or we can use addProviderTiles(one_providers)
  
  setMapWidgetStyle(list(background= "#ffffff"))

# map_mc=leaflet(datamc) %>% 
#   addProviderTiles(providers[[6]]) %>%
#   setView(lng=1.3333,lat=30.6167,zoom=6)
#   


############# wilayas58



algeria580 <- geojsonio::geojson_read("geoData.geojson", what = "sp")
algeria58 <- rmapshaper::ms_simplify(algeria580, keep = 0.05, keep_shapes = TRUE)

wilayas58 <- read_excel(paste0(getwd(),"/wilayas58.xlsx"))


wilayas58$field=round(rnorm(58,1500,400))



wilayas58=wilayas58 %>% arrange(c(1,28,24,50,43,39,44,12,34,36,5,49,
                                  15,21,35,33,38,42,40,18,55,14,54,52,
                                  57,32,23,37,19,8,17,20,7,41,58,53,11,
                                  26,47,48,51,31,56,29,13,16,27,22,2,3,
                                  30,10,9,4,46,6,45,25))



gps=data.frame(longitude=rep(0,58),latitude=rep(0,58))
gps=data.frame(longitude=rep(0,58),latitude=rep(0,58))
for(i in 1:58){
  gps[i,]=algeria58@polygons[[i]]@labpt
}
algeria58@data$longitude=gps$longitude
algeria58@data$latitude=gps$latitude
algeria58@data$wilayas=wilayas58$wilaya




mapdz58=leaflet(algeria58)%>%
  setView(lng = 1.63333 , lat = 28.3667, zoom = 6)%>%
  #clearBounds()%>%
  
  addProviderTiles("OpenStreetMap.BZH",
                   options = leafletOptions(minZoom = 6, maxZoom = 6,dragging = TRUE)) %>%   #or we can use addProviderTiles(one_providers)
  setMapWidgetStyle(list(background= "#ffffff"))



mytheme0=create_theme(
  theme = "united",
  bs_vars_wells(
    bg = "#fff"
  ),
  bs_vars_wells(
    
  )
)

jsCode88 <- "shinyjs.opac88 = function(params){$('#well1 .pretty.p-default.p-switch.p-slim').css('display', params);}"
jsCode99 <- "shinyjs.opac99 = function(params){$('.indent').css('margin-left', params);}"


ui <- fluidPage(
  #includeScript("www/check.js"),
  shinyjs::useShinyjs(),
  
  extendShinyjs(text = jsCode88, functions = c("opac88")),
  
  extendShinyjs(text = jsCode99, functions = c("opac99")),
  #extendShinyjs(text = jsCode1010, functions = c("opac1010")),
  #extendShinyjs(text = jsCode1010, functions = c("opac1111")),
  
  
  
  use_theme(create_theme(theme="united",bs_vars_wells(bg="#fff"))),
  setBackgroundColor(
    color = c("#f2f2f2", "#f2f2f2"), 
    gradient = "radial",
    direction = c("top", "left")
  ),
  
  tags$head(tags$script(HTML(
    "
  const {container-fluid} = require('electron')
  container-fluid.setZoomFactor(3);

  
    "
  ))),
  navbarPage(
    HTML("Ministère de l'Habitat,<br/> de l'Urbanisme et de la Ville"),
    id = "main_navbar",
    selected='Consommations',
    #navbarMenu("Logements",
    tags$head(HTML('<style type="text/css">
    
    
 .ReactTable.rt-bordered.rt-borderless.rt-inline{
    zoom:0.91;
    height:925px;
    }
    
  div#pf1 {
    height: 1582.49 px;
    width: 1121.48 px;
}
    
.slider-animate-container {
    text-align: right;
    margin-top: -113px;
    margin-right: 209px;
    font-size: 37px;
}

a.slider-animate-button {
    color: slategrey;
    text-decoration: none;
    opacity: 1;
}

.irs--shiny .irs-single {
    color: #fff;
    text-shadow: none;
    padding: 1px 3px;
    background-color: #428bca;
    border-radius: 3px;
    font-size: 22px;
    line-height: 1.333;
    margin-top: -11px;
}

#rowselect .shiny-input-container:not(.shiny-input-container-inline) {
    width: 300px;
    max-width: 100%;
    position: absolute;
    z-index: 999;
    right: 945px;
    top:7px;
}


.navbar>.container-fluid .navbar-brand {
    margin-left: -15px;
    margin-top: -14px;
    line-height: 110%;
    font-family: system-ui;
    font-weight: 500;
}
 
 
.well .shiny-input-container {
    width: auto;
    display: inline;
    margin-right: 30px;
    zoom: 1.05;
    margin-left: 110px;
}


#tabmodalserie2 {
position: absolute;
top: -6px;
right: 15px;
}

#tabmodalserie2_co21 {
position: absolute;
top: -6px;
right: 15px;
}



#tabmodalserie2_livraison {
position: absolute;
top: -6px;
right: 15px;
}


#tabmodalserie2_encours {
position: absolute;
top: -6px;
right: 15px;
}


#tabmodalserie2_nonlances {
position: absolute;
top: -6px;
right: 15px;
}

span.indent {
    margin-left: -32px;
    margin-right: 10px;
}





.modal-open .modal {
    overflow-x: hidden;
    overflow-y: auto;
    z-index: 99999999;
}


.f1e{
padding-bottom: 34px;
padding-left: 0px;
font-family: inherit;
font-size: 26px;
text-align:end;
}



.highcharts-text-outline{
stroke-width:0px;
}


#rowselect .shiny-input-container:not(.shiny-input-container-inline) {
    width: 300px;
    max-width: 100%;
    position: absolute;
    z-index: 999;
    right: 945px;
    top:7px;
}


.btn-primary:active.focus, .btn-primary.active:hover {
    color: #fff;
    background-color: rebeccapurple;
    border-color: darkslategray;
}


.btn-primary.active {
    color: #fff;
    background-color: rebeccapurple;
    background-image: none;
    border-color: darkslategray;
}



#table .ReactTable .rt-tbody {
    -webkit-box-flex: 99999;
    flex: 99999 1 auto;
    display: -webkit-box;
    display: flex;
    -webkit-box-orient: vertical;
    -webkit-box-direction: normal;
    flex-direction: column;
    padding-top: 105px;
}
  
  #table .ReactTable .rt-tr-group {
    -webkit-box-flex: 1;
    -ms-flex: 1 0 auto;
    flex: 1 0 auto;
    display: -webkit-box;
    display: -ms-flexbox;
    display: flex;
    -webkit-box-orient: vertical;
    -webkit-box-direction: normal;
    -ms-flex-direction: column;
    flex-direction: column;
    -webkit-box-align: stretch;
    -ms-flex-align: stretch;
    align-items: stretch;
    cursor: pointer;
    padding-bottom: 0px;
    margin: -2px;
    font-family: system-ui;
    font-size: 16px;
  }


#table .ReactTable .rt-table {
    flex: auto 1;
    flex-direction: column;
    -webkit-box-align: stretch;
    align-items: stretch;
    width: 100%;
    border-collapse: collapse;
    overflow: auto;
    overflow-y: hidden;
    overflow-x: hidden;
}
  

#table .ReactTable .rt-thead.-header {
    position: fixed;
    background: inherit;
    z-index: 2;
    width: 277px;
    padding-top:70px;
}

 
.jexcel_content {  display: inline-block;
    box-sizing: border-box;
    padding-right: 3px;
    padding-bottom: 3px;
    position: relative;
    max-height: 120% !important;
          }
                            
.navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {
    color: #fff;
    background-color: transparent;
}

.navbar-default .navbar-nav>.open>a, .navbar-default .navbar-nav>.open>a:hover, .navbar-default .navbar-nav>.open>a:focus {
    color: #fff;
    background-color: transparent;
}

.dropdown-menu>li>a:hover, .dropdown-menu>li>a:focus {
    color: #fff;
    text-decoration: none;
    background-color: lightseagreen;
}

.dropdown-menu>li>a:hover {
    background-color: lightseagreen;
}

.dropdown-menu>.active>a, .dropdown-menu>.active>a:hover, .dropdown-menu>.active>a:focus {
    color: #fff;
    text-decoration: none;
    background-color: lightseagreen;
    outline: 0;
}

.modal-body {
    position: relative;
    padding: 20px;
    overflow-x: auto;
}


.bttn-bordered.bttn-success{
border-color: transparent;
color:darkslategrey;
}

.bttn-bordered.bttn-success:focus, .bttn-bordered.bttn-success:hover{
border-color: transparent;
color:black;
}


#titre_serie2 {
display: inline
}


#titre_serie2_co21 {
display: inline
}



#titre_serie2_livraison {
display: inline
}




#titre_serie2_encours {
display: inline
}


#titre_serie2_nonlances {
display: inline
}






#titre_serie3 {
display: inline;
margin-left:90px;
}



#titre_serie3_co21 {
display: inline;
margin-left:90px;
}

#titre_serie3_livraison {
display: inline;
margin-left:90px;
}


#titre_serie3_encours {
display: inline;
margin-left:90px;
}



#titre_serie3_nonlances {
display: inline;
margin-left:90px;
}

#titre_serie {
display: inline
}

#titre_serie_co21 {
display: inline
}

#titre_serie_livraison {
display: inline
}


#titre_serie_encours {
display: inline
}



#titre_serie_nonlances {
display: inline
}

.chart-title {
    width:107%;
    height:20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 13px;
    font-weight: 300;
    margin-left:-20px;
    margin-top:4px;
    display: flex;
}



.pretty {
position : absolute;
right: 250px;
}
   
             

.sw-dropdown {
    position: absolute;
    /* display: inline-block; */
    top: 10px;
    right: 30px;
    z-index: 1000;
}

.leaflet-top, .leaflet-bottom {
    position: absolute;
    z-index: 100;
    pointer-events: none;
}

.sw-dropdown-content {
    display: none;
    position: absolute;
    right: 0px;
    -moz-border-radius: 10px;
    -webkit-border-radius: 10px;
    border-radius: 10px;
    background: none repeat scroll 0% 0% #FFF;
    -moz-box-shadow: 0px 0px 15px 0px #c0c0c0;
    -webkit-box-shadow: 0px 0px 15px 0px #c0c0c0;
    -o-box-shadow: 0px 0px 15px 0px #c0c0c0;
    box-shadow: 0px 0px 15px 0px #c0c0c0;
    z-index: 5;
}

.irs-with-grid {
    height: 60px;
    
}

.irs-min, .irs-max {
    color: #333;
    font-size: 10px;
    line-height: 1.333;
    text-shadow: none;
    top: 0;
    padding: 1px 3px;
    background: rgba(0,0,0,0.1);
    border-radius: 3px;
    -moz-border-radius: 3px;
}

.irs-from, .irs-to, .irs-single {
    color: #fff;
    font-size: 16px;
    line-height: 1.333;
    text-shadow: none;
    padding: 0px 2px;
    background: #428bca;
    border-radius: 3px;
    -moz-border-radius: 3px;
}

.navbar-default {
  background-color: #007bff !important;
    border-bottom: 3px solid #0062cc;
  box-shadow: 0px 5px 15px grey;
}

.navbar {
  position: relative;
  min-height: 63px;
  margin-bottom: 20px;
  border: 1px solid transparent;
  margin-right: -15px;
  margin-left: -15px;
  padding-top: 5px;
}

navbar-default .navbar-brand {
  color: #fff;
    font-size: 20px;
}

.navbar-brand {
  float: left;
  height: 50px;
  padding: 18px 15px;
  font-size: 20px;
  line-height: 15px;
}

.navbar-nav {
  float: left;
  margin: 0;
  font-size: 16px;
  border-left: 3px solid #f2f2f2;
}


.navbar-default .navbar-nav > .active > a {
  color: #fff;
  background-color: transparent;
  font-size: 19px;
}



.navbar-default .navbar-nav > li > a {
  color: #e5e5e5;
}


.navbar-default .navbar-nav > .active > a:link {
  background-color: transparent;
}


.navbar-default .navbar-nav > li > a:hover {
  background-color: transparent;
}

.title {
    font-size: 16px;
    font-weight: 500;
    margin: 0;
}

.well#well1 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:230px;
}


.well#well12 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:840px;
}


.well#well3 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:590px;
}


.well#well1_co21 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:295px;
}


.well#well12_co21 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:900px;
}


.well#well3_co21 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:586px;
}


.well#well1_livraison {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:230px;
}


.well#well12_livraison {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:840px;
}

.well#well3_livraison {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:590px;
}





.well#well1_encours {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:230px;
}


.well#well12_encours {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:840px;
}

.well#well3_encours {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:590px;
}






.well#well1_nonlances {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:230px;
}


.well#well12_nonlances {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:840px;
}

.well#well3_nonlances {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
    height:590px;
}



.btn-primary {
    color: #fff;
    background-color: cadetblue;
    border: 3px solid grey;
}

:not(.input-group)>.bootstrap-select.form-control:not([class*=col-]){
width:80%
}


.btn dropdown-toggle btn-primary bs-placeholder{

background-color:cadetblue

}


.bootstrap-select .dropdown-toggle .filter-option {
    position: static;
    top: 0;
    left: 0;
    float: left;
    height: 100%;
    width: 100%;
    text-align: left;
    font-weight: 500;
    font-family: system-ui;
    font-size: 16px;
    color: white;
    overflow: hidden;
    -webkit-box-flex: 0;
    -webkit-flex: 0 1 auto;
    -ms-flex: 0 1 auto;
    flex: 0 1 auto;
}

.well .shiny-input-container {
    width: auto;
    display: inline;
    margin-right: 30px;
    zoom: 1.05;
    margin-left: 110px;
}


.btn-primary {
    color: #fff;
    background-color: rgb(44, 168, 116);
    border: 3px solid grey;
}

.btn-primary:hover {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.btn-primary:active {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.open>.btn-primary.dropdown-toggle:focus{

    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.dropdown-menu>li>a:hover{
    background-color: mediumseagreen;

}


.btn-primary:active:hover {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.btn-primary:focus {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}

.btn-primary:visited {
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.open>.btn-primary.dropdown-toggle{
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}



.open>.btn-primary.dropdown-toggle:selected{
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.open>.btn-primary.dropdown-toggle:hover{
    color: #fff;
    background-color: rgb(34, 118, 96);
    border: 3px solid grey;
}


.title i[class^="fa "] {
    color: #a9a9a9;
    font-size: 14px;
    position: relative;
    top: 10px;
    right: 10px
}

.metric {
    font-size: 33px;
    font-weight: 700;
    margin: .25em 0
}

.color__nb {
  display: inline;
  font-size: 16px;
  font-weight: 700;
  margin: .25em 0
}


.title {
  background: transparent ;
  text-align: left;

}

.title h2 {
  font-size: 16px;
  font-weight: 530;
  margin-bottom: 5;
  margin-right:-1em;
  margin-top:-2em;
  padding: 0px 15px;
  padding-left: 0px;
  padding-right: 2px;

  color: #595959;
  display: inline-block;
  font-family: Verdana;
}


.color__nb span {
    font-weight: 100;
    color: #000000;
}


.color__nb {
    color: green;
}


.card{
	border-radius:1px;
  font-family: sans-serif;
  padding: 1rem;
  width: 30rem;
  height: 12rem;
  float: left;
  margin-top: 0rem;
  margin-bottom: 1.66rem;
  background: white;
  box-shadow: 4px 5px 11px grey;
  transition: all .2s ease;
  border-bottom: 4px solid transparent;
  border-top: 4px solid #109485;
}
.card:hover{
  border-bottom: 4px solid #008571;
      z-index: 2;
    -webkit-transition: all 200ms ease-in;
    -webkit-transform: scale(1.5);
    -ms-transition: all 200ms ease-in;
    -ms-transform: scale(1.5);   
    -moz-transition: all 200ms ease-in;
    -moz-transform: scale(1.5);
    transition: all 200ms ease-in;
    transform: scale(1.03);
}
.card.selected{
  transform: scale(1.075);
  box-shadow: 0 0 16px 1px rgba(0,0,0,.3);
}

#card_mc:hover{
    border-bottom: 4px solid #008571;
    z-index: 2;
    -webkit-transition: all 200ms ease-in;
    -webkit-transform: scale(1.5);
    -ms-transition: all 200ms ease-in;
    -ms-transform: scale(1.5);   
    -moz-transition: all 200ms ease-in;
    -moz-transform: scale(1.5);
    transition: all 200ms ease-in;
    transform: scale(1.03);
    
}

#card_mc.selected{
  transform: scale(1.075);
  box-shadow: 0 0 16px 1px rgba(0,0,0,.3);
}

</style>')),
    navbarMenu("Consommations",
               tabPanel("Consommations",                      
                        fluidRow(
                          column(width=8,style = "background-color:#eaeaea;width:61%",
                                 fluidRow(
                                   column(width=12,style = "background-color: transparent;zoom:100%",
                                          wellPanel(id="well12",
                                                    tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                                                             
                                                             actionBttn(
                                                               inputId = "tabmodalserie2",
                                                               label = NULL,
                                                               style = "bordered", 
                                                               color = "success",
                                                               icon = icon("table")
                                                             )
                                                             
                                                             
                                                    )
                                                    ,div(style = "margin-top:0em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                                         #highchartOutput("pie", height = 330)
                                                         reactableOutput("table_conso",height = 930
                                                         )
                                                    ))
                                   )
                                 )
                          ),
                          column(width=4,style = "background-color:#eaeaea;;width:37%",
                                 wellPanel(id="well1",
                                           tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                                    
                                           ),
                                           div(
                                             style = "padding-top: 30px;zoom:100%;"
                                             ,highchartOutput("hchart1", height = 130)
                                             
                                           )
                                 ),
                                 div(id="widget_div",style="
                                 width: 33%;position: absolute;z-index: 9;top: 720px;right: 420px;"
                                     
                                     #width: 33%;position: absolute;z-index: 9;top: 495px;right: 302px;zoom: 1.4;
                                     ,
                                     sliderTextInput(
                                       inputId = "annee_input",
                                       label = "", 
                                       choices = c(2013:2021),
                                       grid=TRUE,
                                       animate=animationOptions(interval = 1000, loop = TRUE)
                                     )
                                 ),
                                 wellPanel(id="well3",
                                           tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                           )
                                           
                                           
                                           ,div(style = "padding: 0px 0px;zoom:109%;height:500px;",
                                                #shinycssloaders::withSpinner(
                                                leafletOutput("distPlot2",height=502
                                                              #ifelse(abad()==1,700,568) 
                                                )
                                                # )
                                           ))
                          )
                          
                          
                        ),
                        #tabmodalserie_eqp
                        
                        bsModal("modal2",htmlOutput("tablededonnes2"), "tabmodalserie2", size = "large"
                                ,excelOutput("excel2"))
               ),
               tabPanel("Consommations en 2021",
                        fluidRow(
                          column(width=8,style = "background-color:#eaeaea;width:63%",
                                 fluidRow(
                                   column(width=12,style = "background-color: transparent;zoom:100%",
                                          wellPanel(id="well12_co21",
                                                    tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2_co21"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                                                             
                                                             actionBttn(
                                                               inputId = "tabmodalserie2_co21",
                                                               label = NULL,
                                                               style = "bordered", 
                                                               color = "success",
                                                               icon = icon("table")
                                                             )
                                                             
                                                             
                                                    )
                                                    ,div(style = "margin-top:0em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                                         #highchartOutput("pie", height = 330)
                                                         shinycssloaders::withSpinner(
                                                           reactableOutput("table_conso21",height = 330)
                                                         )
                                                    ))
                                   )
                                 )
                          ),
                          column(width=4,style = "background-color:#eaeaea;;width:37%",
                                 wellPanel(id="well1_co21",
                                           tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1_co21"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie_co21"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                           ),tags$div(id="div_taux_hchart1co21",
                                                      style='position: absolute;top: 246px;z-index: 15;right: 200px;',
                                                      tags$span(style="font-size: 25px;color: #007f24;","???"),tags$span(style="padding-right:15px;","Taux") 
                                           ),
                                           #),
                                           div(
                                             style = "padding-top: 30px;zoom:100%;"
                                             ,highchartOutput("hchart1_co21"
                                                              ,height = 240
                                                              #,width=617
                                             )
                                             
                                           )
                                 ),
                                 div(id="widget_div_co21",style="
                                 width: 33%;position: absolute;z-index: 9;top: 805px;right: 420px;"
                                     
                                     #width: 33%;position: absolute;z-index: 9;top: 495px;right: 302px;zoom: 1.4;
                                     ,
                                     sliderTextInput(
                                       inputId = "annee_input_co21",
                                       label = "", 
                                       choices = c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre"),
                                       grid=TRUE,
                                       animate=animationOptions(interval = 1000, loop = TRUE)
                                     )
                                 ),
                                 wellPanel(id="well3_co21",
                                           tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map_co21"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3_co21"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                           )
                                           
                                           
                                           ,div(style = "padding: 0px 0px;zoom:109%;height:500px;",
                                                #shinycssloaders::withSpinner(
                                                leafletOutput("distPlot2_co21",height=502
                                                              #ifelse(abad()==1,700,568) 
                                                ),
                                                tags$div(class='info legend leaflet-control',
                                                         style='position: absolute;margin-right: 10px;margin-top: 0px;padding-right: 45px;top: 428px;right: 29px;',
                                                         tags$div(style='margin-bottom:3px;'),
                                                         tags$i(style='background: #315223;opacity:1;'),
                                                         ">100%"
                                                )
                                                # )
                                           ))
                          )
                          
                          
                        ),
                        #tabmodalserie_eqp
                        
                        bsModal("modal2_co21",htmlOutput("tablededonnes2_co21"), "tabmodalserie2_co21", size = "large"
                                ,excelOutput("excel2_co21"))
                        
               )
               
    ),
    ############################ tab Livraisons :
    tabPanel("Livraison de Logements",                      
             fluidRow(
               column(width=8,style = "background-color:#eaeaea;width:61%",
                      fluidRow(
                        column(width=12,style = "background-color: transparent;zoom:100%",
                               wellPanel(id="well12_livraison",
                                         tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2_livraison"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                                                  
                                                  actionBttn(
                                                    inputId = "tabmodalserie2_livraison",
                                                    label = NULL,
                                                    style = "bordered", 
                                                    color = "success",
                                                    icon = icon("table")
                                                  )
                                                  
                                                  
                                         )
                                         ,div(style = "margin-top:0em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                              #highchartOutput("pie", height = 330)
                                              reactableOutput("table_livrai",height = 330)
                                         ))
                        )
                      )
               ),
               column(width=4,style = "background-color:#eaeaea;;width:39%",
                      wellPanel(id="well1_livraison",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1_livraison"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie_livraison"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                         
                                ),
                                div(
                                  style = "padding-top: 30px;zoom:100%;"
                                  ,highchartOutput("hchart1_livraison", height = 130)
                                  
                                )
                      ),
                      div(id="widget_div",style="
                                 width: 33%;position: absolute;z-index: 9;top: 720px;right: 420px;"
                          
                          #width: 33%;position: absolute;z-index: 9;top: 495px;right: 302px;zoom: 1.4;
                          ,
                          sliderTextInput(
                            inputId = "annee_input_livraison",
                            label = "", 
                            choices = c(2013:2021),
                            grid=TRUE,
                            animate=animationOptions(interval = 1000, loop = TRUE)
                          )
                      ),
                      wellPanel(id="well3_livraison",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map_livraison"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3_livraison"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                )
                                
                                
                                ,div(style = "padding: 0px 0px;zoom:109%;height:500px;",
                                     #shinycssloaders::withSpinner(
                                     leafletOutput("distPlot2_livraison",height=502
                                                   #ifelse(abad()==1,700,568) 
                                     )
                                     # )
                                ))
               )
               
               
             ),
             #tabmodalserie_eqp
             
             bsModal("modal2_livraison",htmlOutput("tablededonnes2_livraison"), "tabmodalserie2_livraison", size = "large"
                     ,excelOutput("excel2_livraison"))
    ),
    tabPanel("Logements En Cours",                      
             fluidRow(
               column(width=8,style = "background-color:#eaeaea;width:61%",
                      fluidRow(
                        column(width=12,style = "background-color: transparent;zoom:100%",
                               wellPanel(id="well12_encours",
                                         tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2_encours"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                                                  
                                                  actionBttn(
                                                    inputId = "tabmodalserie2_encours",
                                                    label = NULL,
                                                    style = "bordered", 
                                                    color = "success",
                                                    icon = icon("table")
                                                  )
                                                  
                                                  
                                         )
                                         ,div(style = "margin-top:0em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                              #highchartOutput("pie", height = 330)
                                              reactableOutput("table_encou",height = 330)
                                         ))
                        )
                      )
               ),
               column(width=4,style = "background-color:#eaeaea;;width:39%",
                      wellPanel(id="well1_encours",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1_encours"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie_encours"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                         
                                ),
                                div(
                                  style = "padding-top: 30px;zoom:100%;"
                                  ,highchartOutput("hchart1_encours", height = 130)
                                  
                                )
                      ),
                      div(id="widget_div",style="
                                 width: 33%;position: absolute;z-index: 9;top: 720px;right: 420px;"
                          
                          #width: 33%;position: absolute;z-index: 9;top: 495px;right: 302px;zoom: 1.4;
                          ,
                          sliderTextInput(
                            inputId = "annee_input_encours",
                            label = "", 
                            choices = c(2013:2020),
                            grid=TRUE,
                            animate=animationOptions(interval = 1000, loop = TRUE)
                          )
                      ),
                      wellPanel(id="well3_encours",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map_encours"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3_encours"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                )
                                
                                
                                ,div(style = "padding: 0px 0px;zoom:109%;height:500px;",
                                     #shinycssloaders::withSpinner(
                                     leafletOutput("distPlot2_encours",height=502
                                                   #ifelse(abad()==1,700,568) 
                                     )
                                     # )
                                ))
               )
               
               
             ),
             #tabmodalserie_eqp
             
             bsModal("modal2_encours",htmlOutput("tablededonnes2_encours"), "tabmodalserie2_encours", size = "large"
                     ,excelOutput("excel2_encours"))
    ),
    
    tabPanel("Logements Non Lancés",                      
             fluidRow(
               column(width=8,style = "background-color:#eaeaea;width:61%",
                      fluidRow(
                        column(width=12,style = "background-color: transparent;zoom:100%",
                               wellPanel(id="well12_nonlances",
                                         tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2_nonlances"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                                                  
                                                  actionBttn(
                                                    inputId = "tabmodalserie2_nonlances",
                                                    label = NULL,
                                                    style = "bordered", 
                                                    color = "success",
                                                    icon = icon("table")
                                                  )
                                                  
                                                  
                                         )
                                         ,div(style = "margin-top:0em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                              #highchartOutput("pie", height = 330)
                                              reactableOutput("table_nonlan",height = 330)
                                         ))
                        )
                      )
               ),
               column(width=4,style = "background-color:#eaeaea;;width:39%",
                      wellPanel(id="well1_nonlances",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1_nonlances"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie_nonlances"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                         
                                ),
                                div(
                                  style = "padding-top: 30px;zoom:100%;"
                                  ,highchartOutput("hchart1_nonlances", height = 130)
                                  
                                )
                      ),
                      div(id="widget_div",style="
                                 width: 33%;position: absolute;z-index: 9;top: 720px;right: 420px;"
                          
                          #width: 33%;position: absolute;z-index: 9;top: 495px;right: 302px;zoom: 1.4;
                          ,
                          sliderTextInput(
                            inputId = "annee_input_nonlances",
                            label = "", 
                            choices = c(2013:2020),
                            grid=TRUE,
                            animate=animationOptions(interval = 1000, loop = TRUE)
                          )
                      ),
                      wellPanel(id="well3_nonlances",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map_nonlances"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3_nonlances"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;')
                                )
                                
                                
                                ,div(style = "padding: 0px 0px;zoom:109%;height:500px;",
                                     #shinycssloaders::withSpinner(
                                     leafletOutput("distPlot2_nonlances",height=502
                                                   #ifelse(abad()==1,700,568) 
                                     )
                                     # )
                                ))
               )
               
               
             ),
             #tabmodalserie_eqp
             
             bsModal("modal2_nonlances",htmlOutput("tablededonnes2_nonlances"), "tabmodalserie2_nonlances", size = "large"
                     ,excelOutput("excel2_nonlances"))
    ),
    tabPanel("Analyse",
             htmlOutput("analyse_doctohtml")
    )
    
    
    
    
  )
  
  #)
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  addResourcePath("pdf_files_DGCMR", paste0(getwd(),"/pdf_files_DGCMR"))
  
  #   observe({
  #     runjs("
  # gh=document.getElementById('table_conso21');
  # gh.children[0].style.zoom=0.91;
  #                    ")
  #   })
  #   
  
  
  
  output$analyse_doctohtml<-renderUI({
    tags$iframe(
      seamless="seamless",
      src=paste0("pdf_files_DGCMR/niveau3/Analyse0.html"),
      width="1750px",
      height="780px"
    )
  })
  
  output$excel2<-renderExcel({
    excelTable(
      data=rbind(colnames(dd0d),dd0d),
      showToolbar = TRUE,editable = FALSE
      ,columnSorting=FALSE)
  })
  
  
  
  
  output$tablededonnes2<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Consommations (en DA)</span> <br> <span style='font-size:13px;'>-2013 au Septembre 2021 - </span>"
    ))
  )
  
  
  
  output$titre_hchart1<-renderText({
    paste("Consommation par Années : (de 2013 au au Septembre 2021) ")
  })
  
  
  output$titre_map<-renderText({
    paste("Consommations par Wilaya :")
  })
  
  output$titre_map_co21<-renderText({
    paste("Taux de consommations par Wilayas :")
  })
  
  
  
  observe({
    js$opac99('-32px')
  })
  
  
  
  selected_table_conso <- reactive(
    `if`(length(getReactableState("table_conso", "selected"))==0,1:50,getReactableState("table_conso", "selected"))
  )
  
  selected_table_conso21 <- reactive(
    `if`(length(getReactableState("table_conso21", "selected"))==0,1:50,getReactableState("table_conso21", "selected"))
  )
  
  output$titre_serie3<-renderText({
    `if`(input$annee_input==2021,
         paste("-Année : 2021 (jusqu'au Septembre 2021) -"),
         paste0("-Année :",input$annee_input,"-")
    )
  })
  
  
  output$titre_serie3_co21<-renderText({
    paste0("-Mois :",input$annee_input_co21,"-")
  })
  
  
  
  
  output$titre_serie3<-renderText({
    `if`(input$annee_input==2021,
         paste("-Année : 2021 (jusqu'au Septembre 2021) -"),
         paste0("-Année :",input$annee_input,"-")
    )
  })
  
  
  
  output$titre_serie2<-renderText({
    paste0("Consommations par OPGI et Par Année :")
  })
  
  output$titre_serie2_co21<-renderText({
    paste0("Consommations et Prévisions en 2021 par OPGI et Par Mois :  - en Million DA -")
  })
  
  output$titre_serie<-renderText({
    `if`(length(selected_table_conso()) %in% c(0,50),paste0("Toutes les Wilayas"),
         paste0(dd0$OPGI[selected_table_conso()])
    )
  })
  
  
  output$titre_serie_co21<-renderText({
    `if`(length(selected_table_conso21()) %in% c(0,50),paste0("Consommations par Mois- Tous les OPGI -"),
         paste0(paste0("Consommations par Mois-",dd0_21$OPGI[selected_table_conso21()]))
    )
  })
  
  
  ###############
  
  
  
  output$distPlot2_co21<-renderLeaflet({
    mapdz %>%
      clearControls()%>%
      addLegend(
        title="Taux",
        position = "topright",
        
        pal=colorBin(palette=BuYlRd((0:100)/100),bins=c(0,25,50,75,100),
                     #  "Blues",bins = c(0,5,10,20,30),
                     # colorNumeric("Blues",
                     
                     consommation2021_data_distplot2 %>%
                       filter(Mois %in% input$annee_input_co21) %>% 
                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                       select(Taux) %>% .$Taux %>% as.numeric()
        ),
        
        opacity = 1,
        values=
          consommation2021_data_distplot2 %>%
          filter(Mois %in% input$annee_input_co21) %>% 
          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
          select(Taux) %>% .$Taux %>% as.numeric(),
        
        labFormat=labelFormat(
          suffix = "%",
          between = "% - "
        )
      ) %>%
      addPolygons(weight=1,
                  fillColor = colorBin(
                    palette=BuYlRd((0:100)/100),
                    bins = c(0,25,50,75,100),
                    na.color = "#315223",
                    #colorNumeric("Blues",
                    consommation2021_data_distplot2 %>%
                      filter(Mois %in% input$annee_input_co21) %>% 
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      select(Taux) %>% .$Taux %>% as.numeric()
                    
                    # 
                    # dd48 %>%
                    #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                    #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                    # 
                    #select(Total) %>% .$Total %>% as.numeric()
                  )(
                    
                    consommation2021_data_distplot2 %>%
                      filter(Mois %in% input$annee_input_co21) %>% 
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      select(Taux) %>% .$Taux %>% as.numeric()
                    
                    # 
                    # dd48 %>%
                    #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                    #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                    # 
                    #  #select(Total) %>% .$Total %>% as.numeric()
                  ),
                  
                  color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:16px;">%s</strong>  -   %s<br/>
<table style="font-size:14px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;padding-bottom:2px;">Taux : </td>
            <td style="width: 0.263361;padding-bottom:2px; font-weight:bold"> %s %%<br></td>
        </tr>
                <tr>
            <td style="width: 0.592437;padding-bottom:0px;">Consommations : </td>
            <td style="width: 0.263361;padding-bottom:0px; font-weight:bold"> %s <br></td>
        </tr>
                <tr>
            <td style="width: 0.592437;padding-bottom:0px;">Previsions : </td>
            <td style="width: 0.263361;padding-bottom:0px; font-weight:bold"> %s <br></td>
        </tr>
        
    </tbody>
</table>')  ,
                            consommation2021_data_distplot2 %>%
                              filter(Mois %in% input$annee_input_co21) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(OPGI) %>% .$OPGI,
                            
                            input$annee_input_co21,
                            
                            
                            consommation2021_data_distplot2 %>%
                              filter(Mois %in% input$annee_input_co21) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              mutate(Taux=round(Taux,1)) %>% 
                              select(Taux) %>% .$Taux %>% as.numeric(),
                            
                            consommation2021_data_distplot2 %>%
                              filter(Mois %in% input$annee_input_co21) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              mutate(Taux=round(Taux,1)) %>%
                              mutate(Consommations=paste(format(Realisation,digits=3,big.mark=" ",scientific=FALSE),"DA")) %>% 
                              mutate(Prevision=paste(format(Prevision,digits=3,big.mark=" ",scientific=FALSE),"DA")) %>% 
                              select(Consommations) %>% .$Consommations,
                            
                            consommation2021_data_distplot2 %>%
                              filter(Mois %in% input$annee_input_co21) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              mutate(Taux=round(Taux,1)) %>%
                              mutate(Consommations=paste(format(Realisation,digits=3,big.mark=" ",scientific=FALSE),"DA")) %>% 
                              mutate(Prevision=paste(format(Prevision,digits=3,big.mark=" ",scientific=FALSE),"DA")) %>% 
                              select(Prevision) %>% .$Prevision
                            
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(OPGI) %>% .$OPGI,
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                            # 
                            #   #select(Total) %>% .$Total %>% as.numeric()
                            # 
                            # 
                            # 
                    ) %>% lapply(htmltools::HTML),
                  fillOpacity = 1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "left",
                    offset = c(-130,35)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 1,bringToFront=TRUE
                  )
      )
  })
  
  
  ########
  
  
  
  output$distPlot2<-renderLeaflet({
    mapdz %>%
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        #title=HTML("<p><h6><b>Consommations<h6/><p/><b/>"),
        title=HTML("Consommations <br/> <h6 style='margin-top:-2px;'>(en Milliard DA)</h6>"),
        #title=HTML("Consommations <br/> <h6>(en Milliard DA)</h6>"),
        
        
        
        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        
        pal=colorBin("Blues",bins = c(0,5,10,20,30),
                     # colorNumeric("Blues",
                     dd48 %>% 
                       select(1:10) %>% 
                       gather("Annee","Consommation",2:10) %>% 
                       filter(Annee %in% input$annee_input) %>% 
                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                       select(Consommation) %>% .$Consommation %>% as.numeric()
                     
                     
                     
                     # dd48 %>%
                     #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                     #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                     # select(Total) %>% .$Total %>% as.numeric()
        ),
        
        opacity = 1,
        values=
          dd48 %>% 
          select(1:10) %>% 
          gather("Annee","Consommation",2:10) %>% 
          filter(Annee %in% input$annee_input) %>% 
          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
          select(Consommation) %>% .$Consommation %>% as.numeric()
        
        
        # dd48 %>%
        # arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
        # select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
        # 
        #select(Total) %>% .$Total %>% as.numeric()
      ) %>%
      addPolygons(weight=1,
                  fillColor = colorBin("Blues",bins = c(0,5,10,20,30),na.color = "#08081a",
                                       #colorNumeric("Blues",
                                       dd48 %>% 
                                         select(1:10) %>% 
                                         gather("Annee","Consommation",2:10) %>% 
                                         filter(Annee %in% input$annee_input) %>% 
                                         arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                         select(Consommation) %>% .$Consommation %>% as.numeric()
                                       
                                       
                                       # 
                                       # dd48 %>%
                                       #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                       #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                                       # 
                                       #select(Total) %>% .$Total %>% as.numeric()
                  )(
                    
                    dd48 %>% 
                      select(1:10) %>% 
                      gather("Annee","Consommation",2:10) %>% 
                      filter(Annee %in% input$annee_input) %>% 
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      select(Consommation) %>% .$Consommation %>% as.numeric()
                    
                    
                    # 
                    # dd48 %>%
                    #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                    #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                    # 
                    #  #select(Total) %>% .$Total %>% as.numeric()
                  ),
                  
                  color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:16px;">%s</strong>   -   %s -<br/>
<table style="font-size:14px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;padding-bottom:5px;">Consommations : </td>
            <td style="width: 0.263361;padding-bottom:5px; font-weight:bold"> %s Millard DA<br></td>
        </tr>
        
    </tbody>
</table>')  ,
                            dd48 %>% 
                              select(1:10) %>% 
                              gather("Annee","Consommation",2:10) %>% 
                              filter(Annee %in% input$annee_input) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(OPGI) %>% .$OPGI,
                            
                            input$annee_input,
                            
                            dd48 %>% 
                              select(1:10) %>% 
                              gather("Annee","Consommation",2:10) %>% 
                              filter(Annee %in% input$annee_input) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(Consommation) %>% .$Consommation %>% as.numeric()
                            
                            
                            
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(OPGI) %>% .$OPGI,
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                            # 
                            #   #select(Total) %>% .$Total %>% as.numeric()
                            # 
                            # 
                            # 
                    ) %>% lapply(htmltools::HTML),
                  fillOpacity = 1,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "left",
                    offset = c(-130,35)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 1,bringToFront=TRUE
                  )
      )
  })
  
  
  
  output$excel2_co21<-renderExcel({
    excelTable(
      data=rbind(colnames(dco21),dco21),
      showToolbar = TRUE,editable = FALSE
      ,columnSorting=FALSE)
  })
  
  output$tablededonnes2_co21<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Taux, Consommations et Prévisions</span> <br> <span style='font-size:13px;'>-de Janvier au Septembre 2021 - </span>"
    ))
  )
  
  
  
  
  output$table_conso21 <- renderReactable({
    
    reactable(dco21 ,
              defaultPageSize = 48, striped = TRUE,borderless = TRUE,
              pagination = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "16px"),
              height="925px",  selection = "single",
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              columns = list(
                OPGI=colDef(
                  width=130,
                  style = list(fontSize = "16px"
                               #,paddingTop="18px"
                  )
                  ,footer="TOTAL"
                ),
                taux1 = colDef(
                  width=95,
                  name = "Janvier",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real1 <- dco21$Real1[index]
                    Prev1 <- dco21$Prev1[index]
                    
                    #Real1 <- paste("Conso:",Real1)
                    #Prev1 <- paste("Previ:",Prev1)
                    
                    
                    
                    div(
                      #div(style = list(fontWeight = 600),value),
                      
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                          #(as.numeric(str_sub(value,-8,-2)) - min(as.numeric(str_sub(dco21$taux1,-8,-2)))) / (max(as.numeric(str_sub(dco21$taux1,-8,-2))) - min(as.numeric(str_sub(dco21$taux1,-8,-2))))
                          #(as.numeric(str_sub(value,-8,-2)) - 0) / (100 - 0)
                          
                          
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real1)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev1)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  }
                  ,footer=sprintf("%1.1f%%", 100*sum(dco21$Real1)/sum(dco21$Prev1))
                  
                ),
                ######################################
                ######################################
                taux2 = colDef(
                  width=95,
                  name = "Février",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real2 <- dco21$Real2[index]
                    Prev2 <- dco21$Prev2[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                          #(as.numeric(str_sub(value,-8,-2)) - min(as.numeric(str_sub(dco21$taux1,-8,-2)))) / (max(as.numeric(str_sub(dco21$taux1,-8,-2))) - min(as.numeric(str_sub(dco21$taux1,-8,-2))))
                          #(as.numeric(str_sub(value,-8,-2)) - 0) / (100 - 0)
                          
                          
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real2)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev2)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real2)/sum(dco21$Prev2))
                  
                ),
                ######################################
                ######################################
                ######################################
                ######################################
                taux3 = colDef(
                  width=95,
                  name = "Mars",
                  
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real3 <- dco21$Real3[index]
                    Prev3 <- dco21$Prev3[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real3)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev3)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real3)/sum(dco21$Prev3))
                  
                ),
                ######################################
                ######################################
                ######################################
                ######################################
                taux4 = colDef(
                  width=95,
                  name = "Avril",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real4 <- dco21$Real4[index]
                    Prev4 <- dco21$Prev4[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real4)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev4)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real4)/sum(dco21$Prev4))
                  
                ),
                ######################################
                ######################################
                
                ######################################
                ######################################
                taux5 = colDef(
                  width=95,
                  name = "Mai",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real5 <- dco21$Real5[index]
                    Prev5 <- dco21$Prev5[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real5)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev5)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real5)/sum(dco21$Prev5))
                  
                ),
                ######################################
                ######################################
                ######################################
                ######################################
                taux6 = colDef(
                  width=98,
                  name = "Juin",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real6 <- dco21$Real6[index]
                    Prev6 <- dco21$Prev6[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real6)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev6)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real6)/sum(dco21$Prev6))
                  
                ),
                ######################################
                ######################################
                ######################################
                ######################################
                taux7 = colDef(
                  width=100,
                  name = "Juillet",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real7 <- dco21$Real7[index]
                    Prev7 <- dco21$Prev7[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real7)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev7)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real7)/sum(dco21$Prev7))
                  
                ),
                ######################################
                ######################################
                ######################################
                ######################################
                taux8 = colDef(
                  width=100,
                  name = "Août",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real8 <- dco21$Real8[index]
                    Prev8 <- dco21$Prev8[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real8)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev8)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real8)/sum(dco21$Prev8))
                  
                ),
                ######################################
                ######################################
                ######################################
                ######################################
                taux9 = colDef(
                  width=105,
                  name = "Septembre",
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Real9 <- dco21$Real9[index]
                    Prev9 <- dco21$Prev9[index]
                    
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Real9)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prev9)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Real9)/sum(dco21$Prev9))
                  
                ),
                ######################################
                ######################################
                ######################################
                ######################################
                tauxtotal = colDef(
                  width=105,
                  name = "Total",
                  headerStyle = list(
                    textAlign="center"
                  ),
                  html = TRUE,style=list(fontSize='18px'),
                  header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">en Million DA</div>'
      }
    "),
                  cell = function(value, index) {
                    Realtotal <- dco21$Realtotal[index]
                    Prevtotal <- dco21$Prevtotal[index]
                    
                    div(
                      div(style = list(
                        paddingLeft="3px",color="black",textAlign="center",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),sprintf("%1.1f%%", 100*value)),
                      
                      #div(style = list(fontSize = 12), rea_prev1)
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Cons:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Realtotal)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      )
                      
                      ),
                      div(style = list(
                        fontSize=13,color="black",textAlign="left",
                        background = BuYlRd(
                          `if`(as.numeric(value)>1,1,
                               as.numeric(value)                                        
                          )
                        )
                      ),
                      htmltools::tags$span(tags$span(style='font-size:11px',"Prev:"),
                                           tags$span(
                                             as.numeric(format(round(as.numeric(Prevtotal)/ 1e6, 0), trim = TRUE)),
                                             tags$span(style='font-size:10px'," MD")
                                           )
                      ) )
                    )
                  },footer=sprintf("%1.1f%%", 100*sum(dco21$Realtotal)/sum(dco21$Prevtotal))
                  
                ),
                ######################################
                ######################################
                
                Real1 = colDef(show = FALSE),
                Prev1 = colDef(show = FALSE),
                
                Real2 = colDef(show = FALSE),
                Prev2 = colDef(show = FALSE),
                
                Real3 = colDef(show = FALSE),
                Prev3 = colDef(show = FALSE),
                
                Real4 = colDef(show = FALSE),
                Prev4 = colDef(show = FALSE),
                
                Real5 = colDef(show = FALSE),
                Prev5 = colDef(show = FALSE),
                
                Real6 = colDef(show = FALSE),
                Prev6 = colDef(show = FALSE),
                
                Real7 = colDef(show = FALSE),
                Prev7 = colDef(show = FALSE),
                
                Real8 = colDef(show = FALSE),
                Prev8 = colDef(show = FALSE),
                
                Real9 = colDef(show = FALSE),
                Prev9 = colDef(show = FALSE),
                
                Realtotal = colDef(show = FALSE),
                Prevtotal = colDef(show = FALSE)
                
              ),
              #defaultColDef = colDef(vAlign = "center", headerVAlign = "bottom"),
              bordered = TRUE,
              fullWidth = FALSE
    )
    
    
    
  })
  
  consommation2021_data_hchart_reactive<-reactive({
    
    #filter(Wilayas %in% c(dd0_livraison$Wilayas[selected_table_livrai()])) %>%
    
    consommation2021_data_hchart %>% 
      filter(id_mois %in% c(1:9),OPGI %in% c(unique(consommation2021_data_hchart$OPGI))[selected_table_conso21()]) %>% 
      #filter(id_wilaya %in% 1:50) %>% 
      group_by(id_mois) %>% 
      summarise(Prevision=sum(Prevision),
                Realisation=sum(Realisation),
                Taux=Realisation/Prevision) %>%
      mutate(Mois=c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre")) %>% 
      select(5,2,3,4) %>% 
      mutate(Taux=round(100*Taux,1)) %>% 
      mutate(Prevision=round(Prevision,2))
  })
  
  
  consommation2021_data_hchart_reactive2<-reactive({
    coco2=consommation2021_data_hchart_reactive() %>% 
      select(1:3) %>% 
      gather("Etat","Nb",2:3)
    
    coco2$Taux22=rep(NA,18)
    coco2$Taux22[10:18]=paste0(round(coco1$Taux,0),"%")
    
    coco2=coco2 %>% arrange(desc(Etat))
    coco2
  })
  
  
  consommation2021_data_hchart_reactive11<-reactive({
    coco11=consommation2021_data_hchart_reactive()
    colnames(coco11)=c("Mois","Prevision01","Realisation01","Taux01")
    coco11
  })
  
  
  
  output$hchart1_co21<-renderHighchart({
    
    highchart() %>%
      hc_xAxis(
        labels=list(style=list(fontSize= "11px",fontWeight="normal"),rotation=0)
        #,categories=c("Janvier","Février","Mars","Avril","Mai","Juin","Juillet","Août","Septembre")
        ,categories=c("Janv","Fév","Mars","Avril","Mai","Juin","Juil","Août","Sept")
        
      ) %>%
      hc_yAxis_multiples(
        list(title=list(text=HTML("Consommations")),opposite=FALSE,
             min=0,max=max(consommation2021_data_hchart_reactive()$Realisation)+quantile(consommation2021_data_hchart_reactive()$Realisation,0.05) %>% as.vector(),
             labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ') + ' DA'
		}"))
             
        )) %>%
      hc_add_series(consommation2021_data_hchart_reactive()    %>% mutate(Taux22=paste0(round(Taux,0),"%")
                                                                          ,Prevision22=paste0(format(Prevision,big.mark = " ",trim=TRUE,digits = 3,scientific=FALSE)," DA")
                                                                          ,Consommations22=paste0(format(Realisation,big.mark = " ",trim=TRUE,digits = 3,scientific=FALSE)," DA")
                                                                          
      )
      ,type="column",hcaes(x=Mois,y=Realisation),name=c("Consommations (DA)")) %>%
      hc_add_series(consommation2021_data_hchart_reactive11(),type="line",hcaes(x=Mois,y=Realisation01,),name="",color='#072238',showInLegend=FALSE,enableMouseTracking=FALSE) %>%
      
      
      hc_plotOptions(
        series = list(
          showInLegend = TRUE,
          pointFormat = "{point.Taux22}",
          dataLabels=list(style=list(fontSize= "15px",fontWeight="bold",color='#007f24')
                          ,format="{point.Taux22}"
                          ,enabled=TRUE
                          ,verticalAlign='top'
                          ,x=3
                          ,y=-30
                          ,inside=FALSE
                          ,allowOverlap=FALSE
                          ,crop=TRUE
                          ,overflow='none'
          )
        )
      ) %>% 
      
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = FALSE, 
        borderWidth = 3,
        useHTML=TRUE,
        headerFormat='<span style="font-size:14px;" {point.x} </span>',
        style=list(fontSize='16px')
        #sort=TRUE
        ,pointFormat='
                <span style="color:#0266C8"> \u25CF </span> Consommations : <b>{point.Consommations22}</b>
                <br/>
                <span style="color:#F90101"> \u25CF </span> Prevision : <b>{point.Prevision22}</b>
                <br/>
                <span style="color:#007f24"> \u25CF </span> Taux : <b>{point.Taux22}
                <br/>
                 '
      ) %>% 
      hc_legend(align= 'right') %>% 
      hc_add_theme(hc_theme_google())
    
  })
  
  
  
  ###########
  
  output$table_conso <- renderReactable({
    #reactable(donnees_consomation,
    reactable(dd0,
              defaultPageSize = 48, striped = TRUE,borderless = TRUE,
              pagination = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "16px"),
              height="800px",  selection = "single",
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              columns = list(
                OPGI=colDef(width = 158,footer="Total"),
                `2013`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2013`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2014`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2014`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2015`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2015`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2016`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2016`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2017`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2017`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2018`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2018`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2019`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2019`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                              
                ),
                `2020`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2020`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2021`=colDef(name="2021",html = TRUE,
                              header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">au mois de Septembre</div>'
      }
    "),
                              footer=paste(format(digits=3,big.mark=" ",sum(dd0$`2021`),scientific = FALSE),"Md"),width=79,
                              format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0[,2:10])) / (max(dd0[,2:10]) - min(dd0[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `Total`=colDef(footer=paste(format(digits=3,big.mark=" ",sum(dd0$`Total`),scientific = FALSE),"Md"),width=90,
                               format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                               style = function(value) {
                                 normalized <- (value - min(dd0[,11])) / (max(dd0[,11]) - min(dd0[,11]))
                                 color <- blues_pal(normalized)
                                 
                                 fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                 list(background = color,color=fontcolor,fontWeight = "bold",textAlign="right")
                               }
                )
                
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  
  output$hchart1<-renderHighchart({
    hchart(
      dd %>% 
        filter(OPGI %in% c(dd0$OPGI[selected_table_conso()])) %>%
        group_by(Annee) %>% 
        summarise(Consommation=sum(consomation)),
      
      "line",hcaes(x=Annee,y=Consommation))%>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        pointFormat='Consommation : <b>{point.y} DA</b><br/>'
        
        
      )%>%
      hc_chart(
        backgroundColor = "#ffffff"
      )%>%
      hc_yAxis(
        labels=list(style=list(
          #fontSize= "15px",
          fontWeight="normal"),
          formatter=JS('function() {
			if ( this.value > 1000000000 ) return Highcharts.numberFormat(this.value/1000000000,0)+" Milliard DA";
			return Highcharts.numberFormat(this.value, 0, "."," ") + " DA";
		}')),
        title=list(text = ""),
        reversedStacks=FALSE
        
      )%>%
      hc_xAxis(
        title=list(text = ""),
        tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
                      }")
      )
  })
  
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ################ tab LIVRAISON  ########################### 
  ################ tab LIVRAISON  ########################### 
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  
  
  
  output$excel2_livraison<-renderExcel({
    excelTable(
      data=rbind(colnames(dd0d_livraison),dd0d_livraison),
      showToolbar = TRUE,editable = FALSE
      ,columnSorting=FALSE)
  })
  
  
  output$tablededonnes2_livraison<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Livraisons</span> <br> <span style='font-size:13px;'>-2013 au Juin 2021 - </span>"
    ))
  )
  
  
  
  output$titre_hchart1_livraison<-renderText({
    paste("Livraisons par Années : (de 2013 au au Juin 2021) ")
  })
  
  
  output$titre_map_livraison<-renderText({
    paste("Livraisons par Wilaya :")
  })
  
  
  
  
  selected_table_livrai <- reactive(
    `if`(length(getReactableState("table_livrai", "selected"))==0,1:48,getReactableState("table_livrai", "selected"))
  )
  
  output$titre_serie3_livraison<-renderText({
    `if`(input$annee_input_livraison==2021,
         paste("-Année : 2021 (jusqu'au Juin 2021) -"),
         paste0("-Année :",input$annee_input_livraison,"-")
    )
  })
  
  
  
  output$titre_serie2_livraison<-renderText({
    paste0("Livraisons par Wilayas et Par Année :")
  })
  
  output$titre_serie_livraison<-renderText({
    `if`(length(selected_table_livrai()) %in% c(0,48),paste0("Toutes les Wilayas"),
         paste0(dd_livraison$Wilayas[selected_table_livrai()])
    )
  })
  
  
  
  
  output$distPlot2_livraison<-renderLeaflet({
    mapdz %>%
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        #title=HTML("<p><h6><b>Consommations<h6/><p/><b/>"),
        title=HTML("Livraisons"),
        #title=HTML("Consommations <br/> <h6>(en Milliard DA)</h6>"),
        
        
        
        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        
        pal=colorBin("Blues",
                     #bins = c(0,1000,2500,5000,10000,20000,30000),
                     #bins=4,
                     #bins=c(0,500,1000,1500,2000,5000,10000,20000),
                     bins=c(0,500,1000,2000,5000,10000,15000),
                     
                     #bins = c(0,200,500,1000,2500,5000,10000,20000,30000),
                     #pretty=TRUE,
                     
                     
                     # colorNumeric("Blues",
                     dd48_livraison %>% 
                       select(1:10) %>% 
                       gather("Annee","Livraison",2:10) %>% 
                       filter(Annee %in% input$annee_input_livraison) %>% 
                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                       select(Livraison) %>% .$Livraison %>% as.numeric()
                     
                     
                     
                     # dd48 %>%
                     #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                     #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                     # select(Total) %>% .$Total %>% as.numeric()
        ),
        
        opacity = 1,
        values=
          dd48_livraison %>% 
          select(1:10) %>% 
          gather("Annee","Livraison",2:10) %>% 
          filter(Annee %in% input$annee_input_livraison) %>% 
          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
          select(Livraison) %>% .$Livraison %>% as.numeric()
        
        
        # dd48 %>%
        # arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
        # select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
        # 
        #select(Total) %>% .$Total %>% as.numeric()
      ) %>%
      addPolygons(weight=1,
                  fillColor = colorBin("Blues",
                                       #c(0,1000,2500,5000,10000,20000,30000)
                                       #bins=4
                                       #bins=c(0,500,1000,1500,2000,5000,10000,20000)
                                       bins=c(0,500,1000,2000,5000,10000,15000)
                                       
                                       
                                       #c(0,200,500,1000,2500,5000,10000,20000,30000),
                                       #pretty=TRUE
                                       #,na.color = "#02101d"
                                       ,
                                       #colorNumeric("Blues",
                                       dd48_livraison %>% 
                                         select(1:10) %>% 
                                         gather("Annee","Livraison",2:10) %>% 
                                         filter(Annee %in% input$annee_input_livraison) %>% 
                                         arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                         select(Livraison) %>% .$Livraison %>% as.numeric()
                                       
                                       
                                       # 
                                       # dd48 %>%
                                       #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                       #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                                       # 
                                       #select(Total) %>% .$Total %>% as.numeric()
                  )(
                    
                    dd48_livraison %>% 
                      select(1:10) %>% 
                      gather("Annee","Livraison",2:10) %>% 
                      filter(Annee %in% input$annee_input_livraison) %>% 
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      select(Livraison) %>% .$Livraison %>% as.numeric()
                    
                    
                    # 
                    # dd48 %>%
                    #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                    #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                    # 
                    #  #select(Total) %>% .$Total %>% as.numeric()
                  ),
                  
                  color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:16px;">%s</strong><br/>
<table style="font-size:14px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;padding-bottom:5px;">Livraisons : </td>
            <td style="width: 0.263361;padding-bottom:5px; font-weight:bold"> %s<br></td>
        </tr>
        
    </tbody>
</table>')  ,
                            dd48_livraison %>% 
                              select(1:10) %>% 
                              gather("Annee","Livraison",2:10) %>% 
                              filter(Annee %in% input$annee_input_livraison) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(Wilayas) %>% .$Wilayas,
                            
                            dd48_livraison %>% 
                              select(1:10) %>% 
                              gather("Annee","Livraison",2:10) %>% 
                              filter(Annee %in% input$annee_input_livraison) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(Livraison) %>% .$Livraison %>% as.numeric()
                            
                            
                            
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(OPGI) %>% .$OPGI,
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                            # 
                            #   #select(Total) %>% .$Total %>% as.numeric()
                            # 
                            # 
                            # 
                    ) %>% lapply(htmltools::HTML),
                  fillOpacity = 0.7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "left",
                    offset = c(-130,35)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 0.7,bringToFront=TRUE
                  )
      )
  })
  
  
  
  
  
  
  
  output$table_livrai <- renderReactable({
    #reactable(donnees_consomation,
    reactable(dd0_livraison,
              defaultPageSize = 48, striped = TRUE,borderless = TRUE,
              pagination = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "16px"),
              height="800px",  selection = "single",
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              columns = list(
                Wilayas=colDef(width = 158,footer="Total"),
                `2013`=colDef(footer=paste(format(sum(dd0_livraison$`2013`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2014`=colDef(footer=paste(format(sum(dd0_livraison$`2014`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)                              }
                ),
                `2015`=colDef(footer=paste(format(sum(dd0_livraison$`2015`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)                              }
                ),
                `2016`=colDef(footer=paste(format(sum(dd0_livraison$`2016`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2017`=colDef(footer=paste(format(sum(dd0_livraison$`2017`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2018`=colDef(footer=paste(format(sum(dd0_livraison$`2018`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2019`=colDef(footer=paste(format(sum(dd0_livraison$`2019`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                              
                ),
                `2020`=colDef(footer=paste(format(sum(dd0_livraison$`2020`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2021`=colDef(name="2021",html = TRUE,
                              header = JS("
      function(colInfo) {
        return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">au mois de Juin</div>'
      }
    "),
                              footer=paste(format(sum(dd0_livraison$`2021`),scientific = FALSE)),width=79,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_livraison[,2:10])) / (max(dd0_livraison[,2:10]) - min(dd0_livraison[,2:10]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `Total`=colDef(footer=paste(format(sum(dd0_livraison$`Total`),scientific = FALSE)),width=90,
                               # format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                               style = function(value) {
                                 normalized <- (value - min(dd0_livraison[,11])) / (max(dd0_livraison[,11]) - min(dd0_livraison[,11]))
                                 color <- blues_pal(normalized)
                                 fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                 list(background = color,color=fontcolor,fontWeight = "bold",textAlign="right")
                               }
                )
                
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  
  output$hchart1_livraison<-renderHighchart({
    hchart(
      dd_livraison %>% 
        filter(Wilayas %in% c(dd0_livraison$Wilayas[selected_table_livrai()])) %>%
        group_by(Annee) %>% 
        summarise(Livraison=sum(Livraison)),
      
      "line",hcaes(x=Annee,y=Livraison))%>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        pointFormat='Livraisons : <b>{point.y}</b><br/>'
        
        
      )%>%
      hc_chart(
        backgroundColor = "#ffffff"
      )%>%
      hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
      ) %>% 
      #       hc_yAxis(
      #         labels=list(style=list(
      #           #fontSize= "15px",
      #           fontWeight="normal")
      # #           formatter=JS('function() {
      # # 			if ( this.value > 1000000000 ) return Highcharts.numberFormat(this.value/1000000000,0)+" Milliard DA";
      # # 			return Highcharts.numberFormat(this.value, 0, "."," ") + " DA";
      # # 		}')
      #           ),
      #         title=list(text = ""),
      #         reversedStacks=FALSE
    #         
    #       )%>%
    hc_xAxis(
      title=list(text = ""),
      tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
                      }")
    )
  })
  
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  
  
  
  
  
  
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ################ tab EnCours  ########################### 
  ################ tab EnCours  ########################### 
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  
  
  
  output$excel2_encours<-renderExcel({
    excelTable(
      data=rbind(colnames(dd0d_encours),dd0d_encours),
      showToolbar = TRUE,editable = FALSE
      ,columnSorting=FALSE)
  })
  
  
  output$tablededonnes2_encours<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Logements En Cours</span> <br> <span style='font-size:13px;'>-2013 au 2020 - </span>"
    ))
  )
  
  
  
  output$titre_hchart1_encours<-renderText({
    paste("Logements En Cours par Années : (de 2013 au 2020) ")
  })
  
  
  output$titre_map_livraison<-renderText({
    paste("Logements En Cours par Wilaya :")
  })
  
  
  
  
  selected_table_encou <- reactive(
    `if`(length(getReactableState("table_encou", "selected"))==0,1:48,getReactableState("table_encou", "selected"))
  )
  
  output$titre_serie3_encours<-renderText({
    `if`(input$annee_input_encours==2020,
         #paste("-Année : 2021 (jusqu'au Juin 2021) -"),
         paste0("-Année :",input$annee_input_encours,"-"),
         paste0("-Année :",input$annee_input_encours,"-")
    )
  })
  
  
  
  output$titre_serie2_encours<-renderText({
    paste0("Logements En Cours par Wilayas et Par Année :")
  })
  
  output$titre_serie_encours<-renderText({
    `if`(length(selected_table_encou()) %in% c(0,48),paste0("Toutes les Wilayas"),
         paste0(dd_encours$Wilayas[selected_table_encou()])
    )
  })
  
  
  
  
  output$distPlot2_encours<-renderLeaflet({
    mapdz %>%
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        #title=HTML("<p><h6><b>Consommations<h6/><p/><b/>"),
        title=HTML("Lgts En Cours"),
        #title=HTML("Consommations <br/> <h6>(en Milliard DA)</h6>"),
        
        
        
        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        
        pal=colorBin("Blues",
                     #bins = c(0,1000,2500,5000,10000,20000,30000),
                     #bins=4,
                     #bins=c(0,500,1000,1500,2000,5000,10000,20000),
                     bins=c(0,2500,5000,7500,10000,15000,20000,30000),
                     
                     #bins = c(0,200,500,1000,2500,5000,10000,20000,30000),
                     #pretty=TRUE,
                     
                     
                     # colorNumeric("Blues",
                     dd48_encours %>% 
                       select(1:9) %>% 
                       gather("Annee","EnCours",2:9) %>% 
                       filter(Annee %in% input$annee_input_encours) %>% 
                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                       select(EnCours) %>% .$EnCours %>% as.numeric()
                     
                     
                     
                     # dd48 %>%
                     #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                     #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                     # select(Total) %>% .$Total %>% as.numeric()
        ),
        
        opacity = 1,
        values=
          dd48_encours %>% 
          select(1:9) %>% 
          gather("Annee","EnCours",2:9) %>% 
          filter(Annee %in% input$annee_input_encours) %>% 
          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
          select(EnCours) %>% .$EnCours %>% as.numeric()
        
        
        # dd48 %>%
        # arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
        # select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
        # 
        #select(Total) %>% .$Total %>% as.numeric()
      ) %>%
      addPolygons(weight=1,
                  fillColor = colorBin("Blues",
                                       #c(0,1000,2500,5000,10000,20000,30000)
                                       #bins=4
                                       #bins=c(0,500,1000,1500,2000,5000,10000,20000)
                                       bins=c(0,2500,5000,7500,10000,15000,20000,30000)
                                       
                                       
                                       #c(0,200,500,1000,2500,5000,10000,20000,30000),
                                       #pretty=TRUE
                                       ,na.color = "#041c40"
                                       ,
                                       #colorNumeric("Blues",
                                       dd48_encours %>% 
                                         select(1:9) %>% 
                                         gather("Annee","EnCours",2:9) %>% 
                                         filter(Annee %in% input$annee_input_encours) %>% 
                                         arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                         select(EnCours) %>% .$EnCours %>% as.numeric()
                                       
                                       
                                       # 
                                       # dd48 %>%
                                       #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                       #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                                       # 
                                       #select(Total) %>% .$Total %>% as.numeric()
                  )(
                    
                    dd48_encours %>% 
                      select(1:9) %>% 
                      gather("Annee","EnCours",2:9) %>% 
                      filter(Annee %in% input$annee_input_encours) %>% 
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      select(EnCours) %>% .$EnCours %>% as.numeric()
                    
                    
                    # 
                    # dd48 %>%
                    #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                    #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                    # 
                    #  #select(Total) %>% .$Total %>% as.numeric()
                  ),
                  
                  color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:16px;">%s</strong><br/>
<table style="font-size:14px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;padding-bottom:5px;">Lgts En Cours : </td>
            <td style="width: 0.263361;padding-bottom:5px; font-weight:bold"> %s<br></td>
        </tr>
        
    </tbody>
</table>')  ,
                            dd48_encours %>% 
                              select(1:9) %>% 
                              gather("Annee","EnCours",2:9) %>% 
                              filter(Annee %in% input$annee_input_encours) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(Wilayas) %>% .$Wilayas,
                            
                            dd48_encours %>% 
                              select(1:9) %>% 
                              gather("Annee","EnCours",2:9) %>% 
                              filter(Annee %in% input$annee_input_encours) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(EnCours) %>% .$EnCours %>% as.numeric()
                            
                            
                            
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(OPGI) %>% .$OPGI,
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                            # 
                            #   #select(Total) %>% .$Total %>% as.numeric()
                            # 
                            # 
                            # 
                    ) %>% lapply(htmltools::HTML),
                  fillOpacity = 0.7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "left",
                    offset = c(-130,35)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 0.7,bringToFront=TRUE
                  )
      )
  })
  
  
  
  
  
  
  
  output$table_encou <- renderReactable({
    #reactable(donnees_consomation,
    reactable(dd0_encours,
              defaultPageSize = 48, striped = TRUE,borderless = TRUE,
              pagination = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "16px"),
              height="800px",  selection = "single",
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              columns = list(
                Wilayas=colDef(width = 165,footer="Total"),
                # `2013`=colDef(footer=paste(format(sum(dd0_encours$`2013`),scientific = FALSE)),width=79,
                #               #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                #               style = function(value) {
                #                 normalized <- (value - min(dd0_encours[,2:10])) / (max(dd0_encours[,2:10]) - min(dd0_encours[,2:10]))
                #                 color <- greens_pal(normalized)
                #                 list(background = color)
                #               }
                # ),
                `2014`=colDef(footer=paste(format(sum(dd0_encours$`2014`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2015`=colDef(footer=paste(format(sum(dd0_encours$`2015`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2016`=colDef(footer=paste(format(sum(dd0_encours$`2016`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2017`=colDef(footer=paste(format(sum(dd0_encours$`2017`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2018`=colDef(footer=paste(format(sum(dd0_encours$`2018`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2019`=colDef(footer=paste(format(sum(dd0_encours$`2019`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                              
                ),
                `2020`=colDef(footer=paste(format(sum(dd0_encours$`2020`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2013`=colDef(name="2013",
                              #,html = TRUE,
                              #                           header = JS("
                              #   function(colInfo) {
                              #     return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">au mois de Juin</div>'
                              #   }
                              # "),
                              footer=paste(format(sum(dd0_encours$`2013`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_encours[,2:9])) / (max(dd0_encours[,2:9]) - min(dd0_encours[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `Total`=colDef(footer=paste(format(sum(dd0_encours$`Total`),scientific = FALSE)),width=90,
                               # format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                               style = function(value) {
                                 normalized <- (value - min(dd0_encours[,10])) / (max(dd0_encours[,10]) - min(dd0_encours[,10]))
                                 color <- blues_pal(normalized)
                                 fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                 list(background = color,color=fontcolor,fontWeight = "bold",textAlign="right")
                               }
                )
                
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  
  output$hchart1_encours<-renderHighchart({
    hchart(
      dd_encours %>% 
        filter(Wilayas %in% c(dd0_encours$Wilayas[selected_table_encou()])) %>%
        group_by(Annee) %>% 
        summarise(EnCours=sum(EnCours)),
      
      "line",hcaes(x=Annee,y=EnCours))%>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        pointFormat='Logements En Cours : <b>{point.y}</b><br/>'
        
        
      )%>%
      hc_chart(
        backgroundColor = "#ffffff"
      )%>%
      hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
      ) %>% 
      
      
      # hc_yAxis(
      #   labels=list(style=list(
      #     #fontSize= "15px",
      #     fontWeight="normal")
      #     #           formatter=JS('function() {
      #     # 			if ( this.value > 1000000000 ) return Highcharts.numberFormat(this.value/1000000000,0)+" Milliard DA";
      #     # 			return Highcharts.numberFormat(this.value, 0, "."," ") + " DA";
      #     # 		}')
      #   ),
    #   title=list(text = ""),
    #   reversedStacks=FALSE
    #   
    # )%>%
    hc_xAxis(
      title=list(text = ""),
      tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
                      }")
    )
  })
  
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  
  
  
  
  
  
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ################ tab nonlances  ########################### 
  ################ tab nonlances  ########################### 
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  
  
  
  output$excel2_nonlances<-renderExcel({
    excelTable(
      data=rbind(colnames(dd0d_nonlances),dd0d_nonlances),
      showToolbar = TRUE,editable = FALSE
      ,columnSorting=FALSE)
  })
  
  
  output$tablededonnes2_nonlances<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Logements Non Lancés</span> <br> <span style='font-size:13px;'>-2013 au 2020 - </span>"
    ))
  )
  
  
  
  output$titre_hchart1_nonlances<-renderText({
    paste("Logements Non Lancés par Années : (de 2013 au 2020) ")
  })
  
  
  output$titre_map_livraison<-renderText({
    paste("Logements Non Lancés par Wilaya :")
  })
  
  
  
  
  selected_table_nonlan <- reactive(
    `if`(length(getReactableState("table_nonlan", "selected"))==0,1:48,getReactableState("table_nonlan", "selected"))
  )
  
  output$titre_serie3_nonlances<-renderText({
    `if`(input$annee_input_nonlances==2020,
         #paste("-Année : 2021 (jusqu'au Juin 2021) -"),
         paste0("-Année :",input$annee_input_nonlances,"-"),
         paste0("-Année :",input$annee_input_nonlances,"-")
    )
  })
  
  
  
  output$titre_serie2_nonlances<-renderText({
    paste0("Logements Non Lances par Wilayas et Par Année :")
  })
  
  output$titre_serie_nonlances<-renderText({
    `if`(length(selected_table_nonlan()) %in% c(0,48),paste0("Toutes les Wilayas"),
         paste0(dd_nonlances$Wilayas[selected_table_nonlan()])
    )
  })
  
  
  
  
  output$distPlot2_nonlances<-renderLeaflet({
    mapdz %>%
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        #title=HTML("<p><h6><b>Consommations<h6/><p/><b/>"),
        title=HTML("Lgts Non Lances"),
        #title=HTML("Consommations <br/> <h6>(en Milliard DA)</h6>"),
        
        
        
        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        
        pal=colorBin("Blues",
                     #bins = c(0,1000,2500,5000,10000,20000,30000),
                     #bins=4,
                     #bins=c(0,500,1000,1500,2000,5000,10000,20000),
                     bins=c(0,250,500,750,1000,2000,5000,10000),
                     
                     #bins = c(0,200,500,1000,2500,5000,10000,20000,30000),
                     #pretty=TRUE,
                     
                     
                     # colorNumeric("Blues",
                     dd48_nonlances %>% 
                       select(1:9) %>% 
                       gather("Annee","NonLances",2:9) %>% 
                       filter(Annee %in% input$annee_input_nonlances) %>% 
                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                       select(NonLances) %>% .$NonLances %>% as.numeric()
                     
                     
                     
                     # dd48 %>%
                     #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                     #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                     # select(Total) %>% .$Total %>% as.numeric()
        ),
        
        opacity = 1,
        values=
          dd48_nonlances %>% 
          select(1:9) %>% 
          gather("Annee","NonLances",2:9) %>% 
          filter(Annee %in% input$annee_input_nonlances) %>% 
          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
          select(NonLances) %>% .$NonLances %>% as.numeric()
        
        
        # dd48 %>%
        # arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
        # select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
        # 
        #select(Total) %>% .$Total %>% as.numeric()
      ) %>%
      addPolygons(weight=1,
                  fillColor = colorBin("Blues",
                                       #c(0,1000,2500,5000,10000,20000,30000)
                                       #bins=4
                                       #bins=c(0,500,1000,1500,2000,5000,10000,20000)
                                       bins=c(0,250,500,750,1000,2000,5000,10000)
                                       
                                       
                                       #c(0,200,500,1000,2500,5000,10000,20000,30000),
                                       #pretty=TRUE
                                       ,na.color = "#041c40"
                                       ,
                                       #colorNumeric("Blues",
                                       dd48_nonlances %>% 
                                         select(1:9) %>% 
                                         gather("Annee","NonLances",2:9) %>% 
                                         filter(Annee %in% input$annee_input_nonlances) %>% 
                                         arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                         select(NonLances) %>% .$NonLances %>% as.numeric()
                                       
                                       # 
                                       # dd48 %>%
                                       #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                       #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                                       # 
                                       #select(Total) %>% .$Total %>% as.numeric()
                  )(
                    
                    dd48_nonlances %>% 
                      select(1:9) %>% 
                      gather("Annee","NonLances",2:9) %>% 
                      filter(Annee %in% input$annee_input_nonlances) %>% 
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      select(NonLances) %>% .$NonLances %>% as.numeric()
                    # 
                    # dd48 %>%
                    #  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                    #  select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                    # 
                    #  #select(Total) %>% .$Total %>% as.numeric()
                  ),
                  
                  color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:16px;">%s</strong><br/>
<table style="font-size:14px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;padding-bottom:5px;">Lgts Non Lances : </td>
            <td style="width: 0.263361;padding-bottom:5px; font-weight:bold"> %s<br></td>
        </tr>
        
    </tbody>
</table>')  ,
                            dd48_nonlances %>% 
                              select(1:9) %>% 
                              gather("Annee","NonLances",2:9) %>% 
                              filter(Annee %in% input$annee_input_nonlances) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(Wilayas) %>% .$Wilayas,
                            
                            
                            dd48_nonlances %>% 
                              select(1:9) %>% 
                              gather("Annee","NonLances",2:9) %>% 
                              filter(Annee %in% input$annee_input_nonlances) %>% 
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              select(NonLances) %>% .$NonLances %>% as.numeric()
                            
                            
                            
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(OPGI) %>% .$OPGI,
                            # 
                            # dd48 %>%
                            #   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            #   select(input$annee_input) %>% .$input$annee_input %>% as.numeric()
                            # 
                            #   #select(Total) %>% .$Total %>% as.numeric()
                            # 
                            # 
                            # 
                    ) %>% lapply(htmltools::HTML),
                  fillOpacity = 0.7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "left",
                    offset = c(-130,35)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 0.7,bringToFront=TRUE
                  )
      )
  })
  
  
  
  
  
  
  
  output$table_nonlan <- renderReactable({
    #reactable(donnees_consomation,
    reactable(dd0_nonlances,
              defaultPageSize = 48, striped = TRUE,borderless = TRUE,
              pagination = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "16px"),
              height="800px",  selection = "single",
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              columns = list(
                Wilayas=colDef(width = 165,footer="Total"),
                # `2013`=colDef(footer=paste(format(sum(dd0_nonlances$`2013`),scientific = FALSE)),width=79,
                #               #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                #               style = function(value) {
                #                 normalized <- (value - min(dd0_nonlances[,2:10])) / (max(dd0_nonlances[,2:10]) - min(dd0_nonlances[,2:10]))
                #                 color <- greens_pal(normalized)
                #                 list(background = color)
                #               }
                # ),
                `2014`=colDef(footer=paste(format(sum(dd0_nonlances$`2014`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2015`=colDef(footer=paste(format(sum(dd0_nonlances$`2015`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2016`=colDef(footer=paste(format(sum(dd0_nonlances$`2016`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2017`=colDef(footer=paste(format(sum(dd0_nonlances$`2017`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2018`=colDef(footer=paste(format(sum(dd0_nonlances$`2018`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2019`=colDef(footer=paste(format(sum(dd0_nonlances$`2019`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                              
                ),
                `2020`=colDef(footer=paste(format(sum(dd0_nonlances$`2020`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `2013`=colDef(name="2013",
                              #,html = TRUE,
                              #                           header = JS("
                              #   function(colInfo) {
                              #     return colInfo.column.name + '<div style=\"color: #999;font-size:12px;\">au mois de Juin</div>'
                              #   }
                              # "),
                              footer=paste(format(sum(dd0_nonlances$`2013`),scientific = FALSE)),width=88,
                              #format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                              style = function(value) {
                                normalized <- (value - min(dd0_nonlances[,2:9])) / (max(dd0_nonlances[,2:9]) - min(dd0_nonlances[,2:9]))
                                color <- greens_pal(normalized)
                                fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                list(background = color,color=fontcolor)
                              }
                ),
                `Total`=colDef(footer=paste(format(sum(dd0_nonlances$`Total`),scientific = FALSE)),width=90,
                               # format = colFormat(separators = TRUE,locales = "fr-FR",suffix = " Md"),
                               style = function(value) {
                                 normalized <- (value - min(dd0_nonlances[,10])) / (max(dd0_nonlances[,10]) - min(dd0_nonlances[,10]))
                                 color <- blues_pal(normalized)
                                 fontcolor=`if`(normalized>0.9,"#cccccc","#000000")
                                 list(background = color,color=fontcolor,fontWeight = "bold",textAlign="right")
                               }
                )
                
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  
  output$hchart1_nonlances<-renderHighchart({
    hchart(
      dd_nonlances %>% 
        filter(Wilayas %in% c(dd0_nonlances$Wilayas[selected_table_nonlan()])) %>%
        group_by(Annee) %>% 
        summarise(NonLances=sum(NonLances)),
      
      "line",hcaes(x=Annee,y=NonLances))%>%
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        pointFormat='Logements Non Lances : <b>{point.y}</b><br/>'
        
        
      )%>%
      hc_chart(
        backgroundColor = "#ffffff"
      )%>%
      hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
      ) %>% 
      # hc_yAxis(
      #   labels=list(style=list(
      #     #fontSize= "15px",
      #     fontWeight="normal")
      #     #           formatter=JS('function() {
      #     # 			if ( this.value > 1000000000 ) return Highcharts.numberFormat(this.value/1000000000,0)+" Milliard DA";
      #     # 			return Highcharts.numberFormat(this.value, 0, "."," ") + " DA";
      #     # 		}')
      #   ),
      #   title=list(text = ""),
      #   reversedStacks=FALSE
    #   
    # )%>%
    hc_xAxis(
      title=list(text = ""),
      tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
                      }")
    )
  })
  
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  ###########################################################
  
  
  
  
  
  
  
  
  
}
#runApp(list(ui = ui, server = server),port=8180, launch.browser = TRUE)
shinyApp(ui = ui, server = server)