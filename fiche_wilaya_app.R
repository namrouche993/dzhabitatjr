setwd("C:/Users/nadji/amrouche2/electron-quick-start/ElectronShinyAppWindows/electron-quick-start-win32-ia32/resources/app")
#getwd()

.libPaths(paste0(getwd(),"/R-Portable-Win/library"))
.Library=paste0(getwd(),"/R-Portable-Win/library")

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

livraison_wilayas <- read_excel(paste0(getwd(),"/livraison_wilayas.xlsx"))
estimation_tolpopparc <- read_excel(paste0(getwd(),"/Estimation_Population_TOL_Parc_par_Wilaya.xlsx"))
before2000 <- read_excel(paste0(getwd(),"/before2000.xlsx"))
before00=before2000

data_fiche_wilaya <- read_excel(paste0(getwd(),"/data_fiche_wilaya.xlsx"))
data_fiche_wilaya$arretee=as.Date.character(data_fiche_wilaya$arretee)

lancement_wilayas <- read_excel(paste0(getwd(),"/lancement_wilayas.xlsx"))
lancement_wilayas=lancement_wilayas %>% select(c(2,3,4,5)) %>% mutate(Annee=as.numeric(Annee))

estimation_tolpopparc[,3:5]=round(estimation_tolpopparc[,3:5])
sit_fin <- read_excel(paste0(getwd(),"/Situation Financiere et Loi 18-05 .xlsx"))
sit_fin$Arretee=as.Date.character(sit_fin$Arretee)
zones <- read_excel(paste0(getwd(),"/zones2.xlsx"))
zones$Arretee=as.Date.character(zones$Arretee)
zones00=zones
zones=zones[1:48,]
sitphy <- read_excel(paste0(getwd(),"/sitphy.xlsx"))
sitphy$`Type de logements`[which(sitphy$`Type de logements`=="RURAL")]=c("Rural")
sitphy$Arretee=as.Date.character(sitphy$Arretee)

# for(i in 1:nrow(zones)){
#   for(j in 1:ncol(zones)){
#     zones[i,j]=print(zones[i,j])
#   }
# }
# 
# myspread <- function(df, key, value) {
#   # quote key
#   keyq <- rlang::enquo(key)
#   # break value vector into quotes
#   valueq <- rlang::enquo(value)
#   s <- rlang::quos(!!valueq)
#   df %>% gather(variable, value, !!!s) %>%
#     unite(temp, !!keyq, variable) %>%
#     spread(temp, value)
# }

Statistiques_des_projets_par_secteurs <- read_excel(paste0(getwd(),"/Statistiques des projets par secteurs.xlsx"))
Statistiques_des_projets_par_secteurs$Arretee=as.Date.character(Statistiques_des_projets_par_secteurs$Arretee)

equip=Statistiques_des_projets_par_secteurs[,-1]
#equip[,1:7]
colnames(equip)[4]="En Cours"
colnames(equip)[2]="Nbre de Projets"
colnames(equip)[3]="Acheves"
colnames(equip)[5]="Non Lances"
colnames(equip)[7]="Arretee"
colnames(equip)[8]="Dont NIR"
colnames(equip)[9]="Geles"



equip11=equip[,c(1,6,2,3,4,5,8,9,7)]
data_equip = equip %>%
  #  filter(Secteur==secteurselecteqp()) %>%
  group_by(Wilaya,Arretee) %>%
  summarise("Nbre de Projets"=sum(`Nbre de Projets`),Acheves=sum(Acheves),"En Cours"=sum(`En Cours`),"Non Lances"=sum(`Non Lances`),"Dont NIR"=sum(`Dont NIR`),"Geles"=sum(Geles)) %>% 
  select(1,3,4,5,6,7,8,2)


pos <- read_excel("pos.xlsx")
pos$`Non Lancées`[which(pos$URBANISME=='PDAU')]=rep(NA,48)
pos$Arretee=as.Date.character(pos$Arretee)
pos2=pos %>% 
  mutate(Wilaya=rep(unique(livraison_wilayas$waw),3)) %>% 
  select(Wilaya,URBANISME,Achevées,`En Cours`,`Non Lancées`)


s1=sit_fin[1:48,2:5]
s1[,5:7]=sit_fin[49:96,3:5]
s1[,8:10]=sit_fin[97:144,3:5]
s1[,11:13]=sit_fin[145:192,3:5]
s1[49,1]="Total"
for(i in 2:ncol(s1)){
  s1[49,i]=sum(s1[1:48,i])
}

sit_fin1=sit_fin[,c(2,3,6)] %>% spread(key = Type,value = NOTIFICATION)
sit_fin2=sit_fin[,c(2,4,6)] %>% spread(key = Type,value = INSCRIPTION)
sit_fin3=sit_fin[,c(2,5,6)] %>% spread(key = Type,value = Reliquat)

ss=data.frame(sit_fin1[,1:2],sit_fin2[,2],sit_fin3[,2]
              , sit_fin1[,3],sit_fin2[,3],sit_fin3[,3],
              sit_fin1[,4],sit_fin2[,4],sit_fin3[,4],
              sit_fin1[,5],sit_fin2[,5],sit_fin3[,5])
ss1=ss %>% select(1,2,5,8,11,3,6,9,12,4,7,10,13)


for(i in 1:ncol(sitphy)){
  for(j in 1:nrow(sitphy)){
    if(is.na(sitphy[j,i])==TRUE){sitphy[j,i]=0}  
  }
}
green_pal <- function(x) rgb(colorRamp(c("#d1e8d1", "#198c19"))(x), maxColorValue = 255)
blue_pal <- function(x) rgb(colorRamp(c("#d1d1ff", "#1919ff"))(x), maxColorValue = 255)
red_pal <- function(x) rgb(colorRamp(c("#ffcccc", "#ff0000"))(x), maxColorValue = 255)



sitphy0=sitphy %>% 
  group_by(Wilaya_matricule,Arretee) %>% 
  summarise(Livraison=sum(Livraison),Prevision=sum(Prevision),Consistance=sum(Consistance),Achevés=sum(Achevés),"En Cours"=sum(`En Cours`),"Non Lancés"=sum(`Non lancés`)) %>% 
  rename(Wilaya=Wilaya_matricule) %>% rowwise() %>% 
  #mutate(Consistance=format(Consistance2,big.mark = " ",trim=TRUE,digits = 3)) %>% 
  #mutate("Achevés"=sprintf("%1.0f%%", 100*sum(Achevés)/sum(Consistance2))) %>% 
  #mutate("En Cours"=sprintf("%1.0f%%", 100*sum(`En Cours`)/sum(Consistance2))) %>% 
  #mutate("Non Lancés"=sprintf("%1.0f%%", 100*sum(`Non Lancés`)/sum(Consistance2))) %>% 
  mutate("Achevés"=sum(Achevés)/sum(Consistance)) %>% 
  mutate("En Cours"=sum(`En Cours`)/sum(Consistance)) %>% 
  mutate(`Non Lancés`=sum(`Non Lancés`)/sum(Consistance)) %>% 
  mutate(a=as.numeric(str_sub(`Achevés`,-3,-2))) %>% 
  select(Arretee,Wilaya,Consistance,`Achevés`,`En Cours`,`Non Lancés`)

sitphy2=sitphy
sitphy2=add_column(sitphy,"Acheves%"=round(100*sitphy$Achevés/sitphy$Consistance,1),.after=5)
sitphy2=add_column(sitphy2,"En Cours%"=round(100*sitphy$`En Cours`/sitphy$Consistance,1),.after=7)
sitphy2=add_column(sitphy2,"Non Lancés%"=round(100*sitphy$`Non lancés`/sitphy$Consistance,1),.after=9)
colnames(sitphy2)[3]="Segment"

# 
# da0sit=cbind(sitphy2[which(sitphy2$`Type de logements`=="LPL"),c(2,4,5,6,7,8,9,10,11,12)],sitphy2[which(sitphy2$`Type de logements`=="Rural"),4:12],sitphy2[which(sitphy2$`Type de logements`=="LSP"),4:12],sitphy2[which(sitphy2$`Type de logements`=="Location-Vente"),4:12],sitphy2[which(sitphy2$`Type de logements`=="LPP"),4:12])
# da1sit=rbind(colnames(da0sit),da0sit)
# da2sit=rbind(c("Wilaya",rep(unique(sitphy2$`Type de logements`),each=9)),da1sit)
# da2sit[2,1]="Wilaya"
# 
col_stops <- data.frame(
  q = c(0.33, 0.66, .99),
  c = c('#DF5353','#DDDF0D','#55BF3B'),
  stringsAsFactors = FALSE
)


livraison_wilayas=livraison_wilayas%>%
  filter(type_de_logement %in% c( "LPL","Rural","LPP","LSP","Location-Vente"))

#liv=livraison_wilayas%>%
#  filter(annee==2019,type_de_logement=="LPL")
#colnames(liv)[14]="Parc_logement2019"
#colnames(liv)[12]="Population_2019"

###################################### VILLE #########################
data_ville=data.frame(matrix(0,nrow=6,ncol=13))
data_ville$X1=c("BOUINANE","SIDI ABDELLAH","BOUGHEZOUL","DRAA ERICH","ALI MENDJLI","EL MENEAA")
colnames(data_ville)=c("Nom","Wilaya_Matricule","Wilaya","zoom","setview_long","setview_lat","cor_long","cor_lat","logements","habitants","equipements","energie","transport")
data_ville$Wilaya_Matricule=c("09-BLIDA","16-ALGER","17-26 DJELFA MEDEA","23-ANNABA","25-Constantine","47-GHARDAIA")
data_ville$Wilaya=c("BLIDA","ALGER","DJELFA MEDEA","ANNABA","Constantine","GHARDAIA")


#providers[[1]]  #OpenStreetMap.Mapnik
#providers[[44]] # ne s'affiche pas quand on zoom plus
#providers[[55]] # ne s'affiche pas quand on zoom plus
#providers[[57]]  #Satellite


####Sidi abdellah
data_ville[2,4]=14
data_ville[2,5:6]=c(2.853174,36.685945)
data_ville[2,7:8]=c(2.853174,36.689245)

#Bouinane
data_ville[1,4]=15
data_ville[1,5:6]=c(2.957531,36.533262)
data_ville[1,7:8]=c(2.957531,36.533262)

#BOUGHEZOUL
data_ville[3,4]=13
data_ville[3,5:6]=c(2.848758,35.721653)
data_ville[3,7:8]=c(2.848758,35.741653)



#DRAA ERRICH
data_ville[4,4]=13
data_ville[4,5:6]=c(7.528319,36.858245)
data_ville[4,7:8]=c(7.528319,36.872245)

#ALI MENDJLI
data_ville[5,4]=14
data_ville[5,5:6]=c(6.572546,36.249052)
data_ville[5,7:8]=c(6.572546,36.254052)

#EL MENEAA
data_ville[6,4]=14
data_ville[6,5:6]=c(2.905987,30.594278)
data_ville[6,7:8]=c(2.915987,30.594278)



###################################### VILLE #########################



#algeria=rgdal::readOGR("/cloud/project/polbnda_dza.json")
#algeria=rgdal::readOGR(paste0(getwd(),"/polbnda_dza.json"))
countries <- geojsonio::geojson_read("polbnda_dza.json", what = "sp")
algeria <- rmapshaper::ms_simplify(countries, keep = 0.05, keep_shapes = TRUE)


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
algeria@data$wilayas=unique(livraison_wilayas$waw)[id_wilaya]

#algeria@data$nam=unique(livraison_wilayas$waw)[round(livraison_wilayas%>%
#                                                      group_by(id_wilaya)%>%
#                                                     summarise(liv=sum(Livraison))%>%
#                                                    arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,30,44,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
#                                                   select(id_wilaya))$id_wilaya]

mapdz=leaflet(algeria)%>%
  setView(lng = 3.03333 , lat = 28.6167, zoom = 5)%>%
  #clearBounds()%>%
  
  addProviderTiles("OpenStreetMap.BZH",
                   options = leafletOptions(minZoom = 5, maxZoom = 10,dragging = TRUE))%>%   #or we can use addProviderTiles(one_providers)
  
  setMapWidgetStyle(list(background= "#ffffff"))


mytheme0=create_theme(
  theme = "united",
  bs_vars_wells(
    bg = "#fff"
  ),
  bs_vars_wells(
    
  )
)

jsCode1 <- "shinyjs.opac1 = function(params){$('#well_gauge').css('opacity', params);}"
jsCode2 <- "shinyjs.opac2 = function(params){$('#well_gauge2').css('opacity', params);}"
jsCode3 <- "shinyjs.opac3 = function(params){$('#well_gauge3').css('opacity', params);}"
jsCode4 <- "shinyjs.opac4 = function(params){$('#well_gauge4').css('opacity', params);}"
jsCode5 <- "shinyjs.opac5 = function(params){$('#well_gauge5').css('opacity', params);}"
jsCode6 <- "shinyjs.opac6 = function(params){$('#well_gauge6').css('opacity', params);}"


jsCode77 <- "shinyjs.opac77 = function(params){$('#box-score2').css('display', params);}"

#jsCode88 <- "shinyjs.opac88 = function(params){$('#Id027').css('opacity', params);}"


# var df=document.getElementsByClassName('highcharts-data-label')
# df[2].children[0].lastElementChild.textContent="4645"   #(325 458)

# df[2].children[0].style.fontSize="0px"           # LSP 325 458 to 0px


# var arr=document.getElementsByClassName('highcharts-data-label-connector')
# arr[1].style.stroke="#ffffff"             for arrow

###################### fiche wilaya : 

ab5=data.frame(matrix(0,nrow=2,ncol=7))
colnames(ab5)=c("Segment","LPL","LSP","Rural","Location-Vente","LPP","Total")
ab5[,1]=c("Prevus","Livres")

ab=data.frame(matrix(0,nrow=6,ncol=7))
ab[,1]=c("LPL","LSP","Rural","Location-Vente","LPP","Programme Global")
colnames(ab)=c("Segment","Consistance","Acheves","En Cours","Dont A l'Arret","Non Lances","Notifie 2020")


ab2=data.frame(matrix(0,nrow=1,ncol=5))
colnames(ab2)=c("Segment","Consistance Actuelle","Acheves","En Cours","Non Lances")
ab2[1,1]="Réhabilitation"


ab3=data.frame(matrix(0,nrow=1,ncol=10))
colnames(ab3)=c("Acheves","En Cours","Non Lances","Total","Achevee","En cours","Non lances","TotaI","Acheves à 60% et plus","Total general")

ab4=data.frame(matrix(0,nrow=6,ncol=2))
#colnames(ab4)=c("Acheves","En Cours","Non Lances","Total","Achevee","En cours","Non lances","TotaI","Acheves à 60% et plus","Total general")
ab4[,1]=c("Logements acheves, viabilisation acheves","Logements acheves, viabilisation en cours","Logements acheves, viabilisation non entamee","TOTAL","Logements depassant 60% de taux d'avancement pouvant etre pre affectes","Total logements a attribuer et a pre-affecter")
############ fiche wilaya end 

ui <- fluidPage(
  #includeScript("www/check.js"),
  shinyjs::useShinyjs(),
  extendShinyjs(text = jsCode1, functions = c("opac1")),
  extendShinyjs(text = jsCode2, functions = c("opac2")),
  extendShinyjs(text = jsCode3, functions = c("opac3")),
  extendShinyjs(text = jsCode4, functions = c("opac4")),
  extendShinyjs(text = jsCode5, functions = c("opac5")),
  extendShinyjs(text = jsCode6, functions = c("opac6")),
  extendShinyjs(text = jsCode77, functions = c("opac77")),
  #extendShinyjs(text = jsCode88, functions = c("opac88")),
  
  
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
    HTML("Fiche Wilaya"),
    selected="Logements",
    id = "main_navbar",
                      tags$head(HTML('<style type="text/css">



#boxscore1_wilaya_urbanisme {
    display: inline;
    padding-left: 250px;
}

#boxscore2_wilaya_urbanisme {
    display: inline;
    padding-left: 250px;
}

#boxscore3_wilaya_urbanisme {
    display: inline;
    padding-left: 180px;
}

#boxscore4_wilaya_urbanisme {
    display: inline;
    padding-left: 180px;
}


#boxscore1_arretee_urbanisme{
    display: inline;
}


#boxscore2_arretee_urbanisme{
    display: inline;
}


#boxscore3_arretee_urbanisme{
    display: inline;
}

#boxscore4_arretee_urbanisme{
    display: inline;
}


#box-score-title1_urbanisme {
    margin-top: 24px;
    padding: 8px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 871px;
    font-family: system-ui;
}



#box-score-title2_urbanisme {
    margin-top: 24px;
    padding: 8px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1041px;
    font-family: system-ui;
}

#box-score-title3_urbanisme {
  margin-top: -20px;
    padding: 0px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 700px;
    font-family: system-ui;
    margin-left: -19px;
    padding-left: 12px;
    height: 35px;
    }


#box-score-title4_urbanisme {
  margin-top: -20px;
    padding: 0px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 700px;
    font-family: system-ui;
    margin-left: -19px;
    padding-left: 12px;
    height: 35px;
    }




#box-score1_urbanisme {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:188px;
  margin-bottom:50px;
}

#box-score2_urbanisme {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:100px;
  margin-bottom:50px;
}

#box-score3_urbanisme {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:85px;
  margin-bottom:50px;
}

                 
#select_arretee_urbanisme + .dropdown-toggle {
    background-color: #007aff;
    border-color: lightskyblue;
}

                
#select_arretee_urbanisme + .dropdown-toggle:hover {
    background-color: #006de5;
    border-color: black;
}


#taburbanisme {
    margin-right: -15px;
    margin-left: -13px;
    margin-top: -8px;
    overflow: -webkit-paged-x;
    zoom:0.95;

}

#c1_urbanisme {
margin-right:-300px;
}

#c12_urbanisme {
   margin-left: 155px;
   visibility: inherit;
   overflow-y: scroll;
   height: 900px;
}




#taburbanisme + .shiny-input-container {
    position: absolute;
    top: 21px;
    z-index: 999999;
    right: 128px;
}


#table2_urbanisme .ReactTable .rt-tr-group {
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
}


 
#table2_urbanisme .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
}


  
#table2_urbanisme .rt-td rt-align-left{
  width:144px;
}


  
#table2_urbanisme .rt-th-content {
    overflow: hidden;
    text-overflow: ellipsis;
    font-size: 12px;
    text-align: left;
  }











.icon11{
    zoom: 2.1;
    color: aquamarine;
    position: relative;
    top: 47px;
    right: -333px;
    z-index: 5000;
}

#wilayaselecteqp22 {
    display: inline;
    padding-left: 150px;
}




#box-score_equip {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  width:843px;
}

#box-score-title_equip {
    margin-top: 26px;
    padding: 2px;
    padding-left: 5px;
    background-color: #60777d;
    color: white;
    font-size: 18px;
    font-weight: 400;
    width: 837px;
    font-family: system-ui;
    }




#boxscore1_arretee{
    display: inline;
}

#boxscore2_arretee{
    display: inline;
}

#boxscore3_arretee{
    display: inline;
}
#boxscore5_arretee{
    display: inline;
}




#boxscore1_wilaya {
    display: inline;
    padding-left: 250px;
}


#boxscore2_wilaya {
    display: inline;
    padding-left: 150px;
}


#boxscore3_wilaya {
    display: inline;
    padding-left: 250px;
}


#boxscore5_wilaya {
    display: inline;
    padding-left: 250px;
}

#box-score3 {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
}

#box-score-title3 {
    margin-top: 24px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1182px;
    font-family: system-ui;
    }



#box-score2 {
    padding-left: 245px;
    margin-bottom: 50px;
    font-family: "Roboto", Helvetica, Arial, sans-serif;

}

#box-score-title2 {
    margin-top: 24px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 780px;
    font-family: system-ui;
    }


#box-score-title1 {
    margin-top: 24px;
    padding: 8px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1081px;
    font-family: system-ui;
}


#box-score1 {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:100px;
  margin-bottom:50px;
}


.box-score {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:185px;
}

.box-score-title {
    margin-top: 24px;
    padding: 8px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 900px;
    font-family: system-ui;
}
  
  .box-score-header {
  background-color: #333333;
  }




#gt3 {
margin-bottom:-1px;
}


#gt4 {
margin-bottom:70px;
margin-left:267px;
}


#fluidrow1_eqp + .shiny-input-container {
    position: absolute;
    top: 21px;
    z-index: 999999;
    right: 128px;
    }
    
                      
#select_arretee_eqp + .dropdown-toggle {
    background-color: #007aff;
    border-color: lightskyblue;
}
    
                 
#select_arretee_eqp + .dropdown-toggle:hover {
    background-color: #006de5;
    border-color: black;
}












#fluidrow1_urbanisme + .shiny-input-container {
    position: absolute;
    top: 21px;
    z-index: 999999;
    right: 128px;
    }
    
                      
#select_arretee_urbanisme + .dropdown-toggle {
    background-color: #007aff;
    border-color: lightskyblue;
}
    
                 
#select_arretee_urbanisme + .dropdown-toggle:hover {
    background-color: #006de5;
    border-color: black;
}

                      
#tabfichewilaya + .shiny-input-container {
    position: absolute;
    top: 21px;
    z-index: 999999;
    right: 128px;
    }
    

                   
                   
                      
#tabsitphy + .shiny-input-container {
    position: absolute;
    top: 21px;
    z-index: 999999;
    right: 128px;
    }
    
                      
#select_arretee_sitphy + .dropdown-toggle {
    background-color: #007aff;
    border-color: lightskyblue;
}

                  
#select_arretee_fichewilaya + .dropdown-toggle {
    background-color: #007aff;
    border-color: lightskyblue;
}
    
                 
#select_arretee_sitphy + .dropdown-toggle:hover {
    background-color: #006de5;
    border-color: black;
}


                
#select_arretee_fichewilaya + .dropdown-toggle:hover {
    background-color: #006de5;
    border-color: black;
}
    
      

.modal-open .modal {
    overflow-x: hidden;
    overflow-y: auto;
    z-index: 99999999;
}


#display_when_hover_choose_leaflet {
    position: absolute;
    top: 147px;
    right: 482px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 205px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 153px;
    display: none;
}


#display_when_hover_choose_line1 {
    position: absolute;
    top: 334px;
    right: 471px;
    z-index: 999999999;
    background: white;
    border: 3px solid darkgrey;
    width: 210px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 96px;
    display: none
}


#display_when_hover_choose_line1_pie {
    position: absolute;
    top: 263px;
    right: 471px;
    z-index: 999999999;
    background: white;
    border: 3px solid darkgrey;
    width: 210px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 86px;
    display: none
}



#choose_leaflet {
    position: absolute;
    top: 148px;
    right: 645px;
    z-index: 99999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
}

#choose_line1 {
    position: absolute;
    top: 373px;
    right: 619px;
    z-index: 999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
}


#choose_line1_pie {
    position: absolute;
    top: 305px;
    right: 619px;
    z-index: 999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
}




#choose_leaflet .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}

#choose_line1 .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}

#choose_line1_pie .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}

#eptotal{
    margin-top: -25px;
    text-align: end;
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

#tabmodal_sitphy {
    position: absolute;
    z-index: 9999;
    top: 51px;
}

#rowselect .shiny-input-container:not(.shiny-input-container-inline) {
    width: 300px;
    max-width: 100%;
    position: absolute;
    z-index: 999999;
    right: 900px;
    top:7px;
}

#select_segment_gauge{
padding-left:80px
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

#wilayaselectgauge1 {
    padding-top: 8px;
    padding-left: 71px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#wilayaselectgauge2 {
    padding-top: 8px;
    padding-left: 311px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#wilayaselectgauge3 {
    padding-top: 8px;
    padding-left: 308px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}


#wilayaselectgauge4 {
    padding-top: 8px;
    padding-left: 290px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#wilayaselectgauge5 {
    padding-top: 8px;
    padding-left: 165px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}


#wilayaselectgauge6 {
    padding-top: 8px;
    padding-left: 308px;
    color: white;
    font-family: cursive;
    font-size: 18px;
}



#cgauge6{
margin-left:150px;
}

#gauge6 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
}

#livraison_gauge6 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge6_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge6 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge6_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}




#well_gauge6{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge6 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
    display:flex;
    margin-bottom:10px;
}

#titregauge6 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}









#cgauge5{
margin-left:150px;
}

#gauge5 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;

}


#livraison_gauge5 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge5_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge5 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge5_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}




#well_gauge5{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge5 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
            margin-bottom:10px;


}

#titregauge5 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}









#cgauge4{
margin-left:150px;
}

#gauge4 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
    
}



#livraison_gauge4 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge4_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge4 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge4_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge4{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge4 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#titregauge4 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}





#gauge3 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
    
}
#livraison_gauge3 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge3_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge3 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge3_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge3{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge3 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#titregauge3 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}














#cgauge2{
margin-left:150px;
}

#gauge2 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
    
}


#livraison_gauge2 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge2_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge2 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge2_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge2{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge2 {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: darkcyan;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#titregauge2 {
    padding-left: 17px;
    color: white;
    font-size: 29px;
    padding-top: 0px;
    font-family: inherit;
}





#gauge1 {
    margin-left: -160px;
    padding-left: 92px;
    margin-top:-12px;
    height:247px;
}


#livraison_gauge1 {
    position: relative;
    top: -238px;
    right: -393px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#livraison_gauge1_val {
    position: relative;
    top: -250px;
    right: -171px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}

#prevision_gauge1 {
    position: relative;
    top: -246px;
    right: -394px;
    font-size: 24px;
    font-family: system-ui;
    font-weight: 449;
}

#prevision_gauge1_val {
    position: relative;
    top: -260px;
    right: -173px;
    font-size: 39px;
    font-family: system-ui;
    font-weight: 499;
    text-align: center;
}


#well_gauge{
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    width:570px;
    height:268px;
    background-color: ghostwhite;

}

.chart-title-gauge {
    width: 570px;
    height: 20px;
    border-bottom: 34px solid;
    border-bottom-color: lightcoral;
    font-size: 20px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -19px;
        display:flex;
    margin-bottom:10px;

}

#toutsegments_gauge {
    color: white;
    font-size: 21px;
    padding-top: 4px;
    margin-left: -6px;
}

#titregauge1 {
    padding-left: 17px;
    color: white;
    font-size: 27px;
    padding-top: 0px;
    font-family: inherit;
}


                      
#tabsitphy {
    margin-right: -15px;
    margin-left: -13px;
    margin-top: -8px;
    overflow: -webkit-paged-x;
    zoom:0.95;

}



                      
#tabfichewilaya {
    margin-right: -15px;
    margin-left: -13px;
    margin-top: -8px;
    overflow: -webkit-paged-x;
    zoom:0.95;

}



#region {
text-align: center;
    padding-bottom: 1px;
    font-size: 24px;
    margin-top: 6px;
    color: #5a5096;
}


#urbanisme1 {
    width: 100%;
    height: 350px;
    visibility: inherit;
    overflow: hidden;
    zoom:0.89;
}

                      
#urbanisme2 {
    width: 100%;
    height: 350px;
    visibility: inherit;
    overflow: hidden;
    zoom: 0.88;
    padding-top: 10px;
}


#table2 .ReactTable .rt-tr-group {
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
}


#table2_fichewilaya .ReactTable .rt-tr-group {
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
}





  
#table2 .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
  }


  
#table2_fichewilaya .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
}
  

#equip_secteur_reactable .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
  }  




#equip_reactable .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
  }
  
#table2 .rt-td rt-align-left{
  width:144px;
}
  
  
  
#table2_fichewilaya .rt-td rt-align-left{
  width:144px;
  }


#table2 .rt-th-content {
    overflow: hidden;
    text-overflow: ellipsis;
    font-size: 12px;
    text-align: left;
  }

  
#table2_fichewilaya .rt-th-content {
    overflow: hidden;
    text-overflow: ellipsis;
    font-size: 12px;
    text-align: left;
  }



#c1 {
margin-right:-300px;
}


#c1_fichewilaya {
margin-right:-300px;
}


#c12_fichewilaya {
   margin-left: 155px;
   visibility: inherit;
   overflow-y: scroll;
   height: 900px;
}


#cg2{
    margin-right: 150px;
}

  #table .ReactTable .rt-tr-group:hover {
     background-color: lightgrey;
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
  
#side_wilaya {
    min-height: 20px;
    padding: 0px;
    margin-bottom: 20px;
    background-color: transparent;
    box-shadow: inset 0 1px 1px rgba(0,0,0,.05);
    border: 0px solid #e3e3e3;
    border-radius: 0px;
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

         
#excel1{
         width: 100%;
         height: 560px;
         visibility: inherit;
}
      
#excel1.jexcel_content{

    overflow-y: auto;
    max-height: 550px;
}
             
.navbar>.container-fluid .navbar-brand {
    margin-left: -15px;
    margin-top: -6px;
    line-height: 91%;
    font-family: system-ui;
    font-weight: 500;
    font-size: 27px;
    font-variant-caps: petite-caps;
 }

#tabmodalserie_eqp {
    position: absolute;
    top: -12px;
    right: -2px;
    z-index:9999999;
}

#tabmodalserie1 {
position: absolute;
top: -6px;
right: 15px;
}


#tabmodalserie2 {
position: absolute;
top: -6px;
right: 15px;
}


#tabmodalserie3 {
position: absolute;
top: -6px;
right: 15px;
}

.modal-body {
    position: relative;
    padding: 20px;
    overflow-x: auto;
}

#tabmodalserie_urbanisme1 {
position: absolute;
top: -6px;
right: 15px;
}

#tabmodalserie_urbanisme2 {
position: absolute;
top: 396px;
right: 15px;
}


#tabmodalserie_urbanisme3 {
position: absolute;
top: -6px;
right: 15px;
}

#tabmodalserie_urbanisme4 {
position: absolute;
top: 396px;
right: 15px;
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

#periode {
display: inline
}

#periode2 {
display: inline
}



#titre_serie3 {
display: inline
}


#titre_serie {
display: inline
}


.chart-title {
    width:107%;height:20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-left:-20px;
    margin-top:5px;
    display: flex;
}


.chart-title-eqp {
    width: 103%;
    height: 19px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-top: -1px;
    display: flex;
    margin-left: -14px;
    padding-left: 14px;
    padding-top: 1px;

}

.chart-title-urbanisme {
    width: 106%;
    height: 20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -10px;
    display: flex;
}

.chart-title-urbanisme2 {
    width: 106%;
    height: 20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 12px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -10px;
    display: flex;
}


.pretty {
position : absolute;
right: 250px;
}

#livraisons {

    margin-bottom: -5px;

}

#lancements {

    margin-bottom: -5px;

}

#titre_livraison {
    
    display:inline;
    font-size:12px;

}
                

#titre_lancement {
    
    display:inline;
    font-size:12px;

                }                
                
                
#dernier_an {
    display: inline-block;
}

#dernier_an_lanc {
    display: inline-block;
}


#dernier_an2 {
    display: inline-block;
}


#dernier_an3 {
    display: inline-block;
}


#dernier_an4 {
    display: inline-block;
}


#dernier_an8 {
    display: inline-block;
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
}


.well#well12 {
    box-shadow: 4px 5px 11px grey;
    border-radius:2px;
    padding-top:0px;
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

</style>')),

    #################################################### FICHE WILAYA : 
    
    tabPanel("Logements",
             fluidRow(
               id="tabfichewilaya",
               prettySwitch(
                 inputId = "pourcentage",
                 label = "Pourcentage", 
                 status = "primary",
                 slim = TRUE,
                 value=FALSE
               ),
               column(3,id="c1_fichewilaya",
                      reactableOutput("table2_fichewilaya")
               ),
               column(9,id='c12_fichewilaya',style="margin-left:155px;",
                      wellPanel(id="well_fichewilaya",
                                style="box-shadow:4px 5px 11px grey;background-color:white;",
                                icon(class="icon11","share-alt", lib = "glyphicon"),
                                
                                div(id = "box-score1",
                                    div(id = "box-score-title1", "Etat d'Execution du Programme au ",textOutput("boxscore1_arretee"),textOutput("boxscore1_wilaya")),
                                    reactableOutput('gt1',width = '1082px')
                                ),
                                
                                div(id = "box-score2",
                                    div(id = "box-score-title2", "Aides à la réhabilitation au ",textOutput("boxscore2_arretee"),textOutput("boxscore2_wilaya")),
                                    reactableOutput('gt2',width = '782px')
                                ),
                                
                                div(id = "box-score3",
                                    div(id = "box-score-title3", "Logements LPL en instance d’attribution au ",textOutput("boxscore3_arretee"),textOutput("boxscore3_wilaya")),
                                    reactableOutput('gt3',width = 'auto')
                                ),
                                reactableOutput('gt4',width = "675px"),
                                
                                div(class = "box-score",
                                    div(class = "box-score-title", "Etat des livraisons au ",textOutput("boxscore5_arretee"),textOutput("boxscore5_wilaya")),
                                    reactableOutput('gt5')
                                )
                                
                      )
               )
             ),
             pickerInput(
               inputId = "select_arretee_fichewilaya",
               label = "", 
               choices = sort(paste(c("Arretee le :"),unique(data_fiche_wilaya$arretee)),TRUE),
               #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
             )
    ),
    
    
    
    ################################################### FICHE WILAYA 
    
    
    
    # 
    # tabPanel("Bilan2020",
    #          htmlOutput("bilan")
    #          ),
    # 
    
    
    
    
    ################################
    
    ########### URBANISME 
    
    tabPanel("Urbanisme",
             fluidRow(
               id="taburbanisme",
               # prettySwitch(
               #   inputId = "pourcentage_urbanisme",
               #   label = "Pourcentage", 
               #   status = "primary",
               #   slim = TRUE,
               #   value=FALSE
               # ),
               column(3,id="c1_urbanisme",
                      reactableOutput("table2_urbanisme")
               ),
               column(9,id='c12_urbanisme',style="margin-left:155px;",
                      wellPanel(id="well_urbanisme",
                                style="box-shadow:4px 5px 11px grey;background-color:white;",
                          
                                div(id = "box-score1_urbanisme",
                                    div(id = "box-score-title1_urbanisme", "Instruments d'Urbanisme ",textOutput("boxscore1_arretee_urbanisme"),textOutput("boxscore1_wilaya_urbanisme")),
                                    reactableOutput('gt1_urbanisme',width = '873px')
                                ),
                                
                                div(id = "box-score2_urbanisme",
                                    div(id = "box-score-title2_urbanisme", "Situation Financières",textOutput("boxscore2_arretee_urbanisme"),textOutput("boxscore2_wilaya_urbanisme")),
                                    reactableOutput('gt2_urbanisme',width = '1042px')
                                ),
                                ###############################
                                
                                wellPanel(id="urbanisme_well",style="width:700px ; height:380px;margin-left:273px",
                                          
                                          div(style="padding-top:2px;",
                                              div(id = "box-score-title3_urbanisme", "Lotissements sociaux",textOutput("boxscore3_arretee_urbanisme"),textOutput("boxscore3_wilaya_urbanisme")),
                                              
                                              #textOutput("ttwilayas"),
                                              textOutput("region"),
                                              tags$table(class="ta",style="font-size: 24px;font-family: system-ui;",
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Nature juridique",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                                 tags$td(class="f3",textOutput("zones1"),style="padding-bottom: 3px;text-align: end;font-weight: 500;font-size:19px;")
                                                         ),
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Nombre de Communes Concernées",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                                 tags$td(class="f3",textOutput("zones2"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                         ),
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Superficie (ha)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                                 tags$td(class="f3",textOutput("zones3"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                         ),
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Nombre de lots",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                                 tags$td(class="f3",textOutput("zones4"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                         )
                                                         ,
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Nombre de lots retenues",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                                 tags$td(class="f3",textOutput("zones5"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                         ),
                                                         
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Nombre de lots dégagés (Porte feuille)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                                 tags$td(class="f3",textOutput("zones6"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                         ),
                                                         tags$tr(class="f0",style="display:none;",
                                                                 tags$td(class="f1","Surface moyenne des lots (m²)",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-bottom: 3px;padding-right: 40px;padding-left: 45px;"),
                                                                 tags$td(class="f3",textOutput("zones7"),style="padding-bottom: 3px;text-align: end;font-weight: 500;")
                                                         ),
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Nombre de sites",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                                 tags$td(class="f3",textOutput("zones8"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                         ),
                                                         tags$tr(class="f0",
                                                                 tags$td(class="f1","Nombre de permis",style="padding-bottom: 3px;padding-left: 27px;"),
                                                                 tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 3px;"),
                                                                 tags$td(class="f3",textOutput("zones9"),style="text-align: end;padding-bottom: 3px;font-weight: 500;")
                                                         )
                                              )
                                              
                                          )                                 ),
                                
                                ############################## loi :
                                
                                wellPanel(id="urbanisme_well",style="width:700px ; height:380px;margin-left:273px",
                                          
                                          div(style="padding-top:2px;",
                                              div(id = "box-score-title4_urbanisme", "Loi 18-05",textOutput("boxscore4_arretee_urbanisme"),textOutput("boxscore4_wilaya_urbanisme")),
                                              
                                              #textOutput("ttwilayas"),
                                              #textOutput("region"),
                                              
                                              div(style="padding-top:11px;padding-left:72px",
                                                  #textOutput("ttwilayas2"),
                                                  tags$table(class="ta",style="font-size: 21px;font-family: system-ui;",
                                                             tags$tr(class="f0",
                                                                     tags$td(class="f1","Déposés",style="padding-bottom: 13px;padding-left: 27px;"),
                                                                     tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                                     tags$td(class="f3",textOutput("loi1"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                             ),
                                                             tags$tr(class="f0",
                                                                     tags$td(class="f1","Traités",style="padding-bottom: 13px;padding-left: 27px;"),
                                                                     tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                                     tags$td(class="f3",textOutput("loi2"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                             ),
                                                             tags$tr(class="f0",
                                                                     tags$td(class="f1","Favorables",style="padding-bottom: 13px;padding-left: 27px;"),
                                                                     tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                                     tags$td(class="f3",textOutput("loi3"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                             ),
                                                             tags$tr(class="f0",
                                                                     tags$td(class="f1","Défavorables",style="padding-bottom: 13px;padding-left: 27px;"),
                                                                     tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                                     tags$td(class="f3",textOutput("loi4"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                             )
                                                             ,
                                                             tags$tr(class="f0",
                                                                     tags$td(class="f1","Instances",style="padding-bottom: 13px;padding-left: 27px;"),
                                                                     tags$td(class="f2",":",style="padding-right: 40px;padding-left: 45px;padding-bottom: 13px;"),
                                                                     tags$td(class="f3",textOutput("loi5"),style="text-align: end;padding-bottom: 13px;font-weight: 500;")
                                                             ),
                                                             
                                                             tags$tr(class="f0",
                                                                     tags$td(class="f1","Déposés Instruction N°01",style="padding-bottom: 13px;padding-left: 27px;"),
                                                                     tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                                     tags$td(class="f3",textOutput("loi6"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                             ),
                                                             tags$tr(class="f0",
                                                                     tags$td(class="f1","Traités_a",style="padding-bottom: 13px;padding-left: 27px;"),
                                                                     tags$td(class="f2",":",style="padding-bottom: 13px;padding-right: 40px;padding-left: 45px;"),
                                                                     tags$td(class="f3",textOutput("loi7"),style="padding-bottom: 13px;text-align: end;font-weight: 500;")
                                                             )
                                                  )
                                                  
                                              )
                                          )
                                )
                                              
                                
                                
                                ############################# loi f
                                
                                      
               )
               )
             ),
             
             
             pickerInput(
               inputId = "select_arretee_urbanisme",
               label = "", 
               choices =sort(paste(c("Arretee le :"),unique(pos$Arretee)),TRUE),
              #paste("Arretee le : 2019-12-31")
               #sort(paste(c("Arretee le :"),unique(data_fiche_wilaya$arretee)),TRUE),
               #######"  a changer prochainement
               
               #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
             )
             
    ),
    
    
    
    ########### URBANISME
    
    tabPanel("Equipements Publics",
             fluidRow(
               id="fluidrow1_eqp",
               
               column(6,     #id="cequip",
                      reactableOutput("equip_reactable")
                      
               ),
               column(6,style="box-shadow:4px 5px 11px grey;background-color:white;height:799px",
                      tags$div(class="chart-title-eqp",HTML('&nbsp;'),HTML('&nbsp;'),tags$p("Nombre de Projets Par Secteur : "),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselecteqp"),
                               actionBttn(
                                 inputId = "tabmodalserie_eqp",
                                 label = NULL,
                                 style = "bordered", 
                                 color = "success",
                                 icon = icon("table")
                               )
                      ),
                      
                      div(id = "box-score_equip",
                          div(id = "box-score-title_equip", "Nombre de Projets par Secteur",textOutput("wilayaselecteqp22")),
                          reactableOutput('equip_secteur_reactable')
                      )
                      
               ),
               
               bsModal("modal_eqp", htmlOutput("tablededonnes1_eqp"), "tabmodalserie_eqp", size = "large"
                       ,excelOutput("excel_eqp")),
               
             ),
             pickerInput(
               inputId = "select_arretee_eqp",
               label = "", 
               choices = c('Arretee le : 2020-06-30','Arretee le : 2019-12-31')
             )
             
    )
  
)

)


# Define server logic required to draw a histogram
server <- function(input, output) {


    
  ########### nvfiche_d

  output$loi1=renderText({
    format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>% 
              summarise(sumnb=sum(Déposés,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi2=renderText({
    format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>% 
              summarise(sumnb=sum(Traités,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi3=renderText({
    format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>% 
              summarise(sumnb=sum(Favorables,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi4=renderText({
    format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>% 
              summarise(sumnb=sum(Défavorables,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi5=renderText({
    format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>% 
              summarise(sumnb=sum(Instances,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi6=renderText({
    format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>% 
              summarise(sumnb=sum(`Déposés Instruction N°01`,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi7=renderText({
    format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>% 
              summarise(sumnb=sum(Traités_a,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  
  output$zones1=renderText({
    `if`(length(selected20_urbanisme())==48,
         print("Domanial"),
         `if`(is.na((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                       select(`Nature juridique`))$`Nature juridique`
         )==TRUE,print(c()),
         (zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
            select(`Nature juridique`))$`Nature juridique`)
    )
  })
  
  
  output$zones2=renderText({
    `if`(
      (zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
         summarise(sumnb=sum(`Nombre de communes concernées`,na.rm = TRUE)))$sumnb==0,print(""),
      
      format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                summarise(sumnb=sum(`Nombre de communes concernées`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
    )
  })
  
  
  output$zones3=renderText({
    `if`((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
            summarise(sumnb=sum(`Superficie (ha)`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                   summarise(sumnb=sum(`Superficie (ha)`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
    )
  })
  
  output$zones4=renderText({
    `if`((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                   summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  output$zones5=renderText({
    `if`((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                   summarise(sumnb=sum(`Nombre de lots retenues`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  output$zones6=renderText({
    `if`(
      (zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
         summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
      format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                summarise(sumnb=sum(`Nombre de lots dégagés (Porte feuille)`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
      
    )
  })
  
  output$zones7=renderText({
    
    (zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>%
       select(`Surface moyenne des lots (m²)`))$`Surface moyenne des lots (m²)`
    
  })
  
  output$zones8=renderText({
    `if`(
      (zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
         summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
      format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                summarise(sumnb=sum(`Nombre de sites`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
      
    )
  })
  
  output$zones9=renderText({
    `if`((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya==selected20_urbanisme()) %>% 
                   summarise(sumnb=sum(`Nombre des permis`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  
  
  display_zones7=renderText({
    `if`(length(selected20_urbanisme())==48,
         paste0("display:none;"),paste0("display:block;")
    )
  })
  
  
  
  
  output$region=renderText({
    `if`(length(selected20_urbanisme())==48,
         print("Région : HAUT PLATEU - SUD"),
         paste0("Région : ",zones$Zone[selected20_urbanisme()])
    )
  })
  
  
  output$ttwilayas=renderText({
    `if`(length(selected20_urbanisme())==48,
         print("Toutes les Wilayas"),
         print(zones$Wilaya[selected20_urbanisme()])
    )
  })
  
  # output$gt3_urbanisme<-renderReactable({
  #   reactable(
  #     t(
  #       zones %>%
  #         filter(id_wilaya %in% selected20_urbanisme()) %>%
  #         summarise_at(vars(c(8,5,12,6,4,7,12,11)), sum,na.rm=TRUE) %>% 
  #         mutate("Nature juridique"=zones %>% filter(id_wilaya==1) %>% select(`Nature juridique`) %>% .$`Nature juridique`) %>% 
  #         select(8,1,2,3,4,5,6,7)
  #       ),
  #     sortable = FALSE,
  #     bordered = TRUE,
  #     style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
  #     
  #     columns = list(V1=colDef(
  #       name="",
  #       align = "right",
  #       style = list(
  #       background = "#ffffff",
  #       color="#000000"
  #       ),width = 250
  #     )),
  #     defaultColDef = colDef(
  #       sortNALast = TRUE,
  #       format = colFormat(separators = TRUE),
  #       #format = colFormat(digits = 1),
  #       maxWidth = 420,
  #       style = list(
  #         background = "#2E81B0",
  #         color="#ffffff"
  #       ),
  #       headerStyle = list(
  #         marginTop="-15px",width=0,height=0
  #         )
  # 
  #       #headerClass = "box-score-header"
  #     )
  #   )
  # })
  # 
  
  output$gt2_urbanisme<-renderReactable({
    reactable(
      sit_fin %>% 
        filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id %in% selected20_urbanisme()) %>%
        group_by(Type) %>% 
        summarise(Notification=sum(NOTIFICATION),
                  Reliquat=sum(Reliquat)
                  ) %>%
        mutate(idds=c(1,4,2,3)) %>%
        arrange(idds) %>%
        mutate(Inscription=Notification+Reliquat) %>% 
        select(1,2,3,5),
      sortable = FALSE,
      bordered = TRUE,
      style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
      defaultColDef = colDef(
        sortNALast = TRUE,
        format = colFormat(separators = TRUE,digits = 0,suffix = " DA"),
        #format = colFormat(digits = 1),
        maxWidth = 220,
        headerStyle = list(background = "#2E81B0",
                           color="#ffffff"
        )
        
        #headerClass = "box-score-header"
      ),
      columns = list(
        Type = colDef(name = "",
                      format = colFormat(suffix = ""),
                           width=380,
                           style =list(background='#81a47b',color='#ffffff')
        ),
        
        Inscription = colDef(
                      width=220,
                      style =list(fontWeight = "bold")
        )
      )
    )
  })

  output$gt1_urbanisme<-renderReactable({
    reactable(
      pos %>%
        filter(Arretee==substr(input$select_arretee_urbanisme,14,23),id_wilaya %in% selected20_urbanisme()) %>%
        group_by(URBANISME) %>% 
        summarise("Lancés"=sum(`Lancés`),
                  "Achevées"=sum(`Achevées`),
                  "Approuvées"=sum(`Approuvées`),
                  "En Cours"=sum(`En Cours`),
                  "Non Lancées"=sum(`Non Lancées`)
        ) %>% 
        mutate(idd=c(2,3,1)) %>%
        arrange(idd) %>% 
        select(1,2,3,4,5,6)
    ,
    sortable = FALSE,
    bordered = TRUE,
    style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
    defaultColDef = colDef(
      sortNALast = TRUE,
      #format = colFormat(digits = 1),
      maxWidth = 150,
      headerStyle = list(background = "#2E81B0",
                         color="#ffffff"
      )
      
      #headerClass = "box-score-header"
    ),
    columns = list(
      URBANISME = colDef(name = "",
                         width=120,
                         style =list(background='#81a47b',color='#ffffff')
      ),
      
      `Lancés` = colDef(
        headerStyle = list(
          background = "#2E81B0",
          color="#ffffff",
          borderLeft="0px",
          textAlign="center"
          #,align='left'
        ),
        width=150,align = 'right'
        )
      
      
    )
    )
  })
  
    
  selected2_urbanisme <- reactive(getReactableState("table2_urbanisme", "selected"))
  
  selected20_urbanisme <- reactive(
    `if`(length(getReactableState("table2_urbanisme", "selected"))==0,1:48,getReactableState("table2_urbanisme", "selected"))
  )
  
  
  output$table2_urbanisme <- renderReactable({
    reactable(data.frame(Wilaya=unique(data_fiche_wilaya$Wilaya)) ######"  a changer prochainement
              ,defaultPageSize = 48,striped = TRUE,
              selection = "single",
              sortable = FALSE,
              borderless = TRUE,
              height="900px",
              width="66%",
              columns = list(
                Wilaya = colDef(width = 158,align="left")   # 50% width, 200px minimum
                
              ),
              #  columnGroups = list(
              #    colGroup(name = paste("Situation Physiques des logements"," ",paste(select_segment_title(),collapse = "+")), columns = colnames(sitphy00()))
              #  ),
              #  defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              onClick = "select",
              theme = reactableTheme(
                style = list(
                  fontSize="16px"
                  #,fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                ),
                rowSelectedStyle = list(backgroundColor = "#6da7b3", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  
  
  output$boxscore1_arretee_urbanisme=renderText({
    paste(substr(input$select_arretee_urbanisme,14,23))
  })
  
  output$boxscore2_arretee_urbanisme=renderText({
    paste(substr(input$select_arretee_urbanisme,14,23))
  })
  
  output$boxscore3_arretee_urbanisme=renderText({
    paste(substr(input$select_arretee_urbanisme,14,23))
  })
  
  
  output$boxscore4_arretee_urbanisme=renderText({
    paste(substr(input$select_arretee_urbanisme,14,23))
  })
  
  
  
  output$boxscore1_wilaya_urbanisme=renderText({
    `if`(length(selected20_urbanisme())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_urbanisme()]),paste("")
    )
    # a changer prochainement
  })
  
  
  output$boxscore2_wilaya_urbanisme=renderText({
    `if`(length(selected20_urbanisme())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_urbanisme()]),paste("")
    )
    # a changer prochainement
  })
  
  
  output$boxscore3_wilaya_urbanisme=renderText({
    `if`(length(selected20_urbanisme())==1,
         paste(unique(zones$Wilaya)[selected20_urbanisme()]),paste("")
    )
    # a changer prochainement
  })
  
  output$boxscore4_wilaya_urbanisme=renderText({
    `if`(length(selected20_urbanisme())==1,
         paste(unique(zones$Wilaya)[selected20_urbanisme()]),paste("")
    )
    # a changer prochainement
  })
  
  
  ############ nvfiche_f
  data_equip0 = reactive({
    equip %>%
      filter(Secteur %in% secteurselecteqp()) %>%
      group_by(Wilaya,Arretee) %>%
      summarise("Nbre de Projets"=sum(`Nbre de Projets`),Acheves=sum(Acheves),"En Cours"=sum(`En Cours`),"Non Lances"=sum(`Non Lances`),"Dont NIR"=sum(`Dont NIR`),"Geles"=sum(Geles)) %>% 
      select(1,3,4,5,6,7,8,2)
  })
  
  
  select_dataequip0_title=reactive({
    `if`(length(data_equip0()$Secteur) %in% c(0,length(unique(equip$Secteur))),"",paste(data_equip0()$Secteur))
  })
  
  
  
  
  
  
  
  #   observe({
  #     runjs("
  # var wb=document.getElementById('box-score2')
  # var wilay=document.getElementById('boxscore1_wilaya');
  # if(wilay.textContent=='16-ALGER'){
  # wb.style.opacity=0.2
  # } else {
  # wb.style.opacity=1
  # }
  # ")
  #   })
  
  output$boxscore1_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  output$boxscore2_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  output$boxscore3_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  output$boxscore5_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  output$boxscore1_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("")
    )
  })
  output$boxscore2_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("")
    )
  })
  
  output$boxscore3_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("")
    )
  })
  output$boxscore5_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("")
    )
  })
  
  output$gt5<-renderReactable({
    reactable(dfa_fichewilaya5(),
              
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                Segment = colDef(footer = "Taux",name="",
                                 headerStyle = 
                                   list(
                                     background = "#2E81B0",
                                     color="#ffffff",  
                                     borderRight="0px")
                ),
                
                LPL = colDef(
                  headerStyle = 
                    list(
                      background = "#2E81B0",
                      color="#ffffff",  
                      borderLeft="0px"),
                  format = colFormat(separators = TRUE),
                  
                  #format = colFormat(percent = TRUE, digits = 1)
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                ),
                LSP = colDef(
                  format = colFormat(separators = TRUE),
                  
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                ),
                `Location-Vente` = colDef(
                  format = colFormat(separators = TRUE),
                  
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                  ,width=180,
                ),
                Rural = colDef(
                  format = colFormat(separators = TRUE),
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                  
                ),
                LPP = colDef(
                  format = colFormat(separators = TRUE),
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                  
                  
                  
                ),
                
                Total = colDef(
                  format = colFormat(separators = TRUE),
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                  
                  
                )
                
              ),
              defaultColDef = colDef(
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                maxWidth = 120,
                footerStyle = list(fontWeight = "bold"),
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff"
                )
                
                #headerClass = "box-score-header"
              )
    )
    
  })
  
  output$gt4<-renderReactable({
    
    `if`(input$pourcentage==FALSE,
         reactable(dfa_fichewilaya4()[1:6,],
                   sortable = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     X1 = colDef(name="",width = 550,style = list(background="#ffffff",color="#000000")),
                     X2 = colDef(name="",width = 120,
                                 align = "right",
                                 
                                 format = colFormat(separators = TRUE)
                                 
                     )
                   ),
                   rowStyle = function(index) {
                     if(index ==1){
                       list(fontWeight = "normal",background="#8DAD88",color="#ffffff")
                     } else if (index==2) {
                       list(fontWeight = "normal",background="#8DAD88",color="#ffffff")
                     } else if (index==3) {
                       list(fontWeight = "normal",background="#8DAD88",color="#ffffff")
                     } else if (index==4) {
                       list(fontWeight = "bold",background="#5A6F57",color="#ffffff")
                     } else if(index==5) {
                       list(fontWeight = "normal",background="#764AAF",color="#ffffff")
                     } else {
                       list(fontWeight = "bold",background="#1A4963",color="#ffffff")
                     }
                   },
                   defaultColDef = colDef(
                     sortNALast = FALSE,
                     #format = colFormat(digits = 1),
                     #maxWidth = 140
                     headerStyle = list(width=0,height=0)
                   )
                   
         )
         ,
         
         reactable(dfa_fichewilaya4()[c(7,8,9,4,5,6),],
                   sortable = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     X1 = colDef(name="",width = 550,style = list(background="#ffffff",color="#000000")),
                     X2 = colDef(name="",width = 120,
                                 align = "right",
                                 format = colFormat(separators = TRUE)
                                 
                     )
                   ),
                   rowStyle = function(index) {
                     if(index ==1){
                       list(fontWeight = "normal",background="#8DAD88",color="#ffffff")
                     } else if (index==2) {
                       list(fontWeight = "normal",background="#8DAD88",color="#ffffff")
                     } else if (index==3) {
                       list(fontWeight = "normal",background="#8DAD88",color="#ffffff")
                     } else if (index==4) {
                       list(fontWeight = "bold",background="#5A6F57",color="#ffffff")
                     } else if(index==5) {
                       list(fontWeight = "normal",background="#764AAF",color="#ffffff")
                     } else {
                       list(fontWeight = "bold",background="#1A4963",color="#ffffff")
                     }
                   },
                   defaultColDef = colDef(
                     sortNALast = FALSE,
                     #format = colFormat(digits = 1),
                     #maxWidth = 140
                     headerStyle = list(width=0,height=0)
                   )
                   
         )
         
         
    )
  })
  
  output$gt2<-renderReactable({
    `if`(input$pourcentage==FALSE,
         reactable(dfa_fichewilaya2()[1,1:5],
                   sortable = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(width=220,name="",
                                      style =list(background='#81a47b',color='#ffffff'),
                                      headerStyle = list(
                                        background = "#2E81B0",
                                        color="#ffffff",
                                        borderRight="0px"
                                      )
                     ),
                     
                     `Consistance Actuelle` = colDef(
                       name="Consistance",
                       headerStyle = list(
                         background = "#2E81B0",
                         color="#ffffff",
                         borderLeft="0px"
                       ),
                       format = colFormat(separators = TRUE)
                     ),
                     Acheves = colDef(
                       format = colFormat(separators = TRUE)
                       
                     ),
                     `En Cours` = colDef(
                       format = colFormat(separators = TRUE)
                       
                     ),
                     `Non Lances` = colDef(
                       format = colFormat(separators = TRUE)
                     )
                     
                   ),
                   defaultColDef = colDef(
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 140,
                     headerStyle = list(background = "#2E81B0",
                                        color="#ffffff"                                         )
                   )
         )
         ,
         reactable(dfa_fichewilaya2()[1,c(1,2,6,7,8)],
                   sortable = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(width=220,name="",
                                      style =list(background='#81a47b',color='#ffffff'),
                                      headerStyle = list(
                                        background = "#2E81B0",
                                        color="#ffffff",
                                        borderRight="0px"
                                      )
                     ),
                     
                     `Consistance Actuelle` = colDef(
                       name="Consistance",
                       headerStyle = list(
                         background = "#2E81B0",
                         color="#ffffff",
                         borderLeft="0px"
                       ),
                       format = colFormat(separators = TRUE)
                     ),
                     
                     pacheves = colDef(
                       name="Acheves",
                       format = colFormat(percent = TRUE,digits = 0)
                       
                     ),
                     pencours = colDef(
                       name="En Cours",
                       format = colFormat(percent = TRUE,digits = 0)
                       
                     ),
                     pnonlances = colDef(
                       name="Non Lances",
                       format = colFormat(percent = TRUE,digits = 0)
                     )
                     
                   ),
                   defaultColDef = colDef(
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 140,
                     headerStyle = list(background = "#2E81B0",
                                        color="#ffffff"                                         )
                   )
         )
         
         
         
    )
    
    
  })
  
  output$gt3<-renderReactable({
    `if`(input$pourcentage==FALSE,
         reactable(dfa_fichewilaya3()[1,c(1,2,3,4,5,6,7,8,9,10)],
                   sortable = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   fullWidth = FALSE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Achevee=colDef(
                       width=130,  
                       headerStyle = list(
                         borderLeft="25px inset transparent",
                         textAlign="left",
                         background = "#2E81B0",
                         color="#ffffff",
                         width="130px"
                       )
                     ),
                     `En Cours`=colDef(
                       width=130
                     )
                     ,
                     `Non Lances`=colDef(
                       width=145
                     ),
                     `En cours`=colDef(name="En Cours",width=130),
                     `Non lances`=colDef(name="Non Lances",width=145),
                     `Acheves à 60% et plus`=colDef(headerStyle = list(
                       #borderTop="0px",
                       marginTop="-31px",
                       textAlign="left",
                       background = "#2E81B0",
                       color="#ffffff"
                     )),
                     `Total general`=colDef(
                       style = list(fontWeight = "bold",textAlign="center"),
                       headerStyle = list(
                         marginTop="-22px",
                         
                         #border="0px solid #eee",
                         fontWeight = "bold",
                         textAlign="center",
                         background = "#2E81B0",
                         color="#ffffff"
                       )),
                     Total=colDef(style=list(textAlign="center",fontWeight = "bold"),
                                  headerStyle = list(
                                    fontWeight = "bold",
                                    textAlign="center",
                                    background = "#2E81B0",
                                    color="#ffffff"
                                  )),
                     
                     TotaI=colDef(name="Total",style=list(textAlign="center",fontWeight = "bold"),
                                  headerStyle = list(
                                    fontWeight = "bold",
                                    textAlign="center",
                                    background = "#2E81B0",
                                    color="#ffffff"
                                  )
                     )
                     
                   ),
                   columnGroups = list(
                     colGroup(name = "Logts réceptionnés et Notifiés	", columns = c("Acheves","En Cours","Non Lances","Total"),
                              headerStyle =list(
                                borderRight="25px inset transparent",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "Logts réceptionnés et Non Notifiés	", columns = c("Achevee","En cours","Non lances","TotaI"),
                              
                              headerStyle =list(
                                borderLeft="25px inset transparent",
                                borderRight="25px inset transparent",
                                
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "", columns = c("Acheves à 60% et plus"),
                              headerStyle =list(
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "", columns = c("Total general"),
                              headerStyle =list(
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     )
                     
                   ),
                   defaultColDef = colDef(
                     format = colFormat(separators = TRUE),
                     
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 130,
                     headerStyle = list(background = "#2E81B0",
                                        color="#ffffff",
                                        maxWidth=130
                     )
                   )
         )
         
         ,
         
         reactable(dfa_fichewilaya3()[1,c(11,12,13,4,14,15,16,8,9,10)],
                   sortable = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   fullWidth = FALSE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     pacheves1=colDef(
                       name="Acheves",
                       format =colFormat(percent = TRUE,digits = 0),
                       width=130,  
                       headerStyle = list(
                         borderLeft="25px inset transparent",
                         textAlign="left",
                         background = "#2E81B0",
                         color="#ffffff",
                         width="130px"
                       )
                     ),
                     pencours1=colDef(
                       name="En Cours",
                       format =colFormat(percent = TRUE,digits = 0),
                       width=130
                     )
                     ,
                     pnonlances1=colDef(
                       name="Non Lances",
                       format =colFormat(percent = TRUE,digits = 0),
                       
                       width=145
                     ),
                     pacheves2=colDef(
                       name="Acheves",
                       format =colFormat(percent = TRUE,digits = 0),
                       
                     ),
                     pencours2=colDef(
                       name="En Cours",width=130,
                       format =colFormat(percent = TRUE,digits = 0),
                       
                     ),
                     pnonlances2=colDef(
                       name="Non Lances",width=145,
                       format =colFormat(percent = TRUE,digits = 0),
                     ),
                     `Acheves à 60% et plus`=colDef(headerStyle = list(
                       #borderTop="0px",
                       marginTop="-31px",
                       textAlign="left",
                       background = "#2E81B0",
                       color="#ffffff"
                     )),
                     `Total general`=colDef(
                       style = list(fontWeight = "bold",textAlign="center"),
                       headerStyle = list(
                         marginTop="-22px",
                         
                         #border="0px solid #eee",
                         fontWeight = "bold",
                         textAlign="center",
                         background = "#2E81B0",
                         color="#ffffff"
                       )),
                     Total=colDef(style=list(textAlign="center",fontWeight = "bold"),
                                  headerStyle = list(
                                    fontWeight = "bold",
                                    textAlign="center",
                                    background = "#2E81B0",
                                    color="#ffffff"
                                  )),
                     
                     TotaI=colDef(name="Total",style=list(textAlign="center",fontWeight = "bold"),
                                  headerStyle = list(
                                    fontWeight = "bold",
                                    textAlign="center",
                                    background = "#2E81B0",
                                    color="#ffffff"
                                  )
                     )
                     
                   ),
                   columnGroups = list(
                     colGroup(name = "Logts réceptionnés et Notifiés	", columns = c("pacheves1","pencours1","pnonlances1","Total"),
                              headerStyle =list(
                                borderRight="25px inset transparent",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "Logts réceptionnés et Non Notifiés	", columns = c("pacheves2","pencours2","pnonlances2","TotaI"),
                              
                              headerStyle =list(
                                borderLeft="25px inset transparent",
                                borderRight="25px inset transparent",
                                
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "", columns = c("Acheves à 60% et plus"),
                              headerStyle =list(
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "", columns = c("Total general"),
                              headerStyle =list(
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     )
                     
                   ),
                   defaultColDef = colDef(
                     format = colFormat(separators = TRUE),
                     
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 130,
                     headerStyle = list(background = "#2E81B0",
                                        color="#ffffff",
                                        maxWidth=130
                     )
                   )
         )
         
         
         
         
         
         
    )
    
  })
  
  
  output$gt1<-renderReactable({
    `if`(input$pourcentage==FALSE,
         reactable(dfa_fichewilaya1()[1:5,c(1,2,3,4,5,6,7)],
                   
                   sortable = TRUE,
                   defaultSorted = list(Segment = "desc"),
                   #defaultSorted = list(Consistance = "desc", Acheves = "desc"),
                   showSortIcon = FALSE,
                   
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(name = "",
                                      footer = "Programme Global",
                                      width=220,
                                      style =list(background='#81a47b',color='#ffffff')
                     ),
                     
                     `Consistance` = colDef(
                       headerStyle = list(
                         background = "#2E81B0",
                         color="#ffffff",
                         borderLeft="0px",
                         textAlign="center"
                         #,align='left'
                       ),
                       width=150,align = 'right',
                       format = colFormat(separators = TRUE),
                       
                       #format = colFormat(percent = TRUE, digits = 1)
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     Acheves = colDef(
                       format = colFormat(separators = TRUE),
                       
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     `En Cours` = colDef(
                       format = colFormat(separators = TRUE),
                       
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     `Dont A l'Arret` = colDef(
                       headerStyle = list(
                         background = "#9d94ac",
                         color="#ffffff"
                         #,align='left'
                       ),
                       style = list(
                         background = "#9d94ac",
                         color="#ffffff"
                       ),
                       width = 165,
                       align = 'center',
                       format = colFormat(separators = TRUE),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                       ,footerStyle = list(
                         
                         background = "#9d94ac",
                         color="#ffffff"
                       )
                     ),
                     `Non Lances` = colDef(
                       format = colFormat(separators = TRUE),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       },
                       
                       width=155
                       
                     ),
                     
                     `Notifie 2020` = colDef(
                       format = colFormat(separators = TRUE),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       },
                       
                       width=155
                     )
                     
                   ),
                   defaultColDef = colDef(
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 140,
                     footerStyle = list(fontWeight = "bold",
                                        background='#81a47b',color='#ffffff',fontFamily='inherit'
                                        
                     ),
                     headerStyle = list(background = "#2E81B0",
                                        color="#ffffff",
                                        borderLeft="0px solid #eee",
                                        borderRight="0px solid #eee"
                     )
                     
                     #headerClass = "box-score-header"
                   )
         )
         ,
         reactable(dfa_fichewilaya1()[1:5,c(1,2,8,9,10,11,7)],
                   
                   sortable = TRUE,
                   defaultSorted = list(Segment = "desc"),
                   showSortIcon = FALSE,
                   
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(name = "",
                                      footer = "Programme Global",
                                      width=220,
                                      style =list(background='#81a47b',color='#ffffff')
                     ),
                     
                     `Consistance` = colDef(
                       headerStyle = list(
                         background = "#2E81B0",
                         color="#ffffff",
                         borderLeft="0px",
                         textAlign="center"
                         #,align='left'
                       ),
                       width=150,align = 'right',
                       format = colFormat(separators = TRUE),
                       
                       #format = colFormat(percent = TRUE, digits = 1)
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     
                     pacheves = colDef(
                       name="Acheves",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$Acheves)/sum(dfa_fichewilaya1()$Consistance))
                       #function(values) {
                       #format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       #}
                     ),
                     pencours = colDef(
                       name="En Cours",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`En Cours`)/sum(dfa_fichewilaya1()$Consistance))
                     ),
                     pdont = colDef(
                       name="Dont A l'Arret",
                       format = colFormat(percent = TRUE,digits = 0),
                       headerStyle = list(
                         background = "#9d94ac",
                         color="#ffffff"
                         #,align='left'
                       ),
                       style = list(
                         background = "#9d94ac",
                         color="#ffffff"
                       ),
                       width = 165,
                       align = 'center',
                       footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Dont A l'Arret`)/sum(dfa_fichewilaya1()$`En Cours`))
                       
                       ,footerStyle = list(
                         
                         background = "#9d94ac",
                         color="#ffffff"
                       )
                     ),
                     pnonlances = colDef(
                       name="Non Lances",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Non Lances`)/sum(dfa_fichewilaya1()$Consistance))
                       ,
                       
                       width=155
                       
                     ),
                     
                     
                     `Notifie 2020` = colDef(
                       format = colFormat(separators = TRUE),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       },
                       
                       width=155
                     )
                     
                   ),
                   defaultColDef = colDef(
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 140,
                     footerStyle = list(fontWeight = "bold",
                                        background='#81a47b',color='#ffffff',fontFamily='inherit'
                                        
                     ),
                     headerStyle = list(background = "#2E81B0",
                                        color="#ffffff",
                                        borderLeft="0px solid #eee",
                                        borderRight="0px solid #eee"
                     )
                     
                     #headerClass = "box-score-header"
                   )
         )
         
    )
    
  })
  data_fiche_wilaya_reactive=reactive({
    cbind(iad1=0,iad2=0,data_fiche_wilaya %>%
            filter(arretee==substr(input$select_arretee_fichewilaya,14,23),id_wilaya %in% c(selected20_fichewilaya())) %>%
            summarise_at(vars(consistance_lpl:total_livres), sum))
    #  %>%    unlist(use.names = FALSE)
  })
  
  dfa_fichewilaya5=reactive({
    i=1
    ab5[1,2:7]=c(data_fiche_wilaya_reactive()[i,53:58] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab5[2,2:7]=c(data_fiche_wilaya_reactive()[i,59:64] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab5
  })
  
  dfa_fichewilaya4=reactive({
    i=1
    ab4[,2]=c(data_fiche_wilaya_reactive()[i,47:52] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab47=ab4[1,2]/ab4[4,2]
    ab48=ab4[2,2]/ab4[4,2]
    ab49=ab4[3,2]/ab4[4,2]
    
    ab4[c(7,8,9),1]=c("Logements acheves, Viabilisation acheves","Logements acheves, Viabilisation en cours","Logements acheves, Viabilisation non entamee")
    
    ab4$X2[is.nan(ab4$X2)==TRUE]=0
    ab41=ab4[1,2]
    ab42=ab4[2,2]
    ab43=ab4[3,2]
    ab44=ab4[4,2]
    ab45=ab4[5,2]
    ab46=ab4[6,2]
    
    ab4[1,2]=format(ab41,digits = 3,big.mark = " ")
    ab4[2,2]=format(ab42,digits = 3,big.mark = " ")
    ab4[3,2]=format(ab43,digits = 3,big.mark = " ")
    
    ab4[4,2]=format(ab44,digits = 3,big.mark = " ")
    ab4[5,2]=format(ab45,digits = 3,big.mark = " ")
    ab4[6,2]=format(ab46,digits = 3,big.mark = " ")
    
    ab4[7,2]=sprintf("%.0f%%",100*ab47)
    ab4[8,2]=sprintf("%.0f%%",100*ab48)
    ab4[9,2]=sprintf("%.0f%%",100*ab49)
    
    
    ab4
  })
  
  
  dfa_fichewilaya2=reactive({
    i=1
    ab2[1,2:5]=c(data_fiche_wilaya_reactive()[i,33:36])
    ab2$pacheves[1]=ab2$Acheves/ab2$`Consistance Actuelle`
    ab2$pencours[1]=ab2$`En Cours`/ab2$`Consistance Actuelle`
    ab2$pnonlances[1]=ab2$`Non Lances`/ab2$`Consistance Actuelle`
    
    ab2$pacheves[is.nan(ab2$pacheves)==TRUE]=0
    ab2$pencours[is.nan(ab2$pencours)==TRUE]=0
    ab2$pnonlances[is.nan(ab2$pnonlances)==TRUE]=0
    
    
    ab2
    
  })
  
  
  
  dfa_fichewilaya3=reactive({
    i=1
    ab3[1,]=data_fiche_wilaya_reactive()[i,37:46]
    #ab3$`Total general`=ab3$Total+ab3$TotaI+ab3$`Acheves à 60% et plus`
    ab3$pacheves1=ab3$Acheves/ab3$Total
    ab3$pencours1=ab3$`En Cours`/ab3$Total
    ab3$pnonlances1=ab3$`Non Lances`/ab3$Total
    
    ab3$pacheves2=ab3$Achevee/ab3$TotaI
    ab3$pencours2=ab3$`En cours`/ab3$TotaI
    ab3$pnonlances2=ab3$`Non lances`/ab3$TotaI
    
    ab3$pacheves1[is.nan(ab3$pacheves1)==TRUE]=0
    ab3$pencours1[is.nan(ab3$pencours1)==TRUE]=0
    ab3$pnonlances1[is.nan(ab3$pnonlances1)==TRUE]=0
    
    ab3$pacheves2[is.nan(ab3$pacheves2)==TRUE]=0
    ab3$pencours2[is.nan(ab3$pencours2)==TRUE]=0
    ab3$pnonlances2[is.nan(ab3$pnonlances2)==TRUE]=0
    
    
    
    
    ab3  
  })
  
  
  dfa_fichewilaya1=reactive({
    i=1
    ab[1,2:7]=round(data_fiche_wilaya_reactive()[i,3:8]) #lpl
    ab[2,2:7]=round(data_fiche_wilaya_reactive()[i,9:14]) #lsp
    ab[3,2:7]=round(data_fiche_wilaya_reactive()[i,15:20]) #rural
    ab[4,2:7]=round(data_fiche_wilaya_reactive()[i,21:26]) #Location-Vente
    ab[5,2:7]=round(data_fiche_wilaya_reactive()[i,27:32]) #lpp
    ab[6,2]=sum(ab[1:5,2])
    ab[6,3]=sum(ab[1:5,3])
    ab[6,4]=sum(ab[1:5,4])
    ab[6,5]=sum(ab[1:5,5])
    ab[6,6]=sum(ab[1:5,6])
    ab[6,7]=sum(ab[1:5,7])
    ab[,2:7]=round(ab[,2:7])
    
    ab$pacheves=ab$Acheves/ab$Consistance
    ab$pencours=ab$`En Cours`/ab$Consistance
    ab$pdont=ab$`Dont A l'Arret`/ab$`En Cours`
    ab$pnonlances=ab$`Non Lances`/ab$Consistance
    ab$pacheves[is.nan(ab$pacheves)==TRUE]=0
    ab$pencours[is.nan(ab$pencours)==TRUE]=0
    ab$pdont[is.nan(ab$pdont)==TRUE]=0
    ab$pnonlances[is.nan(ab$pnonlances)==TRUE]=0
    
    ab
  })
  
  
  
  
  
  
  output$dfa=renderTable({
    dfa_fichewilaya1()
  })
  
  
  equip11_reactive=reactive({
    equip11 %>% 
      filter(Arretee==substr(input$select_arretee_eqp,14,23)) %>% 
      select(1,2,3,4,5,6,7,8)
  })
  
  output$excel_eqp<-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,
               
               #equip11,
               equip11_reactive(),
               columns = data.frame(width=c(250,400,300,250,250,250,250,250)),
               
               #columns = data.frame(title=rep("",ncol(sitphy2))),
               #mergeCells = list(A1=c(1,2),B1=c(9,1),K1=c(9,1),T1=c(9,1),AC1=c(9,1),AL1=c(9,1)   ),
               columnSorting=FALSE,search=TRUE
    )
  })
  
  
  selected_eqp <- reactive(
    `if`(length(getReactableState("equip_reactable", "selected"))==0,1:48,getReactableState("equip_reactable", "selected"))
  )
  
  data_equip_reactive=reactive({
    data_equip0() %>% 
      filter(Arretee==substr(input$select_arretee_eqp,14,23)) %>% 
      select(1,2,3,4,5,6,7)
  })
  
  
  output$wilayaselecteqp<-renderText({
    # secteurselecteqp()
    `if`(length(selected_eqp())==48,
         print(""),
         print(data_equip_reactive()$Wilaya[selected_eqp()]))
  })
  
  
  output$wilayaselecteqp22<-renderText({
    # secteurselecteqp()
    `if`(length(selected_eqp())==48,
         print(""),
         print(data_equip_reactive()$Wilaya[selected_eqp()]))
  })
  

  output$tablededonnes1_eqp<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de donnees</span> <br> <span style="font-size:13px;">Situation Arretee au :&nbsp;&nbsp; </span> <span style="font-size:14px;">',substr(input$select_arretee_eqp,14,23),'</span>'
    ))
    
  )
  
  
  
  
  selected_equip <- reactive(
    `if`(length(getReactableState("equip_reactable", "selected"))==0,1:48,getReactableState("equip_reactable", "selected"))
  )
  
  # 
  # selected_equip_secteur <- reactive(
  #   `if`(length(getReactableState("equip_secteur_reactable", "selected"))==0,
  #        equip_secteur_reactive()$Secteur,
  #        equip_secteur_reactive()$Secteur[getReactableState("equip_secteur_reactable", "selected")]
  #       )
  # )
  # 
  
  
  
  selected_equip_secteur <- reactive({
    getReactableState("equip_secteur_reactable", "selected")
  })
  
  
  secteurselecteqp<-reactive({
    `if`(length(selected_equip_secteur())==0,
         paste(equip_secteur_reactive()$Secteur),
         paste(equip_secteur_reactive()$Secteur[selected_equip_secteur()]))
  })
  
  
  secteurselecteqp22<-reactive({
    `if`(length(selected_equip_secteur())==0,
         paste(""),
         paste(" "," ","( Secteur : ",equip_secteur_reactive()$Secteur[selected_equip_secteur()]," "," )")
    )
  })
  
  
  
  
  
  
  
  
  equ=reactive({
    equip %>%
      filter(Arretee==substr(input$select_arretee_eqp,14,23),as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip()) %>% 
      group_by(Secteur) %>%
      summarise("Acheves"=sum(Acheves),"En Cours"=sum(`En Cours`),"Non Lances"=sum(`Non Lances`),"Dont NIR"=sum(`Dont NIR`),"Geles"=sum(Geles)
      ) %>% 
      gather("Cas","Nb",2:6) %>% 
      arrange(desc(Cas))
  })
  
  equ_p=reactive({
    equip %>%
      filter(Arretee==substr(input$select_arretee_eqp,14,23),as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip()) %>% 
      group_by(Secteur) %>%
      summarise(pach=sum(Acheves)/sum(`Nbre de Projets`),penc=sum(`En Cours`)/sum(`Nbre de Projets`),
                pnonl=sum(`Non Lances`)/sum(`Nbre de Projets`),pdontnir=sum(`Dont NIR`)/sum(`Nbre de Projets`),
                sum(`Geles`)/sum(`Nbre de Projets`)
      ) %>% 
      gather("Cas","Nb_p",2:6) %>% 
      arrange(desc(Cas))
  })
  
  equ0p=reactive({
    cbind(equ(),p=round(100*equ_p()$Nb_p,0))  
  })
  
  
  equip_secteur_reactive=reactive({
    equip %>% 
      filter(Arretee==substr(input$select_arretee_eqp,14,23),as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip()) %>%
      select(1,2,3,4,5,8,9,6,7) %>% 
      group_by(Secteur) %>% 
      summarise_at(vars(`Nbre de Projets`:Geles),sum) %>%
      arrange(desc(`Nbre de Projets`))
  })
  
  
  output$equip_secteur_reactable<-renderReactable({
    reactable(equip_secteur_reactive(),
              #sortable = TRUE,
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              selection = "single",
              #striped = TRUE,
              #fullWidth = FALSE,
              bordered = TRUE,
              
              #defaultSorted = list(`Nbre de Projets` = "desc", Acheves = "desc"),
              #showSortIcon = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "17px"),
              columns = list(
                .selection = colDef(
                  width = 1,
                  style = list(display = "none"),
                  footerStyle = list(marginLeft="-15px")
                  
                ),
                Secteur = colDef(name="",width = 230,footer = "Total",
                                 
                                 style = list(background="#60777d"  ,color="#ffffff",fontFamily="system-ui",fontWeight="500")),
                `Nbre de Projets` = colDef(width = 160,align = "center",
                                           footer = sum(equip_secteur_reactive()$`Nbre de Projets`),
                                           headerStyle = list(
                                             background = "#2E81B0",
                                             color="#ffffff",  
                                             borderLeft="0px"
                                           )
                ),
                Acheves=colDef(width = 80,
                               footer = sum(equip_secteur_reactive()$Acheves)
                               
                ),
                Geles=colDef(width = 60,
                             footer = sum(equip_secteur_reactive()$Geles)
                             
                ),
                `En Cours`=colDef(width = 90,
                                  footer = sum(equip_secteur_reactive()$`En Cours`)
                                  
                ),
                `Non Lances`=colDef(width=110,
                                    footer = sum(equip_secteur_reactive()$`Non Lances`)
                                    
                ),
                `Dont NIR`=colDef(width=90,
                                  footer = sum(equip_secteur_reactive()$`Dont NIR`)
                                  
                )
              ),
              defaultColDef = colDef(
                sortNALast = FALSE,
                #format = colFormat(digits = 1),
                #maxWidth = 140
                format = colFormat(separators = TRUE),
                footerStyle = list(fontWeight = "bold"
                                   
                                   
                ),
                
                headerStyle = list(
                  background = "#2E81B0",
                  color="#ffffff",  
                  borderRight="0px",
                  fontSize="15px"
                )
                
              ),
              
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#60777d"  , boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
              
    )
  })
  
  output$equip_reactable <- renderReactable({
    reactable(data_equip_reactive(),
              defaultPageSize = 48, striped = TRUE,borderless = TRUE,
              height="800px",  selection = "single",
              rowStyle =list(cursor = "pointer"),
              onClick = "select",
              columns = list(
                Wilaya=colDef(width = 136,footer="Total"),
                `Nbre de Projets`=colDef(footer=sum(data_equip_reactive()$`Nbre de Projets`)),
                Acheves=colDef(footer=sum(data_equip_reactive()$Acheves)),
                `En Cours`=colDef(footer=sum(data_equip_reactive()$`En Cours`)),
                `Non Lances`=colDef(footer=sum(data_equip_reactive()$`Non Lances`)),
                `Dont NIR`=colDef(footer=sum(data_equip_reactive()$`Dont NIR`),width=70),
                `Geles`=colDef(footer=sum(data_equip_reactive()$`Geles`),width=70)
                
                
              ),
              columnGroups = list(
                colGroup(
                  #name = paste(secteurselecteqp()),
                  name = paste("Nombre de Projets Par Wilaya",print(secteurselecteqp22())),
                  
                  columns = colnames(data_equip_reactive()),headerStyle=list(fontWeight='100',fontFamily='Roboto'))
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#7bafba", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
              #  ,
              
              # details = function(index) {
              #   plant_data <- equip11_reactive()[equip11_reactive()$Wilaya == data_equip_reactive()$Wilaya[index], ]
              #   htmltools::div(style = "padding: 1px",borderless = TRUE,compact=TRUE,
              #                  reactable(plant_data[,-1], outlined = TRUE,style=list(fontSize="13px"),
              #                            defaultColDef = colDef(width=110,footerStyle = list(fontWeight = "bold")),
              #                            
              #                  )
              #   )
              # }
    )
    
  })
  
  
  
  
  observe({
    
    `if`(as.numeric(selected20_fichewilaya())==16,
         js$opac77('none'),
         js$opac77('block')
    )
    
    
    
  })
  
  
  selected2_fichewilaya <- reactive(getReactableState("table2_fichewilaya", "selected"))
  
  selected20_fichewilaya <- reactive(
    `if`(length(getReactableState("table2_fichewilaya", "selected"))==0,1:48,getReactableState("table2_fichewilaya", "selected"))
  )
  
  output$table2_fichewilaya <- renderReactable({
    reactable(data.frame(Wilaya=unique(data_fiche_wilaya$Wilaya)),defaultPageSize = 48,striped = TRUE,
              selection = "single",
              sortable = FALSE,
              borderless = TRUE,
              height="900px",
              width="66%",
              columns = list(
                Wilaya = colDef(width = 158,align="left")   # 50% width, 200px minimum
                
              ),
              #  columnGroups = list(
              #    colGroup(name = paste("Situation Physiques des logements"," ",paste(select_segment_title(),collapse = "+")), columns = colnames(sitphy00()))
              #  ),
              #  defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              onClick = "select",
              theme = reactableTheme(
                style = list(
                  fontSize="16px"
                  #,fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
                ),
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  
  #################################
  tpos_reactive=reactive({
    pos %>%
      filter(id_wilaya %in% selected()) %>% 
      group_by(URBANISME) %>%
      summarise(lances=sum(Lancés),"Non Lancées"=sum(`Non Lancées`),Achevées=sum(Achevées),"En Cours"=sum(`En Cours`),approuvees=sum(Approuvées)) %>% 
      gather("etat","nb",2:5) %>% filter(etat %in% c("Non Lancées","Achevées","En Cours"))
  })
  
  

  
  observe({
    runjs("
                   var tgh=document.getElementById('pourcentage');
tgh.parentElement.style.right='7px';
tgh.parentElement.style.top='100px';

var tgh_urbanisme=document.getElementById('pourcentage_urbanisme');
tgh_urbanisme.parentElement.style.right='7px';
tgh_urbanisme.parentElement.style.top='100px';


                   ")
  })
  
}

shinyApp(ui = ui, server = server)