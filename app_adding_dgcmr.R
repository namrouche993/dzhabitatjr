#setwd("C:/electron-quick-start-win32-ia32/resources/app")
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

library(rpivotTable)
library(shinytreeview)


pdf_files_data=data.frame(
  Niveau=c(rep("Niv2:Regle de Conception de de Calcul",length(list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau2")))),rep("Niv3: Regle d'Execution",length(list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau3"))))),
  fichier=c(list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau2")),list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau3")))
)

moyens_realisation <-read_excel(paste0(getwd(),"/moyens_realisations.xlsx"),skip = 1)
moyens_realisation$Arretee=as.Date(moyens_realisation$Arretee)


datamc <- read_excel(paste0(getwd(),"/materiaux_construction.xlsx"))
datamc$`Année d'entrée en production`=as.numeric(datamc$`Année d'entrée en production`)

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
zones <- read_excel(paste0(getwd(),"/zones2.xlsx"))
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
pos2=pos %>% 
  mutate(Wilaya=rep(unique(livraison_wilayas$waw),3)) %>% 
  select(Wilaya,URBANISME,Achevées,`En Cours`,`Non Lancées`)

pos3=matrix(0,ncol=5,nrow=147)
pos3=data.frame(pos3)
colnames(pos3)=colnames(pos2)
pos3=pos2[1:48,]
pos3[49,1]="Total"
pos3[49,2]="POS"
pos3[49,3]=6539
pos3[49,4]=372
pos3[49,5]=9

pos3[50:98,]=pos2[49:97,]
pos3[98,1]="TOTAL"
pos3[98,2]="PDAU"
pos3[98,3]=1508
pos3[98,4]=33
pos3[98,5]=NA

pos3[99:146,]=pos2[97:144,]
pos3[147,1]="TOTAL"
pos3[147,2]="EGU"
pos3[147,3]=1424
pos3[147,4]=251
pos3[147,5]=33
# 
pos5=pos3
pos5=pos3[1:49,]
pos5[,6:8]=pos3[50:98,3:5]
pos5[,9:11]=pos3[99:147,3:5]
pos5=pos5[,-c(2,8)]

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
algeria@data$wilayas=unique(livraison_wilayas$waw)[id_wilaya]

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
    HTML("Ministere de l'Habitat,<br/> de l'Urbanisme et de la Ville"),
    id = "main_navbar",
    tabPanel("Logements",
             dropdown(
               
               tags$h3("Filtrage des données"),
               br(),
               pickerInput(
                 inputId = "wilayas",
                 label = "", 
                 choices = unique(livraison_wilayas$waw),
                 options = list(`actions-box` = TRUE,style = "btn-primary",
                                `selected-text-format`= "count>2",
                                `count-selected-text` = "{0} Wilayas séléctionnés",
                                `none-selected-text` ="Wilaya",`select-all-text`=
                                  tags$div(
                                    "Séléctionner", 
                                    tags$br(),
                                    "Tout"
                                  ),
                                `deselect-all-text`=tags$div(
                                  "Deselectionner", 
                                  tags$br(),
                                  "Tout"
                                )),
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "segments",
                 label = "", 
                 choices = unique(livraison_wilayas$type_de_logement),
                 options = list(`actions-box` = TRUE,style = "btn-primary",
                                `selected-text-format`= "count>2",
                                `count-selected-text` = "{0} Segment séléctionnés",
                                `none-selected-text` ="Segment",`select-all-text`=
                                  tags$div(
                                    "Séléctionner", 
                                    tags$br(),
                                    "Tout"
                                  ),
                                `deselect-all-text`=tags$div(
                                  "Deselectionner", 
                                  tags$br(),
                                  "Tout"
                                )),
                 multiple = TRUE
               ),
               br(),
               sliderInput("annees","Année :",
                           min = min(unique(estimation_tolpopparc$Annee)), max = max(unique(estimation_tolpopparc$Annee)), value = c(min(unique(estimation_tolpopparc$Annee)),max(unique(estimation_tolpopparc$Annee))),sep = ""),
               
               
               style = "material-circle", icon = icon("bars"),
               status = "primary", width = "300px"
               #, animate = animateOptions(
               # enter = animations$fading_entrances$fadeInRightBig,
               #exit = animations$fading_exits$fadeOutRightBig
               # )
             ),
             fluidRow(
               column(width=2,style = "background-color:#eaeaea;",
                      fluidRow( tags$div(class="card",
                                         tags$span(class="title","Livraisons des logements",style="display:inline-block;"),textOutput("titre_livraison"),
                                         tags$h3(class="metric",textOutput("livraisons")),
                                         tags$span(class="color__nb",htmlOutput("taux_livraisons"),style="color:green;display: inline-block;"),textOutput("dernier_an") #style=paste0("color:",ifelse(34>0,"green","red"))
                      ),
                      
                      
                      
                      tags$head(HTML('<style type="text/css">

#annee_na_count {
    color: darkred;
    font-size: 13px;
    text-align: center;
    margin-top: -7px;
}

table.pvtTable{
    font-size: 11pt;
    text-align: left;
    border-collapse: collapse;
}

#mc_annee_debut{
    width: 100%;
    height: 400px;
    visibility: inherit;
    overflow: hidden;
    margin-top:80px;
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
    background-color: #81a47b;
    color: white;
    font-size: 18px;
    font-weight: 400;
    width: 837px;
    font-family: system-ui;
    }




#boxscore1_arretee{
    display: inline;
}

#boxscore1_arretee_mr{
    display: inline;
}



#boxscore2_arretee_mr{
    display: inline;
}

#boxscore3_arretee_mr{
    display: inline;
}

#boxscore4_arretee_mr{
    display: inline;
}

#boxscore5_arretee_mr{
    display: inline;
}

#boxscore6_arretee_mr{
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

#boxscore1_wilaya_mr {
    display: inline;
    padding-left: 306px;
}


#boxscore2_wilaya_mr {
    display: inline;
    padding-left: 306px;
}


#boxscore3_wilaya_mr {
    display: inline;
    padding-left: 306px;
}

#boxscore4_wilaya_mr {
    display: inline;
    padding-left: 306px;
}

#boxscore5_wilaya_mr {
    display: inline;
    padding-left: 213px;
}

#boxscore6_wilaya_mr {
    display: inline;
    padding-left: 237px;
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


#box-score-title1_mr {
    margin-top: 24px;
    padding: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1201px;
    font-family: system-ui;
}


#box-score-title2_mr {
    margin-top: 24px;
    padding: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1082px;
    font-family: system-ui;
}


#box-score-title3_mr {
    margin-top: 24px;
    padding: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1301px;
    font-family: system-ui;
}

#box-score-title4_mr {
    margin-top: 24px;
    padding: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1001px;
    font-family: system-ui;
}

#box-score-title5_mr {
    margin-top: 24px;
    padding: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1000px;
    font-family: system-ui;
}

#box-score-title6_mr {
    margin-top: 24px;
    padding: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 722px;
    font-family: system-ui;
}



#box-score1 {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:100px;
  margin-bottom:50px;
}

#box-score1_mr {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:28px;
  margin-bottom:100px;
}




#box-score2_mr {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:92px;
  margin-bottom:-10px;
}

#box-score3_mr {
    font-family: "Roboto", Helvetica, Arial, sans-serif;
    padding-left: 0px;
    margin-bottom: 100px;
    margin-left: -7px;
}

#box-score4_mr {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:149px;
  margin-bottom:-10px;
}

#box-score5_mr {
    font-family: "Roboto", Helvetica, Arial, sans-serif;
    padding-left: 155px;
    margin-bottom: 100px;
    margin-left: -7px;
}

#box-score6_mr {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:265px;
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
    

#tabfichewilaya_mr + .shiny-input-container {
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
    
#select_arretee_fichewilaya_mr + .dropdown-toggle {
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
    
#select_arretee_fichewilaya_mr + .dropdown-toggle:hover {
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

#display_when_hover_choose_leaflet_mc {
    position: absolute;
    top: 4px;
    right: 503px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 211px;
    color: black;
    font-family: unset;
    font-size: 15px;
    height: 121px;
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

#choose_leaflet_mc {
    position: absolute;
    top: 4px;
    right: 671px;
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

#choose_leaflet_mc .glyphicon-th-list{
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

#tabfichewilaya_mr {
    margin-right: -15px;
    margin-left: -13px;
    margin-top: -8px;
    overflow: -webkit-paged-x;
    zoom:0.95;

}

                      
#ttwilayas {
text-align: center;
font-size:32px;
font-family: "Roboto";
color: #167c76;
margin-top:-6px;
}

#ttwilayas2 {
text-align: center;
font-size:32px;
font-family: "Roboto";
color: #167c76;
margin-top:1px;
padding-bottom:14px;
}


#region {
    text-align: center;
    padding-bottom: 1px;
    font-size: 20px;
    margin-top: -10px;
    font-family: "Roboto";
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

#table2_fichewilaya_mr .ReactTable .rt-tr-group {
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
  
  #table2_fichewilaya_mr .ReactTable .rt-tr-group:hover {
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

#table2_fichewilaya_mr .rt-td rt-align-left{
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
  
  #table2_fichewilaya_mr .rt-th-content {
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

#c1_fichewilaya_mr {
margin-right:-300px;
}

#c12_fichewilaya {
   margin-left: 155px;
   visibility: inherit;
   overflow-y: scroll;
   height: 900px;
}

#c12_fichewilaya_mr {
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
    margin-top: -14px;
    line-height: 110%;
    font-family: system-ui;
    font-weight: 500;
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

</style>')) ),
                      fluidRow( tags$div(class="card",
                                         tags$span(class="title","Lancement des logements",style="display:inline-block;"),textOutput("titre_lancement"),
                                         tags$h3(class="metric",textOutput("lancements"))
                                         ,tags$span(class="color__nb",htmlOutput("taux_lancements"),style="color:green;display: inline-block;"),textOutput("dernier_an_lanc") #style=paste0("color:",ifelse(34>0,"green","red"))
                      )),
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_parclogement")),
                                         tags$h3(class="metric",textOutput("parclogements")),
                                         tags$span(class="color__nb",textOutput("taux_parclogement"),style="color:green;display: inline-block;"),textOutput("dernier_an2") #style=paste0("color:",ifelse(34>0,"green","red"))
                      ) ),
                      
                      
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_population")),
                                         tags$h3(class="metric",textOutput("populations")),
                                         tags$span(class="color__nb",textOutput("taux_population"),style="color:green;display: inline-block;"),textOutput("dernier_an3")
                      ) ),
                      
                      
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_tol")),
                                         tags$h3(class="metric",textOutput("tols")),
                                         tags$span(class="color__nb",textOutput("taux_tol"),style="color:green;display: inline-block;"),textOutput("dernier_an4")
                      ) ),
                      
                      fluidRow( tags$div(class="card",
                                         tags$h4(class="title",textOutput("max_annee_densite")),
                                         tags$h3(class="metric",textOutput("densite")),
                                         tags$span(class="color__nb",textOutput("taux_densite"),style="color:green;display: inline-block;"),textOutput("dernier_an8") #style=paste0("color:",ifelse(34>0,"green","red"))
                      ) )
                      
               ),
               column(width=5,style = "background-color:#eaeaea;",
                      wellPanel(id="well1",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_hchart1"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie1_hchart1"),
                                         
                                         actionBttn(
                                           inputId = "tabmodalserie1",
                                           label = NULL,
                                           style = "bordered", 
                                           color = "success",
                                           icon = icon("table")
                                         ),
                                         
                                         actionBttn(
                                           inputId = "choose_line1",
                                           label = NULL,
                                           style = "simple", 
                                           color = "primary",
                                           icon = icon("th-list",lib = "glyphicon")
                                         ),
                                         div(id="display_when_hover_choose_line1",
                                             
                                             awesomeRadio(
                                               inputId = "radio_choose_line1",
                                               label = "", 
                                               choices = c("Livraisons de Logements","Lancements de Logements","TOL"),
                                               selected = "Livraisons de Logements"
                                               #,color="default"
                                             )
                                             
                                         )
                                ),
                                div(
                                  style = "padding-top: 30px;zoom:100%;"
                                  ,highchartOutput("hchart1", height = 330)
                                  
                                )
                                
                                ,prettySwitch(
                                  inputId = "Id027",
                                  label = "Par Segment", 
                                  status = "primary",
                                  slim = TRUE,
                                  value=TRUE,
                                )),
                      fluidRow(
                        column(width=12,style = "background-color: transparent;zoom:100%",
                               wellPanel(id="well12",
                                         
                                         tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput('titre_pie'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie2"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("periode"),
                                                  
                                                  actionBttn(
                                                    inputId = "tabmodalserie2",
                                                    label = NULL,
                                                    style = "bordered", 
                                                    color = "success",
                                                    icon = icon("table")
                                                  ),
                                                  actionBttn(
                                                    inputId = "choose_line1_pie",
                                                    label = NULL,
                                                    style = "simple", 
                                                    color = "primary",
                                                    icon = icon("th-list",lib = "glyphicon")
                                                  ),
                                                  div(id="display_when_hover_choose_line1_pie",
                                                      
                                                      awesomeRadio(
                                                        inputId = "radio_choose_line1_pie",
                                                        label = "", 
                                                        choices = c("Livraisons de Logements","Lancements de Logements"),
                                                        selected = "Livraisons de Logements"
                                                        #,color="default"
                                                      )
                                                      
                                                  )
                                                  
                                                  
                                                  
                                         )
                                         ,div(style = "margin-top:-1em;margin-bottom:-1em;margin-left:-1em;margin-right:-1em;padding-top: 10px;",
                                              highchartOutput("pie", height = 330)))
                        )
                      )
               ),
               column(width=5,style = "background-color:#eaeaea;",
                      wellPanel(id="well1",
                                tags$div(class="chart-title",HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_map"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("titre_serie3"),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("periode2"),
                                         
                                         actionBttn(
                                           inputId = "tabmodalserie3",
                                           label = NULL,
                                           style = "bordered", 
                                           color = "success",
                                           icon = icon("table")
                                         ),
                                         
                                         actionBttn(
                                           inputId = "choose_leaflet",
                                           label = NULL,
                                           style = "simple", 
                                           color = "primary",
                                           icon = icon("th-list",lib = "glyphicon")
                                         ),
                                         div(id="display_when_hover_choose_leaflet",
                                             
                                             awesomeRadio(
                                               inputId = "radio_choose_leaflet",
                                               label = "", 
                                               choices = c("Livraisons de Logements","Lancements de Logements","TOL", "Population","Parc Logements"),
                                               selected = "Livraisons de Logements"
                                               #,color="default"
                                             )
                                             
                                         )
                                         
                                         
                                )
                                
                                ,div(style = "padding: 0px 0px;zoom:122%;height:620px;",
                                     shinycssloaders::withSpinner(
                                       leafletOutput("distPlot2",height=613
                                                     #ifelse(abad()==1,700,568) 
                                       ))
                                ))
               )
               
               
             ),
             #tabmodalserie_eqp
             bsModal("modal1", htmlOutput("tablededonnes1"), "tabmodalserie1", size = "large"
                     ,excelOutput("excel1")),
             
             bsModal("modal2",htmlOutput("tablededonnes2"), "tabmodalserie2", size = "large"
                     ,excelOutput("excel2")),
             
             bsModal("modal3",htmlOutput("tablededonnes3"), "tabmodalserie3", size = "large"
                     ,excelOutput("excel3"))
    ),
    
    
    
    
    
    tabPanel("Urbanisme",
             fluidRow(
               id='fluidrow1_urbanisme',
               style = "display:flex; align-items:flex-start",
               wellPanel(id="side_wilaya",
                         style = "overflow-y: auto; position:fixed; width:300px; top:0; bottom:0",
                         reactableOutput("table")
               ),
               fluidRow( #~~ Main panel ~~#
                 style = "flex-grow:1; resize:horizontal; overflow: hidden; position:relative; margin-left: 310px",
                 column(6,
                        wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),tags$p("Instruments d'Urbanisme : "),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselect1"),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme1",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                  ),
                                  
                                  highchartOutput("urbanisme1")      
                        ),
                        
                        wellPanel(id="urbanisme_well2",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme2",HTML('&nbsp;'),HTML('&nbsp;'),"Situation Financières : ",HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselect2"),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme2",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                  ),
                                  
                                  highchartOutput("urbanisme2")
                        )
                 ),
                 column(6,
                        wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),"Lotissements sociaux : ",HTML('&nbsp;'),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme3",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                  ),
                                  
                                  div(style="padding-top:2px;",
                                      textOutput("ttwilayas"),
                                      textOutput("region"),
                                      tags$table(class="ta",style="font-size: 21px;font-family: system-ui;",
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
                        
                        wellPanel(id="urbanisme_well",style="width:100% ; height:380px;",
                                  tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),"Loi 18-05 : ",HTML('&nbsp;'),
                                           actionBttn(
                                             inputId = "tabmodalserie_urbanisme4",
                                             label = NULL,
                                             style = "bordered", 
                                             color = "success",
                                             icon = icon("table")
                                           )
                                           
                                  ),
                                  
                                  
                                  div(style="padding-top:2px;",
                                      textOutput("ttwilayas2"),
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
                 
                 
                 #verbatimTextOutput("selected")  
               )
             ),
             pickerInput(
               inputId = "select_arretee_urbanisme",
               label = "", 
               choices = c('Arretee le : 2019-12-31'),
               #c('Arretee le : 2020-06-30','Arretee le : 2019-12-31'),
             ),
             
             
             bsModal("modal_urbanisme1","Table de donnees", "tabmodalserie_urbanisme1", size = "large"
                     ,excelOutput("excel_urbanisme1")),
             
             bsModal("modal_urbanisme2","Table de donnees", "tabmodalserie_urbanisme2", size = "large"
                     ,excelOutput("excel_urbanisme2")),
             
             bsModal("modal_urbanisme3","Table de donnees", "tabmodalserie_urbanisme3", size = "large"
                     ,excelOutput("excel_urbanisme3")),
             
             bsModal("modal_urbanisme4","Table de donnees", "tabmodalserie_urbanisme4", size = "large"
                     ,excelOutput("excel_urbanisme4"))
             
             
             
             
    )
    
    
    
    
    ,tabPanel("Situation Physique des Logements",
              fluidRow(
                id="tabsitphy",
                
                column(6,id="c1",
                       # 
                       # awesomeCheckboxGroup(
                       #   inputId = "select_segment_gauge",
                       #   label = "Segments : ", 
                       #   choices = c("Rural", "LPL", "Location-Vente","LSP","LPP"),
                       #   inline = TRUE, 
                       #   status = "success"
                       # ),
                       # 
                       actionBttn(
                         inputId = "tabmodal_sitphy",
                         label = NULL,
                         style = "bordered", 
                         color = "success",
                         icon = icon("table")
                       ),
                       checkboxGroupButtons(
                         inputId = "select_segment_gauge",
                         choices = c("Rural", "LPL", "Location-Vente","LSP","LPP"),
                         status = "primary",
                         checkIcon = list(
                           yes = icon("ok", 
                                      lib = "glyphicon")
                         ),
                         width = "150%"
                       ),
                       
                       
                       
                       reactableOutput("table2")
                ),
                column(3,id="cg2",
                       wellPanel(id="well_gauge",
                                 tags$div(class="chart-title-gauge",tags$p(id="titregauge1",textOutput("toutsegments_gauge"),textOutput("wilayaselectgauge1")#textOutput("wgauge")
                                 )),
                                 highchartOutput("gauge1",height = "247px"),tags$p(id="livraison_gauge1","Livraison"),tags$span(id="livraison_gauge1_val",textOutput("val_livraison_gauge1")),tags$p(id="prevision_gauge1","Prevision"),tags$span(id="prevision_gauge1_val",textOutput("val_prevision_gauge1"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       ),
                       wellPanel(id="well_gauge2",
                                 tags$div(class="chart-title-gauge2",tags$p(id="titregauge2","LPL",textOutput("wilayaselectgauge2"))),
                                 highchartOutput("gauge2",height = "247px"),tags$p(id="livraison_gauge2","Livraison"),tags$span(id="livraison_gauge2_val",textOutput("val_livraison_gauge2")),tags$p(id="prevision_gauge2","Prevision"),tags$span(id="prevision_gauge2_val",textOutput("val_prevision_gauge2"))
                                 
                                 
                       ),
                       wellPanel(id="well_gauge3",
                                 tags$div(class="chart-title-gauge3",tags$p(id="titregauge3","LSP",textOutput("wilayaselectgauge3"))),
                                 highchartOutput("gauge3",height = "247px"),tags$p(id="livraison_gauge3","Livraison"),tags$span(id="livraison_gauge3_val",textOutput("val_livraison_gauge3")),tags$p(id="prevision_gauge3","Prevision"),tags$span(id="prevision_gauge3_val",textOutput("val_prevision_gauge3"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       )
                       
                ),
                column(3,   #3eme colonne
                       
                       wellPanel(id="well_gauge4",
                                 tags$div(class="chart-title-gauge4",tags$p(id="titregauge4","Rural",textOutput("wilayaselectgauge4"))),
                                 highchartOutput("gauge4",height = "247px"),tags$p(id="livraison_gauge4","Livraison"),tags$span(id="livraison_gauge4_val",textOutput("val_livraison_gauge4")),tags$p(id="prevision_gauge4","Prevision"),tags$span(id="prevision_gauge4_val",textOutput("val_prevision_gauge4"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       ),
                       
                       #column(5,id="cgauge4",
                       wellPanel(id="well_gauge5",
                                 tags$div(class="chart-title-gauge5",tags$p(id="titregauge5","Location-Vente",textOutput("wilayaselectgauge5"))),
                                 highchartOutput("gauge5",height = "247px"),tags$p(id="livraison_gauge5","Livraison"),tags$span(id="livraison_gauge5_val",textOutput("val_livraison_gauge5")),tags$p(id="prevision_gauge5","Prevision"),tags$span(id="prevision_gauge5_val",textOutput("val_prevision_gauge5"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       ),
                       wellPanel(id="well_gauge6",
                                 tags$div(class="chart-title-gauge6",tags$p(id="titregauge6","LPP",textOutput("wilayaselectgauge6"))),
                                 highchartOutput("gauge6",height = "247px"),tags$p(id="livraison_gauge6","Livraison"),tags$span(id="livraison_gauge6_val",textOutput("val_livraison_gauge6")),tags$p(id="prevision_gauge6","Prevision"),tags$span(id="prevision_gauge6_val",textOutput("val_prevision_gauge6"))
                                 
                                 
                                 #,verbatimTextOutput("selected")
                       )
                       
                ),
                
                bsModal("modal_sitphy",htmlOutput("tablededonnes1_sitphy"), "tabmodal_sitphy", size = "large"
                        ,excelOutput("excel_sitphy"))
              ),
              pickerInput(
                inputId = "select_arretee_sitphy",
                label = "", 
                choices = sort(paste(c("Arretee le :"),unique(sitphy$Arretee)),TRUE),
                #c('Arretee le : 2020-06-30','Arretee le : 2019-12-31'),
              )
    ),
    
    #################################################### FICHE WILAYA : 
    
    tabPanel("Fiche Wilayas",
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
    
    
    
    ################################################### FICHE WILAYA Fin
    
    #########################################
    
    navbarMenu("DGCMR",
    tabPanel("Unites de construction",
             dropdown(
               
               tags$h3("Filtrage des données"),
               br(),
               pickerInput(
                 inputId = "wilayas_mc",
                 label = "", 
                 choices = unique(datamc$wilaya_matricule),
                 options = list(`actions-box` = TRUE,style = "btn-primary",
                                `selected-text-format`= "count>2",
                                `count-selected-text` = "{0} Wilayas séléctionnés",
                                `none-selected-text` ="Wilaya",`select-all-text`=
                                  tags$div(
                                    "Séléctionner", 
                                    tags$br(),
                                    "Tout"
                                  ),
                                `deselect-all-text`=tags$div(
                                  "Deselectionner", 
                                  tags$br(),
                                  "Tout"
                                )),
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "filiere_mcmc",
                 label = "", 
                 choices = unique(datamc$Filiere),
                 options = list(`actions-box` = TRUE,style = "btn-primary",
                                `selected-text-format`= "count>2",
                                `count-selected-text` = "{0} Filiere séléctionnés",
                                `none-selected-text` ="Filiere",`select-all-text`=
                                  tags$div(
                                    "Séléctionner", 
                                    tags$br(),
                                    "Tout"
                                  ),
                                `deselect-all-text`=tags$div(
                                  "Deselectionner", 
                                  tags$br(),
                                  "Tout"
                                )),
                 multiple = TRUE
               ),
               pickerInput(
                 inputId = "statut2_mc",
                 label = "", 
                 choices = datamc %>% 
                   #filter() %>% 
                   group_by(Statut2) %>% 
                   summarise(Nb=n()) %>%
                   mutate(ar=c(3,5,4,1,2,6)) %>% 
                   arrange(ar) %>% .$Statut2,
                 options = list(`actions-box` = TRUE,style = "btn-primary",
                                `selected-text-format`= "count>2",
                                `count-selected-text` = "{0} Staut séléctionnés",
                                `none-selected-text` ="Statut",`select-all-text`=
                                  tags$div(
                                    "Séléctionner", 
                                    tags$br(),
                                    "Tout"
                                  ),
                                `deselect-all-text`=tags$div(
                                  "Deselectionner", 
                                  tags$br(),
                                  "Tout"
                                )),
                 multiple = TRUE
               ),
               #br(),
               #sliderInput("annees_mc","Annee dentree en production :",
              #             min = min(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE])), max = max(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE])), value = c(min(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE])),max(unique(datamc$Annee_entree[is.na(datamc$Annee_entree)==FALSE]))),sep = ""),
               
               
               style = "material-circle", icon = icon("bars"),
               status = "primary", width = "300px"
               #, animate = animateOptions(
               # enter = animations$fading_entrances$fadeInRightBig,
               #exit = animations$fading_exits$fadeOutRightBig
               # )
             ),
             fluidRow(
               column(width=3,style="height: 800px;width: 21%;margin-right: 20px;margin-left: 36px;",
                      fluidRow(id="card_mc",
                               style=
                               "height: 184px;margin-bottom:15px;;background-color:white;
                               box-shadow: 4px 5px 11px grey;
                               	border-radius:1px;
                               	font-family: sans-serif;
                               	padding: 1rem;
                               	box-shadow: 4px 5px 11px grey;
                               	transition: all .2s ease;
                               	border-bottom: 4px solid transparent;
                               	border-top: 4px solid #109485;",
                               tags$h3(id="card_mc_titre","Nombre d'unites de construction",
                                       style="text-align: center;margin-top: 8px;font-size: 28px;font-family: unset;"),
                               tags$h3(id="card_mc_chiffre",textOutput("chiffre_mc"),
                                       style="text-align: center;margin-top: -8px;font-size: 71px;font-family: unset;font-weight:550"),
                               
                               ),
                      fluidRow(id="filiere_mc",style="height: 602px;background-color:white;box-shadow: 4px 5px 11px grey;padding:17px;",
                               reactableOutput('mc_filiere_reactable')
                               ),
                      
                      
                      ),
               column(width=9,style="margin-left: 0px;width: 74%;background-color:white;",
                      fluidRow(id="rowselect_mc",style="height: 800px;",
                               column(width=8,style="box-shadow:4px 5px 11px grey;background-color:white;overflow:auto;",
                                      actionBttn(
                                        inputId = "choose_leaflet_mc",
                                        label = NULL,
                                        style = "simple", 
                                        color = "primary",
                                        icon = icon("th-list",lib = "glyphicon")
                                      ),
                                      div(id="display_when_hover_choose_leaflet_mc",
                                          
                                          awesomeRadio(
                                            inputId = "radio_choose_leaflet_mc",
                                            label = "", 
                                            choices = c("Maps","Tableau","Tableau croise dynamique"),
                                            selected = "Maps"
                                            #,color="default"
                                          )
                                          
                                      ),
                                        conditionalPanel(
                                          condition = "input.radio_choose_leaflet_mc == 'Maps'",
                                          selectInput("search", "Rechercher:",
                                                      #textOutput("datamc_identification"),
                                                      datamc$Identification,
                                                      multiple = TRUE
                                                      
                                          ),
                                          shinycssloaders::withSpinner(
                                          leafletOutput("distPlot2_mc",height="800px")
                                      ),
                                      textOutput("nd_mc_maps")
                                      ),
                                      conditionalPanel(
                                        condition = "input.radio_choose_leaflet_mc == 'Tableau'",
                                        shinycssloaders::withSpinner(
                                          
                                        reactableOutput("tableau_mc",height="800px")
                                      )
                                      ),
                                      conditionalPanel(
                                        condition = "input.radio_choose_leaflet_mc == 'Tableau croise dynamique'",
                                        shinycssloaders::withSpinner(
                                          rpivotTableOutput("pivot_table_mc",height="800px")
                                        )
                                      )
                                      
                               ),
                               column(width=4,
                                      style="height: 800px;background-color:white;box-shadow: 4px 5px 11px grey;width: 31%;margin-left: 29px;padding:17px;",
                                      reactableOutput('mc_statut_reactable'),
                                      highchartOutput("mc_annee_debut"),
                                      textOutput("annee_na_count")
                               )
                      )
                      #,
                      #fluidRow(style="height: 250px;background-color:white;box-shadow: 4px 5px 11px grey;")
               )
             )
    ),
    tabPanel("References",
             fluidRow(
               column(id="column1_pdf",width = 3,style="height: 800px;background: white;box-shadow: 4px 5px 11px grey;margin-right:8px;width:23%;overflow-y:scroll;",
                      treeviewInput(
                        inputId = "tree",
                        label = "",
                        choices = make_tree(
                          pdf_files_data, c("Niveau","fichier")
                        ),
                        multiple = FALSE,
                        prevent_unselect = TRUE,
                        width = "100%"
                      )
                      ),
               column(id="column2_pdf2",width=9,style="height: 800px;background: white;box-shadow: 4px 5px 11px grey;"
                      ,
                      #verbatimTextOutput(outputId = "tree_file")
                      htmlOutput("tree_file")
                      #includeHTML(paste0(getwd(),"/pdf_files_DGCMR/niveau3/BE11.html"))
               )
               
             )
             
    ),
    tabPanel("Moyens de Realisation",
             fluidRow(
               id="tabfichewilaya_mr",
               # prettySwitch(
               #   inputId = "pourcentage_mr",
               #   label = "Pourcentage", 
               #   status = "primary",
               #   slim = TRUE,
               #   value=FALSE
               # ),
               column(3,id="c1_fichewilaya_mr",
                      reactableOutput("table2_fichewilaya_mr")
               ),
               column(9,id='c12_fichewilaya_mr',style="margin-left:155px;",
                      wellPanel(id="well_fichewilaya_mr",
                                style="box-shadow:4px 5px 11px grey;background-color:white;",

                                div(id = "box-score1_mr",
                                    div(id = "box-score-title1_mr", "Entreprises qualifiers au ",textOutput("boxscore1_arretee_mr"),textOutput("boxscore1_wilaya_mr")),
                                    reactableOutput('gt1_mr',width = '1082px')
                                ),
                                
                                div(id = "box-score2_mr",
                                    div(id = "box-score-title2_mr", "Promoteurs immobiliers au",textOutput("boxscore2_arretee_mr"),textOutput("boxscore2_wilaya_mr")),
                                    reactableOutput('gt2_mr',width = '1082px')
                                ),
                                div(id = "box-score3_mr",
                                    div(id = "box-score-title3_mr", "Promoteurs immobiliers exercice de l'annee",textOutput("boxscore3_arretee_mr"),textOutput("boxscore3_wilaya_mr")),
                                    reactableOutput('gt3_mr',width = '1302px')
                                ),
                                
                                div(id = "box-score4_mr",
                                    div(id = "box-score-title4_mr", "Agences immobilieres au ",textOutput("boxscore4_arretee_mr"),textOutput("boxscore4_wilaya_mr")),
                                    reactableOutput('gt4_mr',width = '1002px')
                                ),
                                div(id = "box-score5_mr",
                                    div(id = "box-score-title5_mr", "Agences immobilieres exercice de l'annee",textOutput("boxscore5_arretee_mr"),textOutput("boxscore5_wilaya_mr")),
                                    reactableOutput('gt5_mr',width = '1000px')
                                ),
                                div(id = "box-score6_mr",
                                    div(id = "box-score-title6_mr", "Ingenieurs au ",textOutput("boxscore6_arretee_mr"),textOutput("boxscore6_wilaya_mr")),
                                    reactableOutput('gt6_mr',width = '722px')
                                ),
                                
                                
                      )
               )
             ),
             pickerInput(
               inputId = "select_arretee_fichewilaya_mr",
               label = "", 
               choices = sort(paste(c("Arretee le :"),unique(moyens_realisation$Arretee)),TRUE),
               #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
             )
    )
    ),
    ################################### fin DGCMR
    tabPanel("Ville",
             fluidRow(
               column(width=3,style="height: 800px;background-color:white;box-shadow: 4px 5px 11px grey;width: 21%;margin-right: 20px;margin-left: 36px;"),
               column(width=9,style="margin-left: 0px;width: 74%;background-color:white;",
                      fluidRow(id="rowselect",style="height: 550px;",
                               pickerInput(
                                 inputId = "select_villes",
                                 label = "",
                                 #color = "success",
                                 #style = "stretch",
                                 choices = c("Villes", "ALI MENDJLI", "BOUGHEZOUL","BOUINANE", 
                                             "DRAA ERICH","SIDI ABDELLAH","EL MENEAA")
                                 ,choicesOpt = list(
                                   style = c("font-size: 114%;font-weight: bold;"))
                                 #             "color: firebrick; text-align: right;", "font-weight: bold;", 
                                 #             "background: forestgreen; color: white;")
                                 #   )
                                 # 
                               ),
                               column(width=9,style="box-shadow:4px 5px 11px grey;background-color:white;",
                                      shinycssloaders::withSpinner(
                                        leafletOutput("leaflet_ville",height="536px")
                                      )
                               ),
                               column(width=3,
                                      style="height: 534px;background-color:white;box-shadow: 4px 5px 11px grey;width: 23%;margin-left: 24px"
                               )
                      ),
                      fluidRow(style="height: 250px;background-color:white;box-shadow: 4px 5px 11px grey;")
               )
             )
    ),
    
    
    # 
    # tabPanel("Bilan2020",
    #          htmlOutput("bilan")
    #          ),
    # 
    
    
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
             
    ),
    navbarMenu("Données",
               tabPanel("Livraisons des logements ",
                        
                        fluidRow(id="rowdonnees1",style="padding-top: 25px",
                                 column(2,offset = 2,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectwilayas",
                                          label = "", 
                                          choices = unique(livraison_wilayas$waw),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Wilayas séléctionnés",
                                                         `none-selected-text` ="Wilaya",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                        
                                        
                                 ),
                                 column(2,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectsegments",
                                          label = "", 
                                          choices = unique(livraison_wilayas$type_de_logement),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Segment séléctionnés",
                                                         `none-selected-text` ="Segment",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                 ),
                                 column(4,
                                        sliderInput("selectannees","",
                                                    min = min(unique(livraison_wilayas$annee)), max = max(unique(livraison_wilayas$annee)), value = c(min(unique(livraison_wilayas$annee)),max(unique(livraison_wilayas$annee))),sep = "")
                                        
                                 )
                        ),
                        fluidRow(id="rowdonnes2",
                                 column(2,offset = 3,
                                        prettySwitch(
                                          inputId = "parwilayas",
                                          label = "Par Wilaya", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                        
                                 ),
                                 column(2,style="padding-left:310px;",
                                        prettySwitch(
                                          inputId = "parsegments",
                                          label = "Par Segment", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=TRUE
                                        )
                                 ),
                                 column(2,style="padding-left:310px;",
                                        prettySwitch(
                                          inputId = "parperiode",
                                          label = "Par Annee", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                 )
                        ),
                        fluidRow(id="rowdonnes33",style="margin-top:35px;",
                                 htmlOutput('rowdonnees33html')
                        ),
                        fluidRow(
                          
                          excelOutput("donnees_excel",height="100%")
                          
                        )
                        
               ),
               tabPanel("Lancements des logements ",
                        
                        fluidRow(id="rowdonnees1",style="padding-top: 25px",
                                 column(2,offset = 2,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectwilayas_lanc",
                                          label = "", 
                                          choices = unique(livraison_wilayas$waw),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Wilayas séléctionnés",
                                                         `none-selected-text` ="Wilaya",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                        
                                        
                                 ),
                                 column(2,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectsegments_lanc",
                                          label = "", 
                                          choices = unique(livraison_wilayas$type_de_logement),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Segment séléctionnés",
                                                         `none-selected-text` ="Segment",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                 ),
                                 column(4,
                                        sliderInput("selectannees_lanc","",
                                                    min = min(unique(lancement_wilayas$Annee)), max = max(unique(lancement_wilayas$Annee)), value = c(min(unique(lancement_wilayas$Annee)),max(unique(lancement_wilayas$Annee))),sep = "")
                                        
                                 )
                        ),
                        fluidRow(id="rowdonnes2",
                                 column(2,offset = 3,
                                        prettySwitch(
                                          inputId = "parwilayas_lanc",
                                          label = "Par Wilaya", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                        
                                 ),
                                 column(2,style="padding-left:310px;",
                                        prettySwitch(
                                          inputId = "parsegments_lanc",
                                          label = "Par Segment", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=TRUE
                                        )
                                 ),
                                 column(2,style="padding-left:310px;",
                                        prettySwitch(
                                          inputId = "parperiode_lanc",
                                          label = "Par Annee", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                 )
                        ),
                        fluidRow(id="rowdonnes33",style="margin-top:35px;",
                                 htmlOutput('rowdonnees33html_lanc')
                        ),
                        fluidRow(
                          
                          excelOutput("donnees_excel_lanc",height="100%")
                          
                        )
                        
               ),
               
               
               tabPanel("Données des Wilaya",
                        
                        fluidRow(id="rowdonnees1",style="padding-top: 25px",
                                 column(2,offset = 3,style="padding-top: 22px;",
                                        pickerInput(
                                          inputId = "selectwilayas_wilaya",
                                          label = "", 
                                          choices = unique(livraison_wilayas$waw),
                                          options = list(`actions-box` = TRUE,style = "btn-primary",
                                                         `selected-text-format`= "count>2",
                                                         `count-selected-text` = "{0} Wilayas séléctionnés",
                                                         `none-selected-text` ="Wilaya",`select-all-text`=
                                                           tags$div(
                                                             "Séléctionner", 
                                                             tags$br(),
                                                             "Tout"
                                                           ),
                                                         `deselect-all-text`=tags$div(
                                                           "Deselectionner", 
                                                           tags$br(),
                                                           "Tout"
                                                         )),
                                          multiple = TRUE
                                        )
                                        
                                        
                                 ),
                                 
                                 column(4,
                                        sliderInput("selectannees_wilaya","",
                                                    min = min(unique(estimation_tolpopparc$Annee)), max = max(unique(estimation_tolpopparc$Annee)), value = c(min(unique(estimation_tolpopparc$Annee)),max(unique(estimation_tolpopparc$Annee))),sep = "")
                                        
                                 )
                        ),
                        fluidRow(id="rowdonnes2",style="margin-left: -6px;margin-bottom:30px; ",
                                 column(2,offset = 5,
                                        prettySwitch(
                                          inputId = "parwilayas_wilaya",
                                          label = "Par Wilayas", 
                                          status = "primary",
                                          slim = TRUE,
                                          value=FALSE
                                        )
                                        
                                 )),
                        fluidRow(id="rowdonnes332",style="margin-top:35px;",
                                 htmlOutput('rowdonnees33html2')
                        ),
                        fluidRow(
                          
                          excelOutput("donnees_excel_wilaya",height="100%")
                          
                        )
                        
               ))
    
  )
  
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  mc_ingenieur_reactive<-reactive({
    moyens_realisation %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(`Nombre ingenieur agree`:sexe_f),sum) %>% 
      mutate(nbring=`Nombre ingenieur agree`) %>%
      select(7,2,3,4,5,6)
  })
  
  output$gt6_mr<-renderReactable({
    reactable(mc_ingenieur_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                nbring=colDef(
                  name="Nombre d'Ingenieurs Agrees",
                  width=220,
                  align="center",
                  headerStyle = list(
                    #borderTop="0px",
                    marginTop="-25px",
                    textAlign="left",
                    background = "#2E81B0",
                    color="#ffffff",
                    textAlign="center"
                  )
                ),
                entre_25_40=colDef(name="25-40",align = "center"),
                entre_41_60=colDef(name="41-60",align = "center"),
                plus_60=colDef(name=">60",align = "center"),
                sexe_m=colDef(name="M",align = "center"),
                sexe_f=colDef(name="F",align = "center")
                
                
                
                
              ),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                align = "left",
                width = 100,
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                #maxWidth = 260,
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff"
                )
              ),
              columnGroups = list(
                colGroup(name = "", columns = c("nbring"),
                         headerStyle =list(
                           #border="0px solid #eee",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                ),
                colGroup(name = "Par Age", columns = c("entre_25_40","entre_41_60","plus_60"),
                         headerStyle =list(
                           borderRight="25px inset transparent",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                ),
                colGroup(name = "Par Sexe", columns = c("sexe_m","sexe_f"),
                         headerStyle =list(
                           borderLeft="25px inset transparent",
                           #borderRight="55px inset transparent",
                           
                           #border="0px solid #eee",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                )
              )
              
              
              
    )
  })
  
  
  
  
  mc_agence_exercice_reactive<-reactive({
    moyens_realisation %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(`Nouvelle Demande`:`Mise en Conformite`),sum)
  })
  
  
  output$gt5_mr<-renderReactable({
    reactable(mc_agence_exercice_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                align = "left",
                width = 333,
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                #maxWidth = 260,
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff"
                )
              )
    )
  })
  
  

  mc_agence_reactive<-reactive({
    moyens_realisation %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(`AGENTS IMMOB`:COURTIERS),sum)
  })
  
  output$gt4_mr<-renderReactable({
    reactable(mc_agence_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                align = "left",
                width = 250,
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                #maxWidth = 260,
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff"
                )
              )
    )
  })
  
  
  mc_promoteur_exercice_reactive<-reactive({
    moyens_realisation %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(`Nbre de dossiers deposes`:Taux_tnpi),sum) %>%
      mutate(Taux_commision=`Nbre de dossiers examines en commission`/`Nbre de dossiers deposes`,
             Taux_tnpi=`Nbre dattestations TNPI`/`Nbre dagrements etablis`)
  })
  
  
  output$gt3_mr<-renderReactable({
    reactable(mc_promoteur_exercice_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                "Taux_commision"=colDef(
                  name="Taux",
                  width=110,
                  align = "center",
                  format=colFormat(percent = TRUE,digits = 0),
                  style=list(fontWeight="bold",
                             paddingRight="30px"
                  ),
                  headerStyle = list(paddingRight="30px",
                                     background = "#2E81B0",
                                     color="#ffffff"
                                     )
                ),
                "Taux_tnpi"=colDef(
                  name="Taux",
                  width=80,
                  align = "center",
                  format=colFormat(percent = TRUE,digits = 0),
                  style=list(fontWeight="bold")
                ),
                `Nbre de dossiers examines en commission`=colDef(
                  width=270
                ),
                `Nbre davis favorable accordes`=colDef(
                  width=240,
                  headerStyle = list(paddingLeft="40px",
                                     background = "#2E81B0",
                                     color="#ffffff"
                  ),
                  style=list(paddingLeft="40px")
                )
                # 
                # `NBRE DOSSIER DEPOSES`=colDef(
                #   width=290
                # ),
                # `TAUX INSCR AU TNPI`=colDef(
                #   format = colFormat(percent = TRUE,digits = 0),
                #   style = list(fontWeight="bold")
                #   
                # )
              ),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                align = "left",
                width = 200,
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                #maxWidth = 260,
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff"

                                   #,align="left"
                                   
                                   #,maxWidth=260
                )
              )
              
              
    )
    
  })
  
  
  
  mr_promoteur_reactive<-reactive({
    moyens_realisation %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars("NBRE DOSSIER DEPOSES":"NBRE DE PI INSC TNPI"),sum) %>%
      mutate("TAUX INSCR AU TNPI"=`NBRE DE PI INSC TNPI`/`NBRE DE PI AGREES`)
  })
  
  output$gt2_mr<-renderReactable({
        reactable(mr_promoteur_reactive()[1,],
                        sortable = FALSE,
                        #fullWidth = FALSE,
                        bordered = TRUE,
                        #fullWidth = FALSE,
                        style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                        columns = list(
                          `NBRE DOSSIER DEPOSES`=colDef(
                            width=290
                          ),
                          `TAUX INSCR AU TNPI`=colDef(
                            format = colFormat(percent = TRUE,digits = 0),
                            style = list(fontWeight="bold")
                            
                          )
                        ),
                        defaultColDef = colDef(
                          format = colFormat(separators = TRUE,locales = "fr-FR"),
                          align = "center",
                          sortNALast = TRUE,
                          #format = colFormat(digits = 1),
                          #maxWidth = 260,
                          headerStyle = list(background = "#2E81B0",
                                             color="#ffffff"
                                             #,align="left"
                                             
                                             #,maxWidth=260
                          )
                        )
              )
    
  })
  
  
  mr_entreprise_reactive<-reactive({
    moyens_realisation %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(Cat1:Cat9), sum) %>% 
      mutate(Total1=Cat1+Cat2+Cat3+Cat4,.after=4) %>%
      mutate(Total2=Cat5+Cat6+Cat7+Cat8+Cat9) %>% 
      mutate(Total_general=Cat1+Cat2+Cat3+Cat4+Cat5+Cat6+Cat7+Cat8+Cat9)
  })
  

  
  output$gt1_mr<-renderReactable({
           reactable(mr_entreprise_reactive()[1,],
                     sortable = FALSE,
                     #fullWidth = FALSE,
                     bordered = TRUE,
                     fullWidth = FALSE,
                     style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                     columns = list(
                       Total_general=colDef(
                         name="Total general",
                         style = list(fontWeight = "bold",textAlign="center"),
                         headerStyle = list(
                           marginTop="-22px",
                           #border="0px solid #eee",
                           fontWeight = "bold",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )),
                       Total1=colDef(name="Total",style=list(textAlign="center",fontWeight = "bold"),
                                    headerStyle = list(
                                      fontWeight = "bold",
                                      textAlign="center",
                                      background = "#2E81B0",
                                      color="#ffffff"
                                    )),

                       Total2=colDef(name="Total",style=list(textAlign="center",fontWeight = "bold"),
                                    headerStyle = list(
                                      fontWeight = "bold",
                                      textAlign="center",
                                      background = "#2E81B0",
                                      color="#ffffff"
                                    )
                       )

                     ),
                     columnGroups = list(
                       colGroup(name = "Au niveau de Wilaya", columns = c("Cat1","Cat2","Cat3","Cat4","Total1"),
                                headerStyle =list(
                                  borderRight="65px inset transparent",
                                  textAlign="center",
                                  background = "#2E81B0",
                                  color="#ffffff"
                                )
                       ),
                       colGroup(name = "Au niveau de Ministere", columns = c("Cat5","Cat6","Cat7","Cat8","Cat9","Total2"),
                                headerStyle =list(
                                  borderLeft="55px inset transparent",
                                  borderRight="55px inset transparent",

                                  #border="0px solid #eee",
                                  textAlign="center",
                                  background = "#2E81B0",
                                  color="#ffffff"
                                )
                       ),
                       colGroup(name = "", columns = c("Total_general"),
                                headerStyle =list(
                                  #border="0px solid #eee",
                                  textAlign="center",
                                  background = "#2E81B0",
                                  color="#ffffff"
                                )
                       )

                     ),
                     defaultColDef = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR"),

                       sortNALast = TRUE,
                       #format = colFormat(digits = 1),
                       maxWidth = 130,
                       headerStyle = list(background = "#2E81B0",
                                          color="#ffffff",
                                          maxWidth=130
                       )
                     )
           )

  })

  addResourcePath("pdf_files_DGCMR", paste0(getwd(),"/pdf_files_DGCMR"))
  
  # output$tree_file<-renderUI({
  #    `if`(length(input$tree)!=0,
  #    `if`(str_detect(input$tree,".html")==TRUE,
  #         tags$iframe(
  #           seamless="seamless",
  #           src=paste0("pdf_files_DGCMR/",input$tree),
  #           width="1300px",
  #           height="780px"
  #           )
  #       #includeHTML(paste0(getwd(),"/pdf_files_DGCMR/",input$tree))
  #              )
  #    )
  #  })
  
  

  output$tree_file<-renderUI({
    `if`(length(input$tree)!=0,
         `if`(str_detect(input$tree,".html")==TRUE,
              `if`(file.exists(paste0(getwd(),"/pdf_files_DGCMR/niveau3/",input$tree))==TRUE,
                   tags$iframe(
                     seamless="seamless",
                     src=paste0("pdf_files_DGCMR/niveau3/",input$tree),
                     width="1300px",
                     height="780px"
                   ),
                   tags$iframe(
                     seamless="seamless",
                     src=paste0("pdf_files_DGCMR/niveau2/",input$tree),
                     width="1300px",
                     height="780px"
                   )
                   #includeHTML(paste0(getwd(),"/pdf_files_DGCMR/",input$tree))
              )
         )
    )
  })
  
  observe({
    # x <- input$search
    # 
    # # Can use character(0) to remove all choices
    # if (is.null(x))
    #   x <- character(0)
    
    # Can also set the label and select items
    updateSelectInput(inputId ="search",
                      label = paste("Rechercher :"),
                      choices =paste(datamc %>%
                                            filter(wilaya_matricule %in% wilaya_mc_reactive(),
                                                   Filiere %in% filiere_mc_reactive(),
                                                   Statut2 %in% statut_mc_reactive()
                                            ) %>%
                                            select(Identification) %>% .$Identification
                      )
                      
    )
  })

  
  output$nd_mc_maps<-renderText({
    `if`(length(input$search)==0,
                paste(datamc %>%
                        filter(wilaya_matricule %in% wilaya_mc_reactive(),
                               Filiere %in% filiere_mc_reactive(),
                               Statut2 %in% statut_mc_reactive()
                        ) %>%
                        select(longitude,latitude) %>%
                        filter(is.na(longitude)==TRUE | is.na(latitude)==TRUE) %>%
                        nrow(),"ND (sans cordonnees geographiques )"
                      )
                )
  })
  
  output$annee_na_count<-renderText({
    paste(
      nrow(datamc %>%
      filter(wilaya_matricule %in% wilaya_mc_reactive(),
             Filiere %in% filiere_mc_reactive(),
             Statut2 %in% statut_mc_reactive(),
             is.na(Annee_entree)==TRUE
             ))
      ,"ND (sans Annee d'entree)")
  })
  
  output$pivot_table_mc<-renderRpivotTable({
    rpivotTable(
    datamc %>%
      filter(wilaya_matricule %in% wilaya_mc_reactive(),
             Filiere %in% filiere_mc_reactive(),
             Statut2 %in% statut_mc_reactive()
             ) %>%
      mutate(Statut2=replace(Statut2,Statut2=="Mixte (Public et Etrangere)","Public et Etrangere")) %>% 
      mutate(Statut2=replace(Statut2,Statut2=="Mixte (Public et Prive)","Public et Prive")) %>% 
      select(1,4,10,12) %>%
      rename(Wilaya="wilaya_matricule","Statut"="Statut2","Annee dentree"=Annee_entree)
  ,rows="Wilaya",cols="Statut",height="200px"
    )
    })
  
  output$tableau_mc<-renderReactable({

    reactable(
      datamc %>%
        filter(wilaya_matricule %in% wilaya_mc_reactive(),
               Filiere %in% filiere_mc_reactive(),
               Statut2 %in% statut_mc_reactive()
               ) %>%
        select(Identification22,Filiere,wilaya_matricule,Localisation,Statut,Annee_entree,`Capacité nominale installée`,`unité`,`Production effective`,`Produits fabriqués`,TEL,FAX,`Site web`,Email,observations)
      ,
      defaultColDef = colDef(
        align = "left",
        minWidth = 100,
        headerStyle = list(background = "#f7f7f8")
      ),
      bordered = TRUE,
      #striped = TRUE,
      #highlight = TRUE,
      filterable = TRUE,
      defaultPageSize  = 20,
      columns = list(
        Identification22=colDef(name="Identification",width=230,
                              style=list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                                         borderRight = "1px solid #eee"),
                              headerStyle =list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1,
                                                borderRight = "1px solid #eee")
                                
                                ),
        wilaya_matricule=colDef(name="Wilaya",width=100),
        Localisation=colDef(width=160),
        Annee_entree=colDef(name="Annee d'entree en production",width=130),
        `Capacité nominale installée`=colDef(width=160),
        `Production effective`=colDef(width=200),
        `Produits fabriqués`=colDef(width=200),
        `unité`=colDef(name="Unite",width=60),
        Email=colDef(width=220),
        observations=colDef(name="Observations")
        
      )
    )
    
    
  })
  
  output$chiffre_mc<-renderText({
    datamc %>%
      filter(wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
             #,
             #Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
             ) %>%
      summarise(nb=n()) %>% .$nb
  })
  
  mc_annee_debut_reactive<-reactive({
    datamc %>%
      filter(wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
             #,
             #Annee_entree>=min(input$annees_mc),Annee_entree<=max(input$annees_mc)
             ) %>% 
      group_by(Annee_entree) %>% 
      summarise(nb=n()) %>%
      drop_na()
  })
  
  output$mc_annee_debut<-renderHighchart({
    hchart(mc_annee_debut_reactive(),name="Nombre d'unites",type="line",hcaes(x=Annee_entree,y=nb)) %>% 
      hc_title(text = "Nombre d'unite de construction",align="left") %>% 
      hc_subtitle(text = "Par Annee d'entree en production",align="left") %>% 
      hc_xAxis(title=list(text = "Annee d'entree en production")) %>% 
      hc_yAxis(title=list(text=""))
    
  })
  
  
  mc_filiere_reactive=reactive({
    datamc %>% 
      filter(wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
             #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
             ) %>% 
      group_by(Filiere) %>% 
      summarise(Nb=n()) %>%
      arrange(desc(Nb))
  })
  
  output$mc_filiere_reactable=renderReactable({
    reactable(mc_filiere_reactive(),
              pagination = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif",
                           fontSize = "18px"),
              defaultColDef = colDef(
                maxWidth = 180,
                na="ND",
                headerStyle = list(display="none"),
                footerStyle = list(fontWeight = "bold")
                
              ),
              columns = list(
                Filiere=colDef(name="",width = 150,footer ="Total"),
                Nb=colDef(style = list(fontWeight="bold"),footer = sum(mc_filiere_reactive()$Nb))
              )
    )
  })
  
  mc_statut_reactive=reactive({
    `if`(
      nrow(datamc %>% 
        filter(wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
               #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
               ) %>% 
        group_by(Statut2) %>% 
        summarise(Nb=n()))!=6,
         datamc %>% 
           filter(wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
                  #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
                  ) %>% 
           group_by(Statut2) %>% 
           summarise(Nb=n()) %>%
           select(Statut2,Nb)
         ,
         datamc %>% 
           filter(wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
                  #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
                  ) %>% 
           group_by(Statut2) %>% 
           summarise(Nb=n()) %>%
           mutate(ar=c(3,5,4,1,2,6)) %>% 
           arrange(ar) %>% 
           select(Statut2,Nb)
         )
    })
  
  output$mc_statut_reactable<-renderReactable({
    reactable(mc_statut_reactive(),
              pagination = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif",
                           fontSize = "16px"),
              defaultColDef = colDef(
                maxWidth = 180,
                na="ND",
                headerStyle = list(display="none"),
                footerStyle = list(fontWeight = "bold")
              ),
              columns = list(
                Statut2=colDef(name="",width = 210,footer = "Total"),
                Nb=colDef(style = list(fontWeight="bold"),footer = sum(mc_statut_reactive()$Nb))
              )
             )
  })
  
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
  
  output$boxscore1_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  output$boxscore2_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  output$boxscore3_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,17))
  })
 
  output$boxscore4_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  
  output$boxscore5_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,17))
  })
  
  output$boxscore6_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  
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
  
  output$boxscore1_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("")
    )
  })
  
  output$boxscore2_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("")
    )
  })
  
  output$boxscore3_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("")
    )
  })
  
  output$boxscore4_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("")
    )
  })
  
  
  output$boxscore5_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("")
    )
  })
  
  output$boxscore6_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("")
    )
  })
  
  output$boxscore1_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(livraison_wilayas$waw)[selected20_fichewilaya()]),paste("")
    )
  })
  output$boxscore2_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(livraison_wilayas$waw)[selected20_fichewilaya()]),paste("")
    )
  })
  
  output$boxscore3_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(livraison_wilayas$waw)[selected20_fichewilaya()]),paste("")
    )
  })
  output$boxscore5_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(livraison_wilayas$waw)[selected20_fichewilaya()]),paste("")
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
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  #format = colFormat(percent = TRUE, digits = 1)
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                ),
                LSP = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
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
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
                    
                  }
                  
                  
                  
                ),
                
                Total = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
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
                                 
                                 format = colFormat(separators = TRUE,locales = "fr-FR")
                                 
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
                                 format = colFormat(separators = TRUE,locales = "fr-FR")
                                 
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
                       format = colFormat(separators = TRUE,locales = "fr-FR")
                     ),
                     Acheves = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR")
                       
                     ),
                     `En Cours` = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR")
                       
                     ),
                     `Non Lances` = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR")
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
                       format = colFormat(separators = TRUE,locales = "fr-FR")
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
                     format = colFormat(separators = TRUE,locales = "fr-FR"),
                     
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
                     format = colFormat(separators = TRUE,locales = "fr-FR"),
                     
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
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
                       #format = colFormat(percent = TRUE, digits = 1)
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     Acheves = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     `En Cours` = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
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
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                       ,footerStyle = list(
                         
                         background = "#9d94ac",
                         color="#ffffff"
                       )
                     ),
                     `Non Lances` = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       },
                       
                       width=155
                       
                     ),
                     
                     `Notifie 2020` = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
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
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
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
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
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
  
  # output$bilan <- renderUI({
  #   includeHTML(
  #     rmarkdown::render("bilan.Rmd", params = list(data4 = "01-ADRAR"))
  #   )
  # })
  # 
  # 
  
  
  #  abad=reactive({
  #   ifelse(substr(as.character(input$check),12,15)=="TRUE",1,0)
  #  })
  
  
  # var vaz=$('#segments').val();
  # if(vaz.includes('LSP')==true ) {
  #   var dab =document.getElementsByClassName('highcharts-data-label');
  #   dab[2].children[0].style.fontSize='10px';
  # }
  #
  
  
  # document.getElementsByClassName('highcharts-data-label').children[0].style.fontSize="0px"
  # var df=document.getElementsByClassName('highcharts-data-label')
  # df[2].children[0].lastElementChild.textContent="4645"   #(325 458)
  
  # df[2].children[0].style.fontSize="0px"           # LSP 325 458 to 0px
  
  
  # var arr=document.getElementsByClassName('highcharts-data-label-connector')
  # arr[1].style.stroke="#ffffff"             for arrow
  
  
  
  #   threevent_table_de_donnees=reactive({
  #     list(input$annees,input$segments,input$wilayas)
  #   })
  #   
  #   observeEvent(threevent_table_de_donnees(),{
  #   runjs(paste0("
  #         
  # var ann=document.getElementById('annees');
  # var ann0=ann.value.split(';').join(' au ');
  # 
  # var wila=document.querySelector('div#titre_serie.shiny-text-output.shiny-bound-output');
  # var wila0=wila.textContent;
  # 
  # var seg=document.querySelector('div#titre_serie3.shiny-text-output.shiny-bound-output');
  # var seg0=seg.textContent;
  # 
  # 
  # 
  # var abaz=document.getElementById('modal1');
  # var a0a=abaz.children[0].children[0].children[0].children[1];
  # 
  # a0a.innerHTML='<span style=",'"font-size:25px;vertical-align:-37%;">Table de donnees</span> <br> <span style=','"font-size:13px;">Pour : </span> <span style=','"font-size:16px;">',"' + seg0 + ' - </span>';
  #         ")
  #   )
  #      })
  #   
  #   
  
  output$titre_pie=renderText({
    `if`(input$radio_choose_line1_pie=="Livraisons de Logements",
         paste("Livraisons de logements par Segments : "),
         paste("Lancements de logements par Segments : ")
    )
  })
  
  
  hchart12_data=reactive({
    estimation_tolpopparc %>%
      filter(waw %in% wilaya_reactive(),
             Annee>= min(input$annees),Annee <= max(input$annees) ) %>% 
      group_by(Annee) %>% 
      summarise(Population=sum(Population),Parc_logement=sum(Parc_logement)) %>% 
      select(Annee,Population,Parc_logement) %>%
      mutate(TOL=round(Population/Parc_logement,2)) %>% 
      mutate(Population=round(Population),Parc_logement=round(Parc_logement))
  })
  
  
  
  hchart12m_data=reactive({
    estimation_tolpopparc %>%
      filter(waw %in% wilaya_reactive(),
             Annee <= max(input$annees) ) %>% 
      group_by(Annee) %>% 
      summarise(Population=sum(Population),Parc_logement=sum(Parc_logement)) %>% 
      select(Annee,Population,Parc_logement) %>%
      mutate(TOL=round(Population/Parc_logement,2)) %>% 
      mutate(Population=round(Population),Parc_logement=round(Parc_logement))
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
  
  
  ssw1<-reactive({
    `if`(length(input$selectwilayas) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$selectwilayas)>4,paste0("Les ",length(input$selectwilayas)," ","Wilayas selectionnees"),paste(input$selectwilayas,collapse = "+")))
  })
  
  ssw1_lanc<-reactive({
    `if`(length(input$selectwilayas_lanc) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$selectwilayas_lanc)>4,paste0("Les ",length(input$selectwilayas_lanc)," ","Wilayas selectionnees"),paste(input$selectwilayas_lanc,collapse = "+")))
  })
  
  
  output$rowdonnees33html<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Donnees Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive2())==5,paste('Tout les Segments'),paste(segments_reactive2(),collapse = '+')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees)!=max(input$selectannees),paste(min(input$selectannees),' au ',max(input$selectannees)),paste('Annee :',max(input$selectannees)) ),'</span>'
    ))    
  )
  
  
  output$rowdonnees33html_lanc<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Donnees Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw1_lanc(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive2_lanc())==5,paste('Tout les Segments'),paste(segments_reactive2_lanc(),collapse = '+')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees_lanc)!=max(input$selectannees_lanc),paste(min(input$selectannees_lanc),' au ',max(input$selectannees_lanc)),paste('Annee :',max(input$selectannees_lanc)) ),'</span>'
    ))    
  )
  
  
  ssw2<-reactive({
    `if`(length(input$selectwilayas_wilaya) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$selectwilayas_wilaya)>4,paste0("Les ",length(input$selectwilayas_wilaya)," ","Wilayas selectionnees"),paste(input$selectwilayas_wilaya,collapse = "+")))
  })
  
  
  output$rowdonnees33html2<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Donnees Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw2(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees_wilaya)!=max(input$selectannees_wilaya),paste(min(input$selectannees_wilaya),' au ',max(input$selectannees_wilaya)),paste('Annee :',max(input$selectannees_wilaya)) ),'</span>'
    ))    
  )
  
  
  
  ttw1<-reactive({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$wilayas)>4,paste0("Les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = "+")))
  })
  
  
  ttw1_lanc<-reactive({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$wilayas)>4,paste0("Les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = "+")))
  })
  
  
  ttw1_tol<-reactive({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Tout les Wilayas"),`if`(length(input$wilayas)>4,paste0("Les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = "+")))
  })
  
  
  
  
  output$tablededonnes1_sitphy<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de donnees</span> <br> <span style="font-size:13px;">Situation Arretee au :&nbsp;&nbsp; </span> <span style="font-size:14px;">',substr(input$select_arretee_sitphy,14,23),'</span>'
    ))
    
  )
  
  
  
  output$tablededonnes1_eqp<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de donnees</span> <br> <span style="font-size:13px;">Situation Arretee au :&nbsp;&nbsp; </span> <span style="font-size:14px;">',substr(input$select_arretee_eqp,14,23),'</span>'
    ))
    
  )
  
  output$tablededonnes1<-renderUI(
    
    `if`(input$radio_choose_line1=="Livraisons de Logements",
         HTML(paste(
           '<span style="font-size:25px;vertical-align:-25%;">Table de donnees - Livraisons -</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive())==5,paste('Tout les Segments'),paste(segments_reactive(),collapse = ',')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'</span>'
         )),
         `if`(input$radio_choose_line1=="TOL",
              HTML(paste(
                '<span style="font-size:25px;vertical-align:-25%;">Table de donnees  - TOL -</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1_tol(),'&nbsp;&nbsp;&nbsp;</span>'
              ))     
              ,    
              HTML(paste(
                '<span style="font-size:25px;vertical-align:-25%;">Table de donnees  - Lancements -</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1_lanc(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive())==5,paste('Tout les Segments'),paste(segments_reactive(),collapse = ',')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'</span>'
              ))
         )
    )
  )
  
  
  output$tablededonnes2<-renderUI(
    `if`(input$radio_choose_line1_pie=="Livraisons de Logements",
         HTML(paste(
           '<span style="font-size:25px;vertical-align:-25%;">Table de donnees - Livraisons</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'</span>'
         ))
         ,
         HTML(paste(
           '<span style="font-size:25px;vertical-align:-25%;">Table de donnees - Lancements</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1_lanc(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'</span>'
         ))
         
    )
  )
  
  
  output$tablededonnes3<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de donnees</span> <br> <span style="font-size:12px;">Livraisons et Lancements pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',`if`(length(segments_reactive())==5,paste('Tout les Segments'),paste(segments_reactive(),collapse = '+')),'- &nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Annee :',max(input$annees)) ),'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp; (Population,   Parc logements,   TOL) : Annee',max(input$annees),'</span>'
    ))
  )
  
  observe({
    runjs("
          ab=document.getElementById('search');
          ab.parentElement.parentElement.style.position='absolute';
          ab.parentElement.parentElement.style.zIndex='99999';
          ab.parentElement.parentElement.style.right='648px';
          ab.parentElement.parentElement.style.top='80px';
          ab.parentElement.parentElement.style.width='200px';
          ")
  })
  
  observeEvent(input$annees, {
    # Run JS code that simply shows a message
    runjs("
    var pa = document.getElementById('annees');
    var ipa = document.getElementsByClassName('state p-primary');
    var cheka=document.getElementById('radio_choose_line1');

    abv=pa.value.split(';') ;
    if(abv[0]==abv[1] || cheka.checked==false){
    ipa[0].style.display='none'
    } else {
    ipa[0].style.display=''
    }
          ")
  })
  
  
  observeEvent(input$radio_choose_line1, {
    runjs("
        var chek=document.getElementById('radio_choose_line1')
        var ipa = document.getElementsByClassName('state p-primary');
  
        if(chek.checked==false){
          ipa[0].style.display='none'
              } else {
          ipa[0].style.display=''
                      }
                        ")
    
  })
  
  
  output$titre_hchart1<-renderText({
    `if`(input$radio_choose_line1=="TOL",
         paste0('TOL Par Annees'),
         `if`(input$radio_choose_line1=="Livraisons de Logements",
              `if`(min(input$annees)!=max(input$annees),
                   paste("Livraisons de logements par Annees : "),
                   paste("Livraisons de logements de l'annee ",max(input$annees)," :")
              ),
              `if`(min(input$annees)!=max(input$annees),
                   paste("Lancements de logements par Annees : "),
                   paste("Lancements de logements de l'annee ",max(input$annees)," :")
              )
              
         ))
  })
  
  
  output$titre_map<-renderText({
    `if`(input$radio_choose_leaflet=="Livraisons de Logements",
         paste0("Livraisons de logements par Wilaya : "),paste0(input$radio_choose_leaflet," Par Wilaya :")
    )
  })
  
  onevent("mouseenter", "choose_leaflet", show("display_when_hover_choose_leaflet"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet", show("display_when_hover_choose_leaflet"))
  onevent("mouseleave", "display_when_hover_choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  ################################### mc
  
  onevent("mouseenter", "choose_leaflet_mc", show("display_when_hover_choose_leaflet_mc"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet_mc", show("display_when_hover_choose_leaflet_mc"))
  onevent("mouseleave", "display_when_hover_choose_leaflet_mc", hide("display_when_hover_choose_leaflet_mc"))
  
  
  
  
  onevent("mouseenter", "choose_line1", show("display_when_hover_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_line1", show("display_when_hover_choose_line1"))
  onevent("mouseleave", "display_when_hover_choose_line1", hide("display_when_hover_choose_line1"))
  
  
  onevent("mouseenter", "choose_line1_pie", show("display_when_hover_choose_line1_pie"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_line1_pie", show("display_when_hover_choose_line1_pie"))
  onevent("mouseleave", "display_when_hover_choose_line1_pie", hide("display_when_hover_choose_line1_pie"))
  
  
  
  
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
                                 
                                 style = list(background="#81a47b",color="#ffffff",fontFamily="system-ui",fontWeight="500")),
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
                format = colFormat(separators = TRUE,locales = "fr-FR"),
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
                rowSelectedStyle = list(backgroundColor = "#81a47b", boxShadow = "inset 2px 0 0 0 #ffa62d")
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
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
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
  
  
  
  
  
  
  output$excel_sitphy<-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,
               
               sitphy2[,-1] %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23)),
               
               #columns = data.frame(title=rep("",ncol(sitphy2))),
               #mergeCells = list(A1=c(1,2),B1=c(9,1),K1=c(9,1),T1=c(9,1),AC1=c(9,1),AL1=c(9,1)   ),
               columnSorting=FALSE,search=TRUE
    )
  })
  
  select_villes_reactive<-reactive({
    `if`(input$select_villes=="Villes",data.frame(setview_lat=33.994278,setview_long=2.905987,zoom=6),data_ville[data_ville$Nom==input$select_villes,c(8,7,4,6,5)])
  })
  
   output$leaflet_mc<-renderLeaflet({
     leaflet() %>%
       addProviderTiles(providers[[6]]) %>%
       setView(lng=1.3333,lat=30.6167,zoom=6) %>% 
       addAwesomeMarkers(lat=datamc$latitude,lng=datamc$longitude
                         ,label =
                           sprintf('<strong style="font-size:19px;">%s</strong><br/>
<table style="font-size:17px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;">Wilaya</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Localisation</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Statut</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Annee dentree en production</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Capacite nominale installee<br></td>
            <td style="width: 0.263361;">%s %s</td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Production effective<br></td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Produits fabriques<br></td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Tel</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Fax</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Site web</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Email</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Observation</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
    </tbody>
</table>
                          ',
                                   datamc$Identification,
                                   datamc$wilaya_matricule,
                                   datamc$Localisation,
                                   datamc$Statut,
                                   datamc$Annee_entree,
                                   datamc$`Capacité nominale installée`,
                                   datamc$unité,
                                   datamc$`Production effective`,
                                   datamc$`Produits fabriqués`,
                                   datamc$TEL,
                                   datamc$FAX,
                                   datamc$`Site web`,
                                   datamc$Email,
                                   datamc$observations
                                   
                                   
                                   
                                   
                                   
                                   
                           ) %>% lapply(htmltools::HTML)
                         )
   })
   
  output$leaflet_ville<-renderLeaflet({
    leaflet() %>% 
      addProviderTiles(`if`(input$select_villes=="Villes",providers[[6]],providers[[1]]),group = `if`(input$select_villes=="Villes",providers[[6]],providers[[1]])) %>%
      setView(lat=select_villes_reactive()$setview_lat,lng=select_villes_reactive()$setview_long,zoom=select_villes_reactive()$zoom) %>%    ## in default page we set maxzoom=14  in providers[[44]]  
      addProviderTiles(`if`(input$select_villes=="Villes",providers[[1]],providers[[6]]),group=`if`(input$select_villes=="Villes",providers[[1]],providers[[6]])) %>% 
      addProviderTiles(providers[[57]],group="Satellite") %>% 
      addLayersControl(
        baseGroups = c(`if`(input$select_villes=="Villes",providers[[6]],providers[[1]]),`if`(input$select_villes=="Villes",providers[[1]],providers[[6]]),"Satellite")
      ) %>% 
      addMeasure(
        position = "bottomright",
        primaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479",
        localization="fr",
        thousandsSep=" "
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[1],lng=data_ville$cor_long[1],label=data_ville$Nom[1],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "right",offset = c(8,-33),opacity = 0.95)
      ) %>% 
      
      addAwesomeMarkers(lat=data_ville$cor_lat[2],lng=data_ville$cor_long[2],label=data_ville$Nom[2],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "left",offset = c(-10,-40),opacity = 0.95)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[3],lng=data_ville$cor_long[3],label=data_ville$Nom[3],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "bottom",offset = c(0,0),opacity = 0.95)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[4],lng=data_ville$cor_long[4],label=data_ville$Nom[4],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "top",offset = c(0,-35),opacity = 0.95)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[5],lng=data_ville$cor_long[5],label=data_ville$Nom[5],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "bottom",opacity=0.95) #,offset = c(-10,-40),opacity = 0.8)
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[6],lng=data_ville$cor_long[6],label=data_ville$Nom[6],
                        labelOptions = labelOptions(noHide = TRUE,textsize="14px",
                                                    direction = "top",offset = c(0,-35),opacity = 0.95)
      )
  })
  
  output$val_livraison_gauge1<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements` %in% select_segment()) %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge1<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements` %in% select_segment()) %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  output$val_livraison_gauge2<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LPL") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge2<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LPL") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge3<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LSP") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge3<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LSP") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge4<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="Rural") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge4<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="Rural") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge5<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="Location-Vente") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge5<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="Location-Vente") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  output$val_livraison_gauge6<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LPP") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge6<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LPP") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  #####
  
  
  observe({
    `if`(length(input$select_segment_gauge) %in% c(0,2,3,4,5),
         js$opac1('1'),
         js$opac1('0.13')
    )
    
    `if`(length(which(input$select_segment_gauge=="LPL"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac2('1'),
         js$opac2('0.13')
    )
    
    
    `if`(length(which(input$select_segment_gauge=="LSP"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac3('1'),
         js$opac3('0.13')
    )
    
    `if`(length(which(input$select_segment_gauge=="Rural"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac4('1'),
         js$opac4('0.13')
    )
    
    `if`(length(which(input$select_segment_gauge=="Location-Vente"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac5('1'),
         js$opac5('0.13')
    )
    
    
    `if`(length(which(input$select_segment_gauge=="LPP"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac6('1'),
         js$opac6('0.13')
    )
    
    
    `if`(as.numeric(selected20_fichewilaya())==16,
          js$opac77('none'),
          js$opac77('block')
     )
    
    
    
  })
  
  output$toutsegments_gauge<-renderText({
    `if`(length(input$select_segment_gauge) %in% c(0,1,5),
         print("Tout les Segments"),
         paste(input$select_segment_gauge,collapse="+")
    )
  })
  
  
  output$wilayaselectgauge1<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  output$wilayaselectgauge2<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge3<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge4<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge5<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  output$wilayaselectgauge6<-renderText({
    `if`(length(selected20())==48,"",sitphy$Wilaya_matricule[selected20()])
  })
  
  output$gauge6=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LPP") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  
  output$gauge5=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="Location-Vente") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  output$gauge4=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="Rural") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  
  output$gauge3=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LSP") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  
  output$gauge2=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        data=as.numeric(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LPL") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
        
      )
  })
  
  
  output$gauge1=renderHighchart({
    highchart() %>%
      hc_chart(type = "solidgauge",height='88%') %>%
      hc_pane(
        startAngle = -90,
        endAngle = 90,
        background = list(
          outerRadius = '100%',
          innerRadius = '60%',
          shape = "arc"
        )
      ) %>%
      hc_tooltip(enabled = FALSE) %>%
      hc_yAxis(
        stops = list_parse2(col_stops),
        lineWidth = 0,
        minorTickWidth = 0,
        tickAmount = 2,
        min = 0,
        max = 100,
        labels = list(y = 26, style = list(fontSize = "22px"))
      ) %>%
      hc_add_series(
        #data = round(100*sum(sitphy$Livraison[sitphy$id==selected20() & sitphy$`Type de logements` %in% select_segment()])/sum(sitphy$Prevision[sitphy$id==selected20()])),
        data=as.numeric(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements` %in% select_segment()) %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
        dataLabels = list(
          y = -50,
          borderWidth = 0,
          useHTML = TRUE,
          format="<span style='font-size:40px;font-family:inherit;'>{y}</span><span style='font-size:36px;font-family:inherit;opacity:1'>%</span>",
          style = list(fontSize = "40px"  )
        )
      )
  })
  
  select_segment=reactive({
    `if`(length(input$select_segment_gauge)==0,c("Rural","LPL","Location-Vente","LSP","LPP"),input$select_segment_gauge)
  })
  
  
  select_segment_title=reactive({
    `if`(length(input$select_segment_gauge) %in% c(0,5),"",input$select_segment_gauge)
  })
  
  selected2 <- reactive(getReactableState("table2", "selected"))
  
  selected2_fichewilaya <- reactive(getReactableState("table2_fichewilaya", "selected"))
  
  selected2_fichewilaya_mr <- reactive(getReactableState("table2_fichewilaya_mr", "selected"))
  
  
  selected20 <- reactive(
    `if`(length(getReactableState("table2", "selected"))==0,1:48,getReactableState("table2", "selected"))
  )
  
  selected20_fichewilaya <- reactive(
    `if`(length(getReactableState("table2_fichewilaya", "selected"))==0,1:48,getReactableState("table2_fichewilaya", "selected"))
  )
  
  selected20_fichewilaya_mr <- reactive(
    `if`(length(getReactableState("table2_fichewilaya_mr", "selected"))==0,1:48,getReactableState("table2_fichewilaya_mr", "selected"))
  )
  
  output$wgauge=renderText({
    print(selected20())
  })
  
  sitphy00=reactive({
    sitphy %>% 
      filter(Arretee==substr(input$select_arretee_sitphy,14,23),`Type de logements` %in% select_segment()) %>% 
      group_by(Wilaya_matricule) %>% 
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
      select(Wilaya,Consistance,`Achevés`,`En Cours`,`Non Lancés`) %>% 
      replace_na(list(`Achevés`=0,`En Cours`=0,`Non Lancés`=0))
    
  })
  
  
  
  output$table2_fichewilaya_mr <- renderReactable({
    reactable(data.frame(Wilaya=unique(moyens_realisation$Wilaya)),defaultPageSize = 48,striped = TRUE,
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
  
  
  
  
  
  
  
  
  output$table2 <- renderReactable({
    reactable(sitphy00(),defaultPageSize = 48,striped = TRUE,
              selection = "single",
              borderless = TRUE,
              height="800px",
              width="66%",
              columns = list(
                Wilaya = colDef(width = 148,align="left",footer="Total"),   # 50% width, 200px minimum
                Consistance=colDef(format=colFormat(digits = 0,separators = TRUE,locales = "fr-FR"),width = 104,align="center",footer=format(sum(sitphy00()$Consistance),trim=TRUE,digits = 3,big.mark = " ")),
                `Achevés`=colDef(format=colFormat(percent = TRUE,digits = 0),width = 80,align="center",footer=sprintf("%3.0f %%",100*sum(sitphy00()$Consistance*sitphy00()$Achevés)/sum(sitphy00()$Consistance)),
                                 style = function(value) {
                                   color<-green_pal(value)
                                   list(background=color)
                                 }
                ),
                `En Cours`=colDef(format=colFormat(percent = TRUE,digits = 0),width = 85,align="center",footer=sprintf("%3.0f %%",100*sum(sitphy00()$Consistance*sitphy00()$`En Cours`)/sum(sitphy00()$Consistance)),
                                  style = function(value) {
                                    color<-blue_pal(value)
                                    list(background=color)
                                  }),
                `Non Lancés`=colDef(format=colFormat(percent = TRUE,digits = 0),width = 99,align="center",footer=sprintf("%3.0f %%",100*sum(sitphy00()$Consistance*sitphy00()$`Non Lancés`)/sum(sitphy00()$Consistance)),
                                    style = function(value) {
                                      color<-red_pal(value)
                                      list(background=color)
                                    }
                )
              ),
              columnGroups = list(
                colGroup(name = paste("Situation Physiques des logements"," ",paste(select_segment_title(),collapse = "+")), columns = colnames(sitphy00()))
              ),
              defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
              onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  output$selected2 <- renderPrint({
    print(selected2())
  })
  
  observe({
    print(sitphy0[selected2(), ])
  })
  
  #################################
  output$wilayaselect1<-renderText({
    `if`(length(selected())==48,
         print(""),
         print(zones$Wilaya[selected()]))
  })
  
  
  output$wilayaselect2<-renderText({
    `if`(length(selected())==48,
         print(""),
         print(zones$Wilaya[selected()]))
  })
  
  
  output$ttwilayas=renderText({
    `if`(length(selected())==48,
         print("Toutes les Wilayas"),
         print(zones$Wilaya[selected()])
    )
  })
  
  
  output$ttwilayas2=renderText({
    `if`(length(selected())==48,
         print("Toutes les Wilayas"),
         print(zones$Wilaya[selected()])
    )
  })
  
  output$region=renderText({
    `if`(length(selected())==48,
         print("Région : HAUT PLATEU - SUD"),
         paste0("Région : ",zones$Zone[selected()])
    )
  })
  output$zones1=renderText({
    `if`(length(selected())==48,
         print("Domanial"),
         `if`(is.na((zones %>% filter(id_wilaya==selected()) %>% 
                       select(`Nature juridique`))$`Nature juridique`
         )==TRUE,print(c()),
         (zones %>% filter(id_wilaya==selected()) %>% 
            select(`Nature juridique`))$`Nature juridique`)
    )
  })
  
  
  output$zones2=renderText({
    `if`(
      (zones %>% filter(id_wilaya==selected()) %>% 
         summarise(sumnb=sum(`Nombre de communes concernées`,na.rm = TRUE)))$sumnb==0,print(""),
      
      format((zones %>% filter(id_wilaya==selected()) %>% 
                summarise(sumnb=sum(`Nombre de communes concernées`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
    )
  })
  
  
  output$zones3=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Superficie (ha)`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Superficie (ha)`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
    )
  })
  
  output$zones4=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  output$zones5=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Nombre de lots retenues`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  output$zones6=renderText({
    `if`(
      (zones %>% filter(id_wilaya==selected()) %>% 
         summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
      format((zones %>% filter(id_wilaya==selected()) %>% 
                summarise(sumnb=sum(`Nombre de lots dégagés (Porte feuille)`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
      
    )
  })
  
  output$zones7=renderText({
    
    (zones %>% filter(id_wilaya==selected()) %>%
       select(`Surface moyenne des lots (m²)`))$`Surface moyenne des lots (m²)`
    
  })
  
  output$zones8=renderText({
    `if`(
      (zones %>% filter(id_wilaya==selected()) %>% 
         summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
      format((zones %>% filter(id_wilaya==selected()) %>% 
                summarise(sumnb=sum(`Nombre de sites`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
      
    )
  })
  
  output$zones9=renderText({
    `if`((zones %>% filter(id_wilaya==selected()) %>% 
            summarise(sumnb=sum(`Nombre de lots`,na.rm = TRUE)))$sumnb==0,print(""),
         format((zones %>% filter(id_wilaya==selected()) %>% 
                   summarise(sumnb=sum(`Nombre des permis`,na.rm = TRUE)))$sumnb,big.mark =" ",trim=TRUE,digits=3)
         
    )
  })
  
  
  
  display_zones7=renderText({
    `if`(length(selected())==48,
         paste0("display:none;"),paste0("display:block;")
    )
  })
  
  
  output$loi1=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Déposés,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi2=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Traités,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi3=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Favorables,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi4=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Défavorables,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi5=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Instances,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi6=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(`Déposés Instruction N°01`,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$loi7=renderText({
    format((zones %>% filter(id_wilaya %in% selected()) %>% 
              summarise(sumnb=sum(Traités_a,na.rm=TRUE)))$sumnb,big.mark=" ",trim=TRUE,digits=3)
  })
  
  
  output$urbanisme2<-renderHighchart({
    sit_fin %>% 
      filter(id %in% selected()) %>% 
      group_by(Type) %>% 
      summarise(Notification=sum(NOTIFICATION),Reliquat=sum(Reliquat)) %>% 
      gather("etat","nb",2:3) %>% arrange(etat) %>% 
      hchart('column',hcaes(x=Type,y=nb,group=etat),stacking="normal") %>% 
      hc_tooltip(
        crosshairs=TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        formatter=JS(paste0("function() {
        var s = '<p>'+this.points[0].key+'</p>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
             
             
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ Highcharts.numberFormat(point.y,0) +' DA'+'<b/>';
            sum += point.y;
            });

        s += '<br/> <b> Inscription : '+ Highcharts.numberFormat(sum,0) + ' DA </b>' 

        return s;
    }")),
        style=list(
          fontSize="15px"),
        title=list(style=list(fontSize="16px"))
      ) %>% hc_xAxis(
        title=list(text = ""),
        labels=list(style=list(fontSize= "15px",fontWeight="normal"))
        
      ) %>%
      hc_yAxis(
        labels=list(style=list(fontSize= "15px",fontWeight="normal"),
                    formatter=JS('function() {
			if ( this.value > 1000 ) return Highcharts.numberFormat(this.value/1000000000,0)+" Milliard DA";
			return Highcharts.numberFormat(this.value, 0, "."," ");
		}')),
        title=list(text = ""),
        reversedStacks=FALSE
        
      ) %>% hc_legend(
        
        # 
        align= 'left',
        # layout="vertical",
        # verticalAlign= 'middle',
        # itemMarginTop= 6,
        # itemMarginBottom=6,
        margin=-60,
        itemStyle=list(fontSize="15px",fontWeight= 300)
      )
    
  })
  
  tpos_reactive=reactive({
    pos %>%
      filter(id_wilaya %in% selected()) %>% 
      group_by(URBANISME) %>%
      summarise(lances=sum(Lancés),"Non Lancées"=sum(`Non Lancées`),Achevées=sum(Achevées),"En Cours"=sum(`En Cours`),approuvees=sum(Approuvées)) %>% 
      gather("etat","nb",2:5) %>% filter(etat %in% c("Non Lancées","Achevées","En Cours"))
  })
  
  
  output$urbanisme1<-renderHighchart({
    tpos_reactive() %>% 
      hchart(
        'bar', hcaes(x = URBANISME, y = nb, group = etat),
        borderColor="#404040"
      )%>%
      hc_colors(c("#77c663", "#EFC000FF","#e71919")) %>% 
      hc_tooltip(
        crosshairs = TRUE,
        backgroundColor = "#F0F0F0",
        borderColor="#212121",
        shared = TRUE, 
        borderWidth = 3,
        sort=TRUE,
        formatter=JS(paste0("function() {
        var s = '<p>'+this.points[0].key+'</p>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
             
             
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ point.y +'<b/>';
            sum += point.y;
            });

        s += '<br/> <b> Total </b>: '+ sum

        return s;
    }")),
        style=list(
          fontSize="18px")
      ) %>% 
      hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', '')
		}")),title=list(text = "")
      )%>%
      hc_xAxis(
        title=list(text = ""),
        labels=list(style=list(fontSize= "16px",fontWeight="normal"))
        
      ) %>% 
      hc_plotOptions(
        series = list(
          showInLegend = TRUE,
          pointFormat = "{point.nb}",
          dataLabels=list(style=list(fontSize= "14px",fontWeight="normal"),format="{point.nb}",enabled=TRUE)
        )
      ) %>% hc_legend(
        itemStyle=list(fontSize="15px",fontWeight= 300)
      )
    
  })
  
  selected <- reactive(
    `if`(length(getReactableState("table", "selected"))==0,1:48,getReactableState("table", "selected"))
  )
  
  output$table <- renderReactable({
    reactable(data.frame(Wilaya=unique(livraison_wilayas$waw)),defaultPageSize = 48,striped = TRUE,
              selection = "single",
              borderless = TRUE,
              sortable = FALSE,
              onClick = "select",
              theme = reactableTheme(
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d")
              )
    )
  })
  
  output$selected <- renderPrint({
    print(selected())
  })
  
  observe({
    print(data.frame(unique(livraison_wilayas$waw))[selected(), ])
  })
  
  output$dt_wilaya=renderDataTable({
    unique(livraison_wilayas$waw)
  })
  
  segments_reactive2=reactive({
    if(length(input$selectsegments)==0) {
      unique(livraison_wilayas$type_de_logement)
    } else{
      input$selectsegments
    }
  })
  
  segments_reactive2_lanc=reactive({
    if(length(input$selectsegments_lanc)==0) {
      unique(livraison_wilayas$type_de_logement)
    } else{
      input$selectsegments_lanc
    }
  })
  
  
  
  wilaya_reactive2_lanc=reactive({
    if(length(input$selectwilayas_lanc)==0) {
      unique(livraison_wilayas$waw)
    } else{
      input$selectwilayas_lanc
    }  
  })
  
  
  
  
  wilaya_reactive2=reactive({
    if(length(input$selectwilayas)==0) {
      unique(livraison_wilayas$waw)
    } else{
      input$selectwilayas
    }  
  })
  
  livraison_wilayas0_donnees=reactive({
    livraison_wilayas%>%
      select(waw,type_de_logement,Livraison,annee) %>% 
      filter(waw %in% wilaya_reactive2(),type_de_logement %in% segments_reactive2(),annee>= min(input$selectannees),annee <= max(input$selectannees)) %>% 
      rename(Wilaya=waw,Segment=type_de_logement,Annee=annee)
    
  })
  
  
  
  lancement_wilayas0_donnees=reactive({
    lancement_wilayas%>%
      select(Wilaya,Segment,Lancement,Annee) %>% 
      filter(Wilaya %in% wilaya_reactive2_lanc(),Segment %in% segments_reactive2_lanc(),Annee>= min(input$selectannees_lanc),Annee <= max(input$selectannees_lanc))
  })
  
  
  output$donnees_excel<-renderExcel({
    `if`(input$parwilayas==FALSE & input$parsegments==FALSE & input$parperiode==TRUE,
         excelTable(editable = FALSE,
                    rbind(c("Année","Livraisons"),livraison_wilayas0_donnees() %>% 
                            group_by(Annee) %>% 
                            summarise(Livraisons=sum(Livraison)) %>%
                            rbind(c("Total",sum(round(livraison_wilayas0_donnees() %>% summarise(t=sum(Livraison)) %>% select(t))$t) )))
                    ,showToolbar = TRUE,autoFill = TRUE,
                    columns = data.frame(title=c("",""))
                    
         ),
         `if`(input$parwilayas==FALSE & input$parsegments==FALSE & input$parperiode==FALSE,
              (
                excelTable(editable = FALSE,
                           livraison_wilayas0_donnees() %>% 
                             summarise(Livraisons=sum(Livraison))
                           ,showToolbar = TRUE,columns = data.frame(title=c("Livraisons"))
                ) 
              ),
              
              `if`(input$parwilayas==FALSE & input$parsegments==TRUE & input$parperiode==TRUE,
                   excelTable(editable = FALSE,
                              rbind(colnames(livraison_wilayas0_donnees() %>%
                                               group_by(Annee,Segment) %>% 
                                               summarise(Livraisons=sum(Livraison)) %>% 
                                               spread(key=Segment,value=Livraisons) %>% 
                                               mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                              ),
                              livraison_wilayas0_donnees() %>%
                                group_by(Annee,Segment) %>% 
                                summarise(Livraisons=sum(Livraison)) %>% 
                                spread(key=Segment,value=Livraisons) %>% 
                                mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                              ),showToolbar = TRUE,
                              columns = data.frame(title=rep("",length(livraison_wilayas0_donnees() %>%
                                                                         group_by(Annee,Segment) %>% 
                                                                         summarise(Livraisons=sum(Livraison)) %>% 
                                                                         spread(key=Segment,value=Livraisons) %>% 
                                                                         mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                              )))
                   ),
                   `if`(input$parwilayas==FALSE & input$parsegments==TRUE & input$parperiode==FALSE,
                        excelTable(editable = FALSE, 
                                   rbind(c("Segment","Livraisons"),
                                         livraison_wilayas0_donnees() %>%
                                           group_by(Segment) %>% 
                                           summarise(Livraisons=sum(Livraison)) %>% 
                                           rbind(c("Total",round(livraison_wilayas0_donnees() %>% summarise(ta=sum(Livraison)) %>% select(ta))$ta))
                                   ),showToolbar = TRUE,columns = data.frame(title=c("",""))
                        ),
                        `if`(input$parwilayas==TRUE & input$parsegments==FALSE & input$parperiode==TRUE,
                             excelTable(editable = FALSE,
                                        rbind(colnames(livraison_wilayas0_donnees() %>%
                                                         group_by(Annee,Wilaya) %>% 
                                                         summarise(Livraisons=sum(Livraison)) %>% 
                                                         spread(key = Annee,value=Livraisons) %>% 
                                                         rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                                         rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                        ),
                                        livraison_wilayas0_donnees() %>%
                                          group_by(Annee,Wilaya) %>% 
                                          summarise(Livraisons=sum(Livraison)) %>% 
                                          spread(key = Annee,value=Livraisons) %>% 
                                          rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                          rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                        )
                                        ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(  livraison_wilayas0_donnees() %>%
                                                                                                       group_by(Annee,Wilaya) %>% 
                                                                                                       summarise(Livraisons=sum(Livraison)) %>% 
                                                                                                       spread(key = Annee,value=Livraisons) %>% 
                                                                                                       rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                                                                                       rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                        )))
                             ),
                             `if`(input$parwilayas==TRUE & input$parsegments==FALSE & input$parperiode==FALSE,
                                  excelTable(editable = FALSE,
                                             rbind(c("Wilaya","Livraisons"),
                                                   livraison_wilayas0_donnees() %>%
                                                     group_by(Wilaya) %>% 
                                                     summarise(Livraisons=sum(Livraison)) %>% 
                                                     rbind(c("Total",sum(round(livraison_wilayas0_donnees() %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                             )
                                             ,showToolbar = TRUE,columns = data.frame(title=c("",""))
                                  ),
                                  `if`(input$parwilayas==TRUE & input$parsegments==TRUE & input$parperiode==TRUE,
                                       excelTable(editable = FALSE,
                                                  rbind(colnames(livraison_wilayas0_donnees() %>%
                                                                   group_by(Annee,Wilaya,Segment,)%>% 
                                                                   summarise(Livraisons=sum(Livraison)) %>% 
                                                                   spread(Segment,value = Livraisons) %>% 
                                                                   mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  ),
                                                  livraison_wilayas0_donnees() %>%
                                                    group_by(Annee,Wilaya,Segment,)%>% 
                                                    summarise(Livraisons=sum(Livraison)) %>% 
                                                    spread(Segment,value = Livraisons) %>% 
                                                    mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )
                                                  ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(livraison_wilayas0_donnees() %>%
                                                                                                               group_by(Annee,Wilaya,Segment,)%>% 
                                                                                                               summarise(Livraisons=sum(Livraison)) %>% 
                                                                                                               spread(Segment,value = Livraisons) %>% 
                                                                                                               mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )))
                                       ),
                                       excelTable(editable = FALSE,
                                                  rbind(colnames(livraison_wilayas0_donnees() %>%
                                                                   group_by(Wilaya,Segment,)%>% 
                                                                   summarise(Livraisons=sum(Livraison)) %>% 
                                                                   spread(Segment,value = Livraisons) %>% 
                                                                   rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  ),
                                                  livraison_wilayas0_donnees() %>%
                                                    group_by(Wilaya,Segment,)%>% 
                                                    summarise(Livraisons=sum(Livraison)) %>% 
                                                    spread(Segment,value = Livraisons) %>% 
                                                    rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )
                                                  ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(livraison_wilayas0_donnees() %>%
                                                                                                               group_by(Wilaya,Segment,)%>% 
                                                                                                               summarise(Livraisons=sum(Livraison)) %>% 
                                                                                                               spread(Segment,value = Livraisons) %>% 
                                                                                                               rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t,sum(round(livraison_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Livraison)) %>% select(t))$t) ))
                                                  )))
                                       )
                                  )
                             )
                             
                             
                        )
                   )
              )
         )
    )
    
  })
  
  ######################## donnees_excel_lanc 
  ############################################
  ##############
  
  output$donnees_excel_lanc<-renderExcel({
    `if`(input$parwilayas_lanc==FALSE & input$parsegments_lanc==FALSE & input$parperiode_lanc==TRUE,
         excelTable(editable = FALSE,
                    rbind(c("Année","Lancements"),lancement_wilayas0_donnees() %>% 
                            group_by(Annee) %>% 
                            summarise(Lancements=sum(Lancement)) %>%
                            rbind(c("Total",sum(round(lancement_wilayas0_donnees() %>% summarise(t=sum(Lancement)) %>% select(t))$t) )))
                    ,showToolbar = TRUE,autoFill = TRUE,
                    columns = data.frame(title=c("",""))
                    
         ),
         `if`(input$parwilayas_lanc==FALSE & input$parsegments_lanc==FALSE & input$parperiode_lanc==FALSE,
              (
                excelTable(editable = FALSE,
                           lancement_wilayas0_donnees() %>% 
                             summarise(Lancements=sum(Lancement))
                           ,showToolbar = TRUE,columns = data.frame(title=c("Lancements"))
                ) 
              ),
              
              `if`(input$parwilayas_lanc==FALSE & input$parsegments_lanc==TRUE & input$parperiode_lanc==TRUE,
                   excelTable(editable = FALSE,
                              rbind(colnames(lancement_wilayas0_donnees() %>%
                                               group_by(Annee,Segment) %>% 
                                               summarise(Lancements=sum(Lancement)) %>% 
                                               spread(key=Segment,value=Lancements) %>% 
                                               mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                              ),
                              lancement_wilayas0_donnees() %>%
                                group_by(Annee,Segment) %>% 
                                summarise(Lancements=sum(Lancement)) %>% 
                                spread(key=Segment,value=Lancements) %>% 
                                mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                              ),showToolbar = TRUE,
                              columns = data.frame(title=rep("",length(lancement_wilayas0_donnees() %>%
                                                                         group_by(Annee,Segment) %>% 
                                                                         summarise(Lancements=sum(Lancement)) %>% 
                                                                         spread(key=Segment,value=Lancements) %>% 
                                                                         mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                              )))
                   ),
                   `if`(input$parwilayas_lanc==FALSE & input$parsegments_lanc==TRUE & input$parperiode_lanc==FALSE,
                        excelTable(editable = FALSE, 
                                   rbind(c("Segment","Lancements"),
                                         lancement_wilayas0_donnees() %>%
                                           group_by(Segment) %>% 
                                           summarise(Lancements=sum(Lancement)) %>% 
                                           rbind(c("Total",round(lancement_wilayas0_donnees() %>% summarise(ta=sum(Lancement)) %>% select(ta))$ta))
                                   ),showToolbar = TRUE,columns = data.frame(title=c("",""))
                        ),
                        `if`(input$parwilayas_lanc==TRUE & input$parsegments_lanc==FALSE & input$parperiode_lanc==TRUE,
                             excelTable(editable = FALSE,
                                        rbind(colnames(lancement_wilayas0_donnees() %>%
                                                         group_by(Annee,Wilaya) %>% 
                                                         summarise(Lancements=sum(Lancement)) %>% 
                                                         spread(key = Annee,value=Lancements) %>% 
                                                         rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                                         rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                        ),
                                        lancement_wilayas0_donnees() %>%
                                          group_by(Annee,Wilaya) %>% 
                                          summarise(Lancements=sum(Lancement)) %>% 
                                          spread(key = Annee,value=Lancements) %>% 
                                          rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                          rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                        )
                                        ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(  lancement_wilayas0_donnees() %>%
                                                                                                       group_by(Annee,Wilaya) %>% 
                                                                                                       summarise(Lancements=sum(Lancement)) %>% 
                                                                                                       spread(key = Annee,value=Lancements) %>% 
                                                                                                       rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>%
                                                                                                       rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Annee) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                        )))
                             ),
                             `if`(input$parwilayas_lanc==TRUE & input$parsegments_lanc==FALSE & input$parperiode_lanc==FALSE,
                                  excelTable(editable = FALSE,
                                             rbind(c("Wilaya","Lancements"),
                                                   lancement_wilayas0_donnees() %>%
                                                     group_by(Wilaya) %>% 
                                                     summarise(Lancements=sum(Lancement)) %>% 
                                                     rbind(c("Total",sum(round(lancement_wilayas0_donnees() %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                             )
                                             ,showToolbar = TRUE,columns = data.frame(title=c("",""))
                                  ),
                                  `if`(input$parwilayas_lanc==TRUE & input$parsegments_lanc==TRUE & input$parperiode_lanc==TRUE,
                                       excelTable(editable = FALSE,
                                                  rbind(colnames(lancement_wilayas0_donnees() %>%
                                                                   group_by(Annee,Wilaya,Segment,)%>% 
                                                                   summarise(Lancements=sum(Lancement)) %>% 
                                                                   spread(Segment,value = Lancements) %>% 
                                                                   mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                                  ),
                                                  lancement_wilayas0_donnees() %>%
                                                    group_by(Annee,Wilaya,Segment,)%>% 
                                                    summarise(Lancements=sum(Lancement)) %>% 
                                                    spread(Segment,value = Lancements) %>% 
                                                    mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                                  )
                                                  ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(lancement_wilayas0_donnees() %>%
                                                                                                               group_by(Annee,Wilaya,Segment,)%>% 
                                                                                                               summarise(Lancements=sum(Lancement)) %>% 
                                                                                                               spread(Segment,value = Lancements) %>% 
                                                                                                               mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c(" ","Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                                  )))
                                       ),
                                       excelTable(editable = FALSE,
                                                  rbind(colnames(lancement_wilayas0_donnees() %>%
                                                                   group_by(Wilaya,Segment)%>% 
                                                                   summarise(Lancements=sum(Lancement)) %>% 
                                                                   spread(Segment,value = Lancements) %>% 
                                                                   rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                                  ),
                                                  lancement_wilayas0_donnees() %>%
                                                    group_by(Wilaya,Segment)%>% 
                                                    summarise(Lancements=sum(Lancement)) %>% 
                                                    spread(Segment,value = Lancements) %>% 
                                                    rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                                  )
                                                  ,showToolbar = TRUE,columns = data.frame(title=rep("",ncol(lancement_wilayas0_donnees() %>%
                                                                                                               group_by(Wilaya,Segment)%>% 
                                                                                                               summarise(Lancements=sum(Lancement)) %>% 
                                                                                                               spread(Segment,value = Lancements) %>% 
                                                                                                               rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t,sum(round(lancement_wilayas0_donnees() %>% group_by(Segment) %>% summarise(t=sum(Lancement)) %>% select(t))$t) ))
                                                  )))
                                       )
                                  )
                             )
                             
                             
                        )
                   )
              )
         )
    )
    
  })
  
  
  
  
  
  
  #########################################
  #########################################
  
  
  
  
  
  
  wilaya_reactive2_estimation=reactive({
    if(length(input$selectwilayas_wilaya)==0) {
      unique(estimation_tolpopparc$waw)
    } else{
      input$selectwilayas_wilaya
    }
  })
  
  estimation_tolpopparc0_donnees=reactive({
    estimation_tolpopparc%>%
      select(Annee,waw,Population,TOL,Parc_logement) %>%
      mutate(Population=round(Population),Parc_logement=round(Parc_logement)) %>% rowwise() %>% mutate(TOL=round(sum(Population)/sum(Parc_logement),2)) %>% 
      filter(waw %in% wilaya_reactive2_estimation(),Annee>= min(input$selectannees_wilaya),Annee <= max(input$selectannees_wilaya)) %>% 
      rename(Wilaya=waw)
  })
  
  output$donnees_excel_wilaya<-renderExcel({
    `if`(input$parwilayas_wilaya==FALSE,
         excelTable(editable = FALSE,
                    rbind(c("Année","Population","Parc Logement","TOL"),
                          estimation_tolpopparc0_donnees() %>% 
                            select(Annee,Wilaya,Population,Parc_logement,TOL) %>% 
                            group_by(Annee) %>% 
                            summarise(Population=sum(Population),'Parc Logement'=sum(Parc_logement),TOL=round(sum(Population)/sum(Parc_logement),2) )
                    ),showToolbar = TRUE,autoFill = TRUE,columns = data.frame(title=rep("",4))
         ),
         `if`(max(input$selectannees_wilaya)==min(input$selectannees_wilaya),
              excelTable(editable = FALSE,
                         rbind(c("Année","Wilaya","Population","Parc Logement","TOL"),
                               estimation_tolpopparc0_donnees() %>% 
                                 select(Annee,Wilaya,Population,TOL,Parc_logement) %>% 
                                 rbind(c("","Total",round(sum(estimation_tolpopparc0_donnees()$Population)  ),round(sum(estimation_tolpopparc0_donnees()$Population)/sum(estimation_tolpopparc0_donnees()$Parc_logement),2),round(sum(estimation_tolpopparc0_donnees()$Parc_logement)  ) )) %>% 
                                 select(Annee,Wilaya,Population,Parc_logement,TOL) %>% 
                                 rename('Parc Logement'=Parc_logement)
                         )
                         
                         ,showToolbar = TRUE,autoFill = TRUE,columns = data.frame(title=rep("",5))
              ),
              excelTable(editable = FALSE,
                         rbind(c("Année","Wilaya","Population","Parc Logement","TOL"),
                               
                               estimation_tolpopparc0_donnees() %>% 
                                 select(Annee,Wilaya,Population,Parc_logement,TOL) %>% 
                                 rename('Parc Logement'=Parc_logement)
                         )
                         ,showToolbar = TRUE,autoFill = TRUE,columns = data.frame(title=rep("",5))
              )
         ))
  })
  
  
  # 
  # livraison_wilayas0=reactive({
  #   read_excel(paste0(getwd(),"/livraison_wilayas.xlsx")) %>% 
  #     filter(type_de_logement %in% c( "LPL","Rural","LPP","LSP","Location-Vente"))
  # })
  # 
  # estimation_tolpopparc0=reactive({
  #   read_excel(paste0(getwd(),"/Estimation_Population_TOL_Parc_par_Wilaya.xlsx"))
  # })
  # 
  output$periode<-renderText({
    `if`(min(input$annees)!=max(input$annees),paste0("Periode : ",min(input$annees),"-",max(input$annees)),paste0("Annee : ",min(input$annees)) )
  })
  
  
  output$periode2<-renderText({
    `if`(input$radio_choose_leaflet %in% c("Livraisons de Logements","Lancements de Logements") & min(input$annees)!=max(input$annees),paste0("Periode : ",min(input$annees),"-",max(input$annees)),paste0("Annee : ",max(input$annees))
    )
  })
  
  
  output$excel3<-renderExcel({
    excelTable(editable = FALSE,
               rbind(c("Wilaya","Livraisons","Lancements","Surface","Population","Parc Logement","TOL","Daira","Commune"),
                     data.frame(livraison_wilayas%>%
                                  filter(annee>= min(input$annees),annee <= max(input$annees),type_de_logement %in% segments_reactive())%>%
                                  group_by(waw)%>%
                                  summarise(Livraisons=sum(Livraison))%>%
                                  add_column(Lancements=lancement_wilayas %>% 
                                               filter(Annee>= min(input$annees),Annee <= max(input$annees),Segment %in% segments_reactive())%>%
                                               group_by(Wilaya) %>% 
                                               summarise(Lancements=sum(Lancement)) %>% .$Lancements
                                             ,Surface=unique(livraison_wilayas$Surface)
                                             ,Population=round(estimation_tolpopparc$Population[estimation_tolpopparc$Annee==max(input$annees)])
                                             ,"Parc Logement"=round(estimation_tolpopparc$Parc_logement[estimation_tolpopparc$Annee==max(input$annees)])
                                             ,TOL=round(estimation_tolpopparc$Population[estimation_tolpopparc$Annee==max(input$annees)]/estimation_tolpopparc$Parc_logement[estimation_tolpopparc$Annee==max(input$annees)],2)
                                             ,Daira=livraison_wilayas$Daira[seq(1,length(unique(livraison_wilayas$annee))*length(unique(livraison_wilayas$type_de_logement))*length(unique(livraison_wilayas$waw)),length(unique(livraison_wilayas$annee))*length(unique(livraison_wilayas$type_de_logement)))]
                                             ,Commune=livraison_wilayas$Commune[seq(1,length(unique(livraison_wilayas$annee))*length(unique(livraison_wilayas$type_de_logement))*length(unique(livraison_wilayas$waw)),length(unique(livraison_wilayas$annee))*length(unique(livraison_wilayas$type_de_logement)))]
                                             
                                  ) %>% 
                                  rename(Wilaya=waw)
                     ) ),columns = data.frame(title=rep("",9)),showToolbar = TRUE
    )
  })
  
  output$excel1<-renderExcel({
    `if`(input$radio_choose_line1=="Livraisons de Logements",
         excelTable(editable = FALSE,
                    rbind(c(colnames(daa2()%>%
                                       spread(key=type_de_logement,value = liv) %>% rename("Année"=annee) ),"Total"),
                          data.frame(daa2()%>%
                                       spread(key=type_de_logement,value = liv) %>% 
                                       mutate(annee=as.character(annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(daa2() %>% group_by(type_de_logement) %>% summarise(t=sum(liv)) %>% select(t))$t,sum(round(daa2() %>% group_by(type_de_logement) %>% summarise(t=sum(liv)) %>% select(t))$t) ))
                          )
                    ),showToolbar = TRUE,columns = data.frame(title=rep("",1+ncol(data.frame(data.frame(daa2()%>%
                                                                                                          spread(key=type_de_logement,value = liv))))))
         ),
         `if`(input$radio_choose_line1=="TOL",
              excelTable(editable = FALSE,
                         #hchart12_data()
                         rbind(c(colnames(hchart12_data())),
                               hchart12_data()
                         )
                         
                         ,showToolbar = TRUE
                         ,columns = data.frame(title=rep("",4))
              )
              ,
              excelTable(editable = FALSE,
                         rbind(c(colnames(daa2_lancement()%>%
                                            spread(key=Segment,value = lanc) %>% rename("Année"=Annee) ),"Total"),
                               data.frame(daa2_lancement()%>%
                                            spread(key=Segment,value = lanc) %>% 
                                            mutate(Annee=as.character(Annee)) %>% rowwise() %>% mutate(Total=sum(c_across(where(is.numeric)))) %>% rbind(c("Total",round(daa2_lancement() %>% group_by(Segment) %>% summarise(t=sum(lanc)) %>% select(t))$t,sum(round(daa2_lancement() %>% group_by(Segment) %>% summarise(t=sum(lanc)) %>% select(t))$t) ))
                               )
                         ),showToolbar = TRUE,columns = data.frame(title=rep("",1+ncol(data.frame(data.frame(daa2_lancement()%>%
                                                                                                               spread(key=Segment,value = lanc))))))
              )
              
         )
    )
    
  })
  
  
  output$excel2<-renderExcel({
    `if`(input$radio_choose_line1_pie=="Livraisons de Logements",
         
         excelTable(editable = FALSE,
                    rbind(c("Segment","Livraisons","Pourcentage %"),
                          daa()%>%
                            select(label,value,pr) %>% 
                            rename(Segment=label,Livraisons=value,"Pourcentage %"=pr)
                    ),columns = data.frame(title=rep("",3)),showToolbar = TRUE
         )
         ,
         
         
         excelTable(editable = FALSE,
                    rbind(c("Segment","Lancements","Pourcentage %"),
                          daa_lancements()%>%
                            select(label,value,pr) %>% 
                            rename(Segment=label,Lancements=value,"Pourcentage %"=pr)
                    ),columns = data.frame(title=rep("",3)),showToolbar = TRUE
         )
         
         
    )
  })
  output$excel_urbanisme1<-renderExcel({
    excelTable(
      data=rbind(c("Wilaya","POS","POS","POS","PDAU","PDAU","EGU","EGU","EGU"),
                 colnames(pos5),pos5),
      columns = data.frame(title=rep("",9)
                           ,width=c(200,200,200,200,200,200,200,200,200))
      ,mergeCells = list(A1=c(1,2),B1=c(3,1),E1=c(2,1),G1=c(3,1)),showToolbar = TRUE,editable = FALSE
      ,columnSorting=FALSE)
  })
  
  
  
  
  output$excel_urbanisme2<-renderExcel({
    excelTable(search=TRUE,
               showToolbar = TRUE,
               data=rbind(colnames(sit_fin[,2:6]),sit_fin[,2:6] %>% mutate_if(is.numeric,format,scientific=FALSE)),
               #nestedHeaders = list( data.frame(title=c("","Amélioration urbaine","VRD PRIMAIRES ET SECONDAIRES 2010 -2019","VRD TERTIAIRES D'HRG 2012- 2013 -2019","VRD LOTISSEMENTS SOCIAUX 2012 -2013 -2015 -2018 -2019"),colspan=c(1,3, 3, 3,3))),
               columns = data.frame(title=rep("",ncol(sit_fin)-1) )
               #"Wilaya","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat")
               #                                                       ,width=c(200,200,200,200,200,200,200,200,200,200,200,200))
               ,editable = FALSE
    )
  })
  
  
  output$excel_urbanisme3<-renderExcel({
    excelTable(
      data=rbind(colnames(zones00)[2:12],
                 zones00[,2:12]),
      editable=FALSE,showToolbar = TRUE,
      columns = data.frame(title=rep("",11))
    )
    
  })
  
  
  output$excel_urbanisme4<-renderExcel({
    excelTable(
      data=rbind(c("Wilaya",colnames(zones00)[13:ncol(zones00)]),
                 zones00[,c(2,13:ncol(zones00))]),
      editable=FALSE,showToolbar = TRUE,
      columns = data.frame(title=rep("",8))
    )
    
  })
  
  
  output$densite<-renderText({
    
    format((round(estimation_tolpopparc%>%
                    filter(Annee == max(input$annees),waw %in% wilaya_reactive())%>%
                    summarise(pop=sum(Population))%>%
                    select(pop))$pop)/(round(livraison_wilayas%>%
                                               select(Surface)%>%
                                               unique()%>%
                                               mutate(wilaya=unique(livraison_wilayas$waw))%>%
                                               filter(wilaya %in% wilaya_reactive())%>%
                                               summarise(sumsurface=sum(Surface))%>%
                                               select(sumsurface),2)$sumsurface),
           big.mark = " ",trim=TRUE,digits = 3
    )
    
  })
  
  
  
  output$max_an<-renderText({
    max(input$annees)
  })
  
  output$titre_serie3<-renderText({
    `if`(input$radio_choose_leaflet %in% c("Livraisons de Logements","Lancements de Logements"),
         `if`(length(segments_reactive())==5,paste0(""),paste(segments_reactive(),collapse = " + "))
    )
  })
  
  
  
  
  output$titre_serie1_hchart1<-renderText({
    `if`(input$radio_choose_line1=="TOL",paste0(""),
         `if`(input$Id027==FALSE & max(input$annees)!=min(input$annees),paste0(segments_reactive(),collapse = "+"),paste0(""))
    )
  })
  
  output$titre_serie2<-renderText({
    `if`(length(input$wilayas) %in% c(0,48),paste0(""),`if`(length(input$wilayas)>2,paste0("Pour les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = " + ")))
  })
  
  
  output$titre_serie<-renderText({
    `if`(length(input$wilayas) %in% c(0,48),paste0(""),`if`(length(input$wilayas)>2,paste0("Pour les ",length(input$wilayas)," ","Wilayas selectionnees"),paste(input$wilayas,collapse = "+")))
  })
  
  #output$tdaa3<-renderTable({
  #  livraison_wilayas
  #})
  
  output$dernier_an<-renderText({                    #Livraisons
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  
  
  output$dernier_an_lanc<-renderText({                    #Lancement
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  output$dernier_an2<-renderText({                       #Parc logement
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  output$dernier_an3<-renderText({        # POPULATION
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  output$dernier_an4<-renderText({                     #TOL
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  output$dernier_an8<-renderText({                     #densite
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'annee ",min(input$annees)),paste0("par rapport a l'annee ",min(input$annees)-1))
  })
  
  
  
  output$taux_livraisons<-renderText({
    ag=`if`(min(input$annees)!=max(input$annees),
            sprintf("%+3.1f %%",100*(
              
              (
                before00 %>% filter(Segment %in% segments_reactive(),id_wilaya %in% as.numeric(str_sub(wilaya_reactive(),1,2))) %>% 
                  summarise(liv_before=sum(Livraison)) %>% select(liv_before)+
                  round(livraison_wilayas%>%
                          filter(annee <= max(input$annees),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                          summarise(liva2=sum(Livraison))%>%
                          select(liva2))$liva2)
              -(
                before00 %>% filter(Segment %in% segments_reactive(),id_wilaya %in% as.numeric(str_sub(wilaya_reactive(),1,2))) %>% 
                  summarise(liv_before=sum(Livraison)) %>% select(liv_before)+
                  round(livraison_wilayas%>%
                          filter(annee<=min(input$annees),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                          summarise(liva2=sum(Livraison))%>%
                          select(liva2))$liva2)
            )/
              (
                before00 %>% filter(Segment %in% segments_reactive(),id_wilaya %in% as.numeric(str_sub(wilaya_reactive(),1,2))) %>% 
                  summarise(liv_before=sum(Livraison)) %>% select(liv_before)+
                  round(livraison_wilayas%>%
                          filter(annee<=min(input$annees),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                          summarise(liva2=sum(Livraison))%>%
                          select(liva2))$liva2)
            )
            ,
            
            sprintf("%+3.1f %%",100*
                      ((round(livraison_wilayas%>%
                                filter(annee==(max(input$annees)),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                                summarise(liva2=sum(Livraison))%>%
                                select(liva2))$liva2)-(round(livraison_wilayas%>%
                                                               filter(annee==(min(input$annees)-1),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                                                               summarise(liva2=sum(Livraison))%>%
                                                               select(liva2))$liva2))/(round(livraison_wilayas%>%
                                                                                               filter(annee==(min(input$annees)-1),type_de_logement %in% segments_reactive(),waw %in% wilaya_reactive())%>%
                                                                                               summarise(liva2=sum(Livraison))%>%
                                                                                               select(liva2))$liva2)
            )
    )
    paste0('<p style=color:',ifelse(as.numeric(substr(ag,1,nchar(ag)-2))>0,'green','red'),';>',ag,'</p>')
  })
  
  #########
  
  
  output$taux_lancements<-renderText({
    ag2=`if`(min(input$annees)!=max(input$annees),
             sprintf("%+3.1f %%",100*(
               
               (
                 before00 %>% filter(Segment %in% segments_reactive(),id_wilaya %in% as.numeric(str_sub(wilaya_reactive(),1,2))) %>% 
                   summarise(lanc_before=sum(Lancement)) %>% select(lanc_before)+
                   round(lancement_wilayas%>%
                           filter(Annee <= max(input$annees),Segment %in% segments_reactive(),Wilaya %in% wilaya_reactive())%>%
                           summarise(lanc2=sum(Lancement))%>%
                           select(lanc2))$lanc2
               )
               -(
                 before00 %>% filter(Segment %in% segments_reactive(),id_wilaya %in% as.numeric(str_sub(wilaya_reactive(),1,2))) %>% 
                   summarise(lanc_before=sum(Lancement)) %>% select(lanc_before)+
                   round(lancement_wilayas%>%
                           filter(Annee<=min(input$annees),Segment %in% segments_reactive(),Wilaya %in% wilaya_reactive())%>%
                           summarise(lanc2=sum(Lancement))%>%
                           select(lanc2))$lanc2)
             )/
               (
                 before00 %>% filter(Segment %in% segments_reactive(),id_wilaya %in% as.numeric(str_sub(wilaya_reactive(),1,2))) %>% 
                   summarise(lanc_before=sum(Lancement)) %>% select(lanc_before)+
                   round(lancement_wilayas%>%
                           filter(Annee<=min(input$annees),Segment %in% segments_reactive(),Wilaya %in% wilaya_reactive())%>%
                           summarise(lanc2=sum(Lancement))%>%
                           select(lanc2))$lanc2)
             )
             ,
             sprintf("%+3.1f %%",100*
                       ((round(lancement_wilayas%>%
                                 filter(Annee==(max(input$annees)),Segment %in% segments_reactive(),Wilaya %in% wilaya_reactive())%>%
                                 summarise(lanc2=sum(Lancement))%>%
                                 select(lanc2))$lanc2)-(round(lancement_wilayas%>%
                                                                filter(Annee==(min(input$annees)-1),Segment %in% segments_reactive(),Wilaya %in% wilaya_reactive())%>%
                                                                summarise(lanc2=sum(Lancement))%>%
                                                                select(lanc2))$lanc2))/(round(lancement_wilayas%>%
                                                                                                filter(Annee==(min(input$annees)-1),Segment %in% segments_reactive(),Wilaya %in% wilaya_reactive())%>%
                                                                                                summarise(lanc2=sum(Lancement))%>%
                                                                                                select(lanc2))$lanc2)
             )
    )
    paste0('<p style=color:',ifelse(as.numeric(substr(ag2,1,nchar(ag2)-2))>0,'green','red'),';>',ag2,'</p>')
  })
  
  
  
  
  
  
  output$titre_lancement<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         paste0("(",min(input$annees)," au ",max(input$annees),")"),
         paste0("(en ",min(input$annees),")")
    )
  })
  
  
  output$titre_livraison<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         paste0("(",min(input$annees)," au ",max(input$annees),")"),
         paste0("(en ",min(input$annees),")")
    )
  })
  
  
  output$max_annee_population<-renderText({
    paste0("Population en ",max(input$annees))
  })
  
  
  output$max_annee_parclogement<-renderText({
    
    paste0("Parc Logements en ",max(input$annees))
  })
  
  
  
  output$max_annee_densite<-renderText({
    
    paste0("Densite en ",max(input$annees))
  })
  
  output$max_annee_tol<-renderText({
    
    paste0("TOL en ",max(input$annees))
  })
  
  
  ##############  Info Box Livraisons #########################
  ##############  Info Box Livraisons #########################
  
  format_reactive_lancements<-reactive({
    daa_lancements()%>%
      filter(label %in% segments_reactive())%>%
      summarize(lanca=sum(value))%>%
      mutate(ab=format(lanca,big.mark = " ",trim=TRUE))%>%
      select(ab)
  })
  
  
  output$lancements<-renderText({
    
    format_reactive_lancements()$ab
  })
  
  
  output$livraisons<-renderText({
    
    format_reactive_livraisons()$ab
  })
  ##############  Info Box Livraisons #########################
  
  format_reactive_livraisons<-reactive({
    daa()%>%
      filter(label %in% segments_reactive())%>%
      summarize(liva=sum(value))%>%
      mutate(ab=format(liva,big.mark = " ",trim=TRUE))%>%
      select(ab)
  })
  
  output$livraisons<-renderText({
    
    format_reactive_livraisons()$ab
  })
  
  ##############  Info Box Lancement #########################
  ##############  Info Box Lancement #########################
  
  ##############  Info Box Lancement #########################
  
  
  
  ##############  Info Box Parc logement #########################
  ##############  Info Box Parc logement #########################
  
  
  format_reactive_parclogements<-reactive({
    estimation_tolpopparc%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(min(input$annees),max(input$annees)))%>%
      group_by(Annee)%>%
      summarise(pop=sum(Parc_logement))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  format_reactive_parclogements_avantan<-reactive({
    estimation_tolpopparc%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(max(input$annees)-1,max(input$annees)))%>%
      group_by(Annee)%>%
      summarise(pop=sum(Parc_logement))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  
  
  output$parclogements<-renderText({
    format_reactive_parclogements()$ab[length(format_reactive_parclogements()$ab)]
  })
  
  
  output$taux_parclogement<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_parclogements()$pop[length(format_reactive_parclogements()$pop)])-(format_reactive_parclogements()$pop[1]))/(format_reactive_parclogements()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_parclogements_avantan()$pop[2])-(format_reactive_parclogements_avantan()$pop[1]))/(format_reactive_parclogements()$pop[1]) )
    )
  })
  
  
  output$taux_densite<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_population()$pop[length(format_reactive_population()$pop)])-(format_reactive_population()$pop[1]))/(format_reactive_population()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_population_avantan()$pop[2])-(format_reactive_population_avantan()$pop[1]))/(format_reactive_population_avantan()$pop[1]) )
    )
  })
  
  
  
  ##############  Info Box Parc logement #########################
  
  
  
  
  
  ##############  Info Box Population #########################
  ##############  Info Box Population #########################
  
  
  format_reactive_population<-reactive({
    estimation_tolpopparc%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(min(input$annees),max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  
  format_reactive_population_avantan<-reactive({
    estimation_tolpopparc%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(max(input$annees)-1,max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population))%>%
      mutate(ab=format(round(pop),big.mark = " ",trim=TRUE))
  })
  
  
  output$populations<-renderText({
    format_reactive_population()$ab[length(format_reactive_population()$ab)]
  })
  
  output$taux_population<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_population()$pop[length(format_reactive_population()$pop)])-(format_reactive_population()$pop[1]))/(format_reactive_population()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_population_avantan()$pop[2])-(format_reactive_population_avantan()$pop[1]))/(format_reactive_population_avantan()$pop[1]) )
    )     
  })
  
  
  ##############  Info Box Population #########################
  
  
  
  ##############  Info Box TOL #########################
  ##############  Info Box TOL #########################
  
  
  
  format_reactive_tol<-reactive({
    estimation_tolpopparc%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(min(input$annees),max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population)/sum(Parc_logement))%>%
      mutate(ab=format(pop,big.mark = " ",trim=TRUE,digits = 3))
  })
  
  format_reactive_tol_dernieran<-reactive({
    estimation_tolpopparc%>%
      filter(waw %in% wilaya_reactive(),Annee %in% c(max(input$annees)-1,max(input$annees)))%>%
      group_by(Annee)%>%
      summarize(pop=sum(Population)/sum(Parc_logement))%>%
      mutate(ab=format(pop,big.mark = " ",trim=TRUE,digits = 3))
  })
  
  
  output$tols<-renderText({
    format_reactive_tol()$ab[length(format_reactive_tol()$ab)]
  })
  
  output$taux_tol<-renderText({
    `if`(min(input$annees)!=max(input$annees),
         sprintf("%+3.1f %%",100*((format_reactive_tol()$pop[length(format_reactive_tol()$pop)])-(format_reactive_tol()$pop[1]))/(format_reactive_tol()$pop[1]) )
         ,
         sprintf("%+3.1f %%",100*((format_reactive_tol_dernieran()$pop[2])-(format_reactive_tol_dernieran()$pop[1]))/(format_reactive_tol_dernieran()$pop[1]) )
    )
  })
  
  
  
  ##############  Info Box TOL #########################
  
  
  wilaya_mc_reactive=reactive({
    if(length(input$wilayas_mc)==0) {
      unique(datamc$wilaya_matricule)
    } else{
      input$wilayas_mc
    }  
  })
  
  
  filiere_mc_reactive=reactive({
    if(length(input$filiere_mcmc)==0) {
      unique(datamc$Filiere)
    } else{
      input$filiere_mcmc
    }  
  })
  
  
  statut_mc_reactive=reactive({
    if(length(input$statut2_mc)==0) {
      unique(datamc$Statut2)
    } else{
      input$statut2_mc
    }  
  })
  
  
  wilaya_reactive=reactive({
    if(length(input$wilayas)==0) {
      unique(livraison_wilayas$waw)
    } else{
      input$wilayas
    }  
  })
  
  output$table_wilayas_reactive=renderTable({
    wilaya_reactive()
  })
  
  segments_reactive=reactive({
    if(length(input$segments)==0) {
      unique(livraison_wilayas$type_de_logement)
    } else{
      input$segments
    }
  })
  
  
  output$table_segments_reactive=renderTable({
    segments_reactive()
  })
  
  daa_lancements=reactive({
    lancement_wilayas%>%
      filter(
        #type_de_logement %in% segments_reactive(),
        Annee>= min(input$annees),Annee<=max(input$annees),
        Wilaya %in% wilaya_reactive()  ) %>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      
      group_by(Segment)%>%
      summarise(lanc=sum(Lancement))%>%
      rename(label=Segment,value=lanc)%>%
      #arrange(`if`(length(input$segments) %in% c(0,5),c(4,1,5,3,2),c("") ))%>%
      arrange(c(4,1,5,3,2))%>%
      mutate(pr=round(100*value/sum(value),2),value2=format(value, big.mark=" ", trim=TRUE))
  })
  
  
  
  
  daa=reactive({
    livraison_wilayas%>%
      filter(
        #type_de_logement %in% segments_reactive(),
        annee>= min(input$annees),annee<=max(input$annees),
        waw %in% wilaya_reactive()  ) %>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      
      group_by(type_de_logement)%>%
      summarise(liv=sum(Livraison))%>%
      rename(label=type_de_logement,value=liv)%>%
      #arrange(`if`(length(input$segments) %in% c(0,5),c(4,1,5,3,2),c("") ))%>%
      arrange(c(4,1,5,3,2))%>%
      mutate(pr=round(100*value/sum(value),2),value2=format(value, big.mark=" ", trim=TRUE))
  })
  
  inprea=reactive({
    paste0(input$radio_choose_leaflet)
  })
  
  #
  
  livraison_map<-reactive({
    `if`(input$radio_choose_leaflet == c("TOL"),
         round(estimation_tolpopparc%>%
                 select(Annee,waw,Population,TOL,Parc_logement) %>%
                 filter(Annee == max(input$annees)) %>% 
                 mutate(Population=round(Population),Parc_logement=round(Parc_logement),TOL=Population/Parc_logement) %>% 
                 rename(Wilaya=waw,"Parc Logements"=Parc_logement) %>% 
                 arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                 select(TOL),2)$TOL
         ,
         `if`(input$radio_choose_leaflet == c("Livraisons de Logements"),
              round(livraison_wilayas%>%
                      filter(annee>= min(input$annees),annee<=max(input$annees),type_de_logement %in% segments_reactive())%>%
                      group_by(waw)%>%
                      summarise(liv=sum(Livraison))%>%
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                      select(liv))$liv,
              `if`(input$radio_choose_leaflet == c("Lancements de Logements"),
                   round(lancement_wilayas%>%
                           filter(Annee>= min(input$annees),Annee<=max(input$annees),Segment %in% segments_reactive())%>%
                           group_by(Wilaya)%>%
                           summarise(lanc=sum(Lancement))%>%
                           arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                           select(lanc))$lanc,
                   `if`(input$radio_choose_leaflet == c("Parc Logements"),
                        round(estimation_tolpopparc%>%
                                select(Annee,waw,Population,TOL,Parc_logement) %>%
                                filter(Annee == max(input$annees)) %>% 
                                mutate(Population=round(Population),Parc_logement=round(Parc_logement),TOL=Population/Parc_logement) %>% 
                                rename(Wilaya=waw,"Parc Logements"=Parc_logement) %>% 
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                select(`Parc Logements`),2)$`Parc Logements`
                        ,
                        round(estimation_tolpopparc%>%
                                select(Annee,waw,Population,TOL,Parc_logement) %>%
                                filter(Annee == max(input$annees)) %>% 
                                mutate(Population=round(Population),Parc_logement=round(Parc_logement),TOL=Population/Parc_logement) %>% 
                                rename(Wilaya=waw,"Parc Logements"=Parc_logement) %>% 
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                select(Population),2)$Population
                   )
              )
         )
    )
  })
  
  setview_mc<-reactive({
    `if`(length(input$search)==0,
         c(1.3333,30.6167,6),
         `if`(c(datamc %>%
                  filter(wilaya_matricule %in% wilaya_mc_reactive(),
                         Filiere %in% filiere_mc_reactive(),
                         Statut2 %in% statut_mc_reactive()
                  ) %>% 
                filter(Identification==input$search[length(input$search)]) %>%
                select(longitude,latitude) %>%
                anyNA())==TRUE,
              
              c(1.3333,30.6167,6),
              
              datamc %>%
                filter(wilaya_matricule %in% wilaya_mc_reactive(),
                       Filiere %in% filiere_mc_reactive(),
                       Statut2 %in% statut_mc_reactive()
                ) %>%
                filter(Identification==input$search[length(input$search)]) %>%
                select(longitude,latitude) %>%
                mutate(zoom=16) %>% 
                as.matrix %>% c()
         )
    )
     
  })
  
  output$distPlot2_mc<-renderLeaflet({
    leaflet(datamc_maps_reactive()) %>% 
      addProviderTiles(providers[[6]], group = providers[[6]]) %>%
      addProviderTiles(providers[[1]], group = providers[[1]]) %>%
      addProviderTiles(providers[[57]],group="Satellite") %>% 
      addLayersControl(
        baseGroups = c(providers[[6]], providers[[1]], "Satellite")
      ) %>% 
      setView(lng=setview_mc()[1],lat=setview_mc()[2],zoom=setview_mc()[3]) %>% 
      #setView(lng=1.3333,lat=30.6167,zoom=6) %>% 
      addAwesomeMarkers(lat=datamc_maps_reactive()$latitude,lng=datamc_maps_reactive()$longitude,
                        labelOptions=labelOptions(direction = "bottom"),
                        label =
                          sprintf('<strong style="font-size:19px;">%s</strong><br/>
<table style="font-size:17px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;">Wilaya</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Localisation</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Statut</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;">Annee dentree en production</td>
            <td style="width: 0.263361;padding-bottom:8px;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;">Capacite nominale installee<br></td>
            <td style="width: 0.263361;padding-bottom:8px;">%s %s</td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;">Production effective<br></td>
            <td style="width: 0.263361;padding-bottom:8px;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;">Produits fabriques<br></td>
            <td style="width: 0.263361;padding-bottom:8px;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Tel</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Fax</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Site web</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Email</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Observation</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
    </tbody>
</table>
                          ',
                                  datamc_maps_reactive()$Identification,
                                  datamc_maps_reactive()$wilaya_matricule,
                                  datamc_maps_reactive()$Localisation,
                                  datamc_maps_reactive()$Statut,
                                  datamc_maps_reactive()$Annee_entree,
                                  datamc_maps_reactive()$`Capacité nominale installée`,
                                  datamc_maps_reactive()$unité,
                                  datamc_maps_reactive()$`Production effective`,
                                  datamc_maps_reactive()$`Produits fabriqués`,
                                  datamc_maps_reactive()$TEL,
                                  datamc_maps_reactive()$FAX,
                                  datamc_maps_reactive()$`Site web`,
                                  datamc_maps_reactive()$Email,
                                  datamc_maps_reactive()$observations
                                  
                                  
                                  
                                  
                                  
                                  
                          ) %>% lapply(htmltools::HTML)
      )
    
  })
  
  output$distPlot2<-renderLeaflet({
    mapdz
  })
  
  twoevent=reactive({
    list(input$annees,input$segments,input$radio_choose_leaflet)
  })
  
  
  # | | |  OR operation
  
  popolo=reactive({
  })
  
  # 
  # twoevent_mc=reactive({
  #   #list(wilaya_mc_reactive(),filiere_mc_reactive(),statut_mc_reactive())
  #   list(input$annees_mc,input$filiere_mcmc,input$wilayas_mc,input$statut2_mc)
  # })
  
  datamc_maps_reactive<-reactive({
    datamc %>% 
      filter(wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
             #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
             )
  })
  
  
  observeEvent(twoevent(),{
    livrason_maps=livraison_map()
    poloo=
      `if`(input$radio_choose_leaflet==c("Livraisons de Logements"),colorNumeric("YlGnBu",livraison_map()),
           `if`(input$radio_choose_leaflet==c("Lancements de Logements"),colorNumeric("YlGn",livraison_map()),
                `if`(input$radio_choose_leaflet==c("TOL"),colorNumeric("RdYlBu",livraison_map(),reverse=TRUE),
                     `if`(input$radio_choose_leaflet==c("Population"),colorNumeric("YlOrBr",livraison_map()),
                          colorNumeric("BuPu",livraison_map())
                     )  
                )
           )
      )
    
    inpr=input$radio_choose_leaflet
    maxaa=max(input$annees)
    leafletProxy('distPlot2',data=algeria)%>%
      clearControls()%>%
      addLegend(
        position = "topright",
        title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
                   `if`(inpr== c("Lancements de Logements"),"Lancements",
                        paste0(inpr))),
        pal=poloo,
        opacity = 1,
        values=livrason_maps
      ) 
  }
  )
  
  observeEvent(twoevent(),{
    maxa=max(input$annees)
    mixa=min(input$annees)
    livrason_maps=livraison_map()
    poloo=`if`(input$radio_choose_leaflet==c("Livraisons de Logements"),colorNumeric("YlGnBu",livraison_map()),
               `if`(input$radio_choose_leaflet==c("Lancements de Logements"),colorNumeric("YlGn",livraison_map()),
                    `if`(input$radio_choose_leaflet==c("TOL"),colorNumeric("RdYlBu",livraison_map(),reverse=TRUE),
                         `if`(input$radio_choose_leaflet==c("Population"),colorNumeric("YlOrBr",livraison_map()),
                              colorNumeric("BuPu",livraison_map())
                         )  
                    )
               )
    )
    
    leafletProxy('distPlot2',data=algeria)%>%
      
      addPolygons(weight=1,fillColor =poloo(livrason_maps),color ="black",
                  label=sprintf(
                    "<strong style='font-size:16px;'>%s</strong><br/> <p style='font-size:14px;font-weight:normal;display:inline;'> <p style='font-size:15px;font-weight:normal;display:inline;'>Livraisons</p>  <h6 style='font-size:11px;display:inline;'>(%s)</h6>&nbsp;&nbsp;&nbsp;:&nbsp; %s <br/><p style='font-size:14px;font-weight:normal;display:inline;'> <p style='font-size:15px;font-weight:normal;display:inline;'>Lancements</p>  <h6 style='font-size:11px;display:inline;'>(%s)</h6>: %s <br/>Surface &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;:&nbsp; %s <br/>Nombre de Daira &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; :&nbsp; %s <br/>Nombre de Communes  :&nbsp; %s <br/>Population <h6 style='font-size:11px;display:inline;'>(%g)</h6> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; :&nbsp; %s<br/>Parc logements <h6 style='font-size:11px;display:inline;'>(%g)</h6>&nbsp;&nbsp;&nbsp; :&nbsp; %s<br/>TOL <h6 style='font-size:11px;display:inline;'>(%g)</h6>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;:&nbsp; %s</p>",
                    algeria@data$wilayas,
                    
                    
                    `if`(mixa!=maxa,
                         paste0(mixa," au ",maxa),
                         paste0("en    ",mixa)
                    ),
                    
                    format(round(livraison_wilayas%>%
                                   filter(annee>= mixa,annee<=maxa,type_de_logement %in% segments_reactive())%>%
                                   group_by(id_wilaya)%>%
                                   summarise(liv=sum(Livraison))%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(liv))$liv,big.mark = " ",trim=TRUE),
                    
                    `if`(mixa!=maxa,
                         paste0(mixa," au ",maxa),
                         paste0("en    ",mixa)
                    ),
                    
                    format(round(lancement_wilayas%>%
                                   filter(Annee>= mixa,Annee<=maxa,Segment %in% segments_reactive())%>%
                                   group_by(Wilaya)%>%
                                   summarise(lanc=sum(Lancement))%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(lanc))$lanc,big.mark = " ",trim=TRUE),
                    
                    
                    format(round(livraison_wilayas%>%
                                   select(Surface)%>%
                                   unique()%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(Surface),1)$Surface,trim=TRUE,big.mark=" "),
                    
                    
                    format(round(livraison_wilayas%>%
                                   group_by(id_wilaya)%>%
                                   summarise(nbd=min(Daira))%>%
                                   select(nbd)%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(nbd),1)$nbd,trim=TRUE,big.mark=" "),
                    
                    format(round(livraison_wilayas%>%
                                   group_by(id_wilaya)%>%
                                   summarise(nbc=min(Commune))%>%
                                   select(nbc)%>%
                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                                   select(nbc),1)$nbc,trim=TRUE,big.mark=" "),
                    
                    maxa,format(round(
                      estimation_tolpopparc%>%
                        filter(Annee==maxa)%>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                        select(Population))$Population,big.mark = " ",trim=TRUE),
                    maxa,
                    format(round(
                      estimation_tolpopparc%>%
                        filter(Annee==maxa)%>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                        select(Parc_logement))$Parc_logement,big.mark = " ",trim=TRUE),
                    maxa,
                    format(round(
                      estimation_tolpopparc%>%
                        filter(Annee==maxa)%>%
                        mutate(tol1=Population/Parc_logement)%>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))%>%
                        select(tol1),2)$tol1,big.mark = " ",trim=TRUE)
                    
                    
                  ) %>% lapply(htmltools::HTML),
                  fillOpacity = 0.7,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "left",
                    offset = c(-130,100)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 0.7,bringToFront=TRUE
                  ))
    
  })
  
  zoomlevel<-reactive({
    input$distPlot2_zoom
  })
  
  observeEvent(wilaya_reactive(),{
    wi=wilaya_reactive()
    w=as.character(1:48)
    
    `if`(length(wi) %in% c(1:47),
         leafletProxy('distPlot2',data=algeria)%>%
           addAwesomeMarkers(layerId = as.character(algeria@data$id_wilaya[which(algeria@data$wilayas %in% wi)]),
                             lng=algeria@data$longitude[which(algeria@data$wilayas %in% wi)],
                             lat=algeria@data$latitude[which(algeria@data$wilayas %in% wi)],
                             label=algeria@data$nam[which(algeria@data$wilayas %in% wi)],
                             labelOptions=labelOptions(noHide = `if`(length(wi)<6,T,F),textsize = `if`(length(wi) %in% c(4,5) ,"10px","15px"),direction = "bottom",offset = `if`(length(wi)<6,c(0,0),c(-180,-40) ))
           )%>%   
           removeMarker(layerId = as.character(w[-(algeria@data$id_wilaya[which(algeria@data$wilayas %in% wi)])]))
         ,
         
         leafletProxy('distPlot2',data=algeria)%>%
           removeMarker(layerId = w)
    )
  })
  
  
  
  output$pie <- renderHighchart({
    `if`(input$radio_choose_line1_pie=="Livraisons de Logements",
         highchart() %>%
           hc_add_series(
             daa(),
             "pie",
             hcaes(
               name = label,
               y = value
             ),
             name = "Livraison"
           )%>%
           hc_chart(type = "pie",
                    
                    # var arr=document.getElementsByClassName('highcharts-data-label-connector')
                    # arr[1].style.stroke="#ffffff"             for arrow
                    
                    
                    
                    events=list(load=JS("function() {
               var vaz=$('#segments').val();
               if(vaz.length==1 || vaz.length==2 || vaz.length==3 || vaz.length==4){
                if(vaz.includes('LPL')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[0].children[0].style.fontSize='0px';
                  arr[0].style.stroke='#ffffff';
                }
                if(vaz.includes('Rural')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[1].children[0].style.fontSize='0px';
                  arr[1].style.stroke='#ffffff';
                }
                if(vaz.includes('LSP')==false ) {
                
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[2].children[0].style.fontSize='0px';
                  arr[2].style.stroke='#ffffff';}
                  
                  
                if(vaz.includes('Location-Vente')==false ) {
                  
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[3].children[0].style.fontSize='0px';
                  arr[3].style.stroke='#ffffff';
                }
                if(vaz.includes('LPP')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[4].children[0].style.fontSize='0px';
                  arr[4].style.stroke='#ffffff';
                }
               
               }
                  }
                                   ")
                    )
                    
                    
                    #    ,options3d = list(enabled = TRUE
                    #                    , beta = 10
                    #                   , alpha = 38
                    #                  , depth = 400
                    #                 , viewDistance = 8)
           )%>%
           #hc_title(text="Répartition de livraisons de logements par Segments")%>%
           #hc_subtitle(text="de l'année 2000-2019")%>%
           hc_plotOptions(
             series = list(
               alignValue="left",
               showInLegend = TRUE,
               pointFormat = "{point.y}%",
               colorByPoint = TRUE,
               size="180px",
               dataLabels=list(style=list(fontSize= "14px",fontWeight="normal"),format="<p>{point.label}: {point.pr:.1f}%<br />( {point.value2} )</p>"),
               #depth=40,
               allowPointSelect=TRUE
             )
           )%>%
           hc_legend(
             align= 'right',
             layout="vertical",
             verticalAlign= 'middle',
             itemMarginTop= 6,
             itemMarginBottom=6,
             itemStyle=list(fontSize="15px"),
             margin=-60
           )%>%
           hc_add_theme(hc_theme_smpl(    #hc_theme_elementary  hc_theme_ffx #hc_theme_flat
             colors=`if`(length(input$segments) %in% c(0,5),
                         c("#d35400","#2ecc71","#2980b9","#f1c40f","#2c3e50"),
                         
                         c(
                           `if`("LPL" %in% input$segments,"#d35400","#bababa"),
                           `if`("Rural" %in% input$segments,"#2ecc71","#bababa"),
                           `if`("LSP" %in% input$segments,"#2980b9","#bababa"),
                           `if`("Location-Vente" %in% input$segments,"#f1c40f","#bababa"),
                           `if`("LPP" %in% input$segments,"#2c3e50","#bababa")
                         )   ),
             chart = list(backgroundColor = "transparent")
             ,        title=list(align="right",
                                 style=list(fontSize="14px",fontWeight="normal")),
             subtitle=list(align="center",
                           style=list(fontWeight="normal",fontFamily="Roboto Condensed")))
           )
         ,
         ############ here we put pie's Lancements
         #########################################
         
         
         highchart() %>%
           hc_add_series(
             daa_lancements(),
             "pie",
             hcaes(
               name = label,
               y = value
             ),
             name = "Lancements"
           )%>%
           hc_chart(type = "pie",
                    
                    # var arr=document.getElementsByClassName('highcharts-data-label-connector')
                    # arr[1].style.stroke="#ffffff"             for arrow
                    
                    
                    
                    events=list(load=JS("function() {
               var vaz=$('#segments').val();
               if(vaz.length==1 || vaz.length==2 || vaz.length==3 || vaz.length==4){
                if(vaz.includes('LPL')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[0].children[0].style.fontSize='0px';
                  arr[0].style.stroke='#ffffff';
                }
                if(vaz.includes('Rural')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[1].children[0].style.fontSize='0px';
                  arr[1].style.stroke='#ffffff';
                }
                if(vaz.includes('LSP')==false ) {
                
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[2].children[0].style.fontSize='0px';
                  arr[2].style.stroke='#ffffff';}
                  
                  
                if(vaz.includes('Location-Vente')==false ) {
                  
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[3].children[0].style.fontSize='0px';
                  arr[3].style.stroke='#ffffff';
                }
                if(vaz.includes('LPP')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[4].children[0].style.fontSize='0px';
                  arr[4].style.stroke='#ffffff';
                }
               
               }
                  }
                                   ")
                    )
                    
                    
                    #    ,options3d = list(enabled = TRUE
                    #                    , beta = 10
                    #                   , alpha = 38
                    #                  , depth = 400
                    #                 , viewDistance = 8)
           )%>%
           #hc_title(text="Répartition de livraisons de logements par Segments")%>%
           #hc_subtitle(text="de l'année 2000-2019")%>%
           hc_plotOptions(
             series = list(
               alignValue="left",
               showInLegend = TRUE,
               pointFormat = "{point.y}%",
               colorByPoint = TRUE,
               size="180px",
               dataLabels=list(style=list(fontSize= "14px",fontWeight="normal"),format="<p>{point.label}: {point.pr:.1f}%<br />( {point.value2} )</p>"),
               #depth=40,
               allowPointSelect=TRUE
             )
           )%>%
           hc_legend(
             align= 'right',
             layout="vertical",
             verticalAlign= 'middle',
             itemMarginTop= 6,
             itemMarginBottom=6,
             itemStyle=list(fontSize="15px"),
             margin=-60
           )%>%
           hc_add_theme(hc_theme_smpl(    #hc_theme_elementary  hc_theme_ffx #hc_theme_flat
             colors=`if`(length(input$segments) %in% c(0,5),
                         c("#d35400","#2ecc71","#2980b9","#f1c40f","#2c3e50"),
                         
                         c(
                           `if`("LPL" %in% input$segments,"#d35400","#bababa"),
                           `if`("Rural" %in% input$segments,"#2ecc71","#bababa"),
                           `if`("LSP" %in% input$segments,"#2980b9","#bababa"),
                           `if`("Location-Vente" %in% input$segments,"#f1c40f","#bababa"),
                           `if`("LPP" %in% input$segments,"#2c3e50","#bababa")
                         )   ),
             chart = list(backgroundColor = "transparent")
             ,        title=list(align="right",
                                 style=list(fontSize="14px",fontWeight="normal")),
             subtitle=list(align="center",
                           style=list(fontWeight="normal",fontFamily="Roboto Condensed")))
           )
         
         
         
         
         
         ##########################################
    )
    
  })
  
  daa2_lancement=reactive({
    lancement_wilayas%>%
      filter(Segment %in% segments_reactive(),
             Annee>= min(input$annees),Annee<=max(input$annees),
             Wilaya %in% wilaya_reactive()  ) %>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      #filter(type_de_logement %in% c("LSP","Rural","Location-Vente", "LPP","LPL"))%>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      group_by(Annee,Segment)%>%
      summarise(lanc=sum(Lancement))
  })
  
  
  
  
  daa2=reactive({
    livraison_wilayas%>%
      filter(type_de_logement %in% segments_reactive(),
             annee>= min(input$annees),annee<=max(input$annees),
             waw %in% wilaya_reactive()  ) %>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      #filter(type_de_logement %in% c("LSP","Rural","Location-Vente", "LPP","LPL"))%>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      group_by(annee,type_de_logement)%>%
      summarise(liv=sum(Livraison))
  })
  
  
  daa3=reactive({
    daa2()%>%
      group_by(annee)%>%
      summarise(liv2=sum(liv))
    
  })
  
  
  
  daa3_lancement=reactive({
    daa2_lancement()%>%
      group_by(Annee)%>%
      summarise(lanc2=sum(lanc))
    
  })
  
  line_color0_lanc<-reactive({
    data.frame(ns=c("Location-Vente","LPL","LPP","LSP","Rural"),couleur=c("#f1c40f","#d35400","#2c3e50","#2980b9","#2ecc71"))
  })
  
  
  line_color1_lanc<-reactive({
    line_color0_lanc() %>% filter(ns %in% unique(daa2_lancement()$Segment)) %>% select(couleur)
  })
  
  line_color0<-reactive({
    data.frame(ns=c("Location-Vente","LPL","LPP","LSP","Rural"),couleur=c("#f1c40f","#d35400","#2c3e50","#2980b9","#2ecc71"))
  })
  
  line_color1<-reactive({
    line_color0() %>% filter(ns %in% unique(daa2()$type_de_logement)) %>% select(couleur)
  })
  
  output$hchart1<-renderHighchart({
    
    `if`(input$radio_choose_line1=="TOL",
         `if`(min(input$annees)!=max(input$annees),
              highchart() %>%
                
                hc_xAxis(
                  labels=list(style=list(fontSize= "11px",fontWeight="normal"),rotation=0),
                  tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      } 
    }")
                ) %>%
                
                
                hc_yAxis_multiples(
                  list(title=list(text="Population"),opposite=FALSE,
                       min=0,max=max(hchart12_data()$Population),
                       labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))
                  ),
                  list(title=list(text="Parc Logements"),opposite=FALSE,
                       min=0,max=max(hchart12_data()$Population)/3,
                       labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))),
                  list(title=list(text="TOL"),opposite=TRUE,min=0,max=9)
                  #create_yaxis(naxis = 4, title = list(text = NULL))
                ) %>%
                hc_add_series(hchart12_data(),type="line",hcaes(x=Annee,y=Population),name="Population") %>%
                hc_add_series(hchart12_data(),type="line",hcaes(x=Annee,y=Parc_logement),yAxis=1,name="Parc Logements") %>%
                hc_add_series(hchart12_data(),type="line",hcaes(x=Annee,y=TOL),yAxis=2,name="TOL") %>% 
                hc_tooltip(
                  crosshairs = TRUE,
                  backgroundColor = "#F0F0F0",
                  borderColor="#212121",
                  shared = TRUE, 
                  borderWidth = 3,
                  sort=TRUE
                ) %>% 
                hc_legend(align= 'right') %>% 
                hc_add_theme(hc_theme_google()),
              
              
              ##### when min an == max an ::::
              
              highchart() %>%
                hc_xAxis(
                  labels=list(style=list(fontSize= "10px",fontWeight="normal"),rotation=0),
                  tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                ) %>%
                hc_yAxis_multiples(
                  list(title=list(text="Population"),opposite=FALSE,
                       min=0,max=max(hchart12m_data()$Population),
                       labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))
                  ),
                  list(title=list(text="Parc Logements"),opposite=FALSE,
                       min=0,max=max(hchart12m_data()$Population)/3,
                       labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}"))),
                  list(title=list(text="TOL"),opposite=TRUE,min=0,max=9)
                  #create_yaxis(naxis = 4, title = list(text = NULL))
                ) %>%
                hc_add_series(hchart12m_data(),type="line",hcaes(x=Annee,y=Population),name="Population") %>%
                hc_add_series(hchart12m_data(),type="line",hcaes(x=Annee,y=Parc_logement),yAxis=1,name="Parc Logements") %>%
                hc_add_series(hchart12m_data(),type="line",hcaes(x=Annee,y=TOL),yAxis=2,name="TOL") %>% 
                hc_tooltip(
                  crosshairs = TRUE,
                  backgroundColor = "#F0F0F0",
                  borderColor="#212121",
                  shared = TRUE, 
                  borderWidth = 3,
                  sort=TRUE
                ) %>% 
                hc_legend(align= 'right') %>% 
                hc_add_theme(hc_theme_google()),
              
              
              
              
         ),
         `if`(input$radio_choose_line1=="Livraisons de Logements",
              `if`((input$Id027==TRUE) | (min(input$annees)==max(input$annees)),
                   `if`(min(input$annees)!=max(input$annees),
                        `if`(length(segments_reactive())!=1,
                             hchart(daa2(),"line",hcaes(x=annee,y=liv,group=type_de_logement ))%>%
                               hc_tooltip(
                                 crosshairs = TRUE,
                                 backgroundColor = "#F0F0F0",
                                 borderColor="#212121",
                                 shared = TRUE, 
                                 borderWidth = 3,
                                 formatter=JS(paste0("function() {
        var s = '<b>'+ this.x +'</b>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ Highcharts.numberFormat(point.y,0) +'<b/>';
            sum += point.y;
            });


        s += '<br/> <b> Total </b>: '+ Highcharts.numberFormat(sum,0)
        

        return s;
    }")),sort=TRUE
                               )%>%
                               hc_chart(
                                 backgroundColor = "#ffffff"
                               )%>%
                               hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                               )%>%
                               hc_xAxis(
                                 title=list(text = ""),
                                 tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                                 
                               )%>%
                               
                               hc_legend(
                                 # align = "right",
                                 #  verticalAlign = "right",
                                 # backgroundColor='transparent',
                                 #  borderColor="#000000",
                                 # borderWidth=2,
                                 #  layout = "vertical",shadow=TRUE,
                                 #  x = 17, y = 0
                                 itemStyle=list(fontSize="15px",fontWeight= 300)
                               )%>%
                               hc_add_theme(hc_theme_flat(
                                 colors = 
                                   #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                                   line_color1()$couleur
                                 #c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                                 
                                 # 
                                 # colors=`if`(length(input$segments) %in% c(0,5),
                                 #             c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9"),
                                 #             
                                 #             c(
                                 #               `if`("LPL" %in% input$segments,"#d35400"),
                                 #               `if`("Rural" %in% input$segments,"#2980b9"),
                                 #               `if`("LSP" %in% input$segments,"#2ecc71"),
                                 #               `if`("Location-Vente" %in% input$segments,"#f1c40f"),
                                 #               `if`("LPP" %in% input$segments,"#2c3e50")
                                 #             )   )
                                 # 
                                 # 
                                 
                               )),
                             hchart(daa2(),"line",hcaes(x=annee,y=liv,group=type_de_logement))%>%
                               hc_tooltip(
                                 crosshairs = TRUE,
                                 backgroundColor = "#F0F0F0",
                                 borderColor="#212121",
                                 shared = TRUE, 
                                 borderWidth = 3
                                 ,sort=TRUE
                               )%>%
                               hc_colors(
                                 c(as.character(line_color1()$couleur))
                               ) %>%
                               hc_chart(
                                 backgroundColor = "#ffffff"
                               )%>%
                               hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                               )%>%
                               hc_xAxis(
                                 title=list(text = ""),
                                 tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                                 
                               )%>%
                               
                               hc_legend(
                                 # align = "right",
                                 #  verticalAlign = "right",
                                 # backgroundColor='transparent',
                                 #  borderColor="#000000",
                                 # borderWidth=2,
                                 #  layout = "vertical",shadow=TRUE,
                                 #  x = 17, y = 0
                                 itemStyle=list(fontSize="15px",fontWeight= 300)
                               )%>%
                               hc_add_theme(hc_theme_flat(
                                 #colors = 
                                 #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                                 # c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                                 
                                 
                                 
                               ))
                        )
                        
                        ,
                        hchart(daa2()
                               ,"column",colorByPoint=TRUE,hcaes(y=liv,x=type_de_logement))%>% #,color=c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")))%>%
                          hc_tooltip(
                            crosshairs = FALSE,
                            backgroundColor = "#F0F0F0",
                            borderColor="#212121",
                            shared = TRUE, 
                            borderWidth = 3,
                            sort=TRUE,
                            pointFormat='Livraisons: <b>{point.y}</b><br/>'
                          )%>%
                          hc_colors(
                            c(as.character(line_color1()$couleur))
                          ) %>%
                          hc_chart(
                            margin=c(0,0,30,0),
                            backgroundColor = "#ffffff",
                            options3d=list(
                              enabled=TRUE,
                              alpha=15,
                              beta=20,
                              depth=10,
                              viewDistance=110
                            )
                          )%>%
                          hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                          )%>%
                          hc_xAxis(
                            title=list(text = ""),
                            labels=list(style=list(fontSize= "13px",fontWeight="normal"),rotation=9)
                          )%>%
                          
                          hc_legend(
                            # align = "right",
                            #  verticalAlign = "right",
                            # backgroundColor='transparent',
                            #  borderColor="#000000",
                            # borderWidth=2,
                            #  layout = "vertical",shadow=TRUE,
                            #  x = 17, y = 0
                            itemStyle=list(fontSize="15px",fontWeight= 300)
                          ) %>% 
                          hc_add_theme(hc_theme_flat(
                            #colors = 
                            #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                            #line_color1()$couleur
                            #c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                            
                          ))
                   ),
                   
                   
                   
                   hchart(daa3(),"line",hcaes(x=annee,y=liv2))%>%
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
                     )%>%
                     hc_xAxis(
                       title=list(text = ""),
                       tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                       
                     )
                   
              )
              ,
              #################### here we put Lancement 
              
              ##########################################
              
              
              `if`((input$Id027==TRUE) | (min(input$annees)==max(input$annees)),
                   `if`(min(input$annees)!=max(input$annees),
                        `if`(length(segments_reactive())!=1,
                             hchart(daa2_lancement(),"line",hcaes(x=Annee,y=lanc,group=Segment ))%>%
                               hc_tooltip(
                                 crosshairs = TRUE,
                                 backgroundColor = "#F0F0F0",
                                 borderColor="#212121",
                                 shared = TRUE, 
                                 borderWidth = 3,
                                 formatter=JS(paste0("function() {
        var s = '<b>'+ this.x +'</b>',
            sum = 0;

           var sortedPoints = this.points.sort(function(a, b){
                 return ((a.y > b.y) ? -1 : ((a.y < b.y) ? 1 : 0));
             });
           $.each(sortedPoints , function(i, point) {
            s += '<br/>'+'<span",paste0('style="color:'),paste0("'"),paste0('+ point.series.color +'),paste0("'"),paste0(';"')    ,"> \u25CF </span>' + point.series.name +': '+
                '<b>'+ Highcharts.numberFormat(point.y,0) +'<b/>';
            sum += point.y;
            });


        s += '<br/> <b> Total </b>: '+ Highcharts.numberFormat(sum,0)
        

        return s;
    }")),sort=TRUE
                               )%>%
                               hc_chart(
                                 backgroundColor = "#ffffff"
                               )%>%
                               hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                               )%>%
                               hc_xAxis(
                                 title=list(text = ""),
                                 tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                                 
                               )%>%
                               
                               hc_legend(
                                 # align = "right",
                                 #  verticalAlign = "right",
                                 # backgroundColor='transparent',
                                 #  borderColor="#000000",
                                 # borderWidth=2,
                                 #  layout = "vertical",shadow=TRUE,
                                 #  x = 17, y = 0
                                 itemStyle=list(fontSize="15px",fontWeight= 300)
                               )%>%
                               hc_add_theme(hc_theme_flat(
                                 colors = 
                                   #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                                   line_color1_lanc()$couleur
                                 #c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                                 
                                 # 
                                 # colors=`if`(length(input$segments) %in% c(0,5),
                                 #             c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9"),
                                 #             
                                 #             c(
                                 #               `if`("LPL" %in% input$segments,"#d35400"),
                                 #               `if`("Rural" %in% input$segments,"#2980b9"),
                                 #               `if`("LSP" %in% input$segments,"#2ecc71"),
                                 #               `if`("Location-Vente" %in% input$segments,"#f1c40f"),
                                 #               `if`("LPP" %in% input$segments,"#2c3e50")
                                 #             )   )
                                 # 
                                 # 
                                 
                               )),
                             hchart(daa2_lancement(),"line",hcaes(x=Annee,y=lanc,group=Segment))%>%
                               hc_tooltip(
                                 crosshairs = TRUE,
                                 backgroundColor = "#F0F0F0",
                                 borderColor="#212121",
                                 shared = TRUE, 
                                 borderWidth = 3
                                 ,sort=TRUE
                               )%>%
                               hc_colors(
                                 c(as.character(line_color1_lanc()$couleur))
                               ) %>%
                               hc_chart(
                                 backgroundColor = "#ffffff"
                               )%>%
                               hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                               )%>%
                               hc_xAxis(
                                 title=list(text = ""),
                                 tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                                 
                               )%>%
                               
                               hc_legend(
                                 # align = "right",
                                 #  verticalAlign = "right",
                                 # backgroundColor='transparent',
                                 #  borderColor="#000000",
                                 # borderWidth=2,
                                 #  layout = "vertical",shadow=TRUE,
                                 #  x = 17, y = 0
                                 itemStyle=list(fontSize="15px",fontWeight= 300)
                               )%>%
                               hc_add_theme(hc_theme_flat(
                                 #colors = 
                                 #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                                 # c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                                 
                                 
                                 
                               ))
                        )
                        
                        ,
                        hchart(daa2_lancement()
                               ,"column",colorByPoint=TRUE,hcaes(y=lanc,x=Segment))%>% #,color=c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")))%>%
                          hc_tooltip(
                            crosshairs = FALSE,
                            backgroundColor = "#F0F0F0",
                            borderColor="#212121",
                            shared = TRUE, 
                            borderWidth = 3,
                            sort=TRUE,
                            pointFormat='Lancements: <b>{point.y}</b><br/>'
                          )%>%
                          hc_colors(
                            c(as.character(line_color1_lanc()$couleur))
                          ) %>%
                          hc_chart(
                            margin=c(0,0,30,0),
                            backgroundColor = "#ffffff",
                            options3d=list(
                              enabled=TRUE,
                              alpha=15,
                              beta=20,
                              depth=10,
                              viewDistance=110
                            )
                          )%>%
                          hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                          )%>%
                          hc_xAxis(
                            title=list(text = ""),
                            labels=list(style=list(fontSize= "13px",fontWeight="normal"),rotation=9)
                          )%>%
                          
                          hc_legend(
                            # align = "right",
                            #  verticalAlign = "right",
                            # backgroundColor='transparent',
                            #  borderColor="#000000",
                            # borderWidth=2,
                            #  layout = "vertical",shadow=TRUE,
                            #  x = 17, y = 0
                            itemStyle=list(fontSize="15px",fontWeight= 300)
                          ) %>% 
                          hc_add_theme(hc_theme_flat(
                            #colors = 
                            #c("#f1c40f", "#2ecc71", "#9b59b6", "#e74c3c","#004182")
                            #line_color1()$couleur
                            #c("#f1c40f","#d35400","#2c3e50","#2ecc71","#2980b9")
                            
                          ))
                   ),
                   
                   
                   
                   hchart(daa3_lancement(),"line",hcaes(x=Annee,y=lanc2))%>%
                     hc_tooltip(
                       crosshairs = TRUE,
                       backgroundColor = "#F0F0F0",
                       borderColor="#212121",
                       shared = TRUE, 
                       borderWidth = 3,
                       sort=TRUE,
                       pointFormat='Lancements : <b>{point.y}</b><br/>'
                     )%>%
                     hc_chart(
                       backgroundColor = "#ffffff"
                     )%>%
                     hc_yAxis(min=0,labels=list(formatter=JS("function() {
			return Highcharts.numberFormat(this.value, 0, '.', ' ')
		}")),title=list(text = "")
                     )%>%
                     hc_xAxis(
                       title=list(text = ""),
                       tickPositioner=JS("function() {
      var positions = [],
      ext = this.getExtremes(),
      xMax = Math.round(ext.max),
      xMin = Math.round(ext.min);
      
      if(xMax-xMin>11 && xMax-xMin<15){
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 2 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else if(xMax-xMin>14) {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
        if (i % 4 == 0 ) {
          positions.push(i);
        }
      }
      positions.push(xMax);
      return positions;    
      } else {
      positions.push(xMin);
      for (var i = xMin; i < xMax; i++) {
          positions.push(i);
      }
      positions.push(xMax);
      return positions;    
      }
      
                       
                      }")
                       
                     )
                   
              )
              
              
              
              
              
              
              
              #######################################
         )
    )
  })
  
  
  observe({
    runjs("
                   var tgh=document.getElementById('pourcentage');
tgh.parentElement.style.right='12px';
tgh.parentElement.style.top='130px';

                   var tgh2=document.getElementById('pourcentage_mr');
tgh2.parentElement.style.right='12px';
tgh2.parentElement.style.top='130px';

                   ")
  })
  
}

shinyApp(ui = ui, server = server)