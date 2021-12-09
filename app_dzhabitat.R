#setwd("C:/electron-quick-start-win32-ia32/resources/app")
#getwd()

#setwd("C:/kalafat electron/electron-quick-start-win32-ia32/resources/app")

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
library(sparkline)
library(RColorBrewer)




pdf_files_data=data.frame(
  Niveau=c(rep("Niv2:Règles de Conception de Calcul",length(list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau2")))),rep("Niv3: Règle d'Exécution",length(list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau3"))))),
  fichier=c(list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau2")),list.files(paste0(getwd(),"/pdf_files_DGCMR/niveau3")))
)

moyens_realisation <-read_excel(paste0(getwd(),"/moyens_realisations.xlsx"),skip = 1)
moyens_realisation$Arretee=as.Date(moyens_realisation$Arretee)

moyens_realisation2 <-read_excel(paste0(getwd(),"/moyens_realisations.xlsx"),sheet = "Feuil2",skip = 1)
moyens_realisation2$Arretee=as.Date(moyens_realisation2$Arretee)


etp_categoris59 <-read_excel(paste0(getwd(),"/moyens_realisations.xlsx"),sheet = "Feuil3")
etp_categoris59$Arretee=as.Date(etp_categoris59$Arretee)




datamc <- read_excel(paste0(getwd(),"/materiaux_construction.xlsx"))
datamc$`Année d'entrée en production`=as.numeric(datamc$`Année d'entrée en production`)

datamc$Arretee=as.Date(datamc$Arretee)
#datamc=datamc %>% filter(Arretee=="2018-12-31")


livraison_wilayas <- read_excel(paste0(getwd(),"/livraison_wilayas.xlsx"))
livraison_wilayas=livraison_wilayas%>%
  filter(type_de_logement %in% c( "LPL","Rural","LPP","LSP/LPA"
                                  ,"ACLS"
                                  ,"Location-Vente"))


estimation_tolpopparc <- read_excel(paste0(getwd(),"/Estimation_Population_TOL_Parc_par_Wilaya.xlsx"))
before2000 <- read_excel(paste0(getwd(),"/before2000.xlsx"))
before00=before2000

data_fiche_wilaya <- read_excel(paste0(getwd(),"/data_fiche_wilaya.xlsx"))
data_fiche_wilaya$arretee=as.Date.character(data_fiche_wilaya$arretee)



details_encours_lpl_lsp_lv_lpp <- read_excel("data_fiche_wilaya.xlsx",sheet = "details_encours_lpl_lsp_lv_lpp")
details_encours_lpl_lsp_lv_lpp$arretee=as.Date.character(details_encours_lpl_lsp_lv_lpp$arretee)


details_nonlances_lpl <- read_excel("data_fiche_wilaya.xlsx",sheet = "details_nonlances_lpl")
details_nonlances_lpl$arretee=as.Date.character(details_nonlances_lpl$arretee)

details_nonlances_lsp <- read_excel("data_fiche_wilaya.xlsx",sheet = "details_nonlances_lsp")
details_nonlances_lsp$arretee=as.Date.character(details_nonlances_lsp$arretee)


details_nonlances_rural <- read_excel("data_fiche_wilaya.xlsx",sheet = "details_nonlances_rural")
details_nonlances_rural$arretee=as.Date.character(details_nonlances_rural$arretee)

attribution_fw<-read_excel("data_fiche_wilaya.xlsx",sheet = "attribution")
attribution_fw$arretee=as.Date.character(attribution_fw$arretee)


patrimoine_fw <- read_excel("data_fiche_wilaya.xlsx",sheet = "Patrimoine")
patrimoine_fw$arretee=as.Date.character(patrimoine_fw$arretee)


etat_cession_fw <- read_excel("data_fiche_wilaya.xlsx",sheet = "Etat de la cession")
etat_cession_fw$arretee=as.Date.character(etat_cession_fw$arretee)





gt_attribution=data.frame(matrix(0,ncol=9,nrow=3))
colnames(gt_attribution)=c("Etat",unique(attribution_fw$Segment),"Total")
gt_attribution$Etat=c("Attribué","Prévu","Taux")



aides_reha=data.frame(matrix(0,ncol=1+length(unique(data_fiche_wilaya$arretee)),nrow=3))
colnames(aides_reha)=c("t",unique(as.character(data_fiche_wilaya$arretee)))
aides_reha[,1]=c("tranférées au rural","Reste à affecter","Total")


######### Tips : à inverser prochainement : 
aides_reha[,2]=c(1300,1200,95000)
aides_reha[,3]=c(1300,2948,95000)
aides_reha[,4]=c(1300,2344,95000)
aides_reha[,5]=c(1300,3275,95000)
aides_reha[,6]=c(1300,1545,95000)
aides_reha[,7]=c(1300,2948,95000)


lancement_wilayas <- read_excel(paste0(getwd(),"/lancement_wilayas.xlsx"))

lancement_wilayas=lancement_wilayas %>% 
                  #filter(Segment %in% c( "LPL","Rural","LPP","LSP/LPA","Location-Vente")) %>%
                  select(c(2,3,4,5)) %>% 
                  mutate(Annee=as.numeric(Annee))


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

dzh=c("A","M","R","O","U","C","H","E",   "M","O","H","A","M","E","D",   "N","A","D","J","I","B")

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
# da0sit=cbind(sitphy2[which(sitphy2$`Type de logements`=="LPL"),c(2,4,5,6,7,8,9,10,11,12)],sitphy2[which(sitphy2$`Type de logements`=="Rural"),4:12],sitphy2[which(sitphy2$`Type de logements`=="LSP/LPA"),4:12],sitphy2[which(sitphy2$`Type de logements`=="Location-Vente"),4:12],sitphy2[which(sitphy2$`Type de logements`=="LPP"),4:12])
# da1sit=rbind(colnames(da0sit),da0sit)
# da2sit=rbind(c("Wilaya",rep(unique(sitphy2$`Type de logements`),each=9)),da1sit)
# da2sit[2,1]="Wilaya"
# 
col_stops <- data.frame(
  q = c(0.33, 0.66, .99),
  c = c('#DF5353','#DDDF0D','#55BF3B'),
  stringsAsFactors = FALSE
)







#liv=livraison_wilayas%>%
#  filter(annee==2019,type_de_logement=="LPL")
#colnames(liv)[14]="Parc_logement2019"
#colnames(liv)[12]="Population_2019"

###################################### VILLE #########################
data_ville=data.frame(matrix(0,nrow=7,ncol=13))
data_ville$X1=c("BOUINANE","SIDI ABDELLAH","BOUGHEZOUL","DRAA ERICH","ALI MENDJLI","EL MENEAA","HASSI MESSAOUD")
colnames(data_ville)=c("Nom","Wilaya_Matricule","Wilaya","zoom","setview_long","setview_lat","cor_long","cor_lat","logements","habitants","equipements","energie","transport")
data_ville$Wilaya_Matricule=c("09-BLIDA","16-ALGER","17-26 DJELFA MEDEA","23-ANNABA","25-Constantine","47-GHARDAIA","30-Ouargla")
data_ville$Wilaya=c("BLIDA","ALGER","DJELFA MEDEA","ANNABA","Constantine","GHARDAIA","OUARGLA")


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


#HASSI MESSAOUD
data_ville[7,4]=14
data_ville[7,5:6]=c(5.84492,32.34238)
data_ville[7,7:8]=c(5.84492,32.34238)



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


#jsCode1010 <- "shinyjs.opac1010 = function(params){$('#box-score6').css('opacity', params);}"
#jsCode1111 <- "shinyjs.opac1111 = function(params){$('#button_fw5').css('opacity', params);}"


jsCode1 <- "shinyjs.opac1 = function(params){$('#well_gauge').css('opacity', params);}"
jsCode2 <- "shinyjs.opac2 = function(params){$('#well_gauge2').css('opacity', params);}"
jsCode3 <- "shinyjs.opac3 = function(params){$('#well_gauge3').css('opacity', params);}"
jsCode4 <- "shinyjs.opac4 = function(params){$('#well_gauge4').css('opacity', params);}"
jsCode5 <- "shinyjs.opac5 = function(params){$('#well_gauge5').css('opacity', params);}"
jsCode6 <- "shinyjs.opac6 = function(params){$('#well_gauge6').css('opacity', params);}"


#jsCode77 <- "shinyjs.opac77 = function(params){$('#box-score2').css('display', params);}"

jsCode88 <- "shinyjs.opac88 = function(params){$('#well1 .pretty.p-default.p-switch.p-slim').css('display', params);}"

jsCode99 <- "shinyjs.opac99 = function(params){$('.indent').css('margin-left', params);}"



# var df=document.getElementsByClassName('highcharts-data-label')
# df[2].children[0].lastElementChild.textContent="4645"   #(325 458)

# df[2].children[0].style.fontSize="0px"           # LSP/LPA 325 458 to 0px


# var arr=document.getElementsByClassName('highcharts-data-label-connector')
# arr[1].style.stroke="#ffffff"             for arrow

###################### fiche wilaya : 

ab5=data.frame(matrix(0,nrow=2,ncol=7))
colnames(ab5)=c("Segment","LPL","LSP/LPA","Rural","Location-Vente","LPP","Total")
ab5[,1]=c("Prévus","Livrés")

ab6=data.frame(matrix(0,nrow=2,ncol=7))
colnames(ab6)=c("Segment","LPL","LSP/LPA","Rural","Location-Vente","LPP","Total")
ab6[,1]=c("Prévus","Lancés")


ab=data.frame(matrix(0,nrow=8,ncol=7))
ab[,1]=c("LPL","LSP/LPA","Rural","Location-Vente","LPP","LV CNEP Banque","ACLS","Total")
colnames(ab)=c("Segment","Consistance","Acheves","En Cours","Dont A l'Arret","Non Lances","Notifie 2020")

ab_suite=data.frame(matrix(0,nrow=3,ncol=7))
ab_suite[,1]=c("Autre social","Promotionnel Libre","Programme Global")
colnames(ab_suite)=c("Segment","Consistance","Acheves","En Cours","Dont A l'Arret","Non Lances","Notifie 2020")





# df2_ab=data.frame(matrix(0,nrow=6*48,ncol=9))
# colnames(df2_ab)=c("Wilaya","Segment","Consistance","Achevés","En Cours","Dont à l'arrêt","Non Lancés","Notifie","Arrétée")
# df2_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya),6)
# #df2_ab$Arrétée="2021-12-31"
# df2_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP"),each=48)




# df2_ab=data.frame(matrix(0,nrow=6*61,ncol=9))
# colnames(df2_ab)=c("Wilaya","Segment","Consistance","Achevés","En Cours","Dont à l'arrêt","Non Lancés","Notifie","Arrétée")
# df2_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya),6)
# #df2_ab$Arrétée="2021-12-31"
# df2_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP"),each=61)



df2_ab=data.frame(matrix(0,nrow=9*61,ncol=9))
colnames(df2_ab)=c("Wilaya","Segment","Consistance","Achevés","En Cours","Dont à l'arrêt","Non Lancés","Notifie","Arrétée")
df2_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya)[c(1:58,59,60,61)],9)
#df2_ab$Arrétée="2021-12-31"
df2_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP","ACLS","Autre social","Promotionnel Libre"),each=61)



# 
# df3_ab=data.frame(matrix(0,nrow=6*48,ncol=11))
# colnames(df3_ab)=c("Wilaya",
#                    "Segment",
#                    "Livraisons Réalisés","Livraisons Prévus","Taux de Livraisons",
#                    "Lancements Réalisés","Lancements Prévus","Taux de Lancements",
#                    "Annulées","Notifié","Trimestre"
#                    )
# df3_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya),6)
# #df3_ab$Arrétée="2021-12-31"
# df3_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP"),each=48)



df3_ab=data.frame(matrix(0,nrow=9*61,ncol=11))
colnames(df3_ab)=c("Wilaya",
                   "Segment",
                   "Livraisons Réalisés","Livraisons Prévus","Taux de Livraisons",
                   "Lancements Réalisés","Lancements Prévus","Taux de Lancements",
                   "Annulées","Notifié","Saison"
)

df3_ab$Wilaya=rep(unique(data_fiche_wilaya$Wilaya)[c(1:58,59,60,61)],9)
#df3_ab$Arrétée="2021-12-31"
df3_ab$Segment=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LV CNEP BANQUE","LPP","ACLS","Autre social","Promotionnel Libre"),each=61)



ab7=data.frame(matrix(0,nrow=7,ncol=9))
colnames(ab7)=c("Segment","Livres","Prevus_livres","Taux_livraison","Lances","Prevus_lances","Taux_lancement","Annules","Notifie")
ab7[1:7,1]=c("LPL","LSP/LPA","Rural","Location-Vente","LPP","LV CNEP Banque","ACLS")


ab7_suite=data.frame(matrix(0,nrow=2,ncol=9))
colnames(ab7_suite)=c("Segment","Livres","Prevus_livres","Taux_livraison","Lances","Prevus_lances","Taux_lancement","Annules","Notifie")
ab7_suite[1:2,1]=c("Autre social","Promotionnel Libre")




ab2=data.frame(matrix(0,nrow=1,ncol=8))
colnames(ab2)=c("Segment","Consistance Actuelle","Acheves","En Cours","Non Lances","AP","Consomomations","Taux")
ab2[1,1]="Nombre d'aides"





ab3=data.frame(matrix(0,nrow=1,ncol=10))
colnames(ab3)=c("Acheves","En Cours","Non Lances","Total","Achevee","En cours","Non lances","TotaI","Acheves à 60% et plus","Total general")

ab4=data.frame(matrix(0,nrow=6,ncol=2))
#colnames(ab4)=c("Acheves","En Cours","Non Lances","Total","Achevee","En cours","Non lances","TotaI","Acheves à 60% et plus","Total general")
ab4[,1]=c("Logements achevés, viabilisation achevés","Logements achevés, viabilisation en cours","Logements acheves, viabilisation non entamée","TOTAL","Logements dépassant 60% de taux d'avancement pouvant être pré-affectés","Total logements à attribuer et à pré-affecter")
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
  #extendShinyjs(text = jsCode77, functions = c("opac77")),
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
    selected='Logements',
    navbarMenu("Logements",
               tabPanel("Logements",
                        dropdown(
                          
                          tags$h3("Filtrage des données"),
                          br(),
                          pickerInput(
                            inputId = "wilayas",
                            label = "", 
                            choices = unique(livraison_wilayas$waw),
                            options = list(`actions-box` = TRUE,style = "btn-primary",
                                           size = 23,
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
                                                    tags$span(class="title","Livraison des logements",style="display:inline-block;"),textOutput("titre_livraison"),
                                                    tags$h3(class="metric",textOutput("livraisons")),
                                                    tags$span(class="color__nb",htmlOutput("taux_livraisons"),style="color:green;display: inline-block;"),textOutput("dernier_an") #style=paste0("color:",ifelse(34>0,"green","red"))
                                 ),
                                 
                                 
                                 
                                 tags$head(HTML('<style type="text/css">


#mc_wilaya_reactable {
    width: auto;
    height: auto;
    visibility: inherit;
    margin-top: 64px;
    height: 450px;
}


#gt7_suite {
    width: 1060px;
    height: auto;
    visibility: inherit;
    margin-top: -35px;
}

#button_fw1_divhover7 {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1_divhover7:hover {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}


#button_fw1_divhover6 {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1_divhover6:hover {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}


#button_fw1_divhover5 {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1_divhover5:hover {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}

                     
#button_fw1_divhover4 {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1_divhover4:hover {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}
                     
                     
                                 
#button_fw1_divhover3 {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1_divhover3:hover {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}
                                 
                                 
#button_fw1_divhover2 {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1_divhover2:hover {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}



#button_fw1_divhover1 {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1_divhover1:hover {
    top: 12px;
    right: -319px;
    z-index: 99;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}

div#modal_fw1_divhover1 {
    zoom: 1.1;
}

div#modal_fw1_divhover2 {
    zoom: 1.1;
}

div#modal_fw1_divhover3 {
    zoom: 1.1;
}

div#modal_fw1_divhover4 {
    zoom: 1.1;
}

div#modal_fw1_divhover5 {
    zoom: 1.1;
}

div#modal_fw1_divhover6 {
    zoom: 1.1;
}

div#modal_fw1_divhover7 {
    zoom: 1.1;
}
div#modal_fw1 {
    zoom: 1.1;
}

div#modal_fw7 {
    zoom: 1.07;
}


div#modal_fw2 {
    zoom: 1.07;
}

div#modal_fw_attribution {
    zoom: 1.07;
}


div#modal_fw8 {
    zoom: 1.07;
}

div#modal_fw9 {
    zoom: 1.07;
}






div#modal_fw3 {
    zoom: 1.07;
}

div#modal_fw4 {
    zoom: 1.17;
}

div#modal_fw5 {
    zoom: 1.17;
}


#button_fw1 {
    position: absolute;
    top: 95px;
    right: 180px;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw1:hover {
    position: absolute;
    top: 95px;
    right: 180px;
    opacity: 1;
    zoom: 0.9;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}

#button_fw7 {
    position: relative;
    top: -566px;
    right: -1263px;
    opacity: 1;
    zoom: 0.8;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}


#button_fw7:hover {
    position: relative;
    top: -566px;
    right: -1263px;
    opacity: 1;
    zoom: 0.8;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}

#button_fw_attribution {
    position: relative;
    top: 69px;
    right: -1477px;
    opacity: 1;
    zoom: 0.8;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}


#button_fw_attribution:hover {
    position: relative;
    top: 69px;
    right: -1477px;
    opacity: 1;
    zoom: 0.8;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}





#button_fw2 {
    position: relative;
    top: 69px;
    right: -1500px;
    opacity: 1;
    zoom: 0.8;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}


#button_fw2:hover {
    position: relative;
    top: 69px;
    right: -1500px;
    opacity: 1;
    zoom: 0.8;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}

#button_fw8 {
    opacity: 1;
    zoom: 0.80;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw8:hover {
    opacity: 1;
    zoom: 0.80;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}



#button_fw9 {
    opacity: 1;
    zoom: 0.80;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw9:hover {
    opacity: 1;
    zoom: 0.80;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}




#button_fw3 {
    position: relative;
    top: -200px;
    right: -1400px;
    opacity: 1;
    zoom: 0.80;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw3:hover {
    position: relative;
    top: -200px;
    right: -1400px;
    opacity: 1;
    zoom: 0.80;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}

#button_fw4 {
    position: relative;
    top: -227px;
    right: -899px;
    opacity: 1;
    zoom: 0.90;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw4:hover {
    position: relative;
    top: -227px;
    right: -899px;
    opacity: 1;
    zoom: 0.90;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}



#button_fw5 {
    position: relative;
    top: -225px;
    right: -903px;
    opacity: 1;
    zoom: 0.90;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#button_fw5:hover {
    position: relative;
    top: -225px;
    right: -903px;
    opacity: 1;
    zoom: 0.90;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}






#modal_homepage3 {
    z-index: 99999;
} 

#modal_homepage3 .modal-footer{
display:none
}


#homepage_button1 {
    opacity:0;
    position:absolute;
    top: 587px;
    z-index: 99999;
    right: 1363px;
    padding: 82px 86px;
    border: 1px solid #d8be68;
    background-color: #0b2535;
    color: antiquewhite;
    font-size: 18px;
    font-family: "Open Sans", sans-serif;
    letter-spacing: 3px;
    border-radius: 4px;
}


#homepage_button1:hover {
    position:absolute;
    top: 587px;
    z-index: 99999;
    right: 1363px;
    padding: 82px 86px;
    border: 1px solid #d8be68;
    background-color: #d8be68;
    color: #0b2535;
    font-size: 18px;
    font-family: "Open Sans", sans-serif;
    letter-spacing: 3px;
    border-radius: 4px;
}   
                                                



#homepage_button2 {
    opacity:0;
    position:absolute;
    top: 587px;
    z-index: 99999;
    right: 763px;
    padding: 82px 86px;
    border: 1px solid #d8be68;
    background-color: #0b2535;
    color: antiquewhite;
    font-size: 18px;
    font-family: "Open Sans", sans-serif;
    letter-spacing: 3px;
    border-radius: 4px;
}


#homepage_button2:hover {
    position:absolute;
    top: 587px;
    z-index: 99999;
    right: 763px;
    padding: 82px 86px;
    border: 1px solid #d8be68;
    background-color: #d8be68;
    color: #0b2535;
    font-size: 18px;
    font-family: "Open Sans", sans-serif;
    letter-spacing: 3px;
    border-radius: 4px;
}  
                                                



#homepage_button3 {
    opacity:0;
    position:absolute;
    top: 587px;
    z-index: 99999;
    right: 100px;
    padding: 70px 86px;
    border: 1px solid #d8be68;
    background-color: #0b2535;
    color: antiquewhite;
    font-size: 18px;
    font-family: "Open Sans", sans-serif;
    letter-spacing: 3px;
    border-radius: 4px;
    text-align:initial;
}


#homepage_button3:hover {
    position:absolute;
    top: 587px;
    z-index: 99999;
    right: 100px;
    padding: 70px 86px;
    border: 1px solid #d8be68;
    background-color: #d8be68;
    color: #0b2535;
    font-size: 18px;
    font-family: "Open Sans", sans-serif;
    letter-spacing: 3px;
    border-radius: 4px;
    text-align:initial;

}  
      

                                                
                                                
                                                
#modal_wilayas58 {
zoom:1.11;
}

#organisme_fluidrow {
zoom:0.89;
}

#reactable_58wilayas {
padding-left:200px;
}


#sasa {
font-size:22px;
}


span.indent {
    margin-left: -32px;
    margin-right: 10px;
}

#nd_mc_maps{
color:darkred;
}

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
  width:910px;
}

#box-score-title_equip {
    margin-top: 26px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 18px;
    font-weight: 400;
    width: 909px;
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

#boxscore7_arretee{
    display: inline;
}



#boxscore2_arretee{
    display: inline;
}

#boxscore_attribution_arretee{
    display: inline;
}

  



#boxscore3_arretee{
    display: inline;
}

#boxscore8_arretee{
    display: inline;
    margin-right:320px;
}

#boxscore9_arretee{
    display: inline;
}




#boxscore5_arretee{
    display: inline;
}


#boxscore6_arretee{
    display: inline;
}



#boxscore1_wilaya {
    display: inline;
    padding-left: 238px;
}

#boxscore1_wilaya_mr {
    display: inline;
    padding-left: 306px;
}


#boxscore2_wilaya_mr {
    display: inline;
    padding-left: 170px;
}


#boxscore3_wilaya_mr {
    display: inline;
    padding-left: 160px;
}

#boxscore4_wilaya_mr {
    display: inline;
    padding-left: 149px;
}

#boxscore5_wilaya_mr {
    display: inline;
    padding-left: 309px;
}



#boxscore6_wilaya_mr {
    display: inline;
    padding-left: 150px;
}

#boxscore7_wilaya {
    display: inline;
    padding-left: 316px;
}


#boxscore2_wilaya {
    display: inline;
    padding-left: 560px;
}

#boxscore_attribution_wilaya {
    display: inline;
    padding-left: 200px;
}


#boxscore9_wilaya {
    display: inline;
    padding-left: 250px;
    padding-right: 380px;

}



#boxscore3_wilaya {
    display: inline;
    padding-left: 250px;
}


#boxscore5_wilaya {
    display: inline;
    padding-left: 250px;
}


#boxscore6_wilaya {
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



#box-score9 {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
}

#box-score-title9 {
    margin-top: 24px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1278px;
    font-family: system-ui;
    }





#box-score8 {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
}

#box-score-title8 {
    margin-top: 24px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 772px;
    font-family: system-ui;
    }






#box-score7 {
    padding-left: 105px;
    margin-bottom: 50px;
    margin-top: 93px;
    font-family: "Roboto", Helvetica, Arial, sans-serif;

}

#box-score-title7 {
    margin-top: -1px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1050px;
    font-family: system-ui;
    }



#box-score_attribution {
    padding-left: 0px;
    margin-bottom: 50px;
    font-family: "Roboto", Helvetica, Arial, sans-serif;

}


#box-score2 {
    padding-left: 0px;
    margin-bottom: 50px;
    font-family: "Roboto", Helvetica, Arial, sans-serif;

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
    padding: 7px;
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
    width: 920px;
    font-family: system-ui;
}


#box-score-title3_mr {
    margin-top: 24px;
    padding: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1021px;
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
    width: 1161px;
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
  margin-bottom:-3px;
}

#box-score1_mr {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:28px;
  margin-bottom:100px;
}




#box-score2_mr {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:176px;
  margin-bottom:80px;
}

#box-score3_mr {
    font-family: "Roboto", Helvetica, Arial, sans-serif;
    padding-left: 126px;
    margin-bottom: 80px;
    margin-left: -7px;
}

#box-score4_mr {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:131px;
  margin-bottom:80px;
}

#box-score5_mr {
    font-family: "Roboto", Helvetica, Arial, sans-serif;
    padding-left: 40px;
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



#box-score6 {
  font-family: "Roboto", Helvetica, Arial, sans-serif;
  padding-left:185px;
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


#select_title_gt7 + .dropdown-toggle {
    background-color: #81a47b;
    border-color: transparent;
}





#select_arretee_fichewilaya + .dropdown-toggle:hover {
    background-color: #5a7256;
    border-color: black;
}
    


                  
#select_arretee_fichewilaya + .dropdown-toggle {
    background-color: #007aff;
    border-color: lightskyblue;
}




    
#select_arretee_fichewilaya_mr + .dropdown-toggle {
    background-color: #007aff;
    border-color: lightskyblue;
}


#fluidrow1_datamc + .shiny-input-container {
    position: absolute;
    top: 21px;
    z-index: 999999;
    right: 128px;
}

#select_arretee_datamc + .dropdown-toggle {
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

#select_arretee_datamc + .dropdown-toggle:hover {
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



#display_when_hover_choose_leaflet_agrement {
    position: absolute;
    top: 138px;
    right: 514px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 224px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 318px;
    display: none;
    zoom: 0.9;
}

#display_when_hover_choose_leaflet_agrement_prom {
    position: absolute;
    top: 138px;
    right: 514px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 224px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 134px;
    display: none;
    zoom: 0.9;
}


#display_when_hover_choose_leaflet_agrement_agence {
    position: absolute;
    top: 138px;
    right: 514px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 224px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 145px;
    display: none;
    zoom: 0.9;
}

#display_when_hover_choose_leaflet_agrement_ing {
    position: absolute;
    top: 138px;
    right: 457px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 281px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 134px;
    display: none;
    zoom: 0.9;
}


#display_when_hover_choose_leaflet_agrement_ai {
    position: absolute;
    top: 138px;
    right: 514px;
    z-index: 999999;
    background: white;
    border: 3px solid darkgrey;
    width: 224px;
    color: black;
    font-family: unset;
    font-size: 13px;
    height: 170px;
    display: none;
    zoom: 0.9;
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


#choose_leaflet_agrement {
    position: absolute;
    top: 139px;
    right: 694px;
    z-index: 99999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
    zoom: 0.9;
}


#choose_leaflet_agrement_prom {
    position: absolute;
    top: 139px;
    right: 694px;
    z-index: 99999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
    zoom: 0.9;
}

#choose_leaflet_agrement_agence {
    position: absolute;
    top: 139px;
    right: 694px;
    z-index: 99999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
    zoom: 0.9;
}

#choose_leaflet_agrement_ing {
    position: absolute;
    top: 139px;
    right: 694px;
    z-index: 99999;
    background: white;
    border: 3px solid darkgrey;
    width: 43px;
    zoom: 0.9;
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



#divhover_encours_lpp_choose_line1 {
   position: absolute;
    top: 379px;
    right: 696px;
    z-index: 999;
    background: white;
    width: 35px;
    opacity: 1;
    padding-right: 0;
  }

#divhover_encours_lpp_choose_line1 .glyphicon-indent-left{
  color: black;
  margin-left: -3px;
}

#display_when_hover_divhover_encours_lpp_choose_line1 {
    position: absolute;
    top: 379px;
    right: 696px;
    z-index: 999999999;
    background: white;
    border: 5px solid darkgrey;
    width: 342px;
    color: black;
    font-family: auto;
    height: 262px;
    display:none;
}




#divhover_encours_lv_choose_line1 {
   position: absolute;
    top: 294px;
    right: 696px;
    z-index: 999;
    background: white;
    width: 35px;
    opacity: 1;
    padding-right: 0;
  }

#divhover_encours_lv_choose_line1 .glyphicon-indent-left{
  color: black;
  margin-left: -3px;
}

#display_when_hover_divhover_encours_lv_choose_line1 {
    position: absolute;
    top: 294px;
    right: 696px;
    z-index: 999999999;
    background: white;
    border: 5px solid darkgrey;
    width: 342px;
    color: black;
    font-family: auto;
    height: 262px;
    display:none;
}




#divhover_encours_lsp_choose_line1 {
   position: absolute;
    top: 212px;
    right: 696px;
    z-index: 999;
    background: white;
    width: 35px;
    opacity: 1;
    padding-right: 0;
  }

#divhover_encours_lsp_choose_line1 .glyphicon-indent-left{
  color: black;
  margin-left: -3px;
}

#display_when_hover_divhover_encours_lsp_choose_line1 {
    position: absolute;
    top: 212px;
    right: 696px;
    z-index: 999999999;
    background: white;
    border: 5px solid darkgrey;
    width: 342px;
    color: black;
    font-family: auto;
    height: 262px;
    display:none;
}






#divhover_encours_lpl_choose_line1 {
   position: absolute;
    top: 163px;
    right: 696px;
    z-index: 999;
    background: white;
    width: 35px;
    opacity: 1;
    padding-right: 0;
  }

#divhover_encours_lpl_choose_line1 .glyphicon-indent-left{
  color: black;
  margin-left: -3px;
}

#display_when_hover_divhover_encours_lpl_choose_line1 {
    position: absolute;
    top: 163px;
    right: 696px;
    z-index: 999999999;
    background: white;
    border: 5px solid darkgrey;
    width: 342px;
    color: black;
    font-family: auto;
    height: 262px;
    display:none;
}







#divhover_nonlances_rural_choose_line1 {
   position: absolute;
   top: 254px;
   right: 390px;
   z-index: 999;
   background: white;
   width: 51px;
   opacity: 1;
   padding-right: 0;
  }

#divhover_nonlances_rural_choose_line1 .glyphicon-indent-left{
  color: black;
  margin-left: -3px;
}

#display_when_hover_divhover_nonlances_rural_choose_line1 {
    position: absolute;
    top: 254px;
    right: 295px;
    z-index: 999999999;
    background: white;
    border: 5px solid darkgrey;
    width: 342px;
    color: black;
    font-family: auto;
    height: 533px;
    display:none;
}



#divhover_nonlances_lsp_choose_line1 {
   position: absolute;
   top: 209px;
   right: 390px;
   z-index: 999;
   background: white;
   width: 51px;
   opacity: 1;
   padding-right: 0;
  }

#divhover_nonlances_lsp_choose_line1 .glyphicon-indent-left{
  color: black;
  margin-left: -3px;
}

#display_when_hover_divhover_nonlances_lsp_choose_line1 {
    position: absolute;
    top: 209px;
    right: 295px;
    z-index: 999999999;
    background: white;
    border: 5px solid darkgrey;
    width: 342px;
    color: black;
    font-family: auto;
    height: 679px;
    display:none;
}



#divhover_nonlances_lpl_choose_line1 {
   position: absolute;
   top: 163px;
   right: 390px;
   z-index: 999;
   background: white;
   width: 51px;
   opacity: 1;
   padding-right: 0;
  }

#divhover_nonlances_lpl_choose_line1 .glyphicon-indent-left{
  color: black;
  margin-left: -3px;
}

#display_when_hover_divhover_nonlances_lpl_choose_line1 {
position: absolute;
    top: 163px;
    right: 295px;
    z-index: 999999999;
    background: white;
    border: 5px solid darkgrey;
    width: 342px;
    color: black;
    font-family: auto;
    height: 597px;
    display:none;
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

#choose_leaflet_agrement .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}


#choose_leaflet_agrement_prom .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}

#choose_leaflet_agrement_agence .glyphicon-th-list{
  color: black;
  margin-left: -3px;
}

#choose_leaflet_agrement_ing .glyphicon-th-list{
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
    z-index: 999;
    right: 945px;
    top:7px;
}


#box-score-title7 .shiny-input-container {
    width: 470px;
    zoom: 1;
    display: inline-block;
    margin-bottom: -5px;
    margin-left: -5px;
}

#box-score-title7 .filter-option {
    font-size: 22px;
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
    padding-left: 241px;
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

#tabmodalserie1_search {
    position: absolute;
    top: 48px;
    right: 259px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white
}

#tabmodalserie1_search:hover {
    position: absolute;
    top: 48px;
    right: 259px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}






#tabmodalserie1_mapst {
    position: absolute;
    top: 48px;
    right: 137px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white
}

#tabmodalserie1_mapst:hover {
    position: absolute;
    top: 48px;
    right: 137px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}




#tabmodalserie2_mapst {
    position: absolute;
    top: 460px;
    right: 230px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white
}

#tabmodalserie2_mapst:hover {
    position: absolute;
    top: 460px;
    right: 230px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}


#tabmodalserie3_mapst {
    position: absolute;
    top: 683px;
    right: 193px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white
}

#tabmodalserie3_mapst:hover {
    position: absolute;
    top: 683px;
    right: 193px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}



#tabmodalserie4_mapst {
    position: absolute;
    top: 906px;
    right: 189px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white
}

#tabmodalserie4_mapst:hover {
    position: absolute;
    top: 906px;
    right: 189px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}




#tabmodalserie5_mapst {
    position: absolute;
    top: 1130px;
    right: 150px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white
}

#tabmodalserie5_mapst:hover {
    position: absolute;
    top: 1130px;
    right: 150px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}





#tabmodalserie_maps {
    position: absolute;
    top: 48px;
    right: 197px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white
}

#tabmodalserie_maps:hover {
    position: absolute;
    top: 48px;
    right: 197px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}


#tabmodalserie_maps_prom {
    position: absolute;
    top: 460px;
    right: 287px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#tabmodalserie_maps_prom:hover {
    position: absolute;
    top: 460px;
    right: 287px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}


#tabmodalserie_maps_agence {
    position: absolute;
    top: 683px;
    right: 258px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#tabmodalserie_maps_agence:hover {
    position: absolute;
    top: 683px;
    right: 258px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}


#tabmodalserie_maps_ing {
    position: absolute;
    top: 906px;
    right: 238px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#tabmodalserie_maps_ing:hover {
    position: absolute;
    top: 906px;
    right: 238px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
}



#tabmodalserie_maps_ai {
    position: absolute;
    top: 1131px;
    right: 205px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: white;
}

#tabmodalserie_maps_ai:hover {
    position: absolute;
    top: 1131px;
    right: 205px;
    z-index: 999999;
    border-color: black;
    font-size: 16px;
    color: black;
    background: lightgray;
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
    font-size: 14px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -14px;
    display: flex;
}

.chart-title-urbanisme2 {
    width: 106%;
    height: 20px;
    border-bottom: 1px solid #d7d7d7;
    color: #666;
    font-size: 14px;
    font-weight: 300;
    margin-left: -20px;
    margin-top: -14px;
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
                                ,excelOutput("excel1",height="500px")),
                        
                        bsModal("modal2",htmlOutput("tablededonnes2"), "tabmodalserie2", size = "large"
                                ,excelOutput("excel2")),
                        
                        bsModal("modal3",htmlOutput("tablededonnes3"), "tabmodalserie3", size = "large"
                                ,excelOutput("excel3")),
                        textOutput('verification_leaflet')
               ),
               ######### fin tabpanel Logements
               
               
               #################################################### FICHE WILAYA : 
               
               tabPanel("Fiche Wilaya",
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
                                               div(id = "box-score-title1", "État d'Exécution du Programme - Situation au ",textOutput("boxscore1_arretee"),textOutput("boxscore1_wilaya")),
                                               reactableOutput('gt1',width = '1082px'),
                                               actionBttn(
                                                 inputId = "button_fw1",
                                                 label = NULL,
                                                 style = "bordered", 
                                                 color = "success",
                                                 icon = icon("table")
                                               ),uiOutput("divhover_nonlances_lpl"),
                                                uiOutput("divhover_nonlances_lsp"),
                                                uiOutput("divhover_nonlances_rural"),
                                                   uiOutput("divhover_encours_lpl"),
                                                   uiOutput("divhover_encours_lsp"),
                                                   uiOutput("divhover_encours_lv"),
                                                   uiOutput("divhover_encours_lpp"),
                                               reactableOutput('gt1_suite',width = '1082px')
                                               
                                               
                                           ),
                                           div(id = "box-score7",
                                               
                                               uiOutput("boxscore7_ui"),
                                              
                                               reactableOutput('gt7',width = '1060px'),
                                               
                                               actionBttn(
                                                 inputId = "button_fw7",
                                                 label = NULL,
                                                 style = "bordered", 
                                                 color = "success",
                                                 icon = icon("table")
                                               ),
                                               reactableOutput('gt7_suite',width = '1060px')
                                               
                                           ),
                                           
                                           uiOutput("ui_boxscore2"),
                                           
                                           uiOutput("ui_boxscore_attribution"),
                                           
                                           
                                           # div(id = "box-score2",
                                           #     actionBttn(
                                           #       inputId = "button_fw2",
                                           #       label = NULL,
                                           #       style = "bordered", 
                                           #       color = "success",
                                           #       icon = icon("table")
                                           #     ),
                                           #     div(id = "box-score-title2", "Aides à la réhabilitation au ",textOutput("boxscore2_arretee"),textOutput("boxscore2_wilaya")),
                                           #     reactableOutput('gt2',width = '860px')
                                           #  
                                           # ),
                                           
                                           
                                           uiOutput("ui_boxscore3"),
                                           uiOutput("ui_boxscore8"),
                                           uiOutput("ui_boxscore9"),
                                           
                                           
                                           
                                           # div(class = "box-score",
                                           #     div(class = "box-score-title", "État des Livraisons au ",textOutput("boxscore5_arretee"),textOutput("boxscore5_wilaya")),
                                           #     reactableOutput('gt5'),
                                           #     actionBttn(
                                           #       inputId = "button_fw4",
                                           #       label = NULL,
                                           #       style = "bordered", 
                                           #       color = "success",
                                           #       icon = icon("table")
                                           #     )
                                           # ),
                                           # uiOutput("ui_boxscore6"),
                                        
                                           bsModal("modal_fw1", htmlOutput("tablededonnes1_fw1"), "button_fw1", size = "large"
                                                   ,excelOutput("excel_fw1")),
                                           
                                           bsModal("modal_fw7", htmlOutput("tablededonnes1_fw7"), "button_fw7", size = "large"
                                                   ,excelOutput("excel_fw7")),
                                           
                                           # bsModal("modal_fw2", htmlOutput("tablededonnes1_fw2"), "button_fw2", size = "large"
                                           #         ,excelOutput("excel_fw2")),
                                           # bsModal("modal_fw3", htmlOutput("tablededonnes1_fw3"), "button_fw3", size = "large"
                                           #         ,excelOutput("excel_fw3"))
                                           # bsModal("modal_fw4", htmlOutput("tablededonnes1_fw4"), "button_fw4", size = "large"
                                           #         ,excelOutput("excel_fw4")),
                                           # bsModal("modal_fw5", htmlOutput("tablededonnes1_fw5"), "button_fw5", size = "large"
                                           #         ,excelOutput("excel_fw5"))
                                           
                                           #bsModal("modal_eqp", htmlOutput("tablededonnes1_eqp"), "tabmodalserie_eqp", size = "large"
                                           #,excelOutput("excel_eqp")),
                                           
                                 )
                          )
                        ),
                        pickerInput(
                          inputId = "select_arretee_fichewilaya",
                          label = "", 
                          choices = sort(paste(c("Arrétée le :"),unique(data_fiche_wilaya$arretee)),TRUE),
                          #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
                        )
               )
               
               
               
               ################################################### FICHE WILAYA Fin
               #, tabPanel("Situation Physique des Logements",
               #          fluidRow(
               #            id="tabsitphy",
               #            
               #            column(6,id="c1",
               #                   # 
               #                   # awesomeCheckboxGroup(
               #                   #   inputId = "select_segment_gauge",
               #                   #   label = "Segments : ", 
               #                   #   choices = c("Rural", "LPL", "Location-Vente","LSP/LPA","LPP"),
               #                   #   inline = TRUE, 
               #                   #   status = "success"
               #                   # ),
               #                   # 
               #                   actionBttn(
               #                     inputId = "tabmodal_sitphy",
               #                     label = NULL,
               #                     style = "bordered", 
               #                     color = "success",
               #                     icon = icon("table")
               #                   ),
               #                   checkboxGroupButtons(
               #                     inputId = "select_segment_gauge",
               #                     choices = c("Rural", "LPL", "Location-Vente","LSP/LPA","LPP"),
               #                     status = "primary",
               #                     checkIcon = list(
               #                       yes = icon("ok", 
               #                                  lib = "glyphicon")
               #                     ),
               #                     width = "150%"
               #                   ),
               #                   
               #                   
               #                   
               #                   reactableOutput("table2")
               #            ),
               #            column(3,id="cg2",
               #                   wellPanel(id="well_gauge",
               #                             tags$div(class="chart-title-gauge",tags$p(id="titregauge1",textOutput("toutsegments_gauge"),textOutput("wilayaselectgauge1")#textOutput("wgauge")
               #                             )),
               #                             highchartOutput("gauge1",height = "247px"),tags$p(id="livraison_gauge1","Livraison"),tags$span(id="livraison_gauge1_val",textOutput("val_livraison_gauge1")),tags$p(id="prevision_gauge1","Prévision"),tags$span(id="prevision_gauge1_val",textOutput("val_prevision_gauge1"))
               #                             
               #                             
               #                             #,verbatimTextOutput("selected")
               #                   ),
               #                   wellPanel(id="well_gauge2",
               #                             tags$div(class="chart-title-gauge2",tags$p(id="titregauge2","LPL",textOutput("wilayaselectgauge2"))),
               #                             highchartOutput("gauge2",height = "247px"),tags$p(id="livraison_gauge2","Livraison"),tags$span(id="livraison_gauge2_val",textOutput("val_livraison_gauge2")),tags$p(id="prevision_gauge2","Prévision"),tags$span(id="prevision_gauge2_val",textOutput("val_prevision_gauge2"))
               #                             
               #                             
               #                   ),
               #                   wellPanel(id="well_gauge3",
               #                             tags$div(class="chart-title-gauge3",tags$p(id="titregauge3","LSP/LPA",textOutput("wilayaselectgauge3"))),
               #                             highchartOutput("gauge3",height = "247px"),tags$p(id="livraison_gauge3","Livraison"),tags$span(id="livraison_gauge3_val",textOutput("val_livraison_gauge3")),tags$p(id="prevision_gauge3","Prévision"),tags$span(id="prevision_gauge3_val",textOutput("val_prevision_gauge3"))
               #                             
               #                             
               #                             #,verbatimTextOutput("selected")
               #                   )
               #                   
               #            ),
               #            column(3,   #3eme colonne
               #                   
               #                   wellPanel(id="well_gauge4",
               #                             tags$div(class="chart-title-gauge4",tags$p(id="titregauge4","Rural",textOutput("wilayaselectgauge4"))),
               #                             highchartOutput("gauge4",height = "247px"),tags$p(id="livraison_gauge4","Livraison"),tags$span(id="livraison_gauge4_val",textOutput("val_livraison_gauge4")),tags$p(id="prevision_gauge4","Prévision"),tags$span(id="prevision_gauge4_val",textOutput("val_prevision_gauge4"))
               #                             
               #                             
               #                             #,verbatimTextOutput("selected")
               #                   ),
               #                   
               #                   #column(5,id="cgauge4",
               #                   wellPanel(id="well_gauge5",
               #                             tags$div(class="chart-title-gauge5",tags$p(id="titregauge5","Location-Vente",textOutput("wilayaselectgauge5"))),
               #                             highchartOutput("gauge5",height = "247px"),tags$p(id="livraison_gauge5","Livraison"),tags$span(id="livraison_gauge5_val",textOutput("val_livraison_gauge5")),tags$p(id="prevision_gauge5","Prévision"),tags$span(id="prevision_gauge5_val",textOutput("val_prevision_gauge5"))
               #                             
               #                             
               #                             #,verbatimTextOutput("selected")
               #                   ),
               #                   wellPanel(id="well_gauge6",
               #                             tags$div(class="chart-title-gauge6",tags$p(id="titregauge6","LPP",textOutput("wilayaselectgauge6"))),
               #                             highchartOutput("gauge6",height = "247px"),tags$p(id="livraison_gauge6","Livraison"),tags$span(id="livraison_gauge6_val",textOutput("val_livraison_gauge6")),tags$p(id="prevision_gauge6","Prévision"),tags$span(id="prevision_gauge6_val",textOutput("val_prevision_gauge6"))
               #                             
               #                             
               #                             #,verbatimTextOutput("selected")
               #                   )
               #                   
               #            ),
               #            
               #            bsModal("modal_sitphy",htmlOutput("tablededonnes1_sitphy"), "tabmodal_sitphy", size = "large"
               #                    ,excelOutput("excel_sitphy"))
               #          ),
               #          pickerInput(
               #            inputId = "select_arretee_sitphy",
               #            label = "", 
               #            choices = sort(paste(c("Arrétée le :"),unique(sitphy$Arretee)),TRUE),
               #            #c('Arretee le : 2020-06-30','Arretee le : 2019-12-31'),
               #          )
               # )
               
               
               
    ),
    
    
    
    ########## fin navmenu logts
    
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
                                  tags$div(class="chart-title-urbanisme",HTML('&nbsp;'),HTML('&nbsp;'),"Lotissement sociaux : ",HTML('&nbsp;'),
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
               choices = c('Arrétée le : 2019-12-31'),
               #c('Arretee le : 2020-06-30','Arretee le : 2019-12-31'),
             ),
             
             
             bsModal("modal_urbanisme1","Table de données", "tabmodalserie_urbanisme1", size = "large"
                     ,excelOutput("excel_urbanisme1")),
             
             bsModal("modal_urbanisme2","Table de données", "tabmodalserie_urbanisme2", size = "large"
                     ,excelOutput("excel_urbanisme2")),
             
             bsModal("modal_urbanisme3","Table de données", "tabmodalserie_urbanisme3", size = "large"
                     ,excelOutput("excel_urbanisme3")),
             
             bsModal("modal_urbanisme4","Table de données", "tabmodalserie_urbanisme4", size = "large"
                     ,excelOutput("excel_urbanisme4"))
             
             
             
             
    ),
    
    
    
    
    
    tabPanel("Equipements Publics",
             fluidRow(
               id="fluidrow1_eqp",
               
               column(6,style="width:780px;",     #id="cequip",
                      reactableOutput("equip_reactable",width = "755px")
                      
               ),
               column(6,style="box-shadow:4px 5px 11px grey;background-color:white;height:828px;width:933px;",
                      tags$div(class="chart-title-eqp",HTML('&nbsp;'),HTML('&nbsp;'),tags$p("Nombre de Projets Par Secteur : "),HTML('&nbsp;'),HTML('&nbsp;'),
                               prettySwitch(
                                 inputId = "pourcentage_equip",
                                 label = "Pourcentage",
                                 status = "primary",
                                 slim = TRUE,
                                 value=FALSE
                               ),
                               HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),textOutput("wilayaselecteqp"),
                               actionBttn(
                                 inputId = "tabmodalserie_eqp",
                                 label = NULL,
                                 style = "bordered", 
                                 color = "success",
                                 icon = icon("table")
                               )
                      ),
                      #HTML('<span style="color:green;" &#9679;<span/>'),
                      tags$div(id="divequip",style="margin-bottom:-26px;margin-top:-7px;text-align:right",
                               
                               tags$span("\u25CF",style="font-size:25px;color:#5F9E4F"),tags$span("Achevés",style=";padding-right:15px;"),
                               tags$span("\u25CF",style="font-size:25px;color:rgb(239, 192, 0)"),tags$span("En Cours",style=";padding-right:15px;"),
                               tags$span("\u25CF",style="font-size:25px;color:rgb(231, 25, 25)"),tags$span("Non Lancés",style=";padding-right:15px;"),
                               tags$span("\u25CF",style="font-size:25px;color:#A9A9A9"),tags$span("Dont NIR",style=";padding-right:15px;"),
                               tags$span("\u25CF",style="font-size:25px;color:#101010"),tags$span("Gelés",style=";padding-right:15px;")
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
               choices = c('Arrétée le : 2020-06-30','Arrétée le : 2019-12-31')
             )
             
    ),
    tabPanel(   HTML('
          <span style="opacity:0" class="glyphicon glyphicon-home"></span>
                     '),
                
                fluidRow(
                  id="fluidrow1_home",
                  wellPanel(id="well_homepage",
                            style=
                              '    min-height: 20px;
    padding: 19px;
    border-radius: 4px;
    -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
    border: 0px;
    background-image: url(Bay_of_Algiers_by_night_2.jpg);
    height: 836px;
    background-size: cover;
    background-attachment: fixed;
    margin-left: -20px;
    margin-right: -15px;
    margin-top: -21px;
    margin-bottom:0px;
    height: 855px;' ,
                            #includeHTML(paste0(getwd(),'/www/index2.html'))
                            #,
                            #tags$head(includeScript(paste0(getwd(),'/www/script2.js'))),
                            #tags$head(includeCSS(paste0(getwd(),'/www/style2.css')))
                            
                            #htmlTemplate(paste0(getwd(),"/www/index2.html"))
                            # ,
                            # includeCSS(paste0(getwd(),'/www/style2.css')),
                            # includeScript(paste0(getwd(),'/www/script2.js'))
                            
                            htmlOutput("homepage_ren"),
                            useShinyjs(),
                            
                            actionBttn(
                              inputId = "homepage_button1",
                              label = "Organigramme",
                              style = "bordered", 
                              color = "primary"
                            ),
                            tags$a(href="http://www.mhuv.gov.dz/fr/accueil/",target="_blank",
                                   actionBttn(
                                     inputId = "homepage_button2",
                                     label = "Site Web",
                                     style = "bordered", 
                                     color = "primary"
                                   )),
                            actionBttn(
                              inputId = "homepage_button3",
                              label = HTML("Organismes <br> sous tutelle"),
                              style = "bordered", 
                              color = "primary"
                            ),
                            
                            bsModal("modal_homepage1",HTML("<h3 style='display:inline'> Organigramme du MHUV</h3> <br> <h6> (Pour faire le Zoom : Appuyer sur Ctrl + ou Ctrl - sur le clavier)"), "homepage_button1", size = "large"
                                    ,htmlOutput("html_homepage1")),
                            bsModal("modal_homepage3",HTML("<h3 style='margin:-5px;font-family:sans-serif;font-size:23px;'>Organismes sous tutelle</h3>"), "homepage_button3", size = "large"
                                    ,uiOutput("html_homepage3"))
                            
                            
                            
                            
                  )
                  
                  
                ),
                textOutput('texta5')
                
                
    ),
    tabPanel("Villes",
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
                                             "DRAA ERICH","SIDI ABDELLAH","EL MENEAA","HASSI MESSAOUD")
                                 ,choicesOpt = list(
                                   style = c("font-size: 114%;font-weight: bold;")
                                   ,selectOnTab=TRUE
                                   )
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
    
    
    
    #########################################
    
    navbarMenu(tags$span("Construction et moyens",tags$br(),"de réalisation"),
               tabPanel("Unités de fabrication",
                    fluidRow(
                        id="fluidrow1_datamc",
                        
                        dropdown(
                          
                          tags$h3("Filtrage des données"),
                          br(),
                          pickerInput(
                            inputId = "wilayas_mc",
                            label = "", 
                            #choices = unique(datamc$wilaya_matricule),
                            
                            choices = datamc %>% 
                                     filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% 
                                     select(wilaya_matricule) %>% 
                                     unique() %>% .$wilaya_matricule,
                              
                            
                            #unique(datamc$wilaya_matricule),
                            
                            
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
                                             "Désélectionner ", 
                                             tags$br(),
                                             "Tout"
                                           )),
                            multiple = TRUE
                          ),
                          pickerInput(
                            inputId = "filiere_mcmc",
                            label = "", 
                            choices = datamc %>% 
                              filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% 
                              select(Filiere) %>% 
                              unique() %>% .$Filiere,
                              
                              
                              #unique(datamc$Filiere),
                            options = list(`actions-box` = TRUE,style = "btn-primary",
                                           `selected-text-format`= "count>2",
                                           `count-selected-text` = "{0} Filière séléctionnés",
                                           `none-selected-text` ="Filière",`select-all-text`=
                                             tags$div(
                                               "Séléctionner", 
                                               tags$br(),
                                               "Tout"
                                             ),
                                           `deselect-all-text`=tags$div(
                                             "Désélectionner", 
                                             tags$br(),
                                             "Tout"
                                           )),
                            multiple = TRUE
                          ),
                          pickerInput(
                            inputId = "statut2_mc",
                            label = "", 
                            choices = datamc %>% 
                              filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% 
                              
                              #filter() %>% 
                              group_by(Statut2) %>% 
                              summarise(Nb=n()) %>%
                              mutate(ar=c(3,5,4,6,1,2)) %>% 
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
                                             "Désélectionner ", 
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
                                          tags$h3(id="card_mc_titre",HTML("Nombre d'unités <br/>de fabrication des matériaux <br/>de construction"),
                                                  style="text-align: center;margin-top: 2px;font-size: 27px;font-family: unset;"),
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
                                                       choices = c("Maps","Tableau","Tableau croisé dynamique"),
                                                       selected = "Maps"
                                                       #,color="default"
                                                     )
                                                     
                                                 ),
                                                 conditionalPanel(
                                                   condition = "input.radio_choose_leaflet_mc == 'Maps'",
                                                   selectInput("search", "Rechercher:",
                                                               #textOutput("datamc_identification"),
                                                               datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% 
                                                                 select(Identification) %>% .$Identification,
                                                               #datamc$Identification,
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
                                                   condition = "input.radio_choose_leaflet_mc == 'Tableau croisé dynamique'",
                                                   shinycssloaders::withSpinner(
                                                     rpivotTableOutput("pivot_table_mc",height="800px")
                                                   )
                                                 )
                                                 
                                          ),
                                          column(width=4,
                                                 style="height: 800px;background-color:white;box-shadow: 4px 5px 11px grey;width: 31%;margin-left: 29px;padding:17px;",
                                                 reactableOutput('mc_statut_reactable'),
                                                 #highchartOutput("mc_annee_debut"),
                                                 #textOutput("annee_na_count"),
                                                 
                                                 reactableOutput('mc_wilaya_reactable',height="450px")

                                          )
                                 )
                                 #,
                                 #fluidRow(style="height: 250px;background-color:white;box-shadow: 4px 5px 11px grey;")
                          )
                        )
               ),
               pickerInput(
                 inputId = "select_arretee_datamc",
                 label = "",
                 #choices = sort(paste(c("Arrétée le :"),unique(moyens_realisation$Arretee)),TRUE),
                 choices = sort(paste(c("Arrétée le :"),unique(datamc$Arretee)),TRUE),
        
                 #choices = c('Arretee le : 2021-03-31','Arretee le : 2018-12-31')
                 )
               
               ),
               tabPanel("DTR",
                        fluidRow(
                          column(id="column1_pdf",width = 3,style="height: 800px;background: white;box-shadow: 4px 5px 11px grey;margin-right:8px;width:23%;overflow-y:scroll;font-size:16px;",
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
               tabPanel("Agréments et certificats de qualification", #c'était sous le nom : Moyens de Realisation
                        
                        bsModal("modal1_mapst", htmlOutput("tablededonnes1_mapst"), "tabmodalserie1_mapst", size = "large",excelOutput("excel_mr1")),
                        bsModal("modal2_mapst", htmlOutput("tablededonnes2_mapst"), "tabmodalserie2_mapst", size = "large",excelOutput("excel_mr2")),
                        bsModal("modal3_mapst", htmlOutput("tablededonnes3_mapst"), "tabmodalserie3_mapst", size = "large",excelOutput("excel_mr3")),
                        bsModal("modal4_mapst", htmlOutput("tablededonnes4_mapst"), "tabmodalserie4_mapst", size = "large",excelOutput("excel_mr4")),
                        bsModal("modal5_mapst", htmlOutput("tablededonnes5_mapst"), "tabmodalserie5_mapst", size = "large",excelOutput("excel_mr5")),
                        
                        
                        
                        bsModal("modal_maps", htmlOutput("tablededonnes1_maps"), "tabmodalserie_maps", size = "large"
                                ,
                                div(
                                  actionBttn(
                                    inputId = "choose_leaflet_agrement",
                                    label = NULL,
                                    style = "simple", 
                                    color = "primary",
                                    icon = icon("th-list",lib = "glyphicon")
                                  ),
                                  div(id="display_when_hover_choose_leaflet_agrement",
                                      
                                      awesomeRadio(
                                        inputId = "radio_choose_leaflet_agrement",
                                        label = "", 
                                        choices = c("Entreprise qualifiees (Cat 1-9)",
                                                    "Entreprise qualifiees (Cat 1-4)",
                                                    "Entreprise qualifiees (Cat 5-9)",
                                                    "Entreprise qualifiees (Cat1)",
                                                    "Entreprise qualifiees (Cat2)",
                                                    "Entreprise qualifiees (Cat3)",
                                                    "Entreprise qualifiees (Cat4)",
                                                    "Entreprise qualifiees (Cat5)",
                                                    "Entreprise qualifiees (Cat6)",
                                                    "Entreprise qualifiees (Cat7)",
                                                    "Entreprise qualifiees (Cat8)",
                                                    "Entreprise qualifiees (Cat9)"
                                                    
                                        ),
                                        selected = "Entreprise qualifiees (Cat 1-9)"
                                        #,color="default"
                                      )
                                      
                                  ),
                                  leafletOutput("distPlot2_agrementmaps",height=553)
                                )
                        ),
                        
                        ################ promoteur immobiliers maps
                        
                        bsModal("modal_maps_prom", htmlOutput("tablededonnes1_maps_prom"), "tabmodalserie_maps_prom", size = "large"
                                ,
                                div(
                                  actionBttn(
                                    inputId = "choose_leaflet_agrement_prom",
                                    label = NULL,
                                    style = "simple", 
                                    color = "primary",
                                    icon = icon("th-list",lib = "glyphicon")
                                  ),
                                  div(id="display_when_hover_choose_leaflet_agrement_prom",
                                      
                                      awesomeRadio(
                                        inputId = "radio_choose_leaflet_agrement_prom",
                                        label = "", 
                                        choices = c("Nbre de dossiers déposés",
                                                    "dont Par l'application E-Certif",
                                                    "Nbre de dossiers éxaminés",
                                                    "dont par l'application E-Certif"
                                                    
                                                    
                                        ),
                                        selected = "Nbre de dossiers déposés"
                                        #,color="default"
                                      )
                                      
                                  ),
                                  leafletOutput("distPlot2_agrementmaps_prom",height=553)
                                )
                        ),
                        
                        
                        ################ promoteur immobiliers maps fin
                        
                        
                        
                        ################ agence immobiliers maps
                        
                        bsModal("modal_maps_agence", htmlOutput("tablededonnes1_maps_agence"), "tabmodalserie_maps_agence", size = "large"
                                ,
                                div(
                                  actionBttn(
                                    inputId = "choose_leaflet_agrement_agence",
                                    label = NULL,
                                    style = "simple", 
                                    color = "primary",
                                    icon = icon("th-list",lib = "glyphicon")
                                  ),
                                  div(id="display_when_hover_choose_leaflet_agrement_agence",
                                      
                                      awesomeRadio(
                                        inputId = "radio_choose_leaflet_agrement_agence",
                                        label = "", 
                                        choices = c("Communes",
                                                    "Nbre de lots",
                                                    "Nbre de bénéficiaires de Lots",
                                                    "Nbre d'aides notifiées",
                                                    "Nbre de bénéficiaires d'aides"
                                                    
                                                    
                                        ),
                                        selected = "Nbre de lots"
                                        #,color="default"
                                      )
                                      
                                  ),
                                  leafletOutput("distPlot2_agrementmaps_agence",height=553)
                                )
                        ),
                        
                        
                        ################ agences immobiliers maps fin
                        
                        ################ ingenieurs maps debut
                        
                        bsModal("modal_maps_ing", htmlOutput("tablededonnes1_maps_ing"), "tabmodalserie_maps_ing", size = "large"
                                ,
                                div(
                                  actionBttn(
                                    inputId = "choose_leaflet_agrement_ing",
                                    label = NULL,
                                    style = "simple",
                                    color = "primary",
                                    icon = icon("th-list",lib = "glyphicon")
                                  ),
                                  div(id="display_when_hover_choose_leaflet_agrement_ing",
                                      
                                      awesomeRadio(
                                        inputId = "radio_choose_leaflet_agrement_ing",
                                        label = "",
                                        choices = c("Nbre dossiers reçus_PI",
                                                    "Nbre dossiers éxaminés par la Commision",
                                                    "Nbre d'agréments établis",
                                                    "Nbre demande d'inscription au TNPI"
                                        ),
                                        selected = "Nbre dossiers reçus_PI"
                                        #,color="default"
                                      )
                                      
                                  ),
                                  leafletOutput("distPlot2_agrementmaps_ing",height=553)
                                )
                        ),
                        
                        ################ ingenieurs maps fin
                        
                        
                        ################ AI maps debut
                        
                        bsModal("modal_maps_ai", htmlOutput("tablededonnes1_maps_ai"), "tabmodalserie_maps_ai", size = "large"
                                ,leafletOutput("distPlot2_agrementmaps_ai",height=553) 
                                ),
                        bsModal("modal1_search", htmlOutput("tablededonnes1_search"), "tabmodalserie1_search", size = "large",reactableOutput("reactable_mr1")),
                        #bsModal("modal1_mapst", htmlOutput("tablededonnes1_mapst"), "tabmodalserie1_mapst", size = "large",excelOutput("excel_mr1")),
                        
                        
                                # ,
                                # div(
                                #   actionBttn(
                                #     inputId = "choose_leaflet_agrement_ai",
                                #     label = NULL,
                                #     style = "simple",
                                #     color = "primary",
                                #     icon = icon("th-list",lib = "glyphicon")
                                #   ),
                                #   div(id="display_when_hover_choose_leaflet_agrement_ai",
                                #       
                                #       awesomeRadio(
                                #         inputId = "radio_choose_leaflet_agrement_ai",
                                #         label = "",
                                #         choices = c("Nombre d'ingenieurs agrees",
                                #                     "Ingenieurs (25ans-40ans)",
                                #                     "Ingenieurs (41ans-60ans)",
                                #                     "Ingenieurs (>60ans)",
                                #                     "Ingenieurs (M)",
                                #                     "Ingenieurs (F)"
                                #                     
                                #                     
                                #                     
                                #                     
                                #         ),
                                #         selected = "Nombre d'ingenieurs agrees"
                                #         #,color="default"
                                #       )
                                #       
                                #   ),
                                #   leafletOutput("distPlot2_agrementmaps_ai",height=553)
                                # )
                        #),
                        
                        
                        ################ AI maps fin
                        
                        

                        
                        
                        
                        
                        
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
                                 actionBttn(
                                   inputId = "tabmodalserie_maps",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("globe", lib = "glyphicon")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie_maps_prom",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("globe", lib = "glyphicon")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie_maps_agence",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("globe", lib = "glyphicon")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie_maps_ing",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("globe", lib = "glyphicon")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie_maps_ai",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("globe", lib = "glyphicon")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie1_mapst",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("table")
                                 ),
                                 
                                 actionBttn(
                                   inputId = "tabmodalserie2_mapst",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("table")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie3_mapst",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("table")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie4_mapst",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("table")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie5_mapst",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("table")
                                 ),
                                 actionBttn(
                                   inputId = "tabmodalserie1_search",
                                   label = NULL,
                                   style = "bordered", 
                                   color = "success",
                                   icon = icon("search")
                                 ),
                                 
                                 
                                 
                                 wellPanel(id="well_fichewilaya_mr",
                                           style="box-shadow:4px 5px 11px grey;background-color:white;",
                                           
                                           div(id = "box-score1_mr",
                                               div(id = "box-score-title1_mr", "Entreprises Qualifiées au ",textOutput("boxscore1_arretee_mr"),textOutput("boxscore1_wilaya_mr")),
                                               reactableOutput('gt1_mr',width = '1082px')
                                           ),
                                           
                                           div(id = "box-score2_mr",
                                               div(id = "box-score-title2_mr",textOutput("boxscore2_arretee_mr"),textOutput("boxscore2_wilaya_mr")),
                                               reactableOutput('gt2_mr',width = '922px')
                                           ),
                                           div(id = "box-score3_mr",
                                               div(id = "box-score-title3_mr", "Lotissement Socioux au",textOutput("boxscore3_arretee_mr"),textOutput("boxscore3_wilaya_mr")),
                                               reactableOutput('gt3_mr',width = '1023px')
                                           ),
                                           
                                           div(id = "box-score4_mr",
                                               div(id = "box-score-title4_mr", "Agréments des Promoteurs Immobiliers au ",textOutput("boxscore4_arretee_mr"),textOutput("boxscore4_wilaya_mr")),
                                               reactableOutput('gt4_mr',width = '1002px')
                                           ),
                                           div(id = "box-score5_mr",
                                               div(id = "box-score-title5_mr", "Agréments des Agents Immobiliers au ",textOutput("boxscore5_arretee_mr"),textOutput("boxscore5_wilaya_mr")),
                                               reactableOutput('gt5_mr',width = '1163px')
                                           )
                                           # ,
                                           # div(id = "box-score6_mr",
                                           #     div(id = "box-score-title6_mr", "Ingénieurs  au ",textOutput("boxscore6_arretee_mr"),textOutput("boxscore6_wilaya_mr")),
                                           #     reactableOutput('gt6_mr',width = '722px')
                                           # ),
                                           
                                           
                                 )
                          )
                        ),
                        pickerInput(
                          inputId = "select_arretee_fichewilaya_mr",
                          label = "", 
                          choices = sort(paste(c("Arrétée le :"),unique(moyens_realisation2$Arretee)),TRUE),
                          #c('Arretee le : 2020-12-31','Arretee le : 2020-09-30'),
                        )
               )
    )
    ################################### fin DGCMR

    #, navbarMenu("Données",
    #            tabPanel("Livraisons des logements ",
    #                   fluidRow(id='donnees1_liv',
    #                     fluidRow(id="rowdonnees1",style="padding-top: 25px",
    #                              column(2,offset = 2,style="padding-top: 22px;",
    #                                     pickerInput(
    #                                       inputId = "selectwilayas",
    #                                       label = "", 
    #                                       choices = unique(livraison_wilayas$waw),
    #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
    #                                                      `selected-text-format`= "count>2",
    #                                                      `count-selected-text` = "{0} Wilayas séléctionnés",
    #                                                      `none-selected-text` ="Wilaya",`select-all-text`=
    #                                                        tags$div(
    #                                                          "Séléctionner", 
    #                                                          tags$br(),
    #                                                          "Tout"
    #                                                        ),
    #                                                      `deselect-all-text`=tags$div(
    #                                                        "Deselectionner", 
    #                                                        tags$br(),
    #                                                        "Tout"
    #                                                      )),
    #                                       multiple = TRUE
    #                                     )
    #                                     
    #                                     
    #                              ),
    #                              column(2,style="padding-top: 22px;",
    #                                     pickerInput(
    #                                       inputId = "selectsegments",
    #                                       label = "", 
    #                                       choices = unique(livraison_wilayas$type_de_logement),
    #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
    #                                                      `selected-text-format`= "count>2",
    #                                                      `count-selected-text` = "{0} Segment séléctionnés",
    #                                                      `none-selected-text` ="Segment",`select-all-text`=
    #                                                        tags$div(
    #                                                          "Séléctionner", 
    #                                                          tags$br(),
    #                                                          "Tout"
    #                                                        ),
    #                                                      `deselect-all-text`=tags$div(
    #                                                        "Deselectionner", 
    #                                                        tags$br(),
    #                                                        "Tout"
    #                                                      )),
    #                                       multiple = TRUE
    #                                     )
    #                              ),
    #                              column(4,
    #                                     sliderInput("selectannees","",
    #                                                 min = min(unique(livraison_wilayas$annee)), max = max(unique(livraison_wilayas$annee)), value = c(min(unique(livraison_wilayas$annee)),max(unique(livraison_wilayas$annee))),sep = "")
    #                                     
    #                              )
    #                     ),
    #                     fluidRow(id="rowdonnes2",
    #                              column(2,offset = 3,
    #                                     prettySwitch(
    #                                       inputId = "parwilayas",
    #                                       label = "Par Wilaya", 
    #                                       status = "primary",
    #                                       slim = TRUE,
    #                                       value=FALSE
    #                                     )
    #                                     
    #                              ),
    #                              column(2,style="padding-left:310px;",
    #                                     prettySwitch(
    #                                       inputId = "parsegments",
    #                                       label = "Par Segment", 
    #                                       status = "primary",
    #                                       slim = TRUE,
    #                                       value=TRUE
    #                                     )
    #                              ),
    #                              column(2,style="padding-left:310px;",
    #                                     prettySwitch(
    #                                       inputId = "parperiode",
    #                                       label = "Par Année", 
    #                                       status = "primary",
    #                                       slim = TRUE,
    #                                       value=FALSE
    #                                     )
    #                              )
    #                     ),
    #                     fluidRow(id="rowdonnes33",style="margin-top:35px;",
    #                              htmlOutput('rowdonnees33html')
    #                     ),
    #                     fluidRow(
    #                       
    #                       excelOutput("donnees_excel",height="100%")
    #                       
    #                     )
    #                     
    #            )),
    #            tabPanel("Lancements des logements ",
    #                     
    #                     fluidRow(id="rowdonnees1",style="padding-top: 25px",
    #                              column(2,offset = 2,style="padding-top: 22px;",
    #                                     pickerInput(
    #                                       inputId = "selectwilayas_lanc",
    #                                       label = "", 
    #                                       choices = unique(livraison_wilayas$waw),
    #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
    #                                                      `selected-text-format`= "count>2",
    #                                                      `count-selected-text` = "{0} Wilayas séléctionnés",
    #                                                      `none-selected-text` ="Wilaya",`select-all-text`=
    #                                                        tags$div(
    #                                                          "Séléctionner", 
    #                                                          tags$br(),
    #                                                          "Tout"
    #                                                        ),
    #                                                      `deselect-all-text`=tags$div(
    #                                                        "Deselectionner", 
    #                                                        tags$br(),
    #                                                        "Tout"
    #                                                      )),
    #                                       multiple = TRUE
    #                                     )
    #                                     
    #                                     
    #                              ),
    #                              column(2,style="padding-top: 22px;",
    #                                     pickerInput(
    #                                       inputId = "selectsegments_lanc",
    #                                       label = "", 
    #                                       choices = unique(livraison_wilayas$type_de_logement),
    #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
    #                                                      `selected-text-format`= "count>2",
    #                                                      `count-selected-text` = "{0} Segment séléctionnés",
    #                                                      `none-selected-text` ="Segment",`select-all-text`=
    #                                                        tags$div(
    #                                                          "Séléctionner", 
    #                                                          tags$br(),
    #                                                          "Tout"
    #                                                        ),
    #                                                      `deselect-all-text`=tags$div(
    #                                                        "Deselectionner", 
    #                                                        tags$br(),
    #                                                        "Tout"
    #                                                      )),
    #                                       multiple = TRUE
    #                                     )
    #                              ),
    #                              column(4,
    #                                     sliderInput("selectannees_lanc","",
    #                                                 min = min(unique(lancement_wilayas$Annee)), max = max(unique(lancement_wilayas$Annee)), value = c(min(unique(lancement_wilayas$Annee)),max(unique(lancement_wilayas$Annee))),sep = "")
    #                                     
    #                              )
    #                     ),
    #                     fluidRow(id="rowdonnes2",
    #                              column(2,offset = 3,
    #                                     prettySwitch(
    #                                       inputId = "parwilayas_lanc",
    #                                       label = "Par Wilaya", 
    #                                       status = "primary",
    #                                       slim = TRUE,
    #                                       value=FALSE
    #                                     )
    #                                     
    #                              ),
    #                              column(2,style="padding-left:310px;",
    #                                     prettySwitch(
    #                                       inputId = "parsegments_lanc",
    #                                       label = "Par Segment", 
    #                                       status = "primary",
    #                                       slim = TRUE,
    #                                       value=TRUE
    #                                     )
    #                              ),
    #                              column(2,style="padding-left:310px;",
    #                                     prettySwitch(
    #                                       inputId = "parperiode_lanc",
    #                                       label = "Par Année", 
    #                                       status = "primary",
    #                                       slim = TRUE,
    #                                       value=FALSE
    #                                     )
    #                              )
    #                     ),
    #                     fluidRow(id="rowdonnes33",style="margin-top:35px;",
    #                              htmlOutput('rowdonnees33html_lanc')
    #                     ),
    #                     fluidRow(
    #                       
    #                       excelOutput("donnees_excel_lanc",height="100%")
    #                       
    #                     )
    #                     
    #            ),
    #            
    #            
    #            tabPanel("Données des Wilaya",
    #                     
    #                     fluidRow(id="rowdonnees1",style="padding-top: 25px",
    #                              column(2,offset = 3,style="padding-top: 22px;",
    #                                     pickerInput(
    #                                       inputId = "selectwilayas_wilaya",
    #                                       label = "", 
    #                                       choices = unique(livraison_wilayas$waw),
    #                                       options = list(`actions-box` = TRUE,style = "btn-primary",
    #                                                      `selected-text-format`= "count>2",
    #                                                      `count-selected-text` = "{0} Wilayas séléctionnés",
    #                                                      `none-selected-text` ="Wilaya",`select-all-text`=
    #                                                        tags$div(
    #                                                          "Séléctionner", 
    #                                                          tags$br(),
    #                                                          "Tout"
    #                                                        ),
    #                                                      `deselect-all-text`=tags$div(
    #                                                        "Deselectionner", 
    #                                                        tags$br(),
    #                                                        "Tout"
    #                                                      )),
    #                                       multiple = TRUE
    #                                     )
    #                                     
    #                                     
    #                              ),
    #                              
    #                              column(4,
    #                                     sliderInput("selectannees_wilaya","",
    #                                                 min = min(unique(estimation_tolpopparc$Annee)), max = max(unique(estimation_tolpopparc$Annee)), value = c(min(unique(estimation_tolpopparc$Annee)),max(unique(estimation_tolpopparc$Annee))),sep = "")
    #                                     
    #                              )
    #                     ),
    #                     fluidRow(id="rowdonnes2",style="margin-left: -6px;margin-bottom:30px; ",
    #                              column(2,offset = 5,
    #                                     prettySwitch(
    #                                       inputId = "parwilayas_wilaya",
    #                                       label = "Par Wilayas", 
    #                                       status = "primary",
    #                                       slim = TRUE,
    #                                       value=FALSE
    #                                     )
    #                                     
    #                              )),
    #                     fluidRow(id="rowdonnes332",style="margin-top:35px;",
    #                              htmlOutput('rowdonnees33html2')
    #                     ),
    #                     fluidRow(
    #                       
    #                       excelOutput("donnees_excel_wilaya",height="100%")
    #                       
    #                     )
    #                     
    #            ))
    
  )
  
)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  output$boxscore7_ui<-renderUI({
    div(id = "box-score-title7",
        pickerInput(
          inputId = "select_title_gt7",
          label = "", 
          choices = paste(`if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==3,
                                    c("Activité du 1er trimestre"),
                                    `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==6,
                                    c("Activité du 2ème trimestre","Activité du 1er semestre"),
                                       `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==9,
                                                  c("Activité du 3ème trimestre","Activité du 01-01 au 30-09"),
                                                  `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==12,
                                                                  c("Activité du 4ème trimestre","Activité du 2ème semestre","Activité de l'année")
                                                  )
                                                  )
                                       )
                                       ),paste0(substr(input$select_arretee_fichewilaya,14,17)))
          
          #choices = c("Activité du 1er semestre","Activité du 2eme trimestre")
          #choices = sort(paste(c("Arrétée le :"),unique(data_fiche_wilaya$arretee)),TRUE),
        ),
        #textOutput("boxscore7_arretee"),
        textOutput("boxscore7_wilaya")
    )
  })
  
  
  ############ pour gt7: 
  
  data_fiche_wilaya_reactive_gt7=reactive({
    cbind(iad1=0,iad2=0,data_fiche_wilaya %>%
            filter(arretee %in% `if`(str_sub(input$select_title_gt7,1,-6)=="Activité du 1er semestre",
                                 c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-03-31")),
                                   as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-06-30"))
                                 ),
                                 `if`(str_sub(input$select_title_gt7,1,-6)=="Activité du 01-01 au 30-09",
                                      c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-03-31")),
                                        as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-06-30")),
                                        as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-09-30"))
                                      ),
                                      `if`(str_sub(input$select_title_gt7,1,-6)=="Activité du 2ème semestre",
                                           c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-09-30")),
                                           as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-12-31"))
                                      ),
                                      `if`(str_sub(input$select_title_gt7,1,-6)=="Activité de l'année",
                                           c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-03-31")),
                                             as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-06-30")),
                                             as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-09-30")),
                                             as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-12-31"))
                                           ),
                                           as.Date(substr(input$select_arretee_fichewilaya,14,23))
                                      )))),
                   #id_wilaya %in% c(selected20_fichewilaya())
                   id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())])
                   
                   
            ) %>%
            summarise_at(vars(consistance_lpl:anuules_durant_ce_trimestre_promotionellelibre), sum,
                         na.rm=TRUE
                         
                         # `if`(length(selected20_fichewilaya())==1,
                         #          `if`(as.numeric(selected20_fichewilaya())==49,
                         #          FALSE,
                         #          TRUE),
                         #          TRUE)
                         
            )
    )
    #summarise_at(vars(consistance_lpl:total_lanc), sum))
    
    #  %>%    unlist(use.names = FALSE)
  })
  
  ############# pour gt7 end
  
  
  
  dfa_fichewilaya7=reactive({
    i=1
    
    ab7$Livres[1:5]        = data_fiche_wilaya_reactive_gt7() %>% select_at(vars(lpl_livres:lpp_livres)) %>% t() %>% as.vector()
    ab7$Prevus_livres[1:5] = data_fiche_wilaya_reactive_gt7() %>% select_at(vars(lpl_prevus:lpp_pruve)) %>% t() %>% as.vector()
    ab7[,4]=ab7[,2]/ab7[,3]
    
    
    ab7$Lances[1:5] = data_fiche_wilaya_reactive_gt7() %>% select_at(vars(lpl_lanc:lpp_lanc)) %>% t() %>% as.vector()
    ab7$Prevus_lances[1:5] = data_fiche_wilaya_reactive_gt7() %>% select_at(vars(lpl_prevus_lanc:lpp_pruve_lanc)) %>% t() %>% as.vector()
    ab7[,7]=ab7[,5]/ab7[,6]
    
    
    ab7$Annules[1:5] = data_fiche_wilaya_reactive_gt7() %>% select_at(vars(anuules_durant_ce_trimestre_lpl:anuules_durant_ce_trimestre_lpp)) %>% t() %>% as.vector()
    
    ab7[6,2:3]=data_fiche_wilaya_reactive_gt7() %>% select_at(vars(lv_cnep_pruve:lv_cnep_lanc)) %>% select(2,1) %>% t() %>% as.vector()
    ab7[6,4]=c(ab7[6,2]/ab7[6,3])
    
    ab7[6,5:6]=data_fiche_wilaya_reactive_gt7() %>% select_at(vars(lv_cnep_pruve:lv_cnep_lanc)) %>% select(4,3) %>% t() %>% as.vector()
    ab7[6,7]=c(ab7[6,5])/c(ab7[6,6])
    
    
    
    # for(kk in 1:5){
    #   
    #   
    #     ab7[kk,2]=c(data_fiche_wilaya_reactive_gt7()[i,58+kk])
    #     ab7[kk,3]=c(data_fiche_wilaya_reactive_gt7()[i,52+kk])
    #     ab7[,4]=ab7[,2]/ab7[,3]
    #     
    #     ab7[kk,5]=c(data_fiche_wilaya_reactive_gt7()[i,70+kk])
    #     ab7[kk,6]=c(data_fiche_wilaya_reactive_gt7()[i,64+kk])
    #     ab7[,7]=ab7[,5]/ab7[,6]
    #     
    #     ab7[kk,8]=c(data_fiche_wilaya_reactive_gt7()[i,76+kk])
    #  }
    
    
    
    # 
    # 
    # ab7[6,2]=c(data_fiche_wilaya_reactive_gt7()[i,89])
    # ab7[6,3]=c(data_fiche_wilaya_reactive_gt7()[i,88])
    # ab7[6,4]=c(ab7[6,2]/ab7[6,3])
    # 
    # ab7[6,5]=c(data_fiche_wilaya_reactive_gt7()[i,91])
    # ab7[6,6]=c(data_fiche_wilaya_reactive_gt7()[i,90])
    # ab7[6,7]=c(ab7[6,5])/c(ab7[6,6])
    # 
    
    
    ab7[7,2:3]=data_fiche_wilaya_reactive_gt7() %>% select_at(vars(acls_pruve_lanc:acls_livres)) %>% select(4,3) %>% t() %>% as.vector()
    ab7[7,4]=c(ab7[7,2]/ab7[7,3])
    
    ab7[7,5:6]=data_fiche_wilaya_reactive_gt7() %>% select_at(vars(acls_pruve_lanc:acls_livres)) %>% select(2,1) %>% t() %>% as.vector()
    ab7[7,7]=c(ab7[7,5])/c(ab7[7,6])
    
    
    # 
    # 
    # ab7[7,2]=c(data_fiche_wilaya_reactive_gt7()$acls_livres[i])
    # ab7[7,3]=c(data_fiche_wilaya_reactive_gt7()$acls_pruve[i])
    # ab7[7,4]=c(ab7[7,2]/ab7[7,3])
    # 
    # ab7[7,5]=c(data_fiche_wilaya_reactive_gt7()$acls_lanc[i])
    # ab7[7,6]=c(data_fiche_wilaya_reactive_gt7()$acls_pruve_lanc[i])
    # ab7[7,7]=c(ab7[7,5])/c(ab7[7,6])
    # 
    
    
    ab7[1,9]=c(data_fiche_wilaya_reactive_gt7()[i,8])
    ab7[2,9]=c(data_fiche_wilaya_reactive_gt7()[i,14])
    ab7[3,9]=c(data_fiche_wilaya_reactive_gt7()[i,20])
    ab7[4,9]=c(data_fiche_wilaya_reactive_gt7()[i,26])
    ab7[5,9]=c(data_fiche_wilaya_reactive_gt7()[i,32])
    
    ab7[6,9]=c(data_fiche_wilaya_reactive_gt7()[i,87])
    
    ab7[7,9]=c(data_fiche_wilaya_reactive_gt7()$notifie2020_acls[i])
    
    
    ab7[mapply(is.infinite, ab7)] <- NA
    
    ab7=ab7 %>% arrange(c(1,2,3,4,6,5,7))
    
    ab7
    
  })
  
  
  
  
  dfa_fichewilaya7_suite=reactive({
    
    
    ab7_suite[1,2]=data_fiche_wilaya_reactive_gt7() %>% select(y=autresocial_livres) %>% .$y
    ab7_suite[2,2]=data_fiche_wilaya_reactive_gt7() %>% select(y=promotionellelibre_livres) %>% .$y
    
    
    ab7_suite[1,3]=data_fiche_wilaya_reactive_gt7() %>% select(y=autresocial_pruve) %>% .$y
    ab7_suite[2,3]=data_fiche_wilaya_reactive_gt7() %>% select(y=promotionellelibre_pruve) %>% .$y
    
    ab7_suite[1,4]=ab7_suite[1,2]/ab7_suite[1,3]
    ab7_suite[2,4]=ab7_suite[2,2]/ab7_suite[2,3]
    
    #########
    ab7_suite[1,5]=data_fiche_wilaya_reactive_gt7() %>% select(y=autresocial_lanc) %>% .$y
    ab7_suite[2,5]=data_fiche_wilaya_reactive_gt7() %>% select(y=promotionellelibre_lanc) %>% .$y
    
    
    ab7_suite[1,6]=data_fiche_wilaya_reactive_gt7() %>% select(y=autresocial_pruve_lanc) %>% .$y
    ab7_suite[2,6]=data_fiche_wilaya_reactive_gt7() %>% select(y=promotionellelibre_pruve_lanc) %>% .$y
    
    ab7_suite[1,7]=ab7_suite[1,5]/ab7_suite[1,6]
    ab7_suite[2,7]=ab7_suite[2,5]/ab7_suite[2,6]
    
    ab7_suite[1,8]=data_fiche_wilaya_reactive_gt7() %>% select(y=anuules_durant_ce_trimestre_autresocial) %>% .$y
    ab7_suite[2,8]=data_fiche_wilaya_reactive_gt7() %>% select(y=anuules_durant_ce_trimestre_promotionellelibre) %>% .$y
    
    ab7_suite[1,9]=data_fiche_wilaya_reactive_gt7() %>% select(y=notifie2020_autresocial) %>% .$y
    ab7_suite[2,9]=data_fiche_wilaya_reactive_gt7() %>% select(y=notifie2020_promotionellelibre) %>% .$y
    
    
    ab7_suite[mapply(is.infinite, ab7_suite)] <- NA
    
    
    ab7_suite
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############## pour gt7 end
  
  output$reactable_mr1<-renderReactable({
    
    reactable(
      etp_categoris59 %>% 
        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>% 
        select(4,3,22,32,2,5:21,23:31,33:40)
       ,
      defaultColDef = colDef(
        align = "left",
        width = 180,
        headerStyle = list(background = "#f7f7f8")
      ),
      language = reactableLang(
        noData = "No entries found",
        pageInfo = "{rows} Entreprises"
      ),
      
      bordered = TRUE,
      resizable = TRUE,
      #striped = TRUE,
      #highlight = TRUE,
      filterable = TRUE,
      defaultPageSize  = 10,
      columns = list(
        `Dénomination`=colDef(name="Dénomination",width=230,
                                style=list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
                                           borderRight = "1px solid #eee"),
                                headerStyle =list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1,
                                                  borderRight = "1px solid #eee")
                                
        ),
        # `N`=colDef(name="N",width=80,
        #                     style=list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
        #                                borderRight = "1px solid #eee"),
        #                     headerStyle =list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1,
        #                                       borderRight = "1px solid #eee")
        #                     
        # ),
        # `Cat.`=colDef(name="Catégoris",width=80,
        #             style=list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
        #                        borderRight = "1px solid #eee"),
        #             headerStyle =list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1,
        #                               borderRight = "1px solid #eee")
        #             
        # ),
        # `Code Wilaya`=colDef(name="Wilaya",width=150,
        #               style=list(position = "sticky", left = 0, background = "#fff", zIndex = 1,
        #                          borderRight = "1px solid #eee"),
        #               headerStyle =list(position = "sticky", left = 0, background = "#f7f7f8", zIndex = 1,
        #                                 borderRight = "1px solid #eee")
        #               
        # ),
        `N`=colDef(name="N°",width=100),
        `Cat.`=colDef(name="Catégorie",width=100),
        `Code Wilaya`=colDef(name="Wilaya",width=150),
        `Code Bat`=colDef(name="Code Bat",width=350),
        `Code TP`=colDef(name="Code TP",width=350),
        `Code Hyd`=colDef(name="Code Hyd",width=350),
        `Adresse`=colDef(name="Adresse",width=350)

        
        
        
        
      )
    )
    
    
    
    
  })
  
  
  gt_attribution_reactive<-reactive({
    ####
    gt_attribution[1,2]=attribution_fw %>%
                       filter(Segment=="LPL",
                              id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                              arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
                       summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,2]=attribution_fw %>%
      filter(Segment=="LPL",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,2]=gt_attribution[1,2]/gt_attribution[2,2]
    ############
    
    ####
    gt_attribution[1,3]=attribution_fw %>%
      filter(Segment=="LSP/LPA",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,3]=attribution_fw %>%
      filter(Segment=="LSP/LPA",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,3]=gt_attribution[1,3]/gt_attribution[2,3]
    ############
    
    ####
    gt_attribution[1,4]=attribution_fw %>%
      filter(Segment=="Location-Vente",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,4]=attribution_fw %>%
      filter(Segment=="Location-Vente",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,4]=gt_attribution[1,4]/gt_attribution[2,4]
    ############
    
    ####
    gt_attribution[1,5]=attribution_fw %>%
      filter(Segment=="LPP",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,5]=attribution_fw %>%
      filter(Segment=="LPP",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,5]=gt_attribution[1,5]/gt_attribution[2,5]
    ############
    
    
    ####
    gt_attribution[1,6]=attribution_fw %>%
      filter(Segment=="Rural",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,6]=attribution_fw %>%
      filter(Segment=="Rural",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,6]=gt_attribution[1,4]/gt_attribution[2,4]
    ############
    
    ####
    gt_attribution[1,7]=attribution_fw %>%
      filter(Segment=="Lotissement sociaux",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,7]=attribution_fw %>%
      filter(Segment=="Lotissement sociaux",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,7]=gt_attribution[1,7]/gt_attribution[2,7]
    ############
    
    
    
    ####
    gt_attribution[1,8]=attribution_fw %>%
      filter(Segment=="Autre Logts",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,8]=attribution_fw %>%
      filter(Segment=="Autre Logts",
             id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,8]=gt_attribution[1,8]/gt_attribution[2,8]
    ############
    
    ####
    gt_attribution[1,9]=attribution_fw %>%
      filter(id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Attribue)) %>% .$nb
    
    gt_attribution[2,9]=attribution_fw %>%
      filter(id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
             arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      summarise(nb=sum(Prevu)) %>% .$nb
    
    gt_attribution[3,9]=gt_attribution[1,9]/gt_attribution[2,9]
    ############    
    
    
    gt_attribution=gt_attribution[1:2,]
    
    #gt_attribution[3,2:9]=sprintf("%.0f%%",100*gt_attribution[3,2:9])
    gt_attribution
  })
  
  
  reactive_details_encours_lpp<-reactive({
    details_encours_lpl_lsp_lv_lpp %>%
      filter(
        Segment=="LPP",
        id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      #group_by(id_wilaya) %>%
      summarise_at(4:7,sum,na.rm=TRUE) %>% t() %>% data.frame()
  })
  
  
  onevent("mouseenter", "divhover_encours_lpp_choose_line1", show("display_when_hover_divhover_encours_lpp_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_divhover_encours_lpp_choose_line1", show("display_when_hover_divhover_encours_lpp_choose_line1"))
  onevent("mouseleave", "display_when_hover_divhover_encours_lpp_choose_line1", hide("display_when_hover_divhover_encours_lpp_choose_line1"))
  
  
  output$reactable_details_encours_lpp<-renderReactable({
    reactable(reactive_details_encours_lpp(),
              sortable = FALSE,
              defaultPageSize = 15,
              #striped = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              fullWidth = FALSE,
              columns=list(
                .=colDef(name="",
                         format = colFormat(separators = TRUE,locales = "fr-FR")
                         
                ),
                .rownames =colDef(name="LPP",width=230)
                
              )
    )
  })
  
  output$divhover_encours_lpp<-renderUI({
    div(
      actionBttn(
        inputId = "divhover_encours_lpp_choose_line1",
        label = NULL,
        style = "simple", 
        color = "primary",
        #icon = icon("th-list",lib = "glyphicon")
        icon = icon("indent-left",lib = "glyphicon")
        
        
      ),
      div(id="display_when_hover_divhover_encours_lpp_choose_line1",
          actionBttn(
            inputId = "button_fw1_divhover7",
            label = NULL,
            style = "bordered", 
            color = "success",
            icon = icon("table")
          ),
          
          reactableOutput("reactable_details_encours_lpp"),
          bsModal("modal_fw1_divhover7", htmlOutput("tablededonnes1_fw1_divhover7"), "button_fw1_divhover7", size = "large"
                  ,excelOutput("excel_fw1_divhover7"))
          
      )
    )
  })
  ############################  
  
  
  reactive_details_encours_lv<-reactive({
    details_encours_lpl_lsp_lv_lpp %>%
      filter(
        Segment=="Location-Vente",
        id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      #group_by(id_wilaya) %>%
      summarise_at(4:7,sum,na.rm=TRUE) %>% t() %>% data.frame()
  })
  
  
  onevent("mouseenter", "divhover_encours_lv_choose_line1", show("display_when_hover_divhover_encours_lv_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_divhover_encours_lv_choose_line1", show("display_when_hover_divhover_encours_lv_choose_line1"))
  onevent("mouseleave", "display_when_hover_divhover_encours_lv_choose_line1", hide("display_when_hover_divhover_encours_lv_choose_line1"))
  
  
  output$reactable_details_encours_lv<-renderReactable({
    reactable(reactive_details_encours_lv(),
              sortable = FALSE,
              defaultPageSize = 15,
              #striped = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              fullWidth = FALSE,
              columns=list(
                .=colDef(name="",
                         format = colFormat(separators = TRUE,locales = "fr-FR")
                         
                ),
                .rownames =colDef(name="Location-Vente",width=230)
                
              )
    )
  })
  
  output$divhover_encours_lv<-renderUI({
    div(
      actionBttn(
        inputId = "divhover_encours_lv_choose_line1",
        label = NULL,
        style = "simple", 
        color = "primary",
        #icon = icon("th-list",lib = "glyphicon")
        icon = icon("indent-left",lib = "glyphicon")
        
        
      ),
      div(id="display_when_hover_divhover_encours_lv_choose_line1",
          actionBttn(
            inputId = "button_fw1_divhover6",
            label = NULL,
            style = "bordered", 
            color = "success",
            icon = icon("table")
          ),
          
          reactableOutput("reactable_details_encours_lv"),
          bsModal("modal_fw1_divhover6", htmlOutput("tablededonnes1_fw1_divhover6"), "button_fw1_divhover6", size = "large"
                  ,excelOutput("excel_fw1_divhover6"))
          
      )
    )
  })
  ############################  
  
  
  
  
  reactive_details_encours_lsp<-reactive({
    details_encours_lpl_lsp_lv_lpp %>%
      filter(
        Segment=="LSP/LPA",
        id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      #group_by(id_wilaya) %>%
      summarise_at(4:7,sum,na.rm=TRUE) %>% t() %>% data.frame()
  })
  
  
  onevent("mouseenter", "divhover_encours_lsp_choose_line1", show("display_when_hover_divhover_encours_lsp_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_divhover_encours_lsp_choose_line1", show("display_when_hover_divhover_encours_lsp_choose_line1"))
  onevent("mouseleave", "display_when_hover_divhover_encours_lsp_choose_line1", hide("display_when_hover_divhover_encours_lsp_choose_line1"))
  
  
  output$reactable_details_encours_lsp<-renderReactable({
    reactable(reactive_details_encours_lsp(),
              sortable = FALSE,
              defaultPageSize = 15,
              #striped = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              fullWidth = FALSE,
              columns=list(
                .=colDef(name="",
                         format = colFormat(separators = TRUE,locales = "fr-FR")
                         
                ),
                .rownames =colDef(name="LSP/LPA",width=230)
                
              )
    )
  })
  
  output$divhover_encours_lsp<-renderUI({
    div(
      actionBttn(
        inputId = "divhover_encours_lsp_choose_line1",
        label = NULL,
        style = "simple", 
        color = "primary",
        #icon = icon("th-list",lib = "glyphicon")
        icon = icon("indent-left",lib = "glyphicon")
        
        
      ),
      div(id="display_when_hover_divhover_encours_lsp_choose_line1",
          actionBttn(
            inputId = "button_fw1_divhover5",
            label = NULL,
            style = "bordered", 
            color = "success",
            icon = icon("table")
          ),
          
          reactableOutput("reactable_details_encours_lsp"),
          bsModal("modal_fw1_divhover5", htmlOutput("tablededonnes1_fw1_divhover5"), "button_fw1_divhover5", size = "large"
                  ,excelOutput("excel_fw1_divhover5"))
          
      )
    )
  })
  ############################   
  
  
  reactive_details_encours_lpl<-reactive({
    details_encours_lpl_lsp_lv_lpp %>%
      filter(
        Segment=="LPL",
        id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      #group_by(id_wilaya) %>%
      summarise_at(4:7,sum,na.rm=TRUE) %>% t() %>% data.frame()
  })
  
  
  onevent("mouseenter", "divhover_encours_lpl_choose_line1", show("display_when_hover_divhover_encours_lpl_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_divhover_encours_lpl_choose_line1", show("display_when_hover_divhover_encours_lpl_choose_line1"))
  onevent("mouseleave", "display_when_hover_divhover_encours_lpl_choose_line1", hide("display_when_hover_divhover_encours_lpl_choose_line1"))
  
  
  output$reactable_details_encours_lpl<-renderReactable({
    reactable(reactive_details_encours_lpl(),
              sortable = FALSE,
              defaultPageSize = 15,
              #striped = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              fullWidth = FALSE,
              columns=list(
                .=colDef(name="",
                         format = colFormat(separators = TRUE,locales = "fr-FR")
                         
                ),
                .rownames =colDef(name="LPL",width=230)
                
              )
    )
  })
  
  output$divhover_encours_lpl<-renderUI({
    div(
      actionBttn(
        inputId = "divhover_encours_lpl_choose_line1",
        label = NULL,
        style = "simple", 
        color = "primary",
        #icon = icon("th-list",lib = "glyphicon")
        icon = icon("indent-left",lib = "glyphicon")
        
        
      ),
      div(id="display_when_hover_divhover_encours_lpl_choose_line1",
          actionBttn(
            inputId = "button_fw1_divhover4",
            label = NULL,
            style = "bordered", 
            color = "success",
            icon = icon("table")
          ),
          
          reactableOutput("reactable_details_encours_lpl"),
          bsModal("modal_fw1_divhover4", htmlOutput("tablededonnes1_fw1_divhover4"), "button_fw1_divhover4", size = "large"
                  ,excelOutput("excel_fw1_divhover4"))
          
          
          
      )
    )
  })
############################    
  
  reactive_details_nonlances_rural<-reactive({
    details_nonlances_rural %>%
      filter(
        id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      #group_by(id_wilaya) %>%
      summarise_at(3:10,sum,na.rm=TRUE) %>% t() %>% data.frame()
  })
  
  
  onevent("mouseenter", "divhover_nonlances_rural_choose_line1", show("display_when_hover_divhover_nonlances_rural_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_divhover_nonlances_rural_choose_line1", show("display_when_hover_divhover_nonlances_rural_choose_line1"))
  onevent("mouseleave", "display_when_hover_divhover_nonlances_rural_choose_line1", hide("display_when_hover_divhover_nonlances_rural_choose_line1"))
  
  
  output$reactable_details_nonlances_rural<-renderReactable({
    reactable(reactive_details_nonlances_rural(),
              sortable = FALSE,
              defaultPageSize = 15,
              #striped = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              fullWidth = FALSE,
              columns=list(
                .=colDef(name="",
                         format = colFormat(separators = TRUE,locales = "fr-FR")
                         
                ),
                .rownames =colDef(name="Rural",width=230)
                
              )
    )
  })
  
  output$divhover_nonlances_rural<-renderUI({
    div(
      actionBttn(
        inputId = "divhover_nonlances_rural_choose_line1",
        label = NULL,
        style = "simple", 
        color = "primary",
        #icon = icon("th-list",lib = "glyphicon")
        icon = icon("indent-left",lib = "glyphicon")
        
        
      ),
      div(id="display_when_hover_divhover_nonlances_rural_choose_line1",
          actionBttn(
            inputId = "button_fw1_divhover3",
            label = NULL,
            style = "bordered", 
            color = "success",
            icon = icon("table")
          ),
          
          reactableOutput("reactable_details_nonlances_rural"),
          bsModal("modal_fw1_divhover3", htmlOutput("tablededonnes1_fw1_divhover3"), "button_fw1_divhover3", size = "large"
                  ,excelOutput("excel_fw1_divhover3"))
          
      )
    )
  })
  
  
  
  
  
  
  
  
  
############  
  reactive_details_nonlances_lsp<-reactive({
    details_nonlances_lsp %>%
      filter(
        id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      #group_by(id_wilaya) %>%
      summarise_at(3:14,sum,na.rm=TRUE) %>% t() %>% data.frame()
  })
  
  
  onevent("mouseenter", "divhover_nonlances_lsp_choose_line1", show("display_when_hover_divhover_nonlances_lsp_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_divhover_nonlances_lsp_choose_line1", show("display_when_hover_divhover_nonlances_lsp_choose_line1"))
  onevent("mouseleave", "display_when_hover_divhover_nonlances_lsp_choose_line1", hide("display_when_hover_divhover_nonlances_lsp_choose_line1"))
  
  
  output$reactable_details_nonlances_lsp<-renderReactable({
    reactable(reactive_details_nonlances_lsp(),
              sortable = FALSE,
              defaultPageSize = 15,
              #striped = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              fullWidth = FALSE,
              columns=list(
                .=colDef(name="",
                         format = colFormat(separators = TRUE,locales = "fr-FR")
                         
                ),
                .rownames =colDef(name="LSP/LPA",width=230)
                
              )
    )
  })
  
  output$divhover_nonlances_lsp<-renderUI({
    div(
      actionBttn(
        inputId = "divhover_nonlances_lsp_choose_line1",
        label = NULL,
        style = "simple", 
        color = "primary",
        #icon = icon("th-list",lib = "glyphicon")
        icon = icon("indent-left",lib = "glyphicon")
        
        
      ),
      div(id="display_when_hover_divhover_nonlances_lsp_choose_line1",
          actionBttn(
            inputId = "button_fw1_divhover2",
            label = NULL,
            style = "bordered", 
            color = "success",
            icon = icon("table")
          ),
          
          reactableOutput("reactable_details_nonlances_lsp"),
          bsModal("modal_fw1_divhover2", htmlOutput("tablededonnes1_fw1_divhover2"), "button_fw1_divhover2", size = "large"
                  ,excelOutput("excel_fw1_divhover2"))
          
      )
    )
  })
  
  
  
  
  
  reactive_details_nonlances_lpl<-reactive({
    details_nonlances_lpl %>%
      filter(
        id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
      #group_by(id_wilaya) %>%
      summarise_at(3:14,sum,na.rm=TRUE) %>% t() %>% data.frame()
    })
  
  
  onevent("mouseenter", "divhover_nonlances_lpl_choose_line1", show("display_when_hover_divhover_nonlances_lpl_choose_line1"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_divhover_nonlances_lpl_choose_line1", show("display_when_hover_divhover_nonlances_lpl_choose_line1"))
  onevent("mouseleave", "display_when_hover_divhover_nonlances_lpl_choose_line1", hide("display_when_hover_divhover_nonlances_lpl_choose_line1"))
  
  
  output$reactable_details_nonlances_lpl<-renderReactable({
    reactable(reactive_details_nonlances_lpl(),
              sortable = FALSE,
              defaultPageSize = 15,
              #striped = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              fullWidth = FALSE,
              columns=list(
                .=colDef(name="",
                         format = colFormat(separators = TRUE,locales = "fr-FR")
                         
                ),
                .rownames =colDef(name="LPL",width=230)
                
              )
    )
    })
  
  output$divhover_nonlances_lpl<-renderUI({
    div(
    actionBttn(
      inputId = "divhover_nonlances_lpl_choose_line1",
      label = NULL,
      style = "simple", 
      color = "primary",
      #icon = icon("th-list",lib = "glyphicon")
      icon = icon("indent-left",lib = "glyphicon")
      
      
    ),
    div(id="display_when_hover_divhover_nonlances_lpl_choose_line1",
        actionBttn(
          inputId = "button_fw1_divhover1",
          label = NULL,
          style = "bordered", 
          color = "success",
          icon = icon("table")
        ),
        reactableOutput("reactable_details_nonlances_lpl"),
        bsModal("modal_fw1_divhover1", htmlOutput("tablededonnes1_fw1_divhover1"), "button_fw1_divhover1", size = "large"
                ,excelOutput("excel_fw1_divhover1"))
        
    )
    )
  })
  #############
  output$ui_boxscore8<-renderUI({
    div(id = "box-score8",
        style=
          `if`(as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]) %in% c(100,101,102) | substr(input$select_arretee_fichewilaya,14,23) %in%  c("2020-03-31","2020-06-30","2020-09-30","2020-12-31"),
               paste0("display:none;"),
               paste0("display:block;margin-left:215px;")
          ),
        div(id = "box-score-title8", "Patrimoine en Location au ",textOutput("boxscore8_arretee"),
            actionBttn(
              inputId = "button_fw8",
              label = NULL,
              style = "bordered", 
              color = "success",
              icon = icon("table")
            )
            #,textOutput("boxscore8_wilaya")
            ),
        reactableOutput('gt8',width = '773px'),
        
        bsModal("modal_fw8", htmlOutput("tablededonnes1_fw8"), "button_fw8", size = "large"
                ,excelOutput("excel_fw8"))
    )
  })
  
  
  output$ui_boxscore9<-renderUI({
    div(id = "box-score9",
        style=
          `if`(as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]) %in% c(100,101,102) | substr(input$select_arretee_fichewilaya,14,23) %in%  c("2020-03-31","2020-06-30","2020-09-30","2020-12-31"),
                   paste0("display:none;"),
                   paste0("display:block;margin-left:0px;")
        ),
        div(id = "box-score-title9", "Etat de la Cession au ",textOutput("boxscore9_arretee"),textOutput("boxscore9_wilaya"),
            actionBttn(
              inputId = "button_fw9",
              label = NULL,
              style = "bordered", 
              color = "success",
              icon = icon("table")
            )
            #,textOutput("boxscore8_wilaya")
        ),
        reactableOutput('gt9',width = 'auto'),
        
        bsModal("modal_fw9", htmlOutput("tablededonnes1_fw9"), "button_fw9", size = "large"
                ,excelOutput("excel_fw9"))
    )
  })
  
  
  
  
  
  ###########
  
  output$ui_boxscore3<-renderUI({
    div(id = "box-score3",
        style=`if`(as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]) %in% c(100,101,102),
                   paste0("display:none;"),
                   paste0("display:block;")
                   
                   
        ),
        div(id = "box-score-title3", "Logements LPL en instance d’attribution au ",textOutput("boxscore3_arretee"),textOutput("boxscore3_wilaya")),
        reactableOutput('gt3',width = 'auto'),
        actionBttn(
          inputId = "button_fw3",
          label = NULL,
          style = "bordered", 
          color = "success",
          icon = icon("table")
        ),
        reactableOutput('gt4',width = "675px"),
        bsModal("modal_fw3", htmlOutput("tablededonnes1_fw3"), "button_fw3", size = "large"
                ,excelOutput("excel_fw3"))
    )
  })
  
  
  output$ui_boxscore_attribution<-renderUI({
    div(id = "box-score_attribution",
        style=`if`(as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]) %in% c(100,101,102) | substr(input$select_arretee_fichewilaya,14,23)=="2020-06-30" |substr(input$select_arretee_fichewilaya,14,23)=="2020-03-31",
                   paste0("display:none;"),
                   paste0("display:block;")
                   
                   
        ),
        #"display:","none;"),
        
        actionBttn(
          inputId = "button_fw_attribution",
          label = NULL,
          style = "bordered",
          color = "success",
          icon = icon("table")
        ),
        div(id = "box-score-title_attribution",
            style="
                margin-top: 24px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1230px;
    font-family: system-ui;
            ",
            "Programme d'attribution de logements et aides arrété au",textOutput("boxscore_attribution_arretee"),textOutput("boxscore_attribution_wilaya")),
        reactableOutput('gt_attribution',width = '1232px'),
        bsModal("modal_fw_attribution", htmlOutput("tablededonnes1_fw_attribution"), "button_fw_attribution", size = "large"
                ,excelOutput("excel_fw_attribution"))
        
    )
    
    
  })
  
  
  
  
  
  
  
  output$ui_boxscore2<-renderUI({
    div(id = "box-score2",
        style=`if`(as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]) %in% c(100,101,102),
                   paste0("display:none;"),
                   paste0("display:block;")
                   
          
        ),
          #"display:","none;"),
        
        actionBttn(
          inputId = "button_fw2",
          label = NULL,
          style = "bordered",
          color = "success",
          icon = icon("table")
        ),
        div(id = "box-score-title2",
            style="
                margin-top: 24px;
    padding: 2px;
    padding-left: 5px;
    background-color: #81a47b;
    color: white;
    font-size: 22px;
    font-weight: 400;
    width: 1257px;
    font-family: system-ui;
            ",
            "Aides à la réhabilitation au ",textOutput("boxscore2_arretee"),textOutput("boxscore2_wilaya")),
        reactableOutput('gt2',width = '1258px'),
        bsModal("modal_fw2", htmlOutput("tablededonnes1_fw2"), "button_fw2", size = "large"
                ,excelOutput("excel_fw2"))

    )
    

  })

  
  
  
  
  reactive_excel_fw7<-reactive({
    dff=df3_ab
    
    # dff$Trimestre=`if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==3,
    #      rep(paste("1er trimestre",substr(input$select_arretee_fichewilaya,14,17)),6*48),
    #      `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==6,
    #           rep(paste("2éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),6*48),
    #           `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==9,
    #                rep(paste("3éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),6*48),
    #                `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==12,
    #                     rep(paste("4éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),6*48)
    #                ))))
    
    
    
    dff$Saison=rep(paste(str_sub(input$select_title_gt7,13,-1)),9*61)
      # `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==3,
      #                  rep(paste("1er trimestre",substr(input$select_arretee_fichewilaya,14,17)),9*61),
      #                  `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==6,
      #                       rep(paste("2éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),9*61),
      #                       `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==9,
      #                            rep(paste("3éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),9*61),
      #                            `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==12,
      #                                 rep(paste("4éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),9*61)
      #                            ))))
    
    
    
    
    
    #dfw=data_fiche_wilaya %>% filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23))
    
    
    dfw=data_fiche_wilaya %>% filter(id_wilaya %in% c(1:58,100,101,102),arretee %in% `if`(str_sub(input$select_title_gt7,1,-6)=="Activité du 1er semestre",
                             c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-03-31")),
                               as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-06-30"))
                             ),
                             `if`(str_sub(input$select_title_gt7,1,-6)=="Activité du 01-01 au 30-09",
                                  c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-03-31")),
                                    as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-06-30")),
                                    as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-09-30"))
                                  ),
                                  `if`(str_sub(input$select_title_gt7,1,-6)=="Activité du 2ème semestre",
                                       c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-09-30")),
                                         as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-12-31"))
                                       ),
                                       `if`(str_sub(input$select_title_gt7,1,-6)=="Activité de l'année",
                                            c(as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-03-31")),
                                              as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-06-30")),
                                              as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-09-30")),
                                              as.Date(paste0(substr(input$select_arretee_fichewilaya,14,17),"-12-31"))
                                            ),
                                            as.Date(substr(input$select_arretee_fichewilaya,14,23))
                                       ))))
    ) %>% group_by(Wilaya) %>% summarise_at(vars(2:126),sum,na.rm=TRUE)
    
    
    
    
    
    dff$`Livraisons Réalisés`=c(dfw$lpl_livres,
                                   dfw$lsp_livres,
                                   dfw$rural_livres,
                                   dfw$lv_livres,
                                   dfw$lv_cnep_livres,
                                   dfw$lpp_livres,
                                   dfw$acls_livres,
                                   dfw$autresocial_livres,
                                   dfw$promotionellelibre_livres
                                
  
                                
                                )
    
    dff$`Livraisons Prévus`=c(dfw$lpl_prevus,
                                 dfw$lsp_prevue,
                                 dfw$rural_prevue,
                                 dfw$lv_pruve,
                                 dfw$lv_cnep_pruve,
                                 dfw$lpp_pruve,
                                 dfw$acls_pruve,
                                 dfw$autresocial_pruve,
                                 dfw$promotionellelibre_pruve
                              
                              )
    
    
    dff$`Lancements Réalisés`=c(dfw$lpl_lanc,
                                   dfw$lsp_lanc,
                                   dfw$rural_lanc,
                                   dfw$lv_lanc,
                                   dfw$lv_cnep_lanc,
                                   dfw$lpp_lanc,
                                   dfw$acls_lanc,
                                dfw$autresocial_lanc,
                                dfw$promotionellelibre_lanc
                                
                                )
    
    
    dff$`Lancements Prévus`=c(dfw$lpl_prevus_lanc,
                                 dfw$lsp_prevue_lanc,
                                 dfw$rural_prevue_lanc,
                                 dfw$lv_pruve_lanc,
                                 dfw$lv_cnep_pruve_lanc,
                                 dfw$lpp_pruve_lanc,
                                 dfw$acls_pruve_lanc,
                                 dfw$autresocial_pruve_lanc,
                                 dfw$promotionellelibre_pruve_lanc
                              )
    
    dff$`Taux de Livraisons`=round(100*(dff$`Livraisons Réalisés`/dff$`Livraisons Prévus`),1)
    dff$`Taux de Lancements`=round(100*(dff$`Lancements Réalisés`/dff$`Lancements Prévus`),1)
    
    
    dff$Annulées=c(dfw$anuules_durant_ce_trimestre_lpl,
                      dfw$anuules_durant_ce_trimestre_lsp,
                      dfw$anuules_durant_ce_trimestre_rural,
                      dfw$anuules_durant_ce_trimestre_lv,
                      dfw$anuules_durant_ce_trimestre_lv_cnep,
                      dfw$anuules_durant_ce_trimestre_lpp,
                      dfw$anuules_durant_ce_trimestre_acls,
                      dfw$anuules_durant_ce_trimestre_autresocial,
                      dfw$anuules_durant_ce_trimestre_promotionellelibre
    )
    
    dff$Notifié=c(dfw$notifie2020_lpl,
                     dfw$notifie2020_lsp,
                     dfw$notifie2020_rural,
                     dfw$notifie2020_lv,
                     dfw$notifie2020_lv_cnep,
                     dfw$notifie2020_lpp,
                     dfw$notifie2020_acls,
                  dfw$notifie2020_autresocial,
                  dfw$notifie2020_promotionellelibre
                  
                  )
    
    dff[mapply(is.infinite, dff)] <- NA
    dff[mapply(is.nan, dff)] <- NA
    
    
    dff=rbind(colnames(df3_ab),
          dff)
    
    
    dff
    
  })
  
  
  reactive_excel_fw1<-reactive({
    dff=df2_ab
    dff$Arrétée=rep(substr(input$select_arretee_fichewilaya,14,23),61)
    
    
    
    dff$Consistance=c(data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_lpl) %>% .$consistance_lpl,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_lsp)%>% .$consistance_lsp,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_rural)%>% .$consistance_rural,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_lv)%>% .$consistance_lv,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_lv_cnep)%>% .$consistance_lv_cnep,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_lpp) %>% .$consistance_lpp,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_acls) %>% .$consistance_acls,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_autresocial) %>% .$consistance_autresocial,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(consistance_promotionellelibre) %>% .$consistance_promotionellelibre
                      
                      
                      
    )
    
    
    dff$Achevés=    c(data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_lpl) %>% .$acheves_lpl,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_lsp)%>% .$acheves_lsp,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_rural)%>% .$acheves_rural,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_lv)%>% .$acheves_lv,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_lv_cnep)%>% .$acheves_lv_cnep,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_lpp) %>% .$acheves_lpp,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_acls) %>% .$acheves_acls,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_autresocial) %>% .$acheves_autresocial,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(acheves_promotionellelibre) %>% .$acheves_promotionellelibre
                      
    )
    
    
    dff$`En Cours`= c(data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_lpl) %>% .$encours_lpl,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_lsp)%>% .$encours_lsp,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_rural)%>% .$encours_rural,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_lv)%>% .$encours_lv,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_lv_cnep)%>% .$encours_lv_cnep,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_lpp) %>% .$encours_lpp,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_acls) %>% .$encours_acls,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_autresocial) %>% .$encours_autresocial,
                      data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(encours_promotionellelibre) %>% .$encours_promotionellelibre
                      
    )
    
    
    dff$`Dont à l'arrêt`= c(data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_lpl) %>% .$dont_arret_encours_lpl,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_lsp)%>% .$dont_arret_encours_lsp,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_rural)%>% .$dont_arret_encours_rural,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_lv)%>% .$dont_arret_encours_lv,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_lv_cnep)%>% .$dont_arret_encours_lv_cnep,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_lpp) %>% .$dont_arret_encours_lpp,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_acls) %>% .$dont_arret_encours_acls,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_autresocial) %>% .$dont_arret_encours_autresocial,
                            data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(dont_arret_encours_promotionellelibre) %>% .$dont_arret_encours_promotionellelibre
                            
    )
    
    dff$`Non Lancés`= c(data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_lpl) %>% .$nonlances_lpl,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_lsp)%>% .$nonlances_lsp,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_rural)%>% .$nonlances_rural,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_lv)%>% .$nonlances_lv,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_lv_cnep)%>% .$nonlances_lv_cnep,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_lpp) %>% .$nonlances_lpp,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_acls) %>% .$nonlances_acls,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_autresocial) %>% .$nonlances_autresocial,
                        data_fiche_wilaya %>%   filter(id_wilaya %in% c(1:58,100,101,102),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>% select(nonlances_promotionellelibre) %>% .$nonlances_promotionellelibre
                        
                        
    )
    
    
    
    #colnames(dff)=rep("",)
    
    
    ghh=data_fiche_wilaya %>%
      filter(id_wilaya %in% c(1:58,100,101,102),
        arretee<=as.Date(substr(input$select_arretee_fichewilaya,14,23)),
        arretee>as.Date(ISOdate(format(as.Date(substr(input$select_arretee_fichewilaya,14,23), format="%Y-%m-%d"),"%Y"), 1, 1))
      ) %>%
      select(id_wilaya,notifie2020_lpl,notifie2020_lsp,notifie2020_rural,notifie2020_lv,notifie2020_lpp,notifie2020_lv_cnep,notifie2020_acls,notifie2020_autresocial,notifie2020_promotionellelibre) %>%
      group_by(id_wilaya) %>% 
      summarise_at(vars(notifie2020_lpl:notifie2020_promotionellelibre),sum) %>% data.frame()
    
    
    dff$Notifie[dff$Segment=="LPL"]=ghh$notifie2020_lpl
    dff$Notifie[dff$Segment=="LSP/LPA"]=ghh$notifie2020_lsp
    dff$Notifie[dff$Segment=="Rural"]=ghh$notifie2020_rural
    dff$Notifie[dff$Segment=="Location-Vente"]=ghh$notifie2020_lv
    dff$Notifie[dff$Segment=="LV CNEP BANQUE"]=ghh$notifie2020_lv_cnep
    dff$Notifie[dff$Segment=="LPP"]=ghh$notifie2020_lpp
    dff$Notifie[dff$Segment=="ACLS"]=ghh$notifie2020_acls
    dff$Notifie[dff$Segment=="Autre social"]=ghh$notifie2020_autresocial
    dff$Notifie[dff$Segment=="Promotionnel Libre"]=ghh$notifie2020_promotionellelibre
    
    
    
    dff
    
    
    }) 

  
  # output$ui_boxscore6<-renderUI({
  #   div(id='box-score6',style=`if`(substr(input$select_arretee_fichewilaya,14,23) %in% c('2020-12-31','2020-09-30','2020-03-31'),paste0('display:none'),paste0('display:block')),
  #        div(class = "box-score-title", "État des Lancements au ",textOutput("boxscore6_arretee"),textOutput("boxscore6_wilaya")),
  #        reactableOutput('gt6'),
  #       actionBttn(
  #         inputId = "button_fw5",
  #         label = NULL,
  #         style = "bordered", 
  #         color = "success",
  #         icon = icon("table")
  #       )
  #   )
  # })

  
  output$excel_mr1 <-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
rbind(moyens_realisation2 %>% select(2,4:30) %>% colnames() %>% unlist(),          
#rbind(c("Wilaya","Cat1","Cat2","Cat3","Cat4","Cat5","Cat6","Cat7","Cat8","Cat9","Total"),  
    moyens_realisation2 %>%
        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
        group_by(Wilaya) %>% 
        summarise_at(vars(Cat1:`Cat9 Dont : Nbre d'entreprises spécialisées`), sum)
    ,
    c("Total",
      moyens_realisation2 %>%
        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>% 
        summarise_at(vars(Cat1:`Cat9 Dont : Nbre d'entreprises spécialisées`), sum) %>% unlist(use.names=FALSE)
      )

    
      ) %>% `colnames<-`(rep("",28))
      )
  })
  
  
  output$excel_mr2 <-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c("Wilaya","Nbre de dossiers déposés","dont Par l'application E-Certif","Nbre de dossiers éxaminés","dont par l'application E-Certif"),  
                     moyens_realisation2 %>%
                       filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                       group_by(Wilaya) %>% 
                       summarise_at(vars(`Nbre de dossiers déposés`:`dont par l'application E-Certif`), sum) %>%
                       `colnames<-`(rep("",4)),
                     
                     
                     c("Total",
                       moyens_realisation2 %>%
                         filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>% 
                         summarise_at(vars(`Nbre de dossiers déposés`:`dont par l'application E-Certif`), sum,na.rm=TRUE) %>%
                         as.double()
                     )
                     
               )
    )
  })
  
  
  
  output$excel_mr3 <-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c("Wilaya","Communes","Nbre de lots","Nbre de bénéficiaires de Lots","Nbre d'aides notifiées","Nbre de bénéficiaires d'aides"),  
                     moyens_realisation2 %>%
                       filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                       group_by(Wilaya) %>% 
                       summarise_at(vars(`Communes`:`Nbre de bénéficiaires d'aides`), sum) %>%
                       `colnames<-`(rep("",6)),
                     
                     
                     c("Total",
                       moyens_realisation2 %>%
                         filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>% 
                         summarise_at(vars(`Communes`:`Nbre de bénéficiaires d'aides`), sum,na.rm=TRUE) %>%
                         as.double()
                     )
                     
               )
    )
  })
  
  
  
  
  output$excel_mr4 <-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c("Wilaya","Nbre dossiers reçus","Nbre dossiers éxaminés par la Commision","Nbre d'agréments établis","Nbre demande d'inscription au TNPI"),  
                     moyens_realisation2 %>%
                       filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                       group_by(Wilaya) %>% 
                       summarise_at(vars(`Nbre dossiers reçus_PI`:`Nbre demande d'inscription au TNPI`), sum) %>%
                       `colnames<-`(rep("",5)),
                     
                     
                     c("Total",
                       moyens_realisation2 %>%
                         filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>% 
                         summarise_at(vars(`Nbre dossiers reçus_PI`:`Nbre demande d'inscription au TNPI`), sum,na.rm=TRUE) %>%
                         as.double()
                     )
                     
               )
    )
  })
  
  

  output$excel_mr5 <-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(
                 #c("Wilaya",moyens_realisation2 %>% select_at(vars(`Nbre dossiers reçus_AI`:`Avisfavorable_C.I`)) %>% colnames()),
                 c("Wilaya","Nbre dossiers reçus",
                   "Depots (A.I)","Depots (A.B.I)","Depots (A.I + A.B.I)","Depots (C.I)",
                   "Examinés (A.I)","Examinés (A.B.I)","Examinés (A.I + A.B.I)","Examinés (C.I)",
                   "Avis Favorables (A.I)","Avis Favorables (A.B.I)","Avis Favorables (A.I + A.B.I)","Avis Favorables (C.I)"
                 ),
                 moyens_realisation2 %>% 
                       filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                       group_by(Wilaya) %>%
                       summarise_at(vars(`Nbre dossiers reçus_AI`:`Avisfavorable_C.I`), sum) %>%
                       `colnames<-`(rep("",14)),


                     c("Total",
                       moyens_realisation2 %>%
                         filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                         summarise_at(vars(`Nbre dossiers reçus_AI`:`Avisfavorable_C.I`), sum,na.rm=TRUE) %>%
                         as.double()
                     )

               )
    )
  })
  
  
  
  

  
  wilaya58_reactive<-reactive({
    wilayas58 %>%
      filter(wilayas58$wilaya==input$mapsalgerie_shape_click[1]) %>%
      select(3,4,5,6) %>% 
      rename("Nombre d'agences CTC"="Nb_CTC",
             "Nombre d'agences LNHC"="Nb_LNHC",
             "Nombre d'agences GEE"="Nb_GEE",
             "Nombre d'agences GREEN"="Nb_GREEN") %>%
      t()
  })
  
  output$reactable_58wilayas<-renderReactable({
    reactable(wilaya58_reactive(),
              fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "16px"),
              sortable = FALSE,
              columns = list(
                V1=colDef(name=""),
                .rownames =colDef(width=350)
                
              )
    )
  })
  
  output$sasa<-renderText({
    paste0(input$mapsalgerie_shape_click[1])
  })
  
  
  observeEvent(input$mapsalgerie_shape_click,{
    click("preview")
  })
  
  observeEvent(input$distPlot2_shape_click,{
    updatePickerInput(session=session,inputId='wilayas',
                      selected=input$distPlot2_shape_click[1]
    )
  })
  

  
  output$mapsalgerie<-renderLeaflet({
    mapdz58 %>%
      clearControls() %>% 
      # addLegend(
      #   position = "topright",
      #   title=HTML("Ingenieurs (F)"),
      #   pal=   colorBin("BrBG",bins = 10,mara$cata14),
      #   opacity = 1,
      #   values=mara$cata14
      # ) %>%
      addPolygons(weight=1,
                  fillColor = 
                    #colorBin(brewer.pal(5,"Spectral"))(wilayas58$Region)
                    #colorBin("Spectral",reverse=TRUE,bins = 10,wilayas58$field)(wilayas58$field)
                    colorBin("Spectral",bins = 10,wilayas58$Region)(wilayas58$Region)
                  
                  
                  
                  ,color ="black",
                  label =
                    sprintf('<strong style="font-size:18px;">%s</strong><br/>',
                            wilayas58$wilaya
                            
                            
                    ) %>% lapply(htmltools::HTML),
                  fillOpacity = 0.99,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal",
                                 "font-size"="25px",
                                 padding = "3px 8px"),
                    textsize = "5px",
                    direction = "left",
                    offset = c(100,45)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 0.7,bringToFront=TRUE,fillColor = "red"
                  ),layerId = wilayas58$wilaya
      )
    
  })
  
  
  
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
  
  
  
  
  mc_ai_reactive<-reactive({
    moyens_realisation2 %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(`Nbre dossiers reçus_AI`:`Avisfavorable_C.I`),sum,na.rm=TRUE)
  })
  
  
  output$gt5_mr<-renderReactable({
    reactable(mc_ai_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                
                `Nbre dossiers reçus_AI`=colDef(
                  name="Nbre dossiers reçus",
                  width=200,
                  style = list(fontFamily = "Work Sans, sans-serif",
                               fontWeight = "bold",textAlign="center",
                               fontSize = "20px"),
                  headerStyle = list(
                    marginTop="-22px",
                    #border="0px solid #eee",
                    fontWeight = "bold",
                    textAlign="center",
                    background = "#2E81B0",
                    color="#ffffff"
                  )),
                
       
                `Depot_A.I`=colDef(
                  name="A.I"
                ),
                `Depot_A.B.I`=colDef(
                  name="A.B.I"
                ),
                `Depot_A.I + A.B.I`=colDef(
                  name="A.I + A.B.I"
                ),
                `Depot_C.I`=colDef(
                  name="C.I"
                ),
                
                
                
                `Examine_A.I`=colDef(
                  name="A.I"
                ),
                `Examine_A.B.I`=colDef(
                  name="A.B.I"
                ),
                `Examine_A.I + A.B.I`=colDef(
                  name="A.I + A.B.I"
                ),
                `Examine_C.I`=colDef(
                  name="C.I"
                ),
                
                
                
                `Avisfavorable_A.I`=colDef(
                  name="A.I"
                ),
                `Avisfavorable_A.B.I`=colDef(
                  name="A.B.I"
                ),
                `Avisfavorable_A.I + A.B.I`=colDef(
                  name="A.I + A.B.I"
                ),
                `Avisfavorable_C.I`=colDef(
                  name="C.I"
                )
                
              ),
              columnGroups = list(
                colGroup(name = "Dépôts", columns = c("Depot_A.I","Depot_A.B.I","Depot_A.I + A.B.I","Depot_C.I"),
                         headerStyle =list(
                           borderRight="65px inset transparent",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                ),
                colGroup(name = "Éxaminés", columns = c("Examine_A.I","Examine_A.B.I","Examine_A.I + A.B.I","Examine_C.I"),
                         headerStyle =list(
                           borderRight="65px inset transparent",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                ),
                
                
                colGroup(name = "Avis Favorable", columns = c("Avisfavorable_A.I","Avisfavorable_A.B.I","Avisfavorable_A.I + A.B.I","Avisfavorable_C.I"),
                         headerStyle =list(
                           borderRight="65px inset transparent",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                ),
                
                colGroup(name = "", columns = c("Nbre dossiers reçus_AI"),
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
                align = "left",
                width = 80,
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                #maxWidth = 260,
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff"
                )
              )
    )
  })
  
  
  
  mc_pi_reactive<-reactive({
    moyens_realisation2 %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(`Nbre dossiers reçus_PI`:`Nbre demande d'inscription au TNPI`),sum,na.rm=TRUE)
  })
  
  output$gt4_mr<-renderReactable({
    reactable(mc_pi_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                `Nbre dossiers reçus_PI`=colDef(
                  name="Nbre dossiers reçus",
                  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px",fontWeight="bold")
                  )
              ),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                align = "center",
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
  
  
  #mc_promoteur_exercice_reactive<-reactive({
  mc_ls_reactive<-reactive({
    moyens_realisation2 %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(`Communes`:`Nbre de bénéficiaires d'aides`),sum,na.rm=TRUE)
  })
  
  
  output$gt3_mr<-renderReactable({
    reactable(mc_ls_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                `Communes`=colDef(
                  width=150
                ),
                `Nbre de lots`=colDef(
                  width=150,
                  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px",fontWeight="bold")
                  
                ),
                  headerStyle = list(paddingRight="30px",
                                     background = "#2E81B0",
                                     color="#ffffff"
                  )
              ),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                align = "center",
                width = 240,
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
    moyens_realisation2 %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars("Nbre de dossiers déposés":"dont par l'application E-Certif"),sum,na.rm=TRUE)
    })
  
  output$gt2_mr<-renderReactable({
    reactable(mr_promoteur_reactive()[1,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              #fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                `Nbre de dossiers déposés`=colDef(
                  width=260,
                  style=list(fontFamily = "Work Sans, sans-serif", fontSize = "20px",fontWeight="bold")
                ),
                `Nbre de dossiers éxaminés`=colDef(
                  width=220,
                  style=list(fontFamily = "Work Sans, sans-serif", fontSize = "20px",fontWeight="bold")
                )
              ),
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                align = "center",
                sortNALast = TRUE,
                width = 220,
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
    # moyens_realisation2 %>%
    #   filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
    #   summarise_at(vars(Cat1:Cat9), sum) %>% 
    #   mutate(Total1=Cat1+Cat2+Cat3+Cat4,.after=4) %>%
    #   mutate(Total2=Cat5+Cat6+Cat7+Cat8+Cat9) %>% 
    #   mutate(Total_general=Cat1+Cat2+Cat3+Cat4+Cat5+Cat6+Cat7+Cat8+Cat9)
    # 
    # 
    
    ##
    
    moyens_realisation2 %>%
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
      summarise_at(vars(Cat1,Cat2,Cat3,Cat4,Cat5,Cat6,Cat7,Cat8,Cat9),sum) %>%
    rbind(
        moyens_realisation2 %>% 
          filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
          
          summarise_at(vars(
            `Cat1 Dont : Nbre d'entreprises spécialisées`,
            `Cat2 Dont : Nbre d'entreprises spécialisées`,
            `Cat3 Dont : Nbre d'entreprises spécialisées`,
            `Cat4 Dont : Nbre d'entreprises spécialisées`,
            `Cat5 Dont : Nbre d'entreprises spécialisées`,
            `Cat6 Dont : Nbre d'entreprises spécialisées`,
            `Cat7 Dont : Nbre d'entreprises spécialisées`,
            `Cat8 Dont : Nbre d'entreprises spécialisées`,
            `Cat9 Dont : Nbre d'entreprises spécialisées`),sum) %>% 
          unlist(use.names = FALSE)
      ) %>%  rbind(
        
        moyens_realisation2 %>% 
          filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23), id_wilaya %in% c(selected20_fichewilaya_mr())) %>%
          
          summarise_at(vars(
            `Cat1 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat2 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat3 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat4 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat5 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat6 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat7 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat8 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`,
            `Cat9 Dont : Crées dans le cadre des dispositifs d'aides ( CNAC - ANSEJ ….)`),sum) %>% 
          unlist(use.names = FALSE)
      ) %>%  mutate(
          atype=c("Nombre d'entreprises",
             "Dont : Nbre d'entreprises spécialisées",
             "Dont : Crées dans le cadre des dispositifs d'aides (CNAC-ANSEJ…)"
             ),
          .before=1
          ) %>% 
         mutate(Total1=Cat1+Cat2+Cat3+Cat4,.after=5) %>%
         mutate(Total2=Cat5+Cat6+Cat7+Cat8+Cat9) %>% 
         mutate(Total_general=Cat1+Cat2+Cat3+Cat4+Cat5+Cat6+Cat7+Cat8+Cat9)
    
    
    
    
    
    
      
  })
  
  
  
  output$gt1_mr<-renderReactable({
    reactable(mr_entreprise_reactive(),
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                atype = colDef(name = "",
                                 width=245,
                                 style =list(
                                   fontFamily = "Work Sans, sans-serif", fontSize = "18px",
                                   background='#81a47b',color='#ffffff'),
                                 headerStyle = list(
                                   #borderTop="0px",
                                   marginTop="-31px",
                                   textAlign="left",
                                   background = "#2E81B0",
                                   color="#ffffff"
                                 )
                ),
                Total_general=colDef(
                  name="Total general",
                  width=90,
                  style = list(fontWeight = "bold",textAlign="center"),

                  headerStyle = list(
                    marginTop="-22px",
                    #border="0px solid #eee",
                    fontWeight = "bold",
                    textAlign="center",
                    background = "#2E81B0",
                    color="#ffffff",
                    width=90
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
                colGroup(name = "", columns = c("atype"),
                         headerStyle =list(
                           #border="0px solid #eee",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                ),
                colGroup(name = "Au niveau de Wilaya", columns = c("Cat1","Cat2","Cat3","Cat4","Total1"),
                         headerStyle =list(
                           borderLeft="65px inset transparent",
                           borderRight="75px inset transparent",
                           textAlign="center",
                           background = "#2E81B0",
                           color="#ffffff"
                         )
                ),
                colGroup(name = "Au niveau du Ministère", columns = c("Cat5","Cat6","Cat7","Cat8","Cat9","Total2"),
                         headerStyle =list(
                           borderLeft="65px inset transparent",
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
                width =  80,
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff",
                                   width=80
                )
              )
    )
    
  })
  
  addResourcePath("pdf_files_DGCMR", paste0(getwd(),"/pdf_files_DGCMR"))
  
#   delay(6000,
#         list(
#   show('homepage_button1',anim=TRUE,time=3,animType='fade'),
#   show('homepage_button2',anim=TRUE,time=3,animType='fade'),
#   show('homepage_button3',anim=TRUE,time=3,animType='fade')
# ))
  
  output$homepage_ren<-renderUI({
    tags$iframe(seamless="seamless",
                src=paste0("pdf_files_DGCMR/","index6.html"),
                width="100%",
                height="830px",
                style='border:0px'
    )
    
  })
  
  
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
  
  
  output$html_homepage1<-renderUI({
    tags$iframe(
      seamless="seamless",
      src=paste0("pdf_files_DGCMR/ORG_MHUV_FR.html"),
      width="1600px",
      height="780px"
    )
  })
  
  output$html_homepage3<-renderUI({
    
    fluidRow(id='organisme_fluidrow',
             actionButton("preview", "Preview",style="display:none;"),
             
             leafletOutput("mapsalgerie",height = 950),
             
             bsModal("modal_wilayas58", textOutput("sasa"), "preview", size = "large"
                     ,reactableOutput("reactable_58wilayas"))
    )
    
  })
  
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
    updateSelectInput(session,inputId ="search",
                      label = paste("Rechercher sur la carte :"),
                      choices =paste(datamc %>%
                                       filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
                                              Filiere %in% filiere_mc_reactive(),
                                              Statut2 %in% statut_mc_reactive()
                                       ) %>%
                                       select(Identification) %>% .$Identification
                      )
                      
    )
  })
  
  
  output$nd_mc_maps<-renderText({
    `if`(length(input$search)==0,
         paste0(datamc %>%
                  filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
                         Filiere %in% filiere_mc_reactive(),
                         Statut2 %in% statut_mc_reactive()
                  ) %>%
                  select(longitude,latitude) %>%
                  filter(is.na(longitude)==TRUE | is.na(latitude)==TRUE) %>%
                  nrow(),"/",
                datamc %>%
                  filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
                         Filiere %in% filiere_mc_reactive(),
                         Statut2 %in% statut_mc_reactive()
                  ) %>% nrow()
                ," ND (sans coordonnées geographiques )"
         )
    )
  })
  
  output$annee_na_count<-renderText({
    paste0(
      nrow(datamc %>%
             filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
                    Filiere %in% filiere_mc_reactive(),
                    Statut2 %in% statut_mc_reactive(),
                    is.na(Annee_entree)==TRUE
             ))
      ,"/",
      nrow(datamc %>%
             filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
                    Filiere %in% filiere_mc_reactive(),
                    Statut2 %in% statut_mc_reactive()
             ))
      ," ND (sans Annee d'entree)")
  })
  
  output$pivot_table_mc<-renderRpivotTable({
    rpivotTable(
      datamc %>%
        filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
               Filiere %in% filiere_mc_reactive(),
               Statut2 %in% statut_mc_reactive()
        ) %>%
        mutate(Statut2=replace(Statut2,Statut2=="Mixte (Public et Etrangere)","Public et Etrangere")) %>% 
        mutate(Statut2=replace(Statut2,Statut2=="Mixte (Public et Prive)","Public et Prive")) %>% 
        select(1,4,10,12) %>%
        rename(Wilaya="wilaya_matricule","Statut"="Statut2","Annee dentree"=Annee_entree) %>% arrange(desc(Statut))
      ,rows="Wilaya",cols="Statut",height="200px"
    )
  })
  
  output$tableau_mc<-renderReactable({
    
    reactable(
      datamc %>%
        filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
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
      filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
             #,
             #Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
      ) %>%
      summarise(nb=n()) %>% .$nb
  })
  
  # mc_annee_debut_reactive<-reactive({
  #   datamc %>%
  #     filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
  #            #,
  #            #Annee_entree>=min(input$annees_mc),Annee_entree<=max(input$annees_mc)
  #     ) %>% 
  #     group_by(Annee_entree) %>% 
  #     summarise(nb=n()) %>%
  #     drop_na()
  # })
  
  # output$mc_annee_debut<-renderHighchart({
  #   hchart(mc_annee_debut_reactive(),name="Nombre d'unités",type="line",hcaes(x=Annee_entree,y=nb)) %>% 
  #     hc_title(text = "Nombre d'unités de construction",align="left") %>% 
  #     hc_subtitle(text = "Par Année d'entrée en production",align="left") %>% 
  #     hc_xAxis(title=list(text = "Année d'entrée en production")) %>% 
  #     hc_yAxis(title=list(text=""))
  #   
  # })
  
  
  mc_filiere_reactive=reactive({
    datamc %>% 
      filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
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
                Filiere=colDef(name="",width = 175,footer ="Total"),
                Nb=colDef(style = list(fontWeight="bold"),footer = sum(mc_filiere_reactive()$Nb))
              )
    )
  })
  
  
  mc_wilaya_reactive=reactive({
      datamc %>% 
        filter(Arretee==substr(input$select_arretee_datamc,14,23),
               #wilaya_matricule %in% wilaya_mc_reactive(),
               Filiere %in% filiere_mc_reactive(),
               Statut2 %in% statut_mc_reactive()
               #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
        ) %>% 
        group_by(wilaya_matricule) %>% 
        summarise(Nb=n()) %>%
        select(wilaya_matricule,Nb)
    
  })
  
  output$mc_wilaya_reactable<-renderReactable({
    reactable(mc_wilaya_reactive(),
              defaultSorted =  list(Nb = "desc"),
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
                wilaya_matricule=colDef(name="",width = 210,footer = "Total"),
                Nb=colDef(style = list(fontWeight="bold"),footer = sum(mc_wilaya_reactive()$Nb))
              ),
              rowStyle = function(index) {
                if (mc_wilaya_reactive()[index, "wilaya_matricule"] %in% input$wilayas_mc) {
                  list(background = "#a8a8a8",fontWeight="bold",fonSize="18px")
                }
              }
              
    )
  })
  
  
  
  
  
  
  
  
  mc_statut_reactive=reactive({
    `if`(
      nrow(datamc %>% 
             filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
                    #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
             ) %>% 
             group_by(Statut2) %>% 
             summarise(Nb=n()))!=6,
      datamc %>% 
        filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
               #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
        ) %>% 
        group_by(Statut2) %>% 
        summarise(Nb=n()) %>%
        select(Statut2,Nb)
      ,
      datamc %>% 
        filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
               #,Annee_entree2>=min(input$annees_mc),Annee_entree2<=max(input$annees_mc)
        ) %>% 
        group_by(Statut2) %>% 
        summarise(Nb=n()) %>%
        mutate(ar=c(3,5,4,6,1,2)) %>% 
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
    paste0("Activité du ",substr(input$select_arretee_fichewilaya_mr,14,17),"-01-01  au ",substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  output$boxscore3_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  output$boxscore4_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  
  output$boxscore5_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  output$boxscore6_arretee_mr=renderText({
    paste(substr(input$select_arretee_fichewilaya_mr,14,23))
  })
  
  
  output$boxscore1_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  output$boxscore7_arretee=renderText({
    `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==3,
         paste("Activité du 1er trimestre",substr(input$select_arretee_fichewilaya,14,17)),
         `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==6,
              paste("Activité du 2éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),
              `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==9,
                   paste("Activité du 3éme trimestre",substr(input$select_arretee_fichewilaya,14,17)),
                   `if`(as.numeric(substr(input$select_arretee_fichewilaya,19,20))==12,
                        paste("Activité du 4éme trimestre",substr(input$select_arretee_fichewilaya,14,17))
                   ))))
    
  })
  
  
  
  output$boxscore2_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  output$boxscore_attribution_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  
  output$boxscore8_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  output$boxscore9_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  
  output$boxscore9_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  
  output$boxscore3_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  
  output$boxscore5_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  output$boxscore6_arretee=renderText({
    paste(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  output$boxscore1_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("Toutes les Wilayas")
    )
  })
  
  output$boxscore2_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("Toutes les Wilayas")
    )
  })
  
  output$boxscore3_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation2$Wilaya)[selected20_fichewilaya_mr()]),paste("Wilayas du Sud et Haut Plateau")
    )
  })
  
  output$boxscore4_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("Toutes les Wilayas")
    )
  })
  
  
  output$boxscore5_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("Toutes les Wilayas")
    )
  })
  
  output$boxscore6_wilaya_mr=renderText({
    `if`(length(selected20_fichewilaya_mr())==1,
         paste(unique(moyens_realisation$Wilaya)[selected20_fichewilaya_mr()]),paste("Toutes les Wilayas")
    )
  })
  
  output$boxscore1_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("Toutes les Wilayas")
    )

    #paste(selected20_fichewilaya())
    
  })

  # output$boxscore1_wilaya=renderText({
  #   `if`(length(selected20_fichewilaya())==1,
  #        paste(selected20_fichewilaya()),paste("Toutes les Wilayas")
  #   )
  # })
  
  
  
  output$boxscore7_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("Toutes les Wilayas")
    )
  })
  
  
  output$boxscore2_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("Toutes les Wilayas")
    )
  })
  
  output$boxscore_attribution_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("Toutes les Wilayas")
    )
  })
  
  
  output$boxscore9_wilaya=renderText({
    `if`(length(selected20_fichewilaya2())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()]),paste("Tout les OPGI")
    )
  })
  
  output$boxscore3_wilaya=renderText({
    `if`(length(selected20_fichewilaya2())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()]),paste("Toutes les Wilayas")
    )
  })
  
  
  output$boxscore8_wilaya=renderText({
    `if`(length(selected20_fichewilaya2())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()]),paste("Toutes les Wilayas")
    )
  })
  
  
  
  output$boxscore5_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("Toutes les Wilayas")
    )
  })
  
  output$boxscore6_wilaya=renderText({
    `if`(length(selected20_fichewilaya())==1,
         paste(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya()]),paste("Toutes les Wilayas")
    )
  })
  
  
  output$gt_attribution<-renderReactable({
    reactable(gt_attribution_reactive(),
              
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              columns = list(
                Etat = colDef(footer = "Taux",name="",
                              width=90,
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
                      borderLeft="0px",
                      paddingTop="16px"
                      
                      ),
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  #format = colFormat(percent = TRUE, digits = 1)
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[2]/values[1])
                    `if`(values[2]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                ),
                `LSP/LPA` = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[1]/values[2])
                    `if`(values[2]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                ),
                `Location-Vente` = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[1]/values[2])
                    `if`(values[2]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                  ,width=180,
                ),
                `Lotissement sociaux` = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  headerStyle = list(background = "#2E81B0",
                                     color="#ffffff",
                                     textAlign="center"
                  ),
                  
                  
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[1]/values[2])
                    `if`(values[2]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                  ,width=180,
                ),
                `Autre Logts` = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[1]/values[2])
                    `if`(values[2]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                  ,width=180,
                ),
                Rural = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[1]/values[2])
                    `if`(values[1]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                  
                ),
                LPP = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[1]/values[2])
                    `if`(values[2]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                  
                  
                  
                ),
                
                Total = colDef(
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  footer = function(values) {
                    # sprintf("%.0f%%",100*values[1]/values[2])
                    `if`(values[2]==0,'-',sprintf("%.1f%%",100*values[1]/values[2]) )
                    
                  }
                  
                  
                )
                
              ),
              defaultColDef = colDef(
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                maxWidth = 120,
                footerStyle = list(fontWeight = "bold"),
                headerStyle = list(background = "#2E81B0",
                                   color="#ffffff",
                                   textAlign="center",
                                   paddingTop="16px"
                                   
                )
                
                #headerClass = "box-score-header"
              )
    )
    
  })
  ############ fin gt6
  
  
  # 
  # output$gt5<-renderReactable({
  #   reactable(dfa_fichewilaya5(),
  #             
  #             sortable = FALSE,
  #             #fullWidth = FALSE,
  #             bordered = TRUE,
  #             style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
  #             columns = list(
  #               Segment = colDef(footer = "Taux",name="",
  #                                headerStyle = 
  #                                  list(
  #                                    background = "#2E81B0",
  #                                    color="#ffffff",  
  #                                    borderRight="0px")
  #               ),
  #               
  #               LPL = colDef(
  #                 headerStyle = 
  #                   list(
  #                     background = "#2E81B0",
  #                     color="#ffffff",  
  #                     borderLeft="0px"),
  #                 format = colFormat(separators = TRUE,locales = "fr-FR"),
  #                 
  #                 #format = colFormat(percent = TRUE, digits = 1)
  #                 footer = function(values) {
  #                   # sprintf("%.0f%%",100*values[2]/values[1])
  #                   `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
  #                   
  #                 }
  #               ),
  #               `LSP/LPA` = colDef(
  #                 format = colFormat(separators = TRUE,locales = "fr-FR"),
  #                 
  #                 footer = function(values) {
  #                   # sprintf("%.0f%%",100*values[2]/values[1])
  #                   `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
  #                   
  #                 }
  #               ),
  #               `Location-Vente` = colDef(
  #                 format = colFormat(separators = TRUE,locales = "fr-FR"),
  #                 
  #                 footer = function(values) {
  #                   # sprintf("%.0f%%",100*values[2]/values[1])
  #                   `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
  #                   
  #                 }
  #                 ,width=180,
  #               ),
  #               Rural = colDef(
  #                 format = colFormat(separators = TRUE,locales = "fr-FR"),
  #                 footer = function(values) {
  #                   # sprintf("%.0f%%",100*values[2]/values[1])
  #                   `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
  #                   
  #                 }
  #                 
  #               ),
  #               LPP = colDef(
  #                 format = colFormat(separators = TRUE,locales = "fr-FR"),
  #                 footer = function(values) {
  #                   # sprintf("%.0f%%",100*values[2]/values[1])
  #                   `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
  #                   
  #                 }
  #                 
  #                 
  #                 
  #               ),
  #               
  #               Total = colDef(
  #                 format = colFormat(separators = TRUE,locales = "fr-FR"),
  #                 footer = function(values) {
  #                   # sprintf("%.0f%%",100*values[2]/values[1])
  #                   `if`(values[1]==0,'-',sprintf("%.0f%%",100*values[2]/values[1]) )
  #                   
  #                 }
  #                 
  #                 
  #               )
  #               
  #             ),
  #             defaultColDef = colDef(
  #               sortNALast = TRUE,
  #               #format = colFormat(digits = 1),
  #               maxWidth = 120,
  #               footerStyle = list(fontWeight = "bold"),
  #               headerStyle = list(background = "#2E81B0",
  #                                  color="#ffffff"
  #               )
  #               
  #               #headerClass = "box-score-header"
  #             )
  #   )
  #   
  # })
  # 
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
  
  
  output$gt7_suite<-renderReactable({
    #`if`(input$pourcentage==FALSE,
    reactable(dfa_fichewilaya7_suite()[1:2,],
              sortable = FALSE,
              #fullWidth = FALSE,
              bordered = TRUE,
              fullWidth = FALSE,
              style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
              #defaultSorted = list(Segment = "desc"),
              showSortIcon = FALSE,
              
              columns = list(
                Segment = colDef(name = "",
                                 footer = "Programme Global",
                                 width=220,
                                 style =list(background='#81a47b',color='#ffffff'),
                                 headerStyle = list(
                                   #borderTop="0px",
                                   marginTop="-31px",
                                   textAlign="left"
                                   #background = "#2E81B0",
                                   #color="#ffffff"
                                 )
                ),
                Lances=colDef(
                  name="",
                  width=130,
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                   headerStyle = list(
                     borderLeft="25px inset transparent",
                     #textAlign="left",
                     #background = "#2E81B0",
                     #color="#ffffff",
                     width="130px"
                   ),
                  
                  footer=format(sum(dfa_fichewilaya7()$Lances,dfa_fichewilaya7_suite()$Lances,na.rm = TRUE)
                    , digits=3,big.mark=" ")
                
                  #   footer = function(values) {
                  #   format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                  # }
                  
                ),
                Livres=colDef(
                  name="",
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  footer=format(sum(dfa_fichewilaya7()$Livres,dfa_fichewilaya7_suite()$Livres,na.rm = TRUE)
                                , digits=3,big.mark=" ")
                  
                  # footer = function(values) {
                  #   format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                  # }
                ),
                Prevus_livres=colDef(
                  name="",
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  
                  footer=format(sum(dfa_fichewilaya7()$Prevus_livres,dfa_fichewilaya7_suite()$Prevus_livres,na.rm = TRUE)
                                , digits=3,big.mark=" "
                                )
                  
                  # footer = function(values) {
                  #   format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                  # }
                  
                ),
                Taux_livraison=colDef(name="",style=list(textAlign="center",fontWeight = "bold"),
                                      # headerStyle = list(
                                      #   fontWeight = "bold",
                                      #   textAlign="center",
                                      #   background = "#2E81B0",
                                      #   color="#ffffff"
                                      # ),
                                      format = colFormat(percent = TRUE,digits = 0),
                                      footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya7()$Livres,dfa_fichewilaya7_suite()$Livres,na.rm = TRUE)/sum(dfa_fichewilaya7()$Prevus_livres,dfa_fichewilaya7_suite()$Prevus_livres,na.rm = TRUE))
                                      
                                      #footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya7()$Livres)/sum(dfa_fichewilaya7()$Prevus_livres))
                                      
                                      
                ),
                Prevus_lances=colDef(
                  name="",
                  format = colFormat(separators = TRUE,locales = "fr-FR"),
                  
                  footer=format(sum(dfa_fichewilaya7()$Prevus_lances,dfa_fichewilaya7_suite()$Prevus_lances,na.rm = TRUE)
                                , digits=3,big.mark=" "
                  )
                  
                  # footer = function(values) {
                  #   format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                  # }
                  
                ),
                Taux_lancement=colDef(name="",
                                      style=list(textAlign="center",fontWeight = "bold"),
                                      format = colFormat(percent = TRUE,digits = 0),
                                      footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya7()$Lances,dfa_fichewilaya7_suite()$Lances,na.rm = TRUE)/sum(dfa_fichewilaya7()$Prevus_lances,dfa_fichewilaya7_suite()$Prevus_lances,na.rm = TRUE))
                                      
                                      #footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya7()$Lances)/sum(dfa_fichewilaya7()$Prevus_lances))
                                      
                                      
                                      # headerStyle = list(
                                      #   fontWeight = "bold",
                                      #   textAlign="center",
                                      #   background = "#2E81B0",
                                      #   color="#ffffff"
                                      # )
                                      ),
                Annules=colDef(name="",
                               format = colFormat(separators = TRUE,locales = "fr-FR"),
                               
                               footer=format(sum(dfa_fichewilaya7()$Annules,dfa_fichewilaya7_suite()$Annules,na.rm = TRUE)
                                             , digits=3,big.mark=" "
                               ),
                               
                               # footer = function(values) {
                               #   format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                               # },
                               
                               headerStyle = list(
                                 #borderTop="0px",
                                 marginTop="-17px"
                                 #textAlign="left",
                                 #background = "#2E81B0",
                                 #color="#ffffff"
                               )
                ),
                Notifie=colDef(name="",
                               format = colFormat(separators = TRUE,locales = "fr-FR"),
                               
                               footer=format(sum(dfa_fichewilaya7()$Notifie,dfa_fichewilaya7_suite()$Notifie,na.rm = TRUE)
                                             , digits=3,big.mark=" "
                               ),
                               
                               # footer = function(values) {
                               #   format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                               # },
                               
                               headerStyle = list(
                                 #borderTop="0px",
                                 marginTop="-17px"
                                 #textAlign="left",
                                 #background = "#2E81B0",
                                 #color="#ffffff"
                               )
                )
                
              ),
              
              ###################
              # columnGroups = list(
              #   colGroup(name = "", columns = c("Segment")
              #            # headerStyle =list(
              #            #   #border="0px solid #eee",
              #            #   textAlign="center",
              #            #   background = "#2E81B0",
              #            #   color="#ffffff"
              #            # )
              #   ),
              #   colGroup(name = "", columns = c("Livres","Prevus_livres","Taux_livraison"),
              #            headerStyle =list(
              #              borderRight="25px inset transparent"
              #              #textAlign="center",
              #              #background = "#2E81B0",
              #              #color="#ffffff"
              #            )
              #   ),
              #   colGroup(name = "", columns = c("Lances","Prevus_lances","Taux_lancement"),
              #            
              #            headerStyle =list(
              #              borderLeft="25px inset transparent",
              #              borderRight="25px inset transparent"
              #              
              #              #border="0px solid #eee",
              #              #textAlign="center",
              #              #background = "#2E81B0",
              #              #color="#ffffff"
              #            )
              #   ),
              #   colGroup(name = "", columns = c("Annules"),
              #            headerStyle =list(
              #              #border="0px solid #eee",
              #              textAlign="center"
              #              #background = "#2E81B0",
              #              #color="#ffffff"
              #            )
              #   ),
              #   colGroup(name = "", columns = c("Notifie")
              #            #headerStyle =list(
              #              #border="0px solid #eee",
              #              #textAlign="center",
              #              #background = "#2E81B0",
              #              #color="#ffffff"
              #            #)
              #   )
              #   
              # ),
              ########
              
              
              
              defaultColDef = colDef(
                format = colFormat(separators = TRUE,locales = "fr-FR"),
                
                sortNALast = TRUE,
                #format = colFormat(digits = 1),
                maxWidth = 140,
                footerStyle = list(fontWeight = "bold",
                                   background='#81a47b',color='#ffffff',fontFamily='inherit',
                                   textAlign="center"
                                   
                                   
                ),
                headerStyle = list(
                                   #background = "#2E81B0",
                                   #color="#ffffff",
                                   borderLeft="0px solid #eee",
                                   borderRight="0px solid #eee"
                )
                
                #headerClass = "box-score-header"
              )
              
              
    )
    
  })
  
  
  
  
  
  #########################
  output$gt7<-renderReactable({
    #`if`(input$pourcentage==FALSE,
         reactable(dfa_fichewilaya7()[1:7,],
                   sortable = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   fullWidth = FALSE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   #defaultSorted = list(Segment = "desc"),
                   showSortIcon = FALSE,
                   
                   columns = list(
                     Segment = colDef(name = "",
                                      footer = "Total",
                                      width=220,
                                      style =list(background='#81a47b',color='#ffffff'),
                                      headerStyle = list(
                                        #borderTop="0px",
                                        marginTop="-31px",
                                        textAlign="left",
                                        background = "#2E81B0",
                                        color="#ffffff"
                                      )
                     ),
                     Lances=colDef(
                       name="Réalisé",
                       width=130,
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       headerStyle = list(
                         borderLeft="25px inset transparent",
                         textAlign="left",
                         background = "#2E81B0",
                         color="#ffffff",
                         width="130px"
                       ),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     Livres=colDef(
                       name="Réalisé",
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                     ),
                     Prevus_livres=colDef(
                       name="Prévus",
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                       
                     ),
                     Taux_livraison=colDef(name="Taux",style=list(textAlign="center",fontWeight = "bold"),
                                  headerStyle = list(
                                    fontWeight = "bold",
                                    textAlign="center",
                                    background = "#2E81B0",
                                    color="#ffffff"
                                  ),
                                  format = colFormat(percent = TRUE,digits = 0),
                                  footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya7()$Livres)/sum(dfa_fichewilaya7()$Prevus_livres))
                                  
                                  
                                  ),
                     Prevus_lances=colDef(
                       name="Prévus",
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       }
                       
                     ),
                     Taux_lancement=colDef(name="Taux",
                                           style=list(textAlign="center",fontWeight = "bold"),
                                           format = colFormat(percent = TRUE,digits = 0),
                                           footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya7()$Lances)/sum(dfa_fichewilaya7()$Prevus_lances)),
                                           
                                           
                                           headerStyle = list(
                                             fontWeight = "bold",
                                             textAlign="center",
                                             background = "#2E81B0",
                                             color="#ffffff"
                                           )),
                     Annules=colDef(name="Annulés",
                                    format = colFormat(separators = TRUE,locales = "fr-FR"),
                                    footer = function(values) {
                                      format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                                    },
                                    
                                    headerStyle = list(
                                      #borderTop="0px",
                                      marginTop="-17px",
                                      textAlign="left",
                                      background = "#2E81B0",
                                      color="#ffffff"
                                    )
                     ),
                     Notifie=colDef(name="Notifie",
                                    format = colFormat(separators = TRUE,locales = "fr-FR"),
                                    footer = function(values) {
                                      format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                                    },
                                    
                                    headerStyle = list(
                                      #borderTop="0px",
                                      marginTop="-17px",
                                      textAlign="left",
                                      background = "#2E81B0",
                                      color="#ffffff"
                                    )
                     )
                     
                   ),
                   
                   ###################
                   columnGroups = list(
                     colGroup(name = "", columns = c("Segment"),
                              headerStyle =list(
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "Livraisons", columns = c("Livres","Prevus_livres","Taux_livraison"),
                              headerStyle =list(
                                borderRight="25px inset transparent",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "Lancements", columns = c("Lances","Prevus_lances","Taux_lancement"),
                              
                              headerStyle =list(
                                borderLeft="25px inset transparent",
                                borderRight="25px inset transparent",
                                
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "", columns = c("Annules"),
                              headerStyle =list(
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     ),
                     colGroup(name = "", columns = c("Notifie"),
                              headerStyle =list(
                                #border="0px solid #eee",
                                textAlign="center",
                                background = "#2E81B0",
                                color="#ffffff"
                              )
                     )
                     
                   ),
                   ########
                   
                   
                   
                   defaultColDef = colDef(
                     format = colFormat(separators = TRUE,locales = "fr-FR"),
                     
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 140,
                     footerStyle = list(fontWeight = "bold",
                                        background='#81a47b',color='#ffffff',fontFamily='inherit',
                                        textAlign="center"
                                        
                                        
                     ),
                     headerStyle = list(background = "#2E81B0",
                                        color="#ffffff",
                                        borderLeft="0px solid #eee",
                                        borderRight="0px solid #eee"
                     )
                     
                     #headerClass = "box-score-header"
                   )
                   
                   
         )
    
  })
  
  
  # output$gt_attribution<-renderReactable({
  #   reactable(gt_attribution_reactive(),
  #             sortable = FALSE,
  #             #fullWidth = FALSE,
  #             bordered = TRUE,
  #             style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
  #             columns = list(
  #               Etat=colDef(
  #                 name="",
  #                 width = 100
  #               )
  #             ),
  #             defaultColDef = colDef(
  #               sortNALast = TRUE,
  #               #format = colFormat(digits = 1),
  #               maxWidth = 140,
  #               format = colFormat(separators = TRUE,locales = "fr-FR"),
  #               style = list(textAlign="center"),
  #               
  #               headerStyle = list(background = "#2E81B0",
  #                                  color="#ffffff"
  #               )
  #             )
  #             
  #             
  #   )
  # })
  
  ##########################
  ############
  
  
  output$gt9<-renderReactable({
    `if`(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()]!="16-ALGER",
         reactable(
           cbind(OPGI=`if`(length(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()])!=1,"Tout les OPGI",
                           unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()]),
                 etat_cession_fw %>% 
                   filter(
                     arretee==substr(input$select_arretee_fichewilaya,14,23),
                     id_wilaya %in% as.numeric(str_sub(unique(data_fiche_wilaya$Wilaya)[c(selected20_fichewilaya2())],1,2))
                   ) %>%
                   #group_by(OPGI) %>%
                   summarise_at(vars(LOGTS1:Logts_et_locx),sum,na.rm=TRUE)
           )
              ,
           columns = list(
             OPGI=colDef(
               name="",
               style = list(fontSize="17px",textAlign="left"),
               width=170,
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 marginTop="-1px"
             
               )
               
             ),
             LOGTS1=colDef(
               name="Logts",
               width = 90,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOCX1=colDef(
               name="Locaux",
               width = 95,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOGTS2=colDef(
               name="Logts",
               width = 120,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOCX2=colDef(
               name="Locaux",
               width = 120,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             Total_logts=colDef(
               name="Total Logts",
               width = 95,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               style = list(fontWeight = "bold",textAlign="center"),
               
             ),
             Total_loccx=colDef(
               name="Total Locaux",
               width = 95,
               style = list(fontWeight = "bold",textAlign="center"),
               
               format = colFormat(separators = TRUE,locales = "fr-FR")
             ),
             Total_generale=colDef(
               name="Total Genérale",
               width = 105,
               style = list(fontWeight = "bold",textAlign="center"),
               
               format = colFormat(separators = TRUE,locales = "fr-FR")
             ),
             LOGTS3=colDef(
               name="Logts",
               width = 118,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOCX3=colDef(
               name="Locaux",
               width = 130,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             Logts_et_locx=colDef(
               name="Logts + Locaux",
               width = 140,
               style = list(fontWeight = "bold"),
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center"
               ),
               
               format = colFormat(separators = TRUE,locales = "fr-FR")
             )
           ),
           sortable = FALSE,
           #fullWidth = FALSE,
           bordered = TRUE,
           style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
           defaultExpanded = TRUE,
           
           
           columnGroups = list(
             
             colGroup(name = "", columns = c("OPGI"),
                      headerStyle =list(
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             
             
             colGroup(name = "Loi 81-01", columns = c("LOGTS1","LOCX1"),
                      headerStyle =list(
                        borderRight="18px inset transparent",
                        paddingTop="16px",
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             colGroup(name = "Cession  03-269 modifié et complété", columns = c("LOGTS2","LOCX2"),
                      headerStyle =list(
                        borderLeft="18px inset transparent",
                        borderRight="18px inset transparent",
                        
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             colGroup(name = "TOTAL BIENS CEDES ", columns = c("Total_logts","Total_loccx","Total_generale"),
                      headerStyle =list(
                        borderLeft="18px inset transparent",
                        borderRight="18px inset transparent",
                        paddingTop="16px",
                        
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             colGroup(name = "Total Général Déposés au niveau des OPGI dans le cadre du DE 08-153", columns = c("LOGTS3","LOCX3","Logts_et_locx"),
                      headerStyle =list(
                        borderLeft="18px inset transparent",
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             )
           ),
           defaultColDef = colDef(
             sortNALast = TRUE,
             #format = colFormat(digits = 1),
             style = list(textAlign="center"),
             
             maxWidth = 290,
             headerStyle = list(background = "#2E81B0",
                                color="#ffffff",
                                textAlign="center"
             )
           )
         ),
         #########################
         
         reactable(
           etat_cession_fw %>% 
             filter(
               #arretee=="2021-03-31",
               #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya2())])
               arretee==substr(input$select_arretee_fichewilaya,14,23),
               id_wilaya %in% as.numeric(str_sub(unique(data_fiche_wilaya$Wilaya)[c(selected20_fichewilaya2())],1,2))
             ) %>%
             group_by(OPGI) %>%
             summarise_at(vars(LOGTS1:Logts_et_locx),sum,na.rm=TRUE)
           ,
           columns = list(
             OPGI=colDef(
               name="",
               style = list(fontSize="17px",textAlign="left"),
               width=170,
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 marginTop="-1px"
               ),
               footer="Total"
               
               
             ),
             LOGTS1=colDef(
               name="Logts",
               width = 90,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOCX1=colDef(
               name="Locaux",
               width = 95,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOGTS2=colDef(
               name="Logts",
               width = 120,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOCX2=colDef(
               name="Locaux",
               width = 120,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             Total_logts=colDef(
               name="Total Logts",
               width = 95,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               style = list(fontWeight = "bold",textAlign="center"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               }
               
             ),
             Total_loccx=colDef(
               name="Total Locaux",
               width = 95,
               style = list(fontWeight = "bold",textAlign="center"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               format = colFormat(separators = TRUE,locales = "fr-FR")
             ),
             Total_generale=colDef(
               name="Total Genérale",
               width = 105,
               style = list(fontWeight = "bold",textAlign="center"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               
               format = colFormat(separators = TRUE,locales = "fr-FR")
             ),
             LOGTS3=colDef(
               name="Logts",
               width = 118,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             LOCX3=colDef(
               name="Locaux",
               width = 130,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center",
                 paddingTop="16px"
               )
             ),
             Logts_et_locx=colDef(
               name="Logts + Locaux",
               width = 140,
               style = list(fontWeight = "bold",textAlign="center"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               },
               headerStyle = list(
                 background = "#2E81B0",
                 color="#ffffff",
                 textAlign="center"
               ),
               
               format = colFormat(separators = TRUE,locales = "fr-FR")
             )
           ),
           sortable = FALSE,
           #fullWidth = FALSE,
           bordered = TRUE,
           style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
           defaultExpanded = TRUE,
           
           
           columnGroups = list(
             
             colGroup(name = "", columns = c("OPGI"),
                      headerStyle =list(
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             
             
             colGroup(name = "Loi 81-01", columns = c("LOGTS1","LOCX1"),
                      headerStyle =list(
                        borderRight="18px inset transparent",
                        paddingTop="16px",
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             colGroup(name = "Cession  03-269 modifié et complété", columns = c("LOGTS2","LOCX2"),
                      headerStyle =list(
                        borderLeft="18px inset transparent",
                        borderRight="18px inset transparent",
                        
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             colGroup(name = "TOTAL BIENS CEDES ", columns = c("Total_logts","Total_loccx","Total_generale"),
                      headerStyle =list(
                        borderLeft="18px inset transparent",
                        borderRight="18px inset transparent",
                        paddingTop="16px",
                        
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             ),
             colGroup(name = "Total Général Déposés au niveau des OPGI dans le cadre du DE 08-153", columns = c("LOGTS3","LOCX3","Logts_et_locx"),
                      headerStyle =list(
                        borderLeft="18px inset transparent",
                        #border="0px solid #eee",
                        textAlign="center",
                        background = "#2E81B0",
                        color="#ffffff"
                      )
             )
           ),
           defaultColDef = colDef(
             sortNALast = TRUE,
             #format = colFormat(digits = 1),
             style = list(textAlign="center"),
             
             maxWidth = 290,
             headerStyle = list(background = "#2E81B0",
                                color="#ffffff",
                                textAlign="center"
             ),
             footerStyle = list(fontWeight = "bold"
                                ,textAlign="center"
                                
             )
           )
         )
         
    )
  })
  
  
  
  
  
  
  
  
  ##########
  ###################
  
  output$gt8<-renderReactable({
    `if`(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()]!="16-ALGER",
              reactable(
                    cbind(OPGI=`if`(length(unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()])!=1,"Tout les OPGI",
                                           unique(data_fiche_wilaya$Wilaya)[selected20_fichewilaya2()]),
                    patrimoine_fw %>% 
                      filter(
                        arretee==substr(input$select_arretee_fichewilaya,14,23),
                        
                        id_wilaya %in% as.numeric(str_sub(unique(data_fiche_wilaya$Wilaya)[c(selected20_fichewilaya2())],1,2))
                        ) %>%
                      #group_by(OPGI) %>%
                      summarise(Logements=sum(logements),Locaux=sum(Locaux)) %>% 
                      mutate(Total=Logements+Locaux) %>% 
                      select(Logements,Locaux,Total)
                    ),
                columns = list(
                  Logements=colDef(
                    width = 160,
                    format = colFormat(separators = TRUE,locales = "fr-FR")
           
                    
                  ),
                  Locaux=colDef(
                    width = 160,
                    format = colFormat(separators = TRUE,locales = "fr-FR")
               
                    
                  ),
                  Total=colDef(
                    width = 160,
                    format = colFormat(separators = TRUE,locales = "fr-FR"),
                    style = list(fontWeight = "bold")
                
                  )
                  
                ),
                    sortable = FALSE,
                        #fullWidth = FALSE,
                        bordered = TRUE,
                        style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                        defaultExpanded = TRUE,
                        defaultColDef = colDef(
                          sortNALast = TRUE,
                          #format = colFormat(digits = 1),
                          maxWidth = 290,
                          headerStyle = list(background = "#2E81B0",
                                             color="#ffffff"
                          )
                        )
              ),
         reactable(
           patrimoine_fw %>% 
             filter(
               #arretee=="2021-03-31",
               #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya2())])
               arretee==substr(input$select_arretee_fichewilaya,14,23),
               
               id_wilaya %in% as.numeric(str_sub(unique(data_fiche_wilaya$Wilaya)[c(selected20_fichewilaya2())],1,2))
             ) %>%
             group_by(OPGI) %>%
             summarise(Logements=sum(logements),Locaux=sum(Locaux)) %>% 
             mutate(Total=Logements+Locaux) %>% 
             select(OPGI,Logements,Locaux,Total),
           columns = list(
             OPGI=colDef(
               footer="Total"
             ),
             Logements=colDef(
               width = 160,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               }
               
               
             ),
             Locaux=colDef(
               width = 160,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               }
             ),
             Total=colDef(
               width = 160,
               format = colFormat(separators = TRUE,locales = "fr-FR"),
               style = list(fontWeight = "bold"),
               footer = function(values) {
                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
               }
               
             )
             
           ),
           sortable = FALSE,
           #fullWidth = FALSE,
           bordered = TRUE,
           style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
           defaultExpanded = TRUE,
           defaultColDef = colDef(
             sortNALast = TRUE,
             #format = colFormat(digits = 1),
             maxWidth = 290,
             headerStyle = list(background = "#2E81B0",
                                color="#ffffff"
             ),
             footerStyle = list(fontWeight = "bold"
                                #,textAlign="center"
                                
             )
           )
           
           
           
         )
         
         
    )
  })
  
  
  
  
  
  output$gt2<-renderReactable({
    #`if`(length(selected20_fichewilaya())==48,
    #`if`(length(selected20_fichewilaya())==51,
    `if`(length(selected20_fichewilaya())==61,
         
         `if`(input$pourcentage==FALSE,
              reactable(dfa_fichewilaya2()[1,1:8],
                        sortable = FALSE,
                        #fullWidth = FALSE,
                        bordered = TRUE,
                        style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                        defaultExpanded = TRUE,
                        
                        details = function(index) {
                          reactable(
                            aides_reha %>%
                              select(t,as.character(substr(input$select_arretee_fichewilaya,14,23))) %>% 
                              `colnames<-`(c("Cas","Total")),
                            defaultColDef = colDef(
                              #sortNALast = TRUE,
                              #format = colFormat(digits = 1),
                              #maxWidth = 140,
                              headerStyle = list(height='0px')
                            ),
                            columns = list(
                              Cas=colDef(
                                name=".",
                                width = 263
                                # headerStyle = list(
                                #   marginTop="-131px"
                                #   #marginTop="55px"
                                # )
                                
                              ),
                              Total=colDef(
                                name="..",
                                format = colFormat(separators = TRUE,locales = "fr-FR"),
                                style = list(fontWeight = "bold"),
                                width=150
                                # headerStyle = list(
                                #   marginTop="-131px"
                                #   #marginTop="55px"
                                # )
                              )
                            ),
                            fullWidth = FALSE
                          )
                        },
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
                            width=135,
                            headerStyle = list(
                              background = "#2E81B0",
                              color="#ffffff",
                              borderLeft="0px"
                            ),
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                          ),
                          Acheves = colDef(
                            name="Achevés",
                            width=125,
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                            
                          ),
                          `En Cours` = colDef(
                            width=125,
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                            
                          ),
                          `Non Lances` = colDef(
                            name="Non Lancés",
                            width=135,
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                          ),
                          `AP` = colDef(
                            name="AP",
                            width=180,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                          ),
                          `Consomomations` = colDef(
                            name="Consomomations",
                            width=190,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                          ),
                          `Taux` = colDef(
                            name="Taux",
                            width=100,
                            #format = colFormat(separators = TRUE,locales = "fr-FR")
                            format = colFormat(percent = TRUE,digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                            
                          )
                          
                          
                        ),
                        defaultColDef = colDef(
                          sortNALast = TRUE,
                          #format = colFormat(digits = 1),
                          maxWidth = 140,
                          headerStyle = list(background = "#2E81B0",
                                             color="#ffffff"
                          )
                        )
              )
              ,
              reactable(dfa_fichewilaya2()[1,c(1,2,9,10,11,6,7,8)],
                        sortable = FALSE,
                        #fullWidth = FALSE,
                        bordered = TRUE,
                        style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                        defaultExpanded = TRUE,
                        
                        details = function(index) {
                          reactable(
                            aides_reha %>%
                              select(t,as.character(substr(input$select_arretee_fichewilaya,14,23))) %>% 
                              `colnames<-`(c("Cas","Total")),
                            defaultColDef = colDef(
                              #sortNALast = TRUE,
                              #format = colFormat(digits = 1),
                              #maxWidth = 140,
                              headerStyle = list(height='0px')
                            ),
                            columns = list(
                              Cas=colDef(
                                name=".",
                                width = 263
                                # headerStyle = list(
                                #   marginTop="-131px"
                                #   #marginTop="55px"
                                # )
                                
                              ),
                              Total=colDef(
                                name="..",
                                format = colFormat(separators = TRUE,locales = "fr-FR"),
                                style = list(fontWeight = "bold"),
                                width=150
                                # headerStyle = list(
                                #   marginTop="-131px"
                                #   #marginTop="55px"
                                # )
                              )
                            ),
                            fullWidth = FALSE
                          )
                        },
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
                            width=135,
                            headerStyle = list(
                              background = "#2E81B0",
                              color="#ffffff",
                              borderLeft="0px"
                            ),
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                          ),
                          
                          pacheves = colDef(
                            width=125,
                            name="Achevés",
                            format = colFormat(percent = TRUE,digits = 0)
                            
                          ),
                          pencours = colDef(
                            width=125,
                            name="En Cours",
                            format = colFormat(percent = TRUE,digits = 0)
                            
                          ),
                          pnonlances = colDef(
                            width=135,
                            name="Non Lancés",
                            format = colFormat(percent = TRUE,digits = 0)
                          ),
                          `AP` = colDef(
                            name="AP",
                            width=180,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                            
                          ),
                          `Consomomations` = colDef(
                            name="Consomomations",
                            width=190,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                          ),
                          `Taux` = colDef(
                            name="Taux",
                            width=100,
                            #format = colFormat(separators = TRUE,locales = "fr-FR")
                            format = colFormat(percent = TRUE,digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                            
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
              
              
              
         ),
         
         `if`(input$pourcentage==FALSE,
              reactable(dfa_fichewilaya2()[1,1:8],
                        sortable = FALSE,
                        #fullWidth = FALSE,
                        bordered = TRUE,
                        style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                        defaultExpanded = TRUE,
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
                            width=135,
                            headerStyle = list(
                              background = "#2E81B0",
                              color="#ffffff",
                              borderLeft="0px"
                            ),
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                          ),
                          Acheves = colDef(
                            name="Achevés",
                            width=125,
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                            
                          ),
                          `En Cours` = colDef(
                            width=125,
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                            
                          ),
                          `Non Lances` = colDef(
                            name="Non Lancés",
                            width=135,
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                          ),
                          `AP` = colDef(
                            name="AP",
                            width=180,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                          ),
                          `Consomomations` = colDef(
                            name="Consomomations",
                            width=190,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                          ),
                          `Taux` = colDef(
                            name="Taux",
                            width=100,
                            #format = colFormat(separators = TRUE,locales = "fr-FR")
                            format = colFormat(percent = TRUE,digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                            
                          )
                          
                        ),
                        defaultColDef = colDef(
                          sortNALast = TRUE,
                          #format = colFormat(digits = 1),
                          maxWidth = 140,
                          headerStyle = list(background = "#2E81B0",
                                             color="#ffffff"
                          )
                        )
              )
              ,
              reactable(dfa_fichewilaya2()[1,c(1,2,9,10,11,6,7,8)],
                        sortable = FALSE,
                        #fullWidth = FALSE,
                        bordered = TRUE,
                        style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                        defaultExpanded = TRUE,
                        
                        # details = function(index) {
                        #   reactable(
                        #     aides_reha %>%
                        #       select(t,as.character(substr(input$select_arretee_fichewilaya,14,23))) %>% 
                        #       `colnames<-`(c("Cas","Total")),
                        #     defaultColDef = colDef(
                        #       #sortNALast = TRUE,
                        #       #format = colFormat(digits = 1),
                        #       #maxWidth = 140,
                        #       headerStyle = list(height='0px')
                        #     ),
                        #     columns = list(
                        #       Cas=colDef(
                        #         name=".",
                        #         width = 263
                        #         # headerStyle = list(
                        #         #   marginTop="-131px"
                        #         #   #marginTop="55px"
                        #         # )
                        #         
                        #       ),
                        #       Total=colDef(
                        #         name="..",
                        #         format = colFormat(separators = TRUE,locales = "fr-FR"),
                        #         style = list(fontWeight = "bold"),
                        #         width=150
                        #         # headerStyle = list(
                        #         #   marginTop="-131px"
                        #         #   #marginTop="55px"
                        #         # )
                        #       )
                        #     ),
                        #     fullWidth = FALSE
                        #   )
                        # },
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
                            width=135,
                            headerStyle = list(
                              background = "#2E81B0",
                              color="#ffffff",
                              borderLeft="0px"
                            ),
                            format = colFormat(separators = TRUE,locales = "fr-FR")
                          ),
                          
                          pacheves = colDef(
                            width=120,
                            name="Achevés",
                            format = colFormat(percent = TRUE,digits = 0)
                            
                          ),
                          pencours = colDef(
                            width=120,
                            name="En Cours",
                            format = colFormat(percent = TRUE,digits = 0)
                            
                          ),
                          pnonlances = colDef(
                            width=135,
                            name="Non Lancés",
                            format = colFormat(percent = TRUE,digits = 0)
                          ),
                          `AP` = colDef(
                            name="AP",
                            width=180,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                          ),
                          `Consomomations` = colDef(
                            name="Consomomations",
                            width=190,
                            format = colFormat(separators = TRUE,locales = "fr-FR",digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                          ),
                          `Taux` = colDef(
                            name="Taux",
                            width=100,
                            #format = colFormat(separators = TRUE,locales = "fr-FR")
                            format = colFormat(percent = TRUE,digits = 2),
                            style = list(textAlign="center"),
                            
                            headerStyle = list(background = "#2E81B0",
                                               color="#ffffff",
                                               textAlign="center"
                            )
                            
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
                       name="Achevés",
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
                       name="Non Lancés",
                       width=145
                     ),
                     `En cours`=colDef(name="En Cours",width=130),
                     `Non lances`=colDef(name="Non Lancés",width=145),
                     `Acheves à 60% et plus`=colDef(name="Achevés à 60% et plus",
                    headerStyle = list(
                       #borderTop="0px",
                       marginTop="-31px",
                       textAlign="left",
                       background = "#2E81B0",
                       color="#ffffff"
                     )),
                     `Total general`=colDef(
                       name="Total général",
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
                       name="Non Lancés",
                       format =colFormat(percent = TRUE,digits = 0),
                       
                       width=145
                     ),
                     pacheves2=colDef(
                       name="Achevés",
                       format =colFormat(percent = TRUE,digits = 0),
                       
                     ),
                     pencours2=colDef(
                       name="En Cours",width=130,
                       format =colFormat(percent = TRUE,digits = 0),
                       
                     ),
                     pnonlances2=colDef(
                       name="Non Lancés",width=145,
                       format =colFormat(percent = TRUE,digits = 0),
                     ),
                     `Acheves à 60% et plus`=colDef(name="Achevés à 60% et plus",headerStyle = list(
                       #borderTop="0px",
                       marginTop="-31px",
                       textAlign="left",
                       background = "#2E81B0",
                       color="#ffffff"
                     )),
                     `Total general`=colDef(
                       name="Total général",
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
  
  ##########################
  ##########################
  ##########################
  
  output$gt1_suite<-renderReactable({
    `if`(input$pourcentage==FALSE,
         reactable(dfa_fichewilaya1_suite()[1:2,c(1,2,3,4,5,6,7)],
                   
                   sortable = FALSE,
                   #defaultSorted = list(Segment = "desc"),
                   #defaultSorted = list(Consistance = "desc", Acheves = "desc"),
                   showSortIcon = FALSE,
                   
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(name = "",
                                      footer = "Programme Global",
                                      width=220,
                                      style =list(background='#81a47b',color='#ffffff'),
                                      footerStyle = list(fontWeight = "bold",
                                                         background='#81a47b',color='#ffffff',fontFamily='inherit',
                                                         textAlign="center"
                                                         
                                      )
                     ),
                     
                     `Consistance` = colDef(
                       name="",
                       # headerStyle = list(
                       #   background = "#2E81B0",
                       #   color="#ffffff",
                       #   borderLeft="0px",
                       #   textAlign="center"
                       #   #,align='left'
                       # ),
                       width=150,align = 'right',
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                     
                       
                       #format = colFormat(percent = TRUE, digits = 1)
                       
                       footer=format(sum(
                         data_fiche_wilaya_reactive() %>% 
                           select(consistance_lpl,
                                  consistance_lpp,
                                  consistance_lsp,
                                  consistance_lv,
                                  consistance_lv_cnep,
                                  consistance_promotionellelibre,
                                  consistance_rural,
                                  consistance_acls,
                                  consistance_autresocial
                                  ),na.rm = TRUE)
                           , digits=3,big.mark=" ")
                       
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       # }
                     ),
                     Acheves = colDef(
                       name="",
                       #width=155,
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer=format(sum(
                         data_fiche_wilaya_reactive() %>% 
                           select(acheves_lpl,
                                  acheves_lpp,
                                  acheves_lsp,
                                  acheves_lv,
                                  acheves_lv_cnep,
                                  acheves_promotionellelibre,
                                  acheves_rural,
                                  acheves_acls,
                                  acheves_autresocial
                           ),na.rm = TRUE)
                         , digits=3,big.mark=" ")
                       
                       
                       
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       # }
                     ),
                     `En Cours` = colDef(
                       name="",
                       
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer=format(sum(
                         data_fiche_wilaya_reactive() %>% 
                           select(encours_lpl,
                                  encours_lpp,
                                  encours_lsp,
                                  encours_lv,
                                  encours_lv_cnep,
                                  encours_promotionellelibre,
                                  encours_rural,
                                  encours_acls,
                                  encours_autresocial
                           ),na.rm = TRUE)
                         , digits=3,big.mark=" ")
                       
                       
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       # }
                     ),
                     `Dont A l'Arret` = colDef(
                       name="",
                       na = "/",
                       # headerStyle = list(
                       #   background = "#9d94ac",
                       #   color="#ffffff"
                       #   #,align='left'
                       # ),
                       style = list(
                         background = "#9d94ac",
                         color="#ffffff"
                       ),
                       width = 165,
                       align = 'center',
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer=format(sum(
                         data_fiche_wilaya_reactive() %>% 
                           select(dont_arret_encours_lpl,
                                  dont_arret_encours_lpp,
                                  dont_arret_encours_lsp,
                                  dont_arret_encours_lv,
                                  dont_arret_encours_lv_cnep,
                                  dont_arret_encours_promotionellelibre,
                                  dont_arret_encours_rural,
                                  dont_arret_encours_acls,
                                  dont_arret_encours_autresocial
                           ),na.rm = TRUE)
                         , digits=3,big.mark=" ")
                       
                       
                       
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       # }
                       # 
                       
                       # footer = `if`(is.na(sum(dfa_fichewilaya1()$`Dont A l'Arret`))==TRUE,
                       #               paste("-"),
                       #               function(values) {
                       #                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
                       #               }
                       #        )
                       
                       ,footerStyle = list(
                         
                         background = "#9d94ac",
                         color="#ffffff"
                       )
                     ),
                     `Non Lances` = colDef(
                       name="",
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer=format(sum(
                         data_fiche_wilaya_reactive() %>% 
                           select(nonlances_lpl,
                                  nonlances_lpp,
                                  nonlances_lsp,
                                  nonlances_lv,
                                  nonlances_lv_cnep,
                                  nonlances_promotionellelibre,
                                  nonlances_rural,
                                  nonlances_acls,
                                  nonlances_autresocial
                           ),na.rm = TRUE)
                         , digits=3,big.mark=" ")
                       
                       
                       
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       # }
                       ,width=155
                       
                     ),
                     
                     `Notifie 2020` = colDef(
                       name="",
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
                       
                       footer=format(sum(notifie_reactive_suite(),notifie_reactive(),na.rm = TRUE)
                         , digits=3,big.mark=" ")
                       
                       
                       
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       # }
                       
                       ,width=155
                     )
                     
                   ),
                   defaultColDef = colDef(
                     na = "-",
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 140,
                     footerStyle = list(fontWeight = "bold",
                                        background='#81a47b',color='#ffffff',fontFamily='inherit'
                                        
                     )
                     # ,
                     # headerStyle = list(background = "#2E81B0",
                     #                    color="#ffffff",
                     #                    borderLeft="0px solid #eee",
                     #                    borderRight="0px solid #eee"
                     # )
                     
                     #headerClass = "box-score-header"
                   )
         )
         ,
         reactable(dfa_fichewilaya1_suite()[1:2,c(1,2,8,9,10,11,7)],
                   
                   sortable = FALSE,
                   #defaultSorted = list(Segment = "desc"),
                   showSortIcon = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(name = "",
                                      footer = "Programme Global",
                                      width=220,
                                      style =list(background='#81a47b',color='#ffffff'),
                                      footerStyle = list(fontWeight = "bold",
                                                         background='#81a47b',color='#ffffff',fontFamily='inherit',
                                                         textAlign="center"
                                                         
                                      )
                     ),
                     
                     `Consistance` = colDef(
                       name="",
                       
                       # headerStyle = list(
                       #   background = "#2E81B0",
                       #   color="#ffffff",
                       #   borderLeft="0px",
                       #   textAlign="center"
                       #   #,align='left'
                       # ),
                       width=150,align = 'right',
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer=format(sum(
                         data_fiche_wilaya_reactive() %>% 
                           select(consistance_lpl,
                                  consistance_lpp,
                                  consistance_lsp,
                                  consistance_lv,
                                  consistance_lv_cnep,
                                  consistance_promotionellelibre,
                                  consistance_rural,
                                  consistance_acls,
                                  consistance_autresocial
                           ),na.rm = TRUE)
                         , digits=3,big.mark=" ")
                       
                       
                       #format = colFormat(percent = TRUE, digits = 1)
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       # }
                     ),
                     
                     pacheves = colDef(
                       name="",
                       format = colFormat(percent = TRUE,digits = 0),
                       #footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$Acheves,na.rm=TRUE)/sum(dfa_fichewilaya1()$Consistance,na.rm=TRUE))
                       footer = sprintf("%.0f%%",100*(
                         (sum(
                           data_fiche_wilaya_reactive() %>% 
                             select(acheves_lpl,
                                    acheves_lpp,
                                    acheves_lsp,
                                    acheves_lv,
                                    acheves_lv_cnep,
                                    acheves_promotionellelibre,
                                    acheves_rural,
                                    acheves_acls,
                                    acheves_autresocial
                             ),na.rm = TRUE))
                         /
                           (sum(
                             data_fiche_wilaya_reactive() %>% 
                               select(consistance_lpl,
                                      consistance_lpp,
                                      consistance_lsp,
                                      consistance_lv,
                                      consistance_lv_cnep,
                                      consistance_promotionellelibre,
                                      consistance_rural,
                                      consistance_acls,
                                      consistance_autresocial
                               )))
                       )
                       
                       )
                       
                       #function(values) {
                       #format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       #}
                     ),
                     pencours = colDef(
                       name="",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer=sprintf("%.0f%%",100*(
                         (sum(
                           data_fiche_wilaya_reactive() %>% 
                             select(encours_lpl,
                                    encours_lpp,
                                    encours_lsp,
                                    encours_lv,
                                    encours_lv_cnep,
                                    encours_promotionellelibre,
                                    encours_rural,
                                    encours_acls,
                                    encours_autresocial
                             ),na.rm = TRUE))
                         /
                        (sum(
                          data_fiche_wilaya_reactive() %>% 
                            select(consistance_lpl,
                                   consistance_lpp,
                                   consistance_lsp,
                                   consistance_lv,
                                   consistance_lv_cnep,
                                   consistance_promotionellelibre,
                                   consistance_rural,
                                   consistance_acls,
                                   consistance_autresocial
                            ),na.rm = TRUE))
                         )
                                      
                                      )
                       
                       
                       #footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`En Cours`,na.rm=TRUE)/sum(dfa_fichewilaya1()$Consistance,na.rm = TRUE))
                     ),
                     pdont = colDef(
                       name="",
                       na = "/",
                       format = colFormat(percent = TRUE,digits = 0),
                       # headerStyle = list(
                       #   background = "#9d94ac",
                       #   color="#ffffff"
                       #   #,align='left'
                       # ),
                       style = list(
                         background = "#9d94ac",
                         color="#ffffff"
                       ),
                       width = 165,
                       align = 'center',
                       #footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Dont A l'Arret`,na.rm=TRUE)/sum(dfa_fichewilaya1()$`En Cours`,na.rm=TRUE))
                       footer = `if`(is.na(sum(dfa_fichewilaya1()$`Dont A l'Arret`,dfa_fichewilaya1_suite()$`Dont A l'Arret`))==TRUE,
                                     paste("-"),
                                     sprintf("%.0f%%",100*(
                                       (sum(
                                         data_fiche_wilaya_reactive() %>% 
                                           select(dont_arret_encours_lpl,
                                                  dont_arret_encours_lpp,
                                                  dont_arret_encours_lsp,
                                                  dont_arret_encours_lv,
                                                  dont_arret_encours_lv_cnep,
                                                  dont_arret_encours_promotionellelibre,
                                                  dont_arret_encours_rural,
                                                  dont_arret_encours_acls,
                                                  dont_arret_encours_autresocial
                                           ),na.rm = TRUE))
                                       /
                                         (sum(
                                           data_fiche_wilaya_reactive() %>% 
                                             select(encours_lpl,
                                                    encours_lpp,
                                                    encours_lsp,
                                                    encours_lv,
                                                    encours_lv_cnep,
                                                    encours_promotionellelibre,
                                                    encours_rural,
                                                    encours_acls,
                                                    encours_autresocial
                                             ),na.rm = TRUE))
                                     )
                                     
                                     )
                                     
                                     
                                     #sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Dont A l'Arret`,na.rm=TRUE)/sum(dfa_fichewilaya1()$`En Cours`,na.rm=TRUE))
                       )
                       ,footerStyle = list(
                         
                         background = "#9d94ac",
                         color="#ffffff"
                       )
                     ),
                     pnonlances = colDef(
                       name="",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer=sprintf("%.0f%%",100*(
                         (sum(
                           data_fiche_wilaya_reactive() %>% 
                             select(nonlances_lpl,
                                    nonlances_lpp,
                                    nonlances_lsp,
                                    nonlances_lv,
                                    nonlances_lv_cnep,
                                    nonlances_promotionellelibre,
                                    nonlances_rural,
                                    nonlances_acls,
                                    nonlances_autresocial
                             ),na.rm = TRUE))
                         /
                           (sum(
                             data_fiche_wilaya_reactive() %>% 
                               select(consistance_lpl,
                                      consistance_lpp,
                                      consistance_lsp,
                                      consistance_lv,
                                      consistance_lv_cnep,
                                      consistance_promotionellelibre,
                                      consistance_rural,
                                      consistance_acls,
                                      consistance_autresocial
                               ),na.rm = TRUE))
                       )
                       
                       )
                       
                       
                       
                       #footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Non Lances`,na.rm=TRUE)/sum(dfa_fichewilaya1()$Consistance,na.rm=TRUE))
                       ,
                       
                       width=155
                       
                     ),
                     
                     
                     `Notifie 2020` = colDef(
                       name="",
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
                       footer=format(sum(notifie_reactive_suite(),notifie_reactive(),na.rm = TRUE)
                                     , digits=3,big.mark=" "),
                       
                       # footer = function(values) {
                       #   format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
                       # },
                       
                       width=155
                     )
                     
                   ),
                   defaultColDef = colDef(
                     na = "-",
                     sortNALast = TRUE,
                     #format = colFormat(digits = 1),
                     maxWidth = 140,
                     footerStyle = list(fontWeight = "bold",
                                        background='#81a47b',color='#ffffff',fontFamily='inherit'
                                        
                     )
                     
                     # ,
                     # headerStyle = list(background = "#2E81B0",
                     #                    color="#ffffff",
                     #                    borderLeft="0px solid #eee",
                     #                    borderRight="0px solid #eee"
                     # )
                     
                     #headerClass = "box-score-header"
                   )
         )
         
    )
    
  })
  
  
  
  
  
  
  
  ##########################
  ##########################
  ##########################
 output$gt1<-renderReactable({
    `if`(input$pourcentage==FALSE,
         reactable(dfa_fichewilaya1()[1:7,c(1,2,3,4,5,6,7)],
                   
                   sortable = FALSE,
                   #defaultSorted = list(Segment = "desc"),
                   #defaultSorted = list(Consistance = "desc", Acheves = "desc"),
                   showSortIcon = FALSE,
                   
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(name = "",
                                      footer = "Total",
                                      width=220,
                                      style =list(background='#81a47b',color='#ffffff'),
                                      footerStyle = list(fontWeight = "bold",
                                                         background='#81a47b',color='#ffffff',fontFamily='inherit',
                                                         textAlign="center"
                                                         
                                      )
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
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       }
                     ),
                     Acheves = colDef(
                       name="Achevés",
                       #width=155,
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
                       footer = function(values) {
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       }
                     ),
                     `En Cours` = colDef(
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       
                       footer = function(values) {
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       }
                     ),
                     `Dont A l'Arret` = colDef(
                       name="Dont à l'Arrêt",
                       na = "/",
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
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       }

                       
                       # footer = `if`(is.na(sum(dfa_fichewilaya1()$`Dont A l'Arret`))==TRUE,
                       #               paste("-"),
                       #               function(values) {
                       #                 format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
                       #               }
                       #        )
                       
                       ,footerStyle = list(

                         background = "#9d94ac",
                         color="#ffffff"
                       )
                     ),
                     `Non Lances` = colDef(
                       name="Non Lancés",
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       },
                       
                       width=155
                       
                     ),
                     
                     `Notifie 2020` = colDef(
                       name=paste("Notifié",format(as.Date(substr(input$select_arretee_fichewilaya,14,23), format="%Y-%m-%d"),"%Y")),
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       },
                       
                       width=155
                     )
                     
                   ),
                   defaultColDef = colDef(
                     na = "-",
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
         reactable(dfa_fichewilaya1()[1:7,c(1,2,8,9,10,11,7)],
                   
                   sortable = FALSE,
                   #defaultSorted = list(Segment = "desc"),
                   showSortIcon = FALSE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "20px"),
                   columns = list(
                     Segment = colDef(name = "",
                                      footer = "Total",
                                      width=220,
                                      style =list(background='#81a47b',color='#ffffff'),
                                      footerStyle = list(fontWeight = "bold",
                                                         background='#81a47b',color='#ffffff',fontFamily='inherit',
                                                         textAlign="center"
                                                         
                                      )
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
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3,scientific=FALSE)
                       }
                     ),
                     
                     pacheves = colDef(
                       name="Achevés",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$Acheves,na.rm=TRUE)/sum(dfa_fichewilaya1()$Consistance,na.rm=TRUE))
                       #function(values) {
                       #format(sum(values),big.mark=" ",trim=TRUE,digits=3)
                       #}
                     ),
                     pencours = colDef(
                       name="En Cours",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`En Cours`,na.rm=TRUE)/sum(dfa_fichewilaya1()$Consistance,na.rm = TRUE))
                     ),
                     pdont = colDef(
                       name="Dont à l'Arrêt",
                       na = "/",
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
                       #footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Dont A l'Arret`,na.rm=TRUE)/sum(dfa_fichewilaya1()$`En Cours`,na.rm=TRUE))
                       footer = `if`(is.na(sum(dfa_fichewilaya1()$`Dont A l'Arret`))==TRUE,
                                     paste("-"),
                                     sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Dont A l'Arret`,na.rm=TRUE)/sum(dfa_fichewilaya1()$`En Cours`,na.rm=TRUE))
                       )
                       ,footerStyle = list(

                       background = "#9d94ac",
                       color="#ffffff"
                       )
                     ),
                     pnonlances = colDef(
                       name="Non Lancés",
                       format = colFormat(percent = TRUE,digits = 0),
                       footer = sprintf("%.0f%%",100*sum(dfa_fichewilaya1()$`Non Lances`,na.rm=TRUE)/sum(dfa_fichewilaya1()$Consistance,na.rm=TRUE))
                       ,
                       
                       width=155
                       
                     ),
                     
                     
                     `Notifie 2020` = colDef(
                       name=paste("Notifié",format(as.Date(substr(input$select_arretee_fichewilaya,14,23), format="%Y-%m-%d"),"%Y")),
                       format = colFormat(separators = TRUE,locales = "fr-FR"),
                       footer = function(values) {
                         format(sum(values,na.rm=TRUE),big.mark=" ",trim=TRUE,digits=3)
                       },
                       
                       width=155
                     )
                     
                   ),
                   defaultColDef = colDef(
                     na = "-",
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
            filter(arretee==substr(input$select_arretee_fichewilaya,14,23),
                   #id_wilaya %in% c(selected20_fichewilaya())
                   
                   id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())])
                   
                   
                   ) %>%
            summarise_at(vars(consistance_lpl:anuules_durant_ce_trimestre_promotionellelibre), sum,
                         na.rm=FALSE
                           
                           # `if`(length(selected20_fichewilaya())==1,
                           #          `if`(as.numeric(selected20_fichewilaya())==49,
                           #          FALSE,
                           #          TRUE),
                           #          TRUE)
                         
                         )
          )
            #summarise_at(vars(consistance_lpl:total_lanc), sum))
            
    #  %>%    unlist(use.names = FALSE)
  })
  
  
  
  data_fiche_wilaya_reactive2=reactive({
    cbind(iad1=0,iad2=0,data_fiche_wilaya %>%
            #filter(id_wilaya %in% c(1:58)) %>% 
            filter(arretee==substr(input$select_arretee_fichewilaya,14,23),
                   #id_wilaya %in% c(selected20_fichewilaya())
                   
                   id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya2())])
                   
                   
            ) %>%
            summarise_at(vars(consistance_lpl:anuules_durant_ce_trimestre_promotionellelibre), sum,
                         na.rm=FALSE
                         
                         # `if`(length(selected20_fichewilaya())==1,
                         #          `if`(as.numeric(selected20_fichewilaya())==49,
                         #          FALSE,
                         #          TRUE),
                         #          TRUE)
                         
            )
    )
    #summarise_at(vars(consistance_lpl:total_lanc), sum))
    
    #  %>%    unlist(use.names = FALSE)
  })
  
  

  
  
  dfa_fichewilaya5=reactive({
    i=1
    ab5[1,2:7]=c(data_fiche_wilaya_reactive()[i,53:58] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab5[2,2:7]=c(data_fiche_wilaya_reactive()[i,59:64] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab5
  })
  
  
  dfa_fichewilaya6=reactive({
    i=1
    ab6[1,2:7]=c(data_fiche_wilaya_reactive()[i,65:70] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab6[2,2:7]=c(data_fiche_wilaya_reactive()[i,71:76] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab6
  })
  
  
  dfa_fichewilaya4=reactive({
    i=1
    ab4[,2]=c(data_fiche_wilaya_reactive2()[i,47:52] %>% select(1:6) %>% unlist(use.names = FALSE))
    ab47=ab4[1,2]/ab4[4,2]
    ab48=ab4[2,2]/ab4[4,2]
    ab49=ab4[3,2]/ab4[4,2]
    
    ab4[c(7,8,9),1]=c("Logements achevés, Viabilisation achevés","Logements achevés, Viabilisation en cours","Logements achevés, Viabilisation non entamée")
    
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
    
    ab2[1,6]=c(data_fiche_wilaya_reactive()$aides_reha_ap[i])
    ab2[1,7]=c(data_fiche_wilaya_reactive()$aides_reha_conso[i])
    ab2[1,8]=c(data_fiche_wilaya_reactive()$aides_reha_conso[i])/c(data_fiche_wilaya_reactive()$aides_reha_ap[i])
    
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
    ab3[1,]=data_fiche_wilaya_reactive2()[i,37:46]
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
  
  
  dfa_fichewilaya1_suite=reactive({
    
    i=1
    ab_suite[1,2:7]=data_fiche_wilaya_reactive() %>% select(consistance_autresocial:notifie2020_autresocial) #Autre social
    ab_suite[2,2:7]=data_fiche_wilaya_reactive() %>% select(consistance_promotionellelibre:notifie2020_promotionellelibre) #Promotionnele libre


    ab_suite[3,2]=sum(ab_suite[1:2,2])
    ab_suite[3,3]=sum(ab_suite[1:2,3])
    ab_suite[3,4]=sum(ab_suite[1:2,4])
    ab_suite[3,5]=sum(ab_suite[1:2,5])
    ab_suite[8,6]=sum(ab_suite[1:2,6])
    ab_suite[3,7]=sum(ab_suite[1:7,7])
    ab_suite[,2:7]=round(ab_suite[,2:7])
    
    
    ab_suite$pacheves=ab_suite$Acheves/ab_suite$Consistance
    ab_suite$pencours=ab_suite$`En Cours`/ab_suite$Consistance
    ab_suite$pdont=ab_suite$`Dont A l'Arret`/ab_suite$`En Cours`
    ab_suite$pnonlances=ab_suite$`Non Lances`/ab_suite$Consistance
    ab_suite$pacheves[is.nan(ab_suite$pacheves)==TRUE]=0
    ab_suite$pencours[is.nan(ab_suite$pencours)==TRUE]=0
    ab_suite$pdont[is.nan(ab_suite$pdont)==TRUE]=0
    ab_suite$pnonlances[is.nan(ab_suite$pnonlances)==TRUE]=0
    
    ab_suite[1:2,7]=notifie_reactive_suite() %>% t() %>% as.vector()
    #ab_suite=ab_suite %>% arrange(c(1,2,3,4,6,5,7,8))
    ab_suite
  })
  
  
  
  
  dfa_fichewilaya1=reactive({
    i=1
    ab[1,2:7]=round(data_fiche_wilaya_reactive()[i,3:8]) #lpl
    ab[2,2:7]=round(data_fiche_wilaya_reactive()[i,9:14]) #LSP/LPA
    ab[3,2:7]=round(data_fiche_wilaya_reactive()[i,15:20]) #rural
    ab[4,2:7]=round(data_fiche_wilaya_reactive()[i,21:26]) #Location-Vente
    ab[5,2:7]=round(data_fiche_wilaya_reactive()[i,27:32]) #lpp
    
    
    ab[6,2:7]=round(data_fiche_wilaya_reactive()[i,82:87]) #lv_cnepbanque

    ab[7,2]=round(data_fiche_wilaya_reactive()$consistance_acls[i]) #acls1
    ab[7,3]=round(data_fiche_wilaya_reactive()$acheves_acls[i]) #acls2
    ab[7,4]=round(data_fiche_wilaya_reactive()$encours_acls[i]) #acls3
    ab[7,5]=round(data_fiche_wilaya_reactive()$dont_arret_encours_acls[i]) #acls4
    ab[7,6]=round(data_fiche_wilaya_reactive()$nonlances_acls[i]) #acls5
    ab[7,7]=round(data_fiche_wilaya_reactive()$notifie2020_acls[i]) #acls6
    
    
    
    #ab[1:6,4]=ab[1:6,4]-ab[1:6,5]    # il faut pas jamais oublier cette case
     
    
    # for(kk1 in 1:6){
    #   if(is.na(ab[kk1,5])==TRUE){
    #     ab[kk1,4]=ab[kk1,4]    # il faut pas jamais oublier cette cas !!!!!
    #   } else{
    #     ab[kk1,4]=ab[kk1,4]-ab[kk1,5]    # il faut pas jamais oublier cette case
    #   }
    # }
    
    
     
    ab[8,2]=sum(ab[1:7,2])
    ab[8,3]=sum(ab[1:7,3])
    ab[8,4]=sum(ab[1:7,4])
    ab[8,5]=sum(ab[1:7,5])
    ab[8,6]=sum(ab[1:7,6])
    ab[8,7]=sum(ab[1:7,7])
    ab[,2:7]=round(ab[,2:7])
    
    ab$pacheves=ab$Acheves/ab$Consistance
    ab$pencours=ab$`En Cours`/ab$Consistance
    ab$pdont=ab$`Dont A l'Arret`/ab$`En Cours`
    ab$pnonlances=ab$`Non Lances`/ab$Consistance
    ab$pacheves[is.nan(ab$pacheves)==TRUE]=0
    ab$pencours[is.nan(ab$pencours)==TRUE]=0
    ab$pdont[is.nan(ab$pdont)==TRUE]=0
    ab$pnonlances[is.nan(ab$pnonlances)==TRUE]=0
    
    ab[1:7,7]=notifie_reactive() %>% t() %>% as.vector()
    ab=ab %>% arrange(c(1,2,3,4,6,5,7,8))
    ab
  })
  
  
  notifie_reactive<-reactive({
    data_fiche_wilaya %>%
            select(id_wilaya,arretee,notifie2020_lpl,notifie2020_lsp,notifie2020_rural,notifie2020_lv,notifie2020_lpp,notifie2020_lv_cnep,notifie2020_acls) %>% 
            filter(id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),   #c(selected20_fichewilaya()),
                   arretee<=as.Date(substr(input$select_arretee_fichewilaya,14,23)),
                   arretee>as.Date(ISOdate(format(as.Date(substr(input$select_arretee_fichewilaya,14,23), format="%Y-%m-%d"),"%Y"), 1, 1))
                   ) %>%
             summarise_at(3:9,sum,na.rm=TRUE)
  })
  
  
  notifie_reactive_suite<-reactive({
    data_fiche_wilaya %>%
      select(id_wilaya,arretee,notifie2020_autresocial,notifie2020_promotionellelibre) %>% 
      filter(id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),   #c(selected20_fichewilaya()),
             arretee<=as.Date(substr(input$select_arretee_fichewilaya,14,23)),
             arretee>as.Date(ISOdate(format(as.Date(substr(input$select_arretee_fichewilaya,14,23), format="%Y-%m-%d"),"%Y"), 1, 1))
      ) %>%
      summarise_at(3:4,sum,na.rm=TRUE)
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
  # if(vaz.includes('LSP/LPA')==true ) {
  #   var dab =document.getElementsByClassName('highcharts-data-label');
  #   dab[2].children[0].style.fontSize='10px';
  # }
  #
  
  
  # document.getElementsByClassName('highcharts-data-label').children[0].style.fontSize="0px"
  # var df=document.getElementsByClassName('highcharts-data-label')
  # df[2].children[0].lastElementChild.textContent="4645"   #(325 458)
  
  # df[2].children[0].style.fontSize="0px"           # LSP/LPA 325 458 to 0px
  
  
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
         paste("Livraison des logements par Segments : "),
         paste("Lancement des logements par Segments : ")
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
  
  # output$excel_fw7<-renderExcel({
  #   excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
  #              dfa_fichewilaya7()
  #   )
  # })
  
  
  output$excel_fw_attribution<-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
                 attribution_fw %>%
                 filter(id_wilaya %in% c(1:58),
                        arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
                   mutate("Taux(%)"=round(100*(Attribue/Prevu),2)) %>% 
                   select(2,3,4,5,7,6) %>% 
                   `colnames<-`(c("Wilaya","Segment","Prévu","Attribué","Taux(%)","Arrétée"))
               
    )
  })
  
  output$excel_fw7<-renderExcel({
  excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,search = TRUE,
             #rbind(colnames(df3_ab),
                   reactive_excel_fw7()
             #)
             )
  })
  
  
  output$excel_fw1_divhover7<-renderExcel({
    
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c("Wilaya","En Cours","Inf à 30%","Entre 30% et 60%","Sup à 60%"),
                     details_encours_lpl_lsp_lv_lpp %>%
                       filter(
                         Segment=="LPP",
                         #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                         id_wilaya %in% c(1:58,100,101,102),
                         arretee==substr(input$select_arretee_fichewilaya,14,23)
                       ) %>%
                       group_by(Wilaya) %>%
                       summarise_at(3:6,sum,na.rm=TRUE)
               )
    )
    
  })
  
  
  
  output$excel_fw1_divhover6<-renderExcel({
    
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c("Wilaya","En Cours","Inf à 30%","Entre 30% et 60%","Sup à 60%"),
                     details_encours_lpl_lsp_lv_lpp %>%
                       filter(
                         Segment=="Location-Vente",
                         id_wilaya %in% c(1:58,100,101,102),
                         
                         #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                         arretee==substr(input$select_arretee_fichewilaya,14,23)
                       ) %>%
                       group_by(Wilaya) %>%
                       summarise_at(3:6,sum,na.rm=TRUE)
               )
    )
    
  })
  
  
  output$excel_fw1_divhover5<-renderExcel({
    
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c("Wilaya","En Cours","Inf à 30%","Entre 30% et 60%","Sup à 60%"),
                     details_encours_lpl_lsp_lv_lpp %>%
                       filter(
                         Segment=="LSP/LPA",
                         id_wilaya %in% c(1:58,100,101,102),
                         
                         #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                         arretee==substr(input$select_arretee_fichewilaya,14,23)
                       ) %>%
                       group_by(Wilaya) %>%
                       summarise_at(3:6,sum,na.rm=TRUE)
               )
    )
    
  })
  
  output$excel_fw1_divhover4<-renderExcel({
    
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c("Wilaya","En Cours","Inf à 30%","Entre 30% et 60%","Sup à 60%"),
                     details_encours_lpl_lsp_lv_lpp %>%
                       filter(
                         Segment=="LPL",
                         id_wilaya %in% c(1:58,100,101,102),
                         
                         #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                        arretee==substr(input$select_arretee_fichewilaya,14,23)
                        ) %>%
                        group_by(Wilaya) %>%
                        summarise_at(3:6,sum,na.rm=TRUE)
               )
    )
    
  })
  
  
  
  output$excel_fw1_divhover3<-renderExcel({
    
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c(colnames(details_nonlances_rural)[2:10]),
                     details_nonlances_rural %>%
                       filter(
                         id_wilaya %in% c(1:58,100,101,102),
                         
                         #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                         arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
                       group_by(Wilaya) %>%
                       summarise_at(2:9,sum,na.rm=TRUE)
               )
    )
    
  })
  
  
  
  
  
  output$excel_fw1_divhover2<-renderExcel({
    
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c(colnames(details_nonlances_lsp)[2:14]),
                     details_nonlances_lsp %>%
                       filter(
                         id_wilaya %in% c(1:58,100,101,102),
                         
                         #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                         arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
                       group_by(Wilaya) %>%
                       summarise_at(2:13,sum,na.rm=TRUE)
               )
    )
    
  })
  
  
  
  
  output$excel_fw1_divhover1<-renderExcel({
    
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c(colnames(details_nonlances_lpl)[2:14]),
                     details_nonlances_lpl %>%
                       filter(
                         id_wilaya %in% c(1:58,100,101,102),
                         
                         #id_wilaya %in% as.numeric(unique(data_fiche_wilaya$id_wilaya)[c(selected20_fichewilaya())]),
                         arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
                       group_by(Wilaya) %>%
                       summarise_at(2:13,sum,na.rm=TRUE)
               )
    )
    
  })
  
  
  
  output$excel_fw1<-renderExcel({

  excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,search = TRUE,
    rbind(c("Wilaya","Segment","Consistance","Achevés","En Cours","Dont à l'arrêt","Non Lancés",
            paste("Notifié",format(as.Date(substr(input$select_arretee_fichewilaya,14,23), format="%Y-%m-%d"),"%Y")),
            "Arrétée"),
          reactive_excel_fw1()
    )
  )
    
  })
  
  
  
  # output$excel_fw1<-renderExcel({
  #   excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
  #     rbind(
  #       #c("Segments","Consistance","Achevés","En Cours","à l'Arrêt","Non Lancés","Notifié"),
  #       data_fiche_wilaya %>%
  #       filter(arretee==substr(input$select_arretee_fichewilaya,14,23),
  #              id_wilaya %in% c(selected20_fichewilaya())) %>%
  #       summarise_at(vars(consistance_lpl:notifie2020_lpp),sum) %>%
  #       select(consistance_lpl:notifie2020_lpp) %>% 
  #       t() %>%
  #       data.frame() %>%
  #       mutate("Activite"=rep(c("Consistance","Acheves","En Cours","à l'Arrêt","Non Lances","Notifie"),5),
  #              "Segments"=rep(c("LPL","LSP/LPA","Rural","Location-Vente","LPP"),each=6)) %>%
  #       `colnames<-`(c("Nb", "Activite", "Segments")) %>%
  #       spread(Activite,Nb) %>% select(1,3,2,5,4,6,7) %>% 
  #         select(1,5,2,4,3,6,7)
  #     #   %>%
  #     #   mutate(Notifie=notifie_reactive() %>% t() %>% as.vector()) %>% 
  #     #   #############################################
  #     # rbind(c("Total",data_fiche_wilaya %>%
  #     #           filter(arretee==substr(input$select_arretee_fichewilaya,14,23),
  #     #                  id_wilaya %in% c(selected20_fichewilaya())) %>%
  #     #           summarise_at(vars(consistance_lpl:notifie2020_lpp),sum) %>%
  #     #           select(consistance_lpl:notifie2020_lpp) %>% 
  #     #           t() %>%
  #     #           data.frame() %>%
  #     #           mutate("Activite"=rep(c("Constitance","Acheves","En Cours","à l'Arrêt","Non Lances","Notifie"),5),
  #     #                  "Segments"=rep(c("LPL","LSP","Rural","Location-Vente","LPP"),each=6)) %>%
  #     #           `colnames<-`(c("Nb", "Activite", "Segments")) %>% group_by(Activite) %>% summarise(Nb=sum(Nb)) %>%
  #     #           #arrange(c(2,1,4,3,5,6)) %>%
  #     #           #arrange(c(5,4,3,2,1,6)) %>% 
  #     #     mutate(Nb = ifelse(Activite == 'Notifie',notifie_reactive() %>% t() %>% as.vector() %>% sum(),Nb))
  #     #     %>% arrange(c(2,1,4,3,5,6)) %>% .$Nb
  #     #     )) %>%
  #     #   
  #     #   `colnames<-`(rep("",7))
  # )
  #     
  #       
  #     
  #     
  #     #############################################
  #   )
  # })
  
   output$excel_fw2<-renderExcel({
     excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
       rbind(c("Wilaya","Consistance","Achevés","En Cours","Non Lancés","AP","Consomations","Taux(%)"),
             data_fiche_wilaya %>%
               filter(id_wilaya %in% c(1:58),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
               select(c(Wilaya,reha_consistance:reha_nonlances,aides_reha_ap,aides_reha_conso)) %>%
               mutate(taux=round(100*(aides_reha_conso/aides_reha_ap),2) ) %>% 
               mutate(aides_reha_ap=format(aides_reha_ap,scientific=FALSE,nsmall=2),
                      aides_reha_conso=format(aides_reha_conso,scientific=FALSE,nsmall=2)) %>% 
               `colnames<-`(c("Wilaya","Consistance","Achevés","En Cours","Non Lancés","AP","Consomations","Taux(%)")),
             c("Total",data_fiche_wilaya %>%
               filter(arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
               summarise_at(vars(reha_consistance:reha_nonlances,aides_reha_ap,aides_reha_conso),sum) %>%
                 mutate(taux=round(100*sum(aides_reha_conso)/sum(aides_reha_ap),2)) %>%
               as.double()
             ))
     )
   })
  # 
  
  
  output$excel_fw9<-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c(colnames(etat_cession_fw %>% filter(
                 arretee==substr(input$select_arretee_fichewilaya,14,23)
               ) %>% select(4:14)),"Total"),
                     etat_cession_fw %>% filter(
                       arretee==substr(input$select_arretee_fichewilaya,14,23)
                     ) %>% select(4:14)
                     )
    )
  })
  
  output$excel_fw8<-renderExcel({
    excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
               rbind(c(colnames(
                 patrimoine_fw %>% filter(
                   arretee==substr(input$select_arretee_fichewilaya,14,23)
               ) %>% select(4:6)
               ),"Total"),
                     patrimoine_fw %>% filter(
                       arretee==substr(input$select_arretee_fichewilaya,14,23)
                     ) %>% select(4:6) %>% 
                 mutate(Total=logements+Locaux),
                     c("Total",sum(patrimoine_fw$logements),sum(patrimoine_fw$Locaux),sum(patrimoine_fw$logements,patrimoine_fw$Locaux))
                     )
    )
  })
  
   output$excel_fw3<-renderExcel({
     excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
       data_fiche_wilaya %>% filter(id_wilaya %in% c(1:58),arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
         select(Wilaya,c(logts_notifie_acheves:logtsache_total)) %>% 
         `colnames<-`(c("Wilaya","Achevés","En Cours","Non Lancés","Total",
                        "Achevés","En Cours","Non Lancés","Total",
                        "Achevés à 60% et plus",
                        "Total général",
                        "Logements achevés, viabilisation achevés",
                        "Logements achevés, viabilisation en cours",
                        "Logements acheves, viabilisation non entamée",
                        "Total"
                        )) %>% 
         rbind(c("Total",data_fiche_wilaya %>% filter(arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
                   summarise_at(vars(logts_notifie_acheves:logtsache_total),sum) %>% as.double()))
       ,nestedHeaders =list( data.frame(title=c(
       " ",
        "Logts réceptionnés et Notifiés",
       "Logts réceptionnés et Non Notifiés",
       "Achevés à 60% et plus",
       "Total général",
       " ",
       " ",
       " ",
       " "
       ),colspan=c(1,4,4,1,1,1,1,1,1)))
     )
   })
  # 
   # output$excel_fw4<-renderExcel({
   #   excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
   #              
   #     data_fiche_wilaya %>%
   #       filter(arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
   #       select(Wilaya,lpl_prevus,lpl_livres,
   #              lsp_prevue,lsp_livres,
   #              rural_prevue,rural_livres,
   #              lv_pruve,lv_livres,
   #              lpp_pruve,lpp_livres) %>%
   #       mutate(lpl_taux=round(100*(lpl_livres/lpl_prevus),1),.after=lpl_livres) %>% 
   #       mutate(lsp_taux=round(100*(lsp_livres/lsp_prevue),1),.after=lsp_livres) %>% 
   #       mutate(rural_taux=round(100*(rural_livres/rural_prevue),1),.after=rural_livres) %>% 
   #       mutate(lv_taux=round(100*(lv_livres/lv_pruve),1),.after=lv_livres) %>% 
   #       mutate(lpp_taux=round(100*(lpp_livres/lpp_pruve),1),.after=lpp_livres) %>% 
   #       mutate(total_livraison=lpl_livres+lsp_livres+rural_livres+lv_livres+lpp_livres,
   #              total_prevision=lpl_prevus+lsp_prevue+rural_prevue+lv_pruve+lpp_pruve) %>% 
   #       mutate(total_taux=round(100*((lpl_livres+lsp_livres+rural_livres+lv_livres+lpp_livres)/
   #                                      (lpl_prevus+lsp_prevue+rural_prevue+lv_pruve+lpp_pruve)),1)
   #              ) %>% 
   #       
   #       
   #       `colnames<-`(c("Wilaya",
   #                      "Livrés","Prévus","Taux(%)",
   #                      "Livrés","Prévus","Taux(%)",
   #                      "Livrés","Prévus","Taux(%)",
   #                      "Livrés","Prévus","Taux(%)",
   #                      "Livrés","Prévus","Taux(%)",
   #                      "Livrés","Prévus","Taux(%)"))
   #     
   #   ,nestedHeaders =list( data.frame(title=c(" ","LPL","LSP/LPA","Rural","Location-Vente","LPP","Total"),
   #                                    colspan=c(1,3,3,3,3,3,3)
   #   ))
   #                                    
   #   )
   # })
   
   
   
   
   # 
   # output$excel_fw5<-renderExcel({
   #   excelTable(editable = FALSE,showToolbar = TRUE,columnSorting=FALSE,
   #              
   #              data_fiche_wilaya %>%
   #                filter(arretee==substr(input$select_arretee_fichewilaya,14,23)) %>%
   #                select(Wilaya,lpl_prevus_lanc,lpl_lanc,
   #                       lsp_prevue_lanc,lsp_lanc,
   #                       rural_prevue_lanc,rural_lanc,
   #                       lv_pruve_lanc,lv_lanc,
   #                       lpp_pruve_lanc,lpp_lanc) %>%
   #                mutate(lpl_taux=round(100*(lpl_lanc/lpl_prevus_lanc),1),.after=lpl_lanc) %>% 
   #                mutate(lsp_taux=round(100*(lsp_lanc/lsp_prevue_lanc),1),.after=lsp_lanc) %>% 
   #                mutate(rural_taux=round(100*(rural_lanc/rural_prevue_lanc),1),.after=rural_lanc) %>% 
   #                mutate(lv_taux=round(100*(lv_lanc/lv_pruve_lanc),1),.after=lv_lanc) %>% 
   #                mutate(lpp_taux=round(100*(lpp_lanc/lpp_pruve_lanc),1),.after=lpp_lanc) %>% 
   #                mutate(total_lancement=lpl_lanc+lsp_lanc+rural_lanc+lv_lanc+lpp_lanc,
   #                       total_prevision=lpl_prevus_lanc+lsp_prevue_lanc+rural_prevue_lanc+lv_pruve_lanc+lpp_pruve_lanc) %>% 
   #                mutate(total_taux=round(100*(total_lancement/total_prevision),1)
   #                ) %>% 
   #                
   #                
   #                `colnames<-`(c("Wilaya",
   #                               "Lancés","Prévus","Taux(%)",
   #                               "Lancés","Prévus","Taux(%)",
   #                               "Lancés","Prévus","Taux(%)",
   #                               "Lancés","Prévus","Taux(%)",
   #                               "Lancés","Prévus","Taux(%)",
   #                               "Lancés","Prévus","Taux(%)"))
   #              
   #              ,nestedHeaders =list( data.frame(title=c(" ","LPL","LSP/LPA","Rural","Location-Vente","LPP","Total"),
   #                                               colspan=c(1,3,3,3,3,3,3)
   #              ))
   #              
   #   )
   # })
   # 
   
   
  
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
         print("Toutes les Wilayas"),
         print(data_equip_reactive()$Wilaya[selected_eqp()]))
  })
  
  
  output$wilayaselecteqp22<-renderText({
    # secteurselecteqp()
    `if`(length(selected_eqp())==48,
         print("Toutes les Wilayas"),
         print(data_equip_reactive()$Wilaya[selected_eqp()]))
  })
  
  
  ssw1<-reactive({
    `if`(length(input$selectwilayas) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$selectwilayas)>4,paste0("Les ",length(input$selectwilayas)," ","Wilayas sélectionnées"),paste(input$selectwilayas,collapse = "+")))
  })
  
  ssw1_lanc<-reactive({
    `if`(length(input$selectwilayas_lanc) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$selectwilayas_lanc)>4,paste0("Les ",length(input$selectwilayas_lanc)," ","Wilayas sélectionnées"),paste(input$selectwilayas_lanc,collapse = "+")))
  })
  
  
  output$rowdonnees33html<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Données Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive2())==6,paste('Tous les Segments'),paste(segments_reactive2(),collapse = '+')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees)!=max(input$selectannees),paste(min(input$selectannees),' au ',max(input$selectannees)),paste('Année :',max(input$selectannees)) ),'</span>'
    ))    
  )
  
  
  output$rowdonnees33html_lanc<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Données Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw1_lanc(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive2_lanc())==6,paste('Tous les Segments'),paste(segments_reactive2_lanc(),collapse = '+')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees_lanc)!=max(input$selectannees_lanc),paste(min(input$selectannees_lanc),' au ',max(input$selectannees_lanc)),paste('Année :',max(input$selectannees_lanc)) ),'</span>'
    ))    
  )
  
  
  ssw2<-reactive({
    `if`(length(input$selectwilayas_wilaya) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$selectwilayas_wilaya)>4,paste0("Les ",length(input$selectwilayas_wilaya)," ","Wilayas sélectionnées"),paste(input$selectwilayas_wilaya,collapse = "+")))
  })
  
  
  output$rowdonnees33html2<-renderUI(
    HTML(paste(
      '<span style="font-size:11px;">Données Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ssw2(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$selectannees_wilaya)!=max(input$selectannees_wilaya),paste(min(input$selectannees_wilaya),' au ',max(input$selectannees_wilaya)),paste('Année :',max(input$selectannees_wilaya)) ),'</span>'
    ))    
  )
  
  
  
  ttw1<-reactive({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$wilayas)>4,paste0("Les ",length(input$wilayas)," ","Wilayas sélectionnées"),paste(input$wilayas,collapse = "+")))
  })
  
  
  ttw1_lanc<-reactive({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$wilayas)>4,paste0("Les ",length(input$wilayas)," ","Wilayas sélectionnées"),paste(input$wilayas,collapse = "+")))
  })
  
  
  ttw1_tol<-reactive({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$wilayas)>4,paste0("Les ",length(input$wilayas)," ","Wilayas sélectionnées"),paste(input$wilayas,collapse = "+")))
  })
  
  
  
  
  output$tablededonnes1_sitphy<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de données</span> <br> <span style="font-size:13px;">Situation Arrétée au :&nbsp;&nbsp; </span> <span style="font-size:14px;">',substr(input$select_arretee_sitphy,14,23),'</span>'
    ))
    
  )
  
  output$tablededonnes1_fw1_divhover7<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Taux d'Avancement du Programme LPP</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
      paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
  
  
  output$tablededonnes1_fw1_divhover6<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Taux d'Avancement du Programme Location-Vente</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
      paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
 
  output$tablededonnes1_fw1_divhover5<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Taux d'Avancement du Programme LSP/LPA</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
      paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
  
  output$tablededonnes1_fw1_divhover4<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Taux d'Avancement du Programme LPL</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
      paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
  
  
  output$tablededonnes1_fw1_divhover3<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Phases du Programme Rural Non Lancés</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
      paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
  
  
  
  output$tablededonnes1_fw1_divhover2<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Phases du Programme LSP/LPA Non Lancés</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
      paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
  
   
  
  output$tablededonnes1_fw1_divhover1<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Phases du Programme LPL Non Lancés</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
      paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
  
  
  
  
  output$tablededonnes1_fw1<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>État d'Exécution du Programme</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"&nbsp;&nbsp;&nbsp;&nbsp;-&nbsp;&nbsp;&nbsp;&nbsp;",
           paste("Toutes les Wilayas")
      ,"</span>"
    ))
    
  )
  
  
  
  output$tablededonnes1_fw7<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>",
      paste("Activité du",str_sub(input$select_title_gt7,13,-1))
      #paste(unlist(strsplit(input$select_title_gt7, split=" "))[unlist(strsplit(input$select_title_gt7, split=" "))!=c("Activité","du")], collapse=" ")
      ,"</span>
      <br> <span style='font-size:13px;'>",
           paste("Toutes les Wilayas")

      ,"</span>"
    ))
    
  )
  
  output$tablededonnes1_fw_attribution<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Programme d'attribution de logements et aides</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"</span>"
    ))
    
  )
  
  
  output$tablededonnes1_fw2<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Aides à la réhabilitation</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"</span>"
    ))
    
  )
  
  
  
  
  output$tablededonnes1_fw8<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Patrimoine en Location</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"</span>"
    ))
    
  )
  
  output$tablededonnes1_fw9<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Etat de la Cession</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"</span>"
    ))
    
  )
  
  
  
  output$tablededonnes1_fw3<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Logements LPL en instance d’attribution</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"</span>"
    ))
    
  )
  
  
  output$tablededonnes1_fw4<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>État des Livraisons</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"</span>"
    ))
    
  )
  
  output$tablededonnes1_fw5<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>État des Lancements</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya,14,23),"</span>"
    ))
    
  )
  
  
  
  output$tablededonnes1_eqp<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de données</span> <br> <span style="font-size:13px;">Situation Arrétée au :&nbsp;&nbsp; </span> <span style="font-size:14px;">',substr(input$select_arretee_eqp,14,23),'</span>'
    ))
    
  )
  
  
  output$tablededonnes1_mapst<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Entreprises qualifiées</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  output$tablededonnes1_search<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Entreprises qualifiées ( Catégoris 5-9 ) </span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  
  
  
  output$tablededonnes2_mapst<-renderUI(
    HTML(paste0(
      "<span style='font-size:25px;vertical-align:-25%;'>Activité</span> <br> <span style='font-size:13px;'>du </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,17),"-01-01  au  ",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  output$tablededonnes3_mapst<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Lotissement Socioux (Wilayas Sud et Haut Plateau)</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
  )
  
  
  output$tablededonnes4_mapst<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Agréments des Promoteurs Immobiliers </span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
  )
  
  output$tablededonnes5_mapst<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Agréments des Agents Immobiliers  </span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
  )
  
  
  output$tablededonnes1_maps<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Nombre d'entreprises qualifiées par Wilaya</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  
  output$tablededonnes1_maps_prom<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Activité par Wilaya</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  
  output$tablededonnes1_maps_agence<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Lotissement Socioux par Wilaya</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  output$tablededonnes1_maps_ing<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Agréments des Promoteurs Immobiliers par Wilaya</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  
  output$tablededonnes1_maps_ai<-renderUI(
    HTML(paste(
      "<span style='font-size:25px;vertical-align:-25%;'>Agréments des Agents Immobiliers par Wilaya</span> <br> <span style='font-size:13px;'>Situation Arrétée au :&nbsp;&nbsp; </span> <span style='font-size:14px;'>",substr(input$select_arretee_fichewilaya_mr,14,23),"</span>"
    ))
    
  )
  
  
  output$tablededonnes1<-renderUI(
    
    `if`(input$radio_choose_line1=="Livraisons de Logements",
         HTML(paste(
           '<span style="font-size:25px;vertical-align:-25%;">Table de données - Livraisons -</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive())==6,paste('Tous les Segments'),paste(segments_reactive(),collapse = ',')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Année :',max(input$annees)) ),'</span>'
         )),
         `if`(input$radio_choose_line1=="TOL",
              HTML(paste(
                '<span style="font-size:25px;vertical-align:-25%;">Table de données  - TOL -</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1_tol(),'&nbsp;&nbsp;&nbsp;</span>'
              ))     
              ,    
              HTML(paste(
                '<span style="font-size:25px;vertical-align:-25%;">Table de données  - Lancements -</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1_lanc(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(length(segments_reactive())==6,paste('Tous les Segments'),paste(segments_reactive(),collapse = ',')),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Année :',max(input$annees)) ),'</span>'
              ))
         )
    )
  )
  
  
  output$tablededonnes2<-renderUI(
    `if`(input$radio_choose_line1_pie=="Livraisons de Logements",
         HTML(paste(
           '<span style="font-size:25px;vertical-align:-25%;">Table de données - Livraisons</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Année :',max(input$annees)) ),'</span>'
         ))
         ,
         HTML(paste(
           '<span style="font-size:25px;vertical-align:-25%;">Table de données - Lancements</span> <br> <span style="font-size:10px;">Pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',ttw1_lanc(),'&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp;&nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Année :',max(input$annees)) ),'</span>'
         ))
         
    )
  )
  
  
  output$tablededonnes3<-renderUI(
    HTML(paste(
      '<span style="font-size:25px;vertical-align:-25%;">Table de données</span> <br> <span style="font-size:12px;">Livraisons et Lancements pour :&nbsp;&nbsp; </span> <span style="font-size:13px;">',`if`(length(segments_reactive())==6,paste('Tous les Segments'),paste(segments_reactive(),collapse = '+')),'- &nbsp;',`if`(min(input$annees)!=max(input$annees),paste(min(input$annees),' au ',max(input$annees)),paste('Année :',max(input$annees)) ),'&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; - &nbsp;&nbsp; (Population,   Parc logements,   TOL) : Année',max(input$annees),'</span>'
    ))
  )
  
  observe({
    runjs("
                    
          ggg=document.getElementsByClassName('glyphicon-home');
          ggg[0].parentElement.style.marginLeft='-278px';
          ggg[0].parentElement.style.opacity='0';
          ggg[0].parentElement.style.left='-423px';

          
          ab=document.getElementById('search');
          ab.parentElement.parentElement.style.position='absolute';
          ab.parentElement.parentElement.style.zIndex='99999';
          ab.parentElement.parentElement.style.right='648px';
          ab.parentElement.parentElement.style.top='80px';
          ab.parentElement.parentElement.style.width='200px';
          
          asas=document.getElementById('modal_maps');
          asas.children[0].children[0].children[1].style.zoom='1.3'
          
          asas2=document.getElementById('modal_maps_prom');
          asas2.children[0].children[0].children[1].style.zoom='1.3'
          
            
          asas3=document.getElementById('modal_maps_agence');
          asas3.children[0].children[0].children[1].style.zoom='1.3'

          asas4=document.getElementById('modal_maps_ing');
          asas4.children[0].children[0].children[1].style.zoom='1.3'
          
          asas5=document.getElementById('modal_maps_ai');
          asas5.children[0].children[0].children[1].style.zoom='1.3'
          
          
          cs2=document.getElementById('modal2_mapst')
          cs2.children[0].children[0].style.width='1000px'
          
          
          cs4=document.getElementById('modal4_mapst')
          cs4.children[0].children[0].style.width='1100px'
          
          
                    
          cs_search=document.getElementById('modal1_search')
          cs_search.children[0].children[0].style.width='1000px'
          
          
          
          
          avc=document.getElementsByClassName('dropdown-toggle');
          avc[1].style.marginTop='-8px';
          
          
          bta1=document.getElementById('modal_homepage1')
          bta1.children[0].style.width='1700px'
          
          
          bh=document.getElementById('modal_homepage3')
          bh.children[0].children[0].children[1].style.padding='0px';
          
          abc_b1=document.getElementById('homepage_button1');
          setTimeout(() => {abc_b1.style.opacity=0.9;
              abc_b1.style.transition='opacity 6s'; }, 4800);
              
              
          abc_b2=document.getElementById('homepage_button2');
          setTimeout(() => {abc_b2.style.opacity=0.9;
              abc_b2.style.transition='opacity 6s'; }, 4800);
              
              
          abc_b3=document.getElementById('homepage_button3');
          setTimeout(() => {abc_b3.style.opacity=0.9;
              abc_b3.style.transition='opacity 6s'; }, 4800);    
              
          
          
          gvg=document.getElementById('modal_fw7');
          gvg.children[0].style.width='1400px';    
          
           
          gvg=document.getElementById('modal_fw1');
          gvg.children[0].style.width='950px';    
      
      

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
    
    
    
      
    fw5_width=document.getElementById('modal_fw5');
    fw5_width.children[0].children[0].style.width='127%'
    
    fw4_width=document.getElementById('modal_fw4');
    fw4_width.children[0].children[0].style.width='127%'
    
    fw3_width=document.getElementById('modal_fw3');
    fw3_width.children[0].children[0].style.width='120%'
    
    fw8_width=document.getElementById('modal_fw8');
    fw8_width.children[0].children[0].style.width='120%'
    
        
    fw9_width=document.getElementById('modal_fw9');
    fw9_width.children[0].children[0].style.width='120%'
    
    
    
    
    
    
    
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
         paste0('TOL Par Année'),
         `if`(input$radio_choose_line1=="Livraisons de Logements",
              `if`(min(input$annees)!=max(input$annees),
                   paste("Livraison des logements par Années : "),
                   paste("Livraison des logements de l'année ",max(input$annees)," :")
              ),
              `if`(min(input$annees)!=max(input$annees),
                   paste("Lancement des logements par Années : "),
                   paste("Lancement des logements de l'année ",max(input$annees)," :")
              )
              
         ))
  })
  
  
  output$titre_map<-renderText({
    `if`(input$radio_choose_leaflet=="Livraisons de Logements",
         paste0("Livraison des logements par Wilaya : "),paste0(input$radio_choose_leaflet," Par Wilaya :")
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
  
  ################################ agrement
  
  onevent("mouseenter", "choose_leaflet_agrement", show("display_when_hover_choose_leaflet_agrement"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet_agrement", show("display_when_hover_choose_leaflet_agrement"))
  onevent("mouseleave", "display_when_hover_choose_leaflet_agrement", hide("display_when_hover_choose_leaflet_agrement"))
  
  
  
  onevent("mouseenter", "choose_leaflet_agrement_prom", show("display_when_hover_choose_leaflet_agrement_prom"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet_agrement_prom", show("display_when_hover_choose_leaflet_agrement_prom"))
  onevent("mouseleave", "display_when_hover_choose_leaflet_agrement_prom", hide("display_when_hover_choose_leaflet_agrement_prom"))
  
  
  
  onevent("mouseenter", "choose_leaflet_agrement_agence", show("display_when_hover_choose_leaflet_agrement_agence"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet_agrement_agence", show("display_when_hover_choose_leaflet_agrement_agence"))
  onevent("mouseleave", "display_when_hover_choose_leaflet_agrement_agence", hide("display_when_hover_choose_leaflet_agrement_agence"))
  
  
  
  onevent("mouseenter", "choose_leaflet_agrement_ing", show("display_when_hover_choose_leaflet_agrement_ing"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet_agrement_ing", show("display_when_hover_choose_leaflet_agrement_ing"))
  onevent("mouseleave", "display_when_hover_choose_leaflet_agrement_ing", hide("display_when_hover_choose_leaflet_agrement_ing"))
  
  
  onevent("mouseenter", "choose_leaflet_agrement_ai", show("display_when_hover_choose_leaflet_agrement_ai"))
  #onevent("mouseleave", "choose_leaflet", hide("display_when_hover_choose_leaflet"))
  
  onevent("mouseenter", "display_when_hover_choose_leaflet_agrement_ai", show("display_when_hover_choose_leaflet_agrement_ai"))
  onevent("mouseleave", "display_when_hover_choose_leaflet_agrement_ai", hide("display_when_hover_choose_leaflet_agrement_ai"))
  
  
  
  
  
  ############################### agrement_fin
  
  
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
         paste(" "," "," (Tous les Secteurs)"),
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
      arrange(desc(`Nbre de Projets`)) %>% 
      mutate(Taux=0)
  })
  
  equip_secteur_reactive_pr=reactive({
    equip %>% 
      filter(Arretee==substr(input$select_arretee_eqp,14,23),as.numeric(substr(equip$Wilaya,1,2)) %in% selected_equip()) %>%
      select(1,2,3,4,5,8,9,6,7) %>% 
      group_by(Secteur) %>% 
      summarise_at(vars(`Nbre de Projets`:Geles),sum) %>%
      mutate(Acheves=Acheves/`Nbre de Projets`,`En Cours`=`En Cours`/`Nbre de Projets`,`Non Lances`=`Non Lances`/`Nbre de Projets`,`Dont NIR`=`Dont NIR`/`Nbre de Projets`,Geles=Geles/`Nbre de Projets`) %>% 
      arrange(desc(`Nbre de Projets`)) %>% 
      mutate(Taux=0)
  })
  
  output$equip_secteur_reactable<-renderReactable({
    `if`(input$pourcentage_equip==FALSE,
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
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "15px"),
                   columns = list(
                     .selection = colDef(
                       width = 1,
                       style = list(display = "none"),
                       footerStyle = list(marginLeft="-15px")
                       
                     ),
                     Secteur = colDef(name="",width = 210,footer = "Total",
                                      
                                      style = list(background="#81a47b",color="#ffffff",fontFamily="system-ui",fontWeight="500")),
                     `Nbre de Projets` = colDef(width = 140,align = "center",
                                                footer = sum(equip_secteur_reactive()$`Nbre de Projets`),
                                                headerStyle = list(
                                                  background = "#2E81B0",
                                                  color="#ffffff",  
                                                  borderLeft="0px"
                                                )
                     ),
                     Acheves=colDef(name="Achevés",width = 80,
                                    footer = sum(equip_secteur_reactive()$Acheves)
                                    
                     ),
                     Geles=colDef(name="Gelés",width = 60,
                                  footer = sum(equip_secteur_reactive()$Geles)
                                  
                     ),
                     `En Cours`=colDef(width = 90,
                                       footer = sum(equip_secteur_reactive()$`En Cours`)
                                       
                     ),
                     `Non Lances`=colDef(name="Non Lancés",width=110,
                                         footer = sum(equip_secteur_reactive()$`Non Lances`)
                                         
                     ),
                     `Dont NIR`=colDef(width=90,
                                       footer = sum(equip_secteur_reactive()$`Dont NIR`)
                                       
                     ),
                     `Taux`=colDef(align = "center",cell = function(value, index) {
                       sparkline(c(equip_secteur_reactive()$Acheves[[index]],equip_secteur_reactive()$`En Cours`[[index]],equip_secteur_reactive()$`Non Lances`[[index]],equip_secteur_reactive()$`Dont NIR`[[index]],equip_secteur_reactive()$Geles[[index]])
                                 ,type="pie",height=50,sliceColors=c("#5F9E4F","rgb(239, 192, 0)","rgb(231, 25, 25)","#A9A9A9","#101010"),
                                 tooltipFormat=HTML('<p id="adsq" style="background-color:#fff;font-size:15px;font-weight:800;color: {{color}}">&#9679; {{offset:names}}:  {{value}} ({{percent.1}}%);</p>'),
                                 tooltipValueLookups=list(names=list(c("Acheves"),c("En Cours"),c("Non Lances"),c("Dont NIR"),c("Geles"))
                                                          #HTML("0: 'Automotive',1: 'Locomotive',2: 'Unmotivated'")
                                 ),
                                 numberDigitGroupSep=" " ,
                                 tooltipFormat=list(shared=TRUE)
                                 
                       )
                     },
                     footer = function(value) {
                       sparkline(c(sum(equip_secteur_reactive()$Acheves),sum(equip_secteur_reactive()$`En Cours`),sum(equip_secteur_reactive()$`Non Lances`),sum(equip_secteur_reactive()$`Dont NIR`),sum(equip_secteur_reactive()$Geles))
                                 ,type="pie",height=50,sliceColors=c("#5F9E4F","rgb(239, 192, 0)","rgb(231, 25, 25)","#A9A9A9","#101010"),
                                 tooltipFormat=HTML('<p id="adsq" style="background-color:#fff;font-size:15px;font-weight:800;color: {{color}}">&#9679; {{offset:names}}:  {{value}} ({{percent.1}}%);</p>'),
                                 tooltipValueLookups=list(names=list(c("Acheves"),c("En Cours"),c("Non Lances"),c("Dont NIR"),c("Geles"))
                                                          #HTML("0: 'Automotive',1: 'Locomotive',2: 'Unmotivated'")
                                 ),
                                 numberDigitGroupSep=" " ,
                                 tooltipFormat=list(shared=TRUE)
                                 
                       )
                     })
                     
                     
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
                       fontSize="15px",
                       marginLeft="-3px"
                     )
                     
                   ),
                   
                   theme = reactableTheme(
                     rowSelectedStyle = list(backgroundColor = "#81a47b", boxShadow = "inset 2px 0 0 0 #ffa62d")
                   )
                   
         ),
         
         reactable(equip_secteur_reactive_pr(),
                   #sortable = TRUE,
                   rowStyle =list(cursor = "pointer"),
                   onClick = "select",
                   selection = "single",
                   #striped = TRUE,
                   #fullWidth = FALSE,
                   bordered = TRUE,
                   
                   #defaultSorted = list(`Nbre de Projets` = "desc", Acheves = "desc"),
                   #showSortIcon = FALSE,
                   style = list(fontFamily = "Work Sans, sans-serif", fontSize = "15px"),
                   columns = list(
                     .selection = colDef(
                       width = 1,
                       style = list(display = "none"),
                       footerStyle = list(marginLeft="-15px")
                       
                     ),
                     Secteur = colDef(name="",width = 210,footer = "Total",
                                      
                                      style = list(background="#81a47b",color="#ffffff",fontFamily="system-ui",fontWeight="500")),
                     `Nbre de Projets` = colDef(width = 140,align = "center",
                                                footer = sum(equip_secteur_reactive()$`Nbre de Projets`),
                                                headerStyle = list(
                                                  background = "#2E81B0",
                                                  color="#ffffff",  
                                                  borderLeft="0px"
                                                )
                     ),
                     Acheves=colDef(name="Achevés",width = 80,
                                    format=colFormat(percent = TRUE,digits = 0),
                                    
                                    footer = sprintf("%.0f%%",100*sum(equip_secteur_reactive()$Acheves)/sum(equip_secteur_reactive()$`Nbre de Projets`))
                                    
                     ),
                     Geles=colDef(name="Gelés",width = 60,
                                  format=colFormat(percent = TRUE,digits = 0),
                                  footer = sprintf("%.0f%%",100*sum(equip_secteur_reactive()$Geles)/sum(equip_secteur_reactive()$`Nbre de Projets`))
                                  
                     ),
                     `En Cours`=colDef(width = 90,
                                       format=colFormat(percent = TRUE,digits = 0),
                                       footer = sprintf("%.0f%%",100*sum(equip_secteur_reactive()$`En Cours`)/sum(equip_secteur_reactive()$`Nbre de Projets`))
                                       
                     ),
                     `Non Lances`=colDef(name="Non Lancés",width=110,
                                         format=colFormat(percent = TRUE,digits = 0),
                                         footer = sprintf("%.0f%%",100*sum(equip_secteur_reactive()$`Non Lances`)/sum(equip_secteur_reactive()$`Nbre de Projets`))
                                         
                     ),
                     `Dont NIR`=colDef(width=90,
                                       format=colFormat(percent = TRUE,digits = 0),
                                       footer = sprintf("%.0f%%",100*sum(equip_secteur_reactive()$`Dont NIR`)/sum(equip_secteur_reactive()$`Nbre de Projets`))
                                       
                     ),
                     `Taux`=colDef(align = "center",cell = function(value, index) {
                       sparkline(c(equip_secteur_reactive()$Acheves[[index]],equip_secteur_reactive()$`En Cours`[[index]],equip_secteur_reactive()$`Non Lances`[[index]],equip_secteur_reactive()$`Dont NIR`[[index]],equip_secteur_reactive()$Geles[[index]])
                                 ,type="pie",height=50,sliceColors=c("#5F9E4F","rgb(239, 192, 0)","rgb(231, 25, 25)","#A9A9A9","#101010"),
                                 tooltipFormat=HTML('<p id="adsq" style="background-color:#fff;font-size:15px;font-weight:800;color: {{color}}">&#9679; {{offset:names}}:  {{value}} ({{percent.1}}%);</p>'),
                                 tooltipValueLookups=list(names=list(c("Acheves"),c("En Cours"),c("Non Lances"),c("Dont NIR"),c("Geles"))
                                                          #HTML("0: 'Automotive',1: 'Locomotive',2: 'Unmotivated'")
                                 ),
                                 numberDigitGroupSep=" " ,
                                 tooltipFormat=list(shared=TRUE)
                                 
                       )
                     },
                     footer = function(value) {
                       sparkline(c(sum(equip_secteur_reactive()$Acheves),sum(equip_secteur_reactive()$`En Cours`),sum(equip_secteur_reactive()$`Non Lances`),sum(equip_secteur_reactive()$`Dont NIR`),sum(equip_secteur_reactive()$Geles))
                                 ,type="pie",height=50,sliceColors=c("#5F9E4F","rgb(239, 192, 0)","rgb(231, 25, 25)","#A9A9A9","#101010"),
                                 tooltipFormat=HTML('<p id="adsq" style="background-color:#fff;font-size:15px;font-weight:800;color: {{color}}">&#9679; {{offset:names}}:  {{value}} ({{percent.1}}%);</p>'),
                                 tooltipValueLookups=list(names=list(c("Acheves"),c("En Cours"),c("Non Lances"),c("Dont NIR"),c("Geles"))
                                                          #HTML("0: 'Automotive',1: 'Locomotive',2: 'Unmotivated'")
                                 ),
                                 numberDigitGroupSep=" " ,
                                 tooltipFormat=list(shared=TRUE)
                                 
                       )
                     })
                     
                     
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
                       fontSize="15px",
                       marginLeft="-3px"
                     )
                     
                   ),
                   
                   theme = reactableTheme(
                     rowSelectedStyle = list(backgroundColor = "#81a47b", boxShadow = "inset 2px 0 0 0 #ffa62d")
                   )
                   
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
                `Nbre de Projets`=colDef(footer=sum(data_equip_reactive()$`Nbre de Projets`),width=130),
                Acheves=colDef(name="Achevés",footer=sum(data_equip_reactive()$Acheves),width=95),
                `En Cours`=colDef(footer=sum(data_equip_reactive()$`En Cours`),width=95),
                `Non Lances`=colDef(name="Non Lancés",footer=sum(data_equip_reactive()$`Non Lances`),width=100),
                `Dont NIR`=colDef(footer=sum(data_equip_reactive()$`Dont NIR`),width=70),
                `Geles`=colDef(name="Gelés ",footer=sum(data_equip_reactive()$`Geles`),width=70)
                
                
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
      addAwesomeMarkers(lat=datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>%
                          select(latitude) %>% .$latitude,
                        
                        lng=datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>%
                          select(longitude) %>% .$longitude
                          
                          #datamc$latitude,lng=datamc$longitude
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
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(Identification) %>% .$Identification,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(wilaya_matricule) %>% .$wilaya_matricule,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(Localisation) %>% .$Localisation,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(Statut) %>% .$Statut,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(Annee_entree) %>% .$Annee_entree,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(`Capacité nominale installée`) %>% .$`Capacité nominale installée`,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(`unité`) %>% .$`unité`,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(`Production effective`) %>% .$`Production effective`,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(`Produits fabriqués`) %>% .$`Produits fabriqués`,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(TEL) %>% .$TEL,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(FAX) %>% .$FAX,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(`Site web`) %>% .$`Site web`,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(Email) %>% .$Email,
                                  datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(observations) %>% .$observations
                                  
                                  
                                  
                                  
                                  
                                  
                          ) %>% lapply(htmltools::HTML)
      )
  })
  
  output$leaflet_ville<-renderLeaflet({  ## change providers[[6]] to providers[[55]]
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
      ) %>% 
      addAwesomeMarkers(lat=data_ville$cor_lat[7],lng=data_ville$cor_long[7],label=data_ville$Nom[7],
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
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LSP/LPA") %>% summarise(sum(Livraison))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
  })
  
  
  output$val_prevision_gauge3<-renderText({
    paste(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LSP/LPA") %>% summarise(sum(Prevision))*1,trim=TRUE,digits=3,big.mark=" ",scientific = FALSE))
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
  
  reactive_arrete_fichewilaya<-reactive({
    paste0(substr(input$select_arretee_fichewilaya,14,23))
  })
  
  # observeEvent(input$select_arretee_fichewilaya,{
  #   `if`(reactive_arrete_fichewilaya()!='2021-03-31',
  #        js$opac1010('0'),
  #        js$opac1010('1')
  #   )
  #   
  #   `if`(reactive_arrete_fichewilaya()!='2021-03-31',
  #        js$opac1111('0'),
  #        js$opac1111('1')
  #   )
  #   
  #  
  #  
  #})
  
  observe({
    `if`(length(input$select_segment_gauge) %in% c(0,2,3,4,5),
         js$opac1('1'),
         js$opac1('0.13')
    )
    
    `if`(length(which(input$select_segment_gauge=="LPL"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
         js$opac2('1'),
         js$opac2('0.13')
    )
    
    
    `if`(length(which(input$select_segment_gauge=="LSP/LPA"))!=0 | length(input$select_segment_gauge) %in% c(0,5),
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
    
    
    # `if`(as.numeric(selected20_fichewilaya())==16,
    #      js$opac77('none'),
    #      js$opac77('block')
    # )
    
    `if`(input$radio_choose_line1=="TOL",
         js$opac88('none'),
         js$opac88('block')
    )
    
    js$opac99('-32px')
    

    
    
  })
  
  output$toutsegments_gauge<-renderText({
    `if`(length(input$select_segment_gauge) %in% c(0,1,5),
         print("Tous les Segments"),
         paste(input$select_segment_gauge,collapse="+")
    )
  })
  
  
  output$wilayaselectgauge1<-renderText({
    `if`(length(selected20())==48,paste("Toutes les Wilayas"),sitphy$Wilaya_matricule[selected20()])
  })
  
  output$wilayaselectgauge2<-renderText({
    `if`(length(selected20())==48,paste("Toutes les Wilayas"),sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge3<-renderText({
    `if`(length(selected20())==48,paste("Toutes les Wilayas"),sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge4<-renderText({
    `if`(length(selected20())==48,paste("Toutes les Wilayas"),sitphy$Wilaya_matricule[selected20()])
  })
  
  
  output$wilayaselectgauge5<-renderText({
    `if`(length(selected20())==48,paste("Toutes les Wilayas"),sitphy$Wilaya_matricule[selected20()])
  })
  
  output$wilayaselectgauge6<-renderText({
    `if`(length(selected20())==48,paste("Toutes les Wilayas"),sitphy$Wilaya_matricule[selected20()])
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
        data=as.numeric(format(sitphy %>% filter(Arretee==substr(input$select_arretee_sitphy,14,23),id %in% selected20(),`Type de logements`=="LSP/LPA") %>% summarise(sum(Livraison)/sum(Prevision))*100,scientific = FALSE,digits = 0)),
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
    `if`(length(input$select_segment_gauge)==0,c("Rural","LPL","Location-Vente","LSP/LPA","LPP"),input$select_segment_gauge)
  })
  
  
  select_segment_title=reactive({
    `if`(length(input$select_segment_gauge) %in% c(0,5),"( Tous les Segments )",input$select_segment_gauge)
  })
  
  selected2 <- reactive(getReactableState("table2", "selected"))
  
  selected2_fichewilaya <- reactive(getReactableState("table2_fichewilaya", "selected"))
  
  selected2_fichewilaya_mr <- reactive(getReactableState("table2_fichewilaya_mr", "selected"))
  
  
  selected20 <- reactive(
    `if`(length(getReactableState("table2", "selected"))==0,1:48,getReactableState("table2", "selected"))
  )
  
  
  selected20_fichewilaya2 <- reactive({
    #`if`(length(getReactableState("table2_fichewilaya", "selected"))==0,1:48,getReactableState("table2_fichewilaya", "selected"))
    #`if`(length(getReactableState("table2_fichewilaya", "selected"))==0,c(1:51),getReactableState("table2_fichewilaya", "selected"))
    `if`(length(getReactableState("table2_fichewilaya", "selected"))==0,c(1:61),getReactableState("table2_fichewilaya", "selected"))
    
  })
  
  
  
  
  selected20_fichewilaya <- reactive({
    #`if`(length(getReactableState("table2_fichewilaya", "selected"))==0,1:48,getReactableState("table2_fichewilaya", "selected"))
    #`if`(length(getReactableState("table2_fichewilaya", "selected"))==0,c(1:51),getReactableState("table2_fichewilaya", "selected"))
    #`if`(length(getReactableState("table2_fichewilaya", "selected"))==0,c(1:61),getReactableState("table2_fichewilaya", "selected"))
    
  `if`( substr(input$select_arretee_fichewilaya,14,23)!=c("2021-03-31","2020-12-31","2020-09-30","2020-06-30","2020-03-31"),
    `if`(length(getReactableState("table2_fichewilaya", "selected"))==0,
         c(1:61),
         getReactableState("table2_fichewilaya", "selected")
         ),
    
    `if`(length(getReactableState("table2_fichewilaya", "selected"))==0,c(1:61),
         `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==49,1,
              `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==50,1,
                   `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==51,7,
                        `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==52,8,
                             `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==53,11,
                                  `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==54,11,
                                       `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==55,30,
                                            `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==56,33,
                                                 `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==57,39,
                                                      `if`(as.numeric(getReactableState("table2_fichewilaya", "selected"))==58,47,
                                                           getReactableState("table2_fichewilaya", "selected"))
                                                      ))))))))))
  )
    })
  
  
  
  
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
    reactable(data.frame(Wilaya=unique(moyens_realisation2$Wilaya)),defaultPageSize = 48,striped = TRUE,
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
    #reactable(data.frame(Wilaya=unique(data_fiche_wilaya$Wilaya)),defaultPageSize = 48,striped = TRUE,
    #reactable(data.frame(Wilaya=unique(data_fiche_wilaya$Wilaya)),defaultPageSize = 51,striped = TRUE,
    reactable(data.frame(Wilaya=unique(data_fiche_wilaya$Wilaya)),defaultPageSize = 61,striped = TRUE,
                        
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
              width="64%",
              
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
                rowSelectedStyle = list(backgroundColor = "#9fc693", boxShadow = "inset 2px 0 0 0 #ffa62d"),
                style = list(fontSize = "15px"),
                
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
         print("Toutes les Wilayas"),
         print(zones$Wilaya[selected()]))
  })
  
  
  output$wilayaselect2<-renderText({
    `if`(length(selected())==48,
         print("Toutes les Wilayas"),
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
  #     filter(type_de_logement %in% c( "LPL","Rural","LPP","LSP/LPA","Location-Vente"))
  # })
  # 
  # estimation_tolpopparc0=reactive({
  #   read_excel(paste0(getwd(),"/Estimation_Population_TOL_Parc_par_Wilaya.xlsx"))
  # })
  # 
  output$periode<-renderText({
    `if`(min(input$annees)!=max(input$annees),paste0("Période : ",min(input$annees),"-",max(input$annees)),paste0("Année : ",min(input$annees)) )
  })
  
  
  output$periode2<-renderText({
    `if`(input$radio_choose_leaflet %in% c("Livraisons de Logements","Lancements de Logements") & min(input$annees)!=max(input$annees),paste0("Période : ",min(input$annees),"-",max(input$annees)),paste0("Année : ",max(input$annees))
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
               #columns = data.frame(title=rep("",ncol(sit_fin)-2) )
               #"Wilaya","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat","Notification","Inscription","Reliquat")
               #                                                       ,width=c(200,200,200,200,200,200,200,200,200,200,200,200))
               editable = FALSE
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
      data=zones00[,c(2,13:ncol(zones00))],
      editable=FALSE,showToolbar = TRUE
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
         `if`(length(segments_reactive())==6,paste0("Tous les Segments"),paste(segments_reactive(),collapse = " + "))
    )
  })
  
  # output$verification_leaflet<-renderText({
  #   paste(input$distPlot2_shape_click)
  # })
  
  
  
  
  output$titre_serie1_hchart1<-renderText({
    `if`(input$radio_choose_line1=="TOL",paste0(""),
         `if`(input$Id027==FALSE & max(input$annees)!=min(input$annees),paste0(segments_reactive(),collapse = "+"),paste0(""))
    )
  })
  
  output$titre_serie2<-renderText({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$wilayas)>2,paste0("Pour les ",length(input$wilayas)," ","Wilayas sélectionnées"),paste(input$wilayas,collapse = " + ")))
  })
  
  
  output$titre_serie<-renderText({
    `if`(length(input$wilayas) %in% c(0,48),paste0("Toutes les Wilayas"),`if`(length(input$wilayas)>2,paste0("Pour les ",length(input$wilayas)," ","Wilayas sélectionnées"),paste(input$wilayas,collapse = "+")))
  })
  
  #output$tdaa3<-renderTable({
  #  livraison_wilayas
  #})
  
  output$dernier_an<-renderText({                    #Livraisons
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'année ",min(input$annees)),paste0("par rapport à l'année ",min(input$annees)-1))
  })
  
  
  
  
  output$dernier_an_lanc<-renderText({                    #Lancement
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'année ",min(input$annees)),paste0("par rapport à l'année ",min(input$annees)-1))
  })
  
  
  output$dernier_an2<-renderText({                       #Parc logement
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'année ",min(input$annees)),paste0("par rapport à l'année ",min(input$annees)-1))
  })
  
  
  output$dernier_an3<-renderText({        # POPULATION
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'année ",min(input$annees)),paste0("par rapport à l'année ",min(input$annees)-1))
  })
  
  
  output$dernier_an4<-renderText({                     #TOL
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'année ",min(input$annees)),paste0("par rapport à l'année ",min(input$annees)-1))
  })
  
  output$dernier_an8<-renderText({                     #densite
    `if`(min(input$annees)!=max(input$annees),paste0("depuis l'année ",min(input$annees)),paste0("par rapport à l'année ",min(input$annees)-1))
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
    
    paste0("Densité en ",max(input$annees))
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
      datamc %>% filter(Arretee==substr('Arretee le : 2018-12-31',14,23)) %>% select(wilaya_matricule) %>% unique() %>% .$wilaya_matricule
      #unique(datamc$wilaya_matricule)
    } else{
      input$wilayas_mc
    }  
  })
  
  
  filiere_mc_reactive=reactive({
    if(length(input$filiere_mcmc)==0) {
      datamc %>% filter(Arretee==substr(input$select_arretee_datamc,14,23)) %>% select(Filiere) %>% unique() %>% .$Filiere
      #unique(datamc$Filiere)
    } else{
      input$filiere_mcmc
    }  
  })
  
  
  statut_mc_reactive=reactive({
    if(length(input$statut2_mc)==0) {
      datamc %>% filter(Arretee==substr(input$select_arretee_datamc,14,23)) %>% select(Statut2) %>% unique() %>% .$Statut2
      
      #unique(datamc$Statut2)
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
      arrange(c(6,4,1,5,3,2))%>%
      mutate(pr=round(100*value/sum(value),2),value2=format(value, big.mark=" ", trim=TRUE)) %>% arrange(c(2,3,4,5,6,1))
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
      arrange(c(6,4,1,5,3,2))%>%
      mutate(pr=round(100*value/sum(value),2),value2=format(value, big.mark=" ", trim=TRUE)) %>% arrange(c(2,3,4,5,6,1))
      
    
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
         c(0.9333,30.6167,6),
         `if`(c(datamc %>%
                  filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
                         Filiere %in% filiere_mc_reactive(),
                         Statut2 %in% statut_mc_reactive()
                  ) %>% 
                  filter(Identification==input$search[length(input$search)]) %>%
                  select(longitude,latitude) %>%
                  anyNA())==TRUE,
              
              c(1.3333,30.6167,6),
              
              datamc %>%
                filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),
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
      addProviderTiles(providers[[6]], group = providers[[6]],
                       options = providerTileOptions(minZoom = 6)
                       ) %>%
      addProviderTiles(providers[[1]], group = providers[[1]],
                       options = providerTileOptions(minZoom = 6)
                       
                       ) %>%
      addProviderTiles(providers[[57]],group="Satellite",
                       options = providerTileOptions(minZoom = 6)
                       
                       ) %>% 
      addLayersControl(
        baseGroups = c(providers[[6]], providers[[1]], "Satellite")
      ) %>% 
      setView(lng=setview_mc()[1],lat=setview_mc()[2],zoom=setview_mc()[3]) %>% 
      #setView(lng=1.3333,lat=30.6167,zoom=6) %>% 
      addAwesomeMarkers(lat=datamc_maps_reactive()$latitude,lng=datamc_maps_reactive()$longitude,
                        labelOptions=labelOptions(direction = "bottom",style="width:0.05"),
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
            <td style="width: 0.592437;">Filiere</td>
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
                                  datamc_maps_reactive()$Filiere,
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
  
  # 
  output$distPlot2_agrementmaps_ing<-renderLeaflet({
      mapdz %>% 
        clearControls()%>%
        addLegend(
          position = "topright",
          #title=HTML("Entreprises <br/> (Cat 1-4)"),
          title=`if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers reçus_PI",HTML("Nbre dossiers<br/>reçus"),
                     `if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers éxaminés par la Commision",HTML("Nbre dossiers éxaminés<br/> par la Commision"),
                          `if`(input$radio_choose_leaflet_agrement_ing=="Nbre d'agréments établis",HTML("Nbre d'agréments <br/>établis"),
                                    HTML("Nbre demande d'inscription<br/>au TNPI")
                               )
                          )),
          
          
          # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
          #            `if`(inpr== c("Lancements de Logements"),"Lancements",
          #                 paste0(inpr))),
          pal=`if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers reçus_PI",colorBin("BrBG",bins = 6,
                                                                                   moyens_realisation2 %>%
                                                                                     filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                     arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                     
                                                                                     select(`Nbre dossiers reçus_PI`) %>% .$`Nbre dossiers reçus_PI`
                                                                                   
                                                                                   
          ),
          `if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers éxaminés par la Commision",colorBin("BrBG",bins = 6,
                                                                                   moyens_realisation2 %>%
                                                                                     filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                     arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                     
                                                                                     select(`Nbre dossiers éxaminés par la Commision`) %>% .$`Nbre dossiers éxaminés par la Commision`                                                                                                           
          ),
          `if`(input$radio_choose_leaflet_agrement_ing=="Nbre d'agréments établis",colorBin("BrBG",bins = 10,
                                                                                                    moyens_realisation2 %>%
                                                                                                      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                      
                                                                                                      select(`Nbre d'agréments établis`) %>% .$`Nbre d'agréments établis`                                                                                                           
                                                                                                    
          ),
          colorBin("BrBG",bins = 10,
                   moyens_realisation2 %>%
                     filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                     arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                     
                     select(`Nbre demande d'inscription au TNPI`) %>% .$`Nbre demande d'inscription au TNPI`                                                                                                           
                   
          )
          )
          )

          ),
          opacity = 1,
          values=`if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers reçus_PI",
                      moyens_realisation2 %>%
                        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                        
                        select(`Nbre dossiers reçus_PI`) %>% .$`Nbre dossiers reçus_PI`
                      
                      ,
                      `if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers éxaminés par la Commision",
                           moyens_realisation2 %>%
                             filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                             arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                             
                             select(`Nbre dossiers éxaminés par la Commision`) %>% .$`Nbre dossiers éxaminés par la Commision`                                                                                                           
                           
                           ,
                           `if`(input$radio_choose_leaflet_agrement_ing=="Nbre d'agréments établis",
                                
                                moyens_realisation2 %>%
                                  filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                  arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                  
                                  select(`Nbre d'agréments établis`) %>% .$`Nbre d'agréments établis`                                                                                                           
                                
                                ,
                                   moyens_realisation2 %>%
                                       filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                       
                                       select(`Nbre demande d'inscription au TNPI`) %>% .$`Nbre demande d'inscription au TNPI`                                                                                                           
                                     
                                     
                                )))
          
          
        ) %>% 
        addPolygons(weight=1,
                    fillColor = `if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers reçus_PI",colorBin("BrBG",bins = 6,
                                                                                                     moyens_realisation2 %>%
                                                                                                       filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                       
                                                                                                       select(`Nbre dossiers reçus_PI`) %>% .$`Nbre dossiers reçus_PI`
                                                                                                     
                                                                                                     
                    )(   moyens_realisation2 %>%
                           filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                           arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                           
                           select(`Nbre dossiers reçus_PI`) %>% .$`Nbre dossiers reçus_PI`
                         
                    ),
                    `if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers éxaminés par la Commision",colorBin("BrBG",bins = 6,
                                                                                             moyens_realisation2 %>%
                                                                                               filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                               arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                               
                                                                                               select(`Nbre dossiers éxaminés par la Commision`) %>% .$`Nbre dossiers éxaminés par la Commision`                                                                                                           
                                                                                             
                    
                                                                                             )(    
                                                                                               moyens_realisation2 %>%
                                                                                                 filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                 arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                 
                                                                                                 select(`Nbre dossiers éxaminés par la Commision`) %>% .$`Nbre dossiers éxaminés par la Commision`                                                                                                           
                                                                                               
                    ),
                    `if`(input$radio_choose_leaflet_agrement_ing=="Nbre d'agréments établis",colorBin("BrBG",bins = 10,
                                                                                                              moyens_realisation2 %>%
                                                                                                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                                
                                                                                                                select(`Nbre d'agréments établis`) %>% .$`Nbre d'agréments établis`     
                                                                                                      
                                                                                                              
                    )(    
                      moyens_realisation2 %>%
                        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                        
                        select(`Nbre d'agréments établis`) %>% .$`Nbre d'agréments établis`     
                      
                    ),
                    colorBin("BrBG",bins = 10,
                             moyens_realisation2 %>%
                               filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                               arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                               
                               select(`Nbre demande d'inscription au TNPI`) %>% .$`Nbre demande d'inscription au TNPI`                                                                                                           
                             
                    )(     moyens_realisation2 %>%
                             filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                             arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                             
                             select(`Nbre demande d'inscription au TNPI`) %>% .$`Nbre demande d'inscription au TNPI`                                                                                                           
                    )
                    
                    
                    )))
                    
                    
                    ,color ="black",
                    label =
                      sprintf(paste0('<strong style="font-size:18px;">%s</strong><br/>
<table style="font-size:15px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers reçus_PI",'font-weight:bold;',''),'">Nbre dossiers reçus_PI :</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers reçus_PI",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers éxaminés par la Commision",'font-weight:bold;',''),'">Nbre dossiers éxaminés par la Commision :</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre dossiers éxaminés par la Commision",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre d'agréments établis",'font-weight:bold;',''),'">Nbre d agréments établis :</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre d'agréments établis",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre demande d'inscription au TNPI",'font-weight:bold;',''),'">Nbre demande d inscription au TNPI:</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_ing=="Nbre demande d'inscription au TNPI",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
       </tbody>
       </table>')  ,
                              
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(Wilaya) %>% .$Wilaya
                              ,
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`Nbre dossiers reçus_PI`) %>% .$`Nbre dossiers reçus_PI`
                              ,
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`Nbre dossiers éxaminés par la Commision`) %>% .$`Nbre dossiers éxaminés par la Commision`
                              ,
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`Nbre d'agréments établis`) %>% .$`Nbre d'agréments établis`
                              ,
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`Nbre demande d'inscription au TNPI`) %>% .$`Nbre demande d'inscription au TNPI`                          
                              
                              
                              
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
    ######################
    
  #############
  ###################
  ######################
  
  output$distPlot2_agrementmaps_ai<-renderLeaflet({
    mapdz %>%
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        title=HTML("Nombre<br/>de dossiers reçus"),


        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        pal=colorBin("BrBG",bins = 6,
                       moyens_realisation2 %>%
                       filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                       arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                     
                       select(`Nbre dossiers reçus_AI`) %>% .$`Nbre dossiers reçus_AI`
                       ),

        opacity = 1,
        values=moyens_realisation2 %>%
          filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
          
          select(`Nbre dossiers reçus_AI`) %>% .$`Nbre dossiers reçus_AI`
      ) %>%
      addPolygons(weight=1,
                  fillColor =colorBin("BrBG",bins = 6,moyens_realisation2 %>%
                                        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                        
                                        select(`Nbre dossiers reçus_AI`) %>% .$`Nbre dossiers reçus_AI`
                                      )(moyens_realisation2 %>%
                                          filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                          
                                          select(`Nbre dossiers reçus_AI`) %>% .$`Nbre dossiers reçus_AI`),



                  color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:16px;">%s</strong><br/>
<table style="font-size:14px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;padding-bottom:5px;">Nbre dossiers reçus</td>
            <td style="width: 0.263361;padding-bottom:5px;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Depots (A.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Depots (A.B.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Depots (A.I + A.B.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;">Depots (C.I)<br></td>
            <td style="width: 0.263361;padding-bottom:8px;">%s</td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Examinés (A.I)<br></td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;;">Examinés (A.B.I)<br></td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Examinés (A.I + A.B.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;">Examinés (C.I)</td>
            <td style="width: 0.263361;padding-bottom:8px;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Avis Favorables (A.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Avis Favorables (A.B.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Avis Favorables (A.I + A.B.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;">Avis Favorables (C.I)</td>
            <td style="width: 0.263361;">%s<br></td>
        </tr>
        
    </tbody>
</table>')  ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(Wilaya) %>% .$Wilaya,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Nbre dossiers reçus_AI`) %>% .$`Nbre dossiers reçus_AI`,
                            
                            
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Depot_A.I`) %>% .$`Depot_A.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Depot_A.B.I`) %>% .$`Depot_A.B.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Depot_A.I + A.B.I`) %>% .$`Depot_A.I + A.B.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Depot_C.I`) %>% .$`Depot_C.I`,
                            
                            
                            
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Examine_A.I`) %>% .$`Examine_A.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Examine_A.B.I`) %>% .$`Examine_A.B.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Examine_A.I + A.B.I`) %>% .$`Examine_A.I + A.B.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Examine_C.I`) %>% .$`Examine_C.I`,
                            
                            
                            
                            
                            
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              select(`Avisfavorable_A.I`) %>% .$`Avisfavorable_A.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              select(`Avisfavorable_A.B.I`) %>% .$`Avisfavorable_A.B.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              select(`Avisfavorable_A.I + A.B.I`) %>% .$`Avisfavorable_A.I + A.B.I`,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              select(`Avisfavorable_C.I`) %>% .$`Avisfavorable_C.I`


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

  
  
  
  
  
  
  
  ##################
  #############
  ###########
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######################################### debut maps promoteur
  # choices = c("Agents Immobiliers",
  #             "ADM de Biens Immobiliers",
  #             "Agents Immobiliers et ABI",
  #             "Courtiers"
  
  
  
  output$distPlot2_agrementmaps_agence<-renderLeaflet({
    mapdz %>% 
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        title=`if`(input$radio_choose_leaflet_agrement_agence=="Communes",HTML("Communes"),
                   `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de lots",HTML("Nbre de lots"),
                        `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires de Lots",HTML("Nbre de bénéficiaires<br/>de Lots"),
                             `if`(input$radio_choose_leaflet_agrement_agence=="Nbre d'aides notifiées",HTML("Nbre d'aides<br/>notifiées"),
                                  
                             HTML("Nbre de bénéficiaires<br/> d'aides")
                        )
                   ))),
        
        
        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        pal=`if`(input$radio_choose_leaflet_agrement_agence=="Communes",colorBin("BrBG",bins = 6,
                                                                                               moyens_realisation2 %>%
                                                                                                 filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                 arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                 
                                                                                                 select(Communes) %>% .$Communes
                                                                                               
                                                                                               
        ),
        `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de lots",colorBin("BrBG",bins = 6,
                                                                                                  moyens_realisation2 %>%
                                                                                                    filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                    arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                    
                                                                                                    select(`Nbre de lots`) %>% .$`Nbre de lots`                                                                                                           
        ),
        `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires de Lots",colorBin("BrBG",bins = 10,
                                                                                            moyens_realisation2 %>%
                                                                                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                              
                                                                                              select(`Nbre de bénéficiaires de Lots`) %>% .$`Nbre de bénéficiaires de Lots`                                                                                                           
                                                                                            
        ),
        `if`(input$radio_choose_leaflet_agrement_agence=="Nbre d'aides notifiées",colorBin("BrBG",bins = 10,
                                                                                            moyens_realisation2 %>%
                                                                                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                              
                                                                                              select(`Nbre d'aides notifiées`) %>% .$`Nbre d'aides notifiées`                                                                                                           
                                                                                            
        ),
        colorBin("BrBG",bins = 10,
                 moyens_realisation2 %>%
                   filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                   
                   select(`Nbre de bénéficiaires d'aides`) %>% .$`Nbre de bénéficiaires d'aides`                                                                                                           
                 
        )
        )
        )
        )
        ),
        opacity = 1,
        values=`if`(input$radio_choose_leaflet_agrement_agence=="Communes",
                    moyens_realisation2 %>%
                      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      
                      select(`Communes`) %>% .$`Communes`
                    
                    ,
                    `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de lots",
                         moyens_realisation2 %>%
                           filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                           arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                           
                           select(`Nbre de lots`) %>% .$`Nbre de lots`                                                                                                           
                         
                         ,
                         `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires de Lots",
                              
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`Nbre de bénéficiaires de Lots`) %>% .$`Nbre de bénéficiaires de Lots`                                                                                                           
                              
                              ,
                              
                              `if`(input$radio_choose_leaflet_agrement_agence=="Nbre d'aides notifiées",
                                   
                                   moyens_realisation2 %>%
                                     filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                     arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                     
                                     select(`Nbre d'aides notifiées`) %>% .$`Nbre d'aides notifiées`                                                                                                           
                                   
                                   ,    
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`Nbre de bénéficiaires d'aides`) %>% .$`Nbre de bénéficiaires d'aides`                                                                                                           
                              
                              
                         ))))
        
        
      ) %>% 
      addPolygons(weight=1,
                  fillColor = `if`(input$radio_choose_leaflet_agrement_agence=="Communes",colorBin("BrBG",bins = 6,
                                                                                                                 moyens_realisation2 %>%
                                                                                                                   filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                                   
                                                                                                                   select(Communes) %>% .$Communes
                                                                                                                 
                                                                                                                 
                  )(   moyens_realisation2 %>%
                         filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                         arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                         
                         select(Communes) %>% .$Communes
                  ),
                  `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de lots",colorBin("BrBG",bins = 6,
                                                                                                            moyens_realisation2 %>%
                                                                                                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                              
                                                                                                              select(`Nbre de lots`) %>% .$`Nbre de lots`                                                                                                           
                                                                                                            
                  )(      moyens_realisation2 %>%
                            filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                            arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                            
                            select(`Nbre de lots`) %>% .$`Nbre de lots`                                                                                                           
                  ),
                  `if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires de Lots",colorBin("BrBG",bins = 10,
                                                                                                      moyens_realisation2 %>%
                                                                                                        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                        
                                                                                                        select(`Nbre de bénéficiaires de Lots`) %>% .$`Nbre de bénéficiaires de Lots`                                                                                                           
                                                                                                      
                  )(    moyens_realisation2 %>%
                          filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                          
                          select(`Nbre de bénéficiaires de Lots`) %>% .$`Nbre de bénéficiaires de Lots`                                                                                                           
                  ),
                  `if`(input$radio_choose_leaflet_agrement_agence=="Nbre d'aides notifiées",colorBin("BrBG",bins = 10,
                                                                                                      moyens_realisation2 %>%
                                                                                                        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                        
                                                                                                        select(`Nbre d'aides notifiées`) %>% .$`Nbre d'aides notifiées`                                                                                                           
                                                                                                      
                  )(    moyens_realisation2 %>%
                          filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                          arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                          
                          select(`Nbre d'aides notifiées`) %>% .$`Nbre d'aides notifiées`                                                                                                           
                  ),
                  colorBin("BrBG",bins = 10,
                           moyens_realisation2 %>%
                             filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                             arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                             
                             select(`Nbre de bénéficiaires d'aides`) %>% .$`Nbre de bénéficiaires d'aides`                                                                                                           
                           
                  )(     moyens_realisation2 %>%
                           filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                           arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                           
                           select(`Nbre de bénéficiaires d'aides`) %>% .$`Nbre de bénéficiaires d'aides`                                                                                                           
                  )
                  
                  
                  ))))
                  
                  
                  ,color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:18px;">%s</strong><br/>
<table style="font-size:15px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_agence=="Communes",'font-weight:bold;',''),'">Communes :</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_agence=="Communes",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre de lots",'font-weight:bold;',''),'">Nbre de lots :</td>
            <td style="width: 0.263361;text-align:right;padding-bottom:8px;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre de lots",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires de Lots",'font-weight:bold;',''),'">Nbre de bénéficiaires de Lots :</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires de Lots",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre d'aides notifiées",'font-weight:bold;',''),'">Nbre d aides notifiées:</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre d'aides notifiées",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires d'aides",'font-weight:bold;',''),'">Nbre de bénéficiaires d aides:</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_agence=="Nbre de bénéficiaires d'aides",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        
       </tbody>
       </table>')  ,
                            
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(Wilaya) %>% .$Wilaya
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(Communes) %>% .$Communes
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Nbre de lots`) %>% .$`Nbre de lots`
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Nbre de bénéficiaires de Lots`) %>% .$`Nbre de bénéficiaires de Lots`
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Nbre d'aides notifiées`) %>% .$`Nbre d'aides notifiées`                            
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Nbre de bénéficiaires d'aides`) %>% .$`Nbre de bénéficiaires d'aides`                               
                            
                            
                            
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
  ######################
  
  
  output$distPlot2_agrementmaps_prom<-renderLeaflet({
    mapdz %>% 
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        title=`if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers déposés",HTML("Nombre de dossiers <br/> deposes"),
                   `if`(input$radio_choose_leaflet_agrement_prom=="dont Par l'application E-Certif",HTML("dont Par l'application<br/> E-Certif"),
                        `if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers éxaminés",HTML("Nbre de dossiers <br/> éxaminé"),
                             HTML("dont par l'application <br/>E-Certif")
                        )
                   )),
        
        
        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        pal=`if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers déposés",colorBin("BrBG",bins = 6,
                                                                                               moyens_realisation2 %>%
                                                                                                 filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                 arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                 
                                                                                                 select(`Nbre de dossiers déposés`) %>% .$`Nbre de dossiers déposés`

                                                                                               
                                                                            ),
                 `if`(input$radio_choose_leaflet_agrement_prom=="dont Par l'application E-Certif",colorBin("BrBG",bins = 6,
                                                                                                           moyens_realisation2 %>%
                                                                                                             filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                             arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                             
                                                                                                             select(`dont Par l'application E-Certif`) %>% .$`dont Par l'application E-Certif`                                                                                                           
                                                                                                           ),
                      `if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers éxaminés",colorBin("BrBG",bins = 10,
                                                                                                         moyens_realisation2 %>%
                                                                                                           filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                           arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                           
                                                                                                           select(`Nbre de dossiers éxaminés`) %>% .$`Nbre de dossiers éxaminés`                                                                                                           
                                                                                                         
                                                                                                         ),
                           colorBin("BrBG",bins = 10,
                                    moyens_realisation2 %>%
                                      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                      
                                      select(`dont par l'application E-Certif`) %>% .$`dont par l'application E-Certif`                                                                                                           
                                    
                                    )
                      )
                 )
        ),
        opacity = 1,
        values=`if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers déposés",
                    moyens_realisation2 %>%
                      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                      
                      select(`Nbre de dossiers déposés`) %>% .$`Nbre de dossiers déposés`
                    
                    ,
                    `if`(input$radio_choose_leaflet_agrement_prom=="dont Par l'application E-Certif",
                         moyens_realisation2 %>%
                           filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                           arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                           
                           select(`dont Par l'application E-Certif`) %>% .$`dont Par l'application E-Certif`                                                                                                           
                         
                         ,
                         `if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers éxaminés",
                              
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`Nbre de dossiers éxaminés`) %>% .$`Nbre de dossiers éxaminés`                                                                                                           
                              
                              ,
                              moyens_realisation2 %>%
                                filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                
                                select(`dont par l'application E-Certif`) %>% .$`dont par l'application E-Certif`                                                                                                           
                              
                              
                         )))
        
        
      ) %>% 
      addPolygons(weight=1,
                  fillColor = `if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers déposés",colorBin("BrBG",bins = 6,
                                                                                                                 moyens_realisation2 %>%
                                                                                                                   filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                                   arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                                   
                                                                                                                   select(`Nbre de dossiers déposés`) %>% .$`Nbre de dossiers déposés`
                                                                                                                 
                                                                              
                                                                              )(   moyens_realisation2 %>%
                                                                                     filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                     arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                     
                                                                                     select(`Nbre de dossiers déposés`) %>% .$`Nbre de dossiers déposés`
                                                                              ),
                                   `if`(input$radio_choose_leaflet_agrement_prom=="dont Par l'application E-Certif",colorBin("BrBG",bins = 6,
                                                                                                                             moyens_realisation2 %>%
                                                                                                                               filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                                               arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                                               
                                                                                                                               select(`dont Par l'application E-Certif`) %>% .$`dont Par l'application E-Certif`                                                                                                           
                                                                                                                             
                                                                                                    )(      moyens_realisation2 %>%
                                                                                                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                              
                                                                                                              select(`dont Par l'application E-Certif`) %>% .$`dont Par l'application E-Certif`                                                                                                           
                                                                                                    ),
                                        `if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers éxaminés",colorBin("BrBG",bins = 10,
                                                                                                                            moyens_realisation2 %>%
                                                                                                                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                                              
                                                                                                                              select(`Nbre de dossiers éxaminés`) %>% .$`Nbre de dossiers éxaminés`                                                                                                           
                                                                                                                            
                                                                                            )(    moyens_realisation2 %>%
                                                                                                    filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                                                                    arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                                                                    
                                                                                                    select(`Nbre de dossiers éxaminés`) %>% .$`Nbre de dossiers éxaminés`                                                                                                           
                                                                                            ),
                                             colorBin("BrBG",bins = 10,
                                                      moyens_realisation2 %>%
                                                        filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                        arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                        
                                                        select(`dont par l'application E-Certif`) %>% .$`dont par l'application E-Certif`                                                                                                           
                                                      
                                                      )(     moyens_realisation2 %>%
                                                               filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                                                               arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                                                               
                                                               select(`dont par l'application E-Certif`) %>% .$`dont par l'application E-Certif`                                                                                                           
                                                      )
                                             
                                             
                                        )))
                  
                  
                  ,color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:18px;">%s</strong><br/>
<table style="font-size:15px; margin-right: calc(0.59);">
    <tbody>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers déposés",'font-weight:bold;',''),'">Nbre de dossiers déposés :</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers déposés",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;',`if`(input$radio_choose_leaflet_agrement_prom=="dont Par l'application E-Certif",'font-weight:bold;',''),'">dont Par l application E-Certif :</td>
            <td style="width: 0.263361;text-align:right;padding-bottom:8px;',`if`(input$radio_choose_leaflet_agrement_prom=="dont Par l'application E-Certif",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers éxaminés",'font-weight:bold;',''),'">Nbre de dossiers éxaminés :</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_prom=="Nbre de dossiers éxaminés",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        <tr>
            <td style="width: 0.592437;',`if`(input$radio_choose_leaflet_agrement_prom=="dont par l'application E-Certif",'font-weight:bold;',''),'">dont par lapplication E-Certif:</td>
            <td style="width: 0.263361;text-align:right;',`if`(input$radio_choose_leaflet_agrement_prom=="dont par l'application E-Certif",'font-weight:bold;',''),'">  %s<br></td>
        </tr>
        
       </tbody>
       </table>')  ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(Wilaya) %>% .$Wilaya
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Nbre de dossiers déposés`) %>% .$`Nbre de dossiers déposés`
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`dont Par l'application E-Certif`) %>% .$`dont Par l'application E-Certif`
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`Nbre de dossiers éxaminés`) %>% .$`Nbre de dossiers éxaminés`                            
                            ,
                            moyens_realisation2 %>%
                              filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
                              arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22)) %>% 
                              
                              select(`dont par l'application E-Certif`) %>% .$`dont par l'application E-Certif`                               
                            
                            
                            
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
  
  
  ######################################### fin maps promoteur
  output$distPlot2_agrementmaps<-renderLeaflet({
    mapdz %>% 
      clearControls()%>%
      addLegend(
        position = "topright",
        #title=HTML("Entreprises <br/> (Cat 1-4)"),
        title=`if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-9)",HTML("Entreprises <br/> (Cat 1-9)"),
                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-4)",HTML("Entreprises <br/> (Cat 1-4)"),
                        `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 5-9)",HTML("Entreprises <br/> (Cat 5-9)"),
                             `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat1)",HTML("Entreprises <br/> (Cat 1)"),
                                  `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat2)",HTML("Entreprises <br/> (Cat 2)"),
                                       `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat3)",HTML("Entreprises <br/> (Cat 3)"),
                                            `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat4)",HTML("Entreprises <br/> (Cat 4)"),
                                                 `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat5)",HTML("Entreprises <br/> (Cat 5)"),
                                                      `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat6)",HTML("Entreprises <br/> (Cat 6)"),
                                                           `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat7)",HTML("Entreprises <br/> (Cat 7)"),
                                                                `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat8)",HTML("Entreprises <br/> (Cat 8)"),
                                                                     HTML("Entreprises <br/> (Cat 9)")
                                                                )
                                                           )
                                                      )
                                                 )
                                            )
                                       )
                                  )
                             )
                        )
                   )),
        
        
        # title=`if`(inpr== c("Livraisons de Logements"),"Livraisons",
        #            `if`(inpr== c("Lancements de Logements"),"Lancements",
        #                 paste0(inpr))),
        pal=`if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-9)",colorBin("BrBG",bins = 6,mapsag1_reactive()$cata19),
                 `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-4)",colorBin("BrBG",bins = 6,mapsag1_reactive()$cata14),
                      `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 5-9)",colorBin("BrBG",bins = 10,mapsag1_reactive()$cata59),
                           `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat1)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat1),
                                `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat2)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat2),
                                     `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat3)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat3),
                                          `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat4)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat4),
                                               `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat5)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat5),
                                                    `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat6)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat6),
                                                         `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat7)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat7),
                                                              `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat8)",colorBin("BrBG",bins = 6,mapsag1_reactive()$Cat8)
                                                                   ,colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat9)
                                                              )
                                                         )
                                                    )
                                               )
                                          )
                                     )
                                )
                           )))),
        opacity = 1,
        values=`if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-9)",mapsag1_reactive()$cata19,
                    `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-4)",mapsag1_reactive()$cata14,
                         `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 5-9)",mapsag1_reactive()$cata59,
                              `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat1)",mapsag1_reactive()$Cat1,
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat2)",mapsag1_reactive()$Cat2,
                                        `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat3)",mapsag1_reactive()$Cat3,
                                             `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat4)",mapsag1_reactive()$Cat4,
                                                  `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat5)",mapsag1_reactive()$Cat5,
                                                       `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat6)",mapsag1_reactive()$Cat6,
                                                            `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat7)",mapsag1_reactive()$Cat7,
                                                                 `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat8)",mapsag1_reactive()$Cat8,
                                                                      mapsag1_reactive()$Cat9)
                                                            )))))))
                         )))
        
        
      ) %>% 
      addPolygons(weight=1,
                  fillColor = `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-9)",colorBin("BrBG",bins = 6,mapsag1_reactive()$cata19)(mapsag1_reactive()$cata19),
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-4)",colorBin("BrBG",bins = 6,mapsag1_reactive()$cata14)(mapsag1_reactive()$cata14),
                                        `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 5-9)",colorBin("BrBG",bins = 10,mapsag1_reactive()$cata59)(mapsag1_reactive()$cata59),
                                             `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat1)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat1)(mapsag1_reactive()$Cat1),
                                                  `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat2)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat2)(mapsag1_reactive()$Cat2),
                                                       `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat3)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat3)(mapsag1_reactive()$Cat3),
                                                            `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat4)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat4)(mapsag1_reactive()$Cat4),
                                                                 `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat5)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat5)(mapsag1_reactive()$Cat5),
                                                                      `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat6)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat6)(mapsag1_reactive()$Cat6),
                                                                           `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat7)",colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat7)(mapsag1_reactive()$Cat7),
                                                                                `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat8)",colorBin("BrBG",bins = 6,mapsag1_reactive()$Cat8)(mapsag1_reactive()$Cat8),
                                                                                     colorBin("BrBG",bins = 10,mapsag1_reactive()$Cat9)(mapsag1_reactive()$Cat9)
                                                                                )
                                                                           )
                                                                      )
                                                                 )
                                                            )
                                                       )
                                                  )
                                             )
                                             
                                        )))
                  
                  
                  ,color ="black",
                  label =
                    sprintf(paste0('<strong style="font-size:18px;">%s</strong><br/>
<table style="font-size:15px; margin-right: calc(0.59);">
    <tbody>',
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-9)",'                      
        <tr>
            <td style="width: 0.592437;font-weight:bold;">Entreprises (Cat 1-9)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight:bold;">  %s<br></td>
        </tr>',
                                        '
        <tr>
            <td style="width: 0.592437;">Entreprises (Cat 1-9)   :</td>
            <td style="width: 0.263361;text-align:right;">  %s<br></td>
        </tr>
        '
                                   ),
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 1-4)",'
        <tr>
            <td style="width: 0.592437;font-weight:bold;">Entreprises (Cat 1-4)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight:bold;">  %s<br></td>
        </tr>
           ',
                                        '
        <tr>
            <td style="width: 0.592437;">Entreprises (Cat 1-4)   :</td>
            <td style="width: 0.263361;text-align:right;">  %s<br></td>
        </tr>
           '
                                   ),
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat 5-9)",
                                        '
        <tr>
            <td style="width: 0.592437;padding-bottom:8px;font-weight:bold;">Entreprises (Cat 5-9)   :</td>
            <td style="width: 0.263361;text-align:right;padding-bottom:8px;font-weight:bold;">  %s<br></td>
        </tr>'
                                        ,
                                        '<tr>
            <td style="width: 0.592437;padding-bottom:8px;">Entreprises (Cat 5-9)   :</td>
            <td style="width: 0.263361;text-align:right;padding-bottom:8px;">  %s<br></td>
        </tr>
           '),
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat1)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat1)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat2)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat2)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   
                                   
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat3)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat3)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   
                                   
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat4)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat4)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat5)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat5)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat6)",'
        <tr>
            <td style="width: 0.592437font-weight: bold;;">Entreprises (Cat6)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat7)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat7)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat8)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat8)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),
                                   
                                   `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat9)",'
        <tr>
            <td style="width: 0.592437;font-weight: bold;">Entreprises (Cat9)   :</td>
            <td style="width: 0.263361;text-align:right;font-weight: bold;">  %s<br></td>
        </tr>',''),'
       </tbody>
       </table>')  ,
                            mapsag1_reactive()$Wilaya,
                            mapsag1_reactive()$cata19,
                            mapsag1_reactive()$cata14,
                            mapsag1_reactive()$cata59,
                            `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat1)",
                                 mapsag1_reactive()$Cat1,
                                 `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat2)",
                                      mapsag1_reactive()$Cat2,
                                      `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat3)",
                                           mapsag1_reactive()$Cat3,     
                                           `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat4)",
                                                mapsag1_reactive()$Cat4,
                                                `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat5)",
                                                     mapsag1_reactive()$Cat5,
                                                     `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat6)",
                                                          mapsag1_reactive()$Cat6,     
                                                          `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat7)",
                                                               mapsag1_reactive()$Cat7,
                                                               `if`(input$radio_choose_leaflet_agrement=="Entreprise qualifiees (Cat8)",
                                                                    mapsag1_reactive()$Cat8,
                                                                    mapsag1_reactive()$Cat9     
                                                               ))))))))
                            
                            
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
  
  mapsag1_reactive<-reactive({
    moyens_realisation2 %>% 
      filter(Arretee==substr(input$select_arretee_fichewilaya_mr,14,23)) %>%
      select(Wilaya,Cat1,Cat2,Cat3,Cat4,Cat5,Cat6,Cat7,Cat8,Cat9) %>% 
      mutate(cata14=Cat1+Cat2+Cat3+Cat4) %>% 
      mutate(cata59=Cat5+Cat6+Cat7+Cat8+Cat9) %>% 
      mutate(cata19=Cat1+Cat2+Cat3+Cat4+Cat5+Cat6+Cat7+Cat8+Cat9) %>% 
      #rename("Categoris au niveau de Wilaya (1-4)"="cata1") %>%
      arrange(c(41,15,42,35,25,9,26,38,13,14,48,36,6,24,8,11,23,30,16,7,27,4,28,31,33,17,1,20,3,44,2,39,46,19,10,29,47,21,45,37,34,12,32,18,40,5,43,22))
    #  %>%     .$`Categoris au niveau de Wilaya (1-5)`
    
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
      filter(Arretee==substr(input$select_arretee_datamc,14,23),wilaya_matricule %in% wilaya_mc_reactive(),Filiere %in% filiere_mc_reactive(),Statut2 %in% statut_mc_reactive()
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
                  ),layerId = algeria@data$wilayas)
    
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
               if(vaz.length==1 || vaz.length==2 || vaz.length==3 || vaz.length==4 || vaz.length==5){
                if(vaz.includes('LPL')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[1].children[0].style.fontSize='0px';
                  arr[1].style.stroke='#ffffff';
                }
                if(vaz.includes('Rural')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[2].children[0].style.fontSize='0px';
                  arr[2].style.stroke='#ffffff';
                }
                if(vaz.includes('LSP/LPA')==false ) {
                
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[3].children[0].style.fontSize='0px';
                  arr[3].style.stroke='#ffffff';}
                  
                  
                if(vaz.includes('Location-Vente')==false ) {
                  
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[4].children[0].style.fontSize='0px';
                  arr[4].style.stroke='#ffffff';
                }
                if(vaz.includes('LPP')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[5].children[0].style.fontSize='0px';
                  arr[5].style.stroke='#ffffff';
                }
                if(vaz.includes('ACLS')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[0].children[0].style.fontSize='0px';
                  arr[0].style.stroke='#ffffff';
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
               size="190px",
               dataLabels=list(style=list(fontSize= "14px",fontWeight="normal"),format="<p>{point.label}: {point.pr:.1f}%<br />( {point.value2} )</p>"),
               #depth=40,
               allowPointSelect=TRUE,
               point=list(events=list(legendItemClick=JS("function () {
                        return false;
                    }")))
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
             colors=`if`(length(input$segments) %in% c(0,6),
                         #c("#d35400","#2ecc71","#2980b9","#f1c40f","#2c3e50","#800080"),
                         
                         c("#800080","#d35400","#2ecc71","#2980b9","#f1c40f","#2c3e50"),
                         
                         c(
                           `if`("ACLS" %in% input$segments,"#800080","#bababa"),
                           `if`("LPL" %in% input$segments,"#d35400","#bababa"),
                           `if`("Rural" %in% input$segments,"#2ecc71","#bababa"),
                           `if`("LSP/LPA" %in% input$segments,"#2980b9","#bababa"),
                           `if`("Location-Vente" %in% input$segments,"#f1c40f","#bababa"),
                           `if`("LPP" %in% input$segments,"#2c3e50","#bababa")
                           
                         )
                         
                         ),
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
               if(vaz.length==1 || vaz.length==2 || vaz.length==3 || vaz.length==4 || vaz.length==5){
                if(vaz.includes('LPL')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[1].children[0].style.fontSize='0px';
                  arr[1].style.stroke='#ffffff';
                }
                if(vaz.includes('Rural')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[2].children[0].style.fontSize='0px';
                  arr[2].style.stroke='#ffffff';
                }
                if(vaz.includes('LSP/LPA')==false ) {
                
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[3].children[0].style.fontSize='0px';
                  arr[3].style.stroke='#ffffff';}
                  
                  
                if(vaz.includes('Location-Vente')==false ) {
                  
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[4].children[0].style.fontSize='0px';
                  arr[4].style.stroke='#ffffff';
                }
                if(vaz.includes('LPP')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[5].children[0].style.fontSize='0px';
                  arr[5].style.stroke='#ffffff';
                }
                if(vaz.includes('ACLS')==false ) {
                  var dab =document.getElementsByClassName('highcharts-data-label');
                  var arr=document.getElementsByClassName('highcharts-data-label-connector');

                  dab[0].children[0].style.fontSize='0px';
                  arr[0].style.stroke='#ffffff';
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
               size="200px",
               dataLabels=list(style=list(fontSize= "14px",fontWeight="normal"),format="<p>{point.label}: {point.pr:.1f}%<br />( {point.value2} )</p>"),
               #depth=40,
               allowPointSelect=TRUE,
               point=list(events=list(legendItemClick=JS("function () {
                        return false;
                    }")))
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
             colors=`if`(length(input$segments) %in% c(0,6),
                         #c("#d35400","#2ecc71","#2980b9","#f1c40f","#2c3e50","#800080"),
                         c("#800080","#d35400","#2ecc71","#2980b9","#f1c40f","#2c3e50"),
                         
                         c(
                           `if`("ACLS" %in% input$segments,"#800080","#bababa"),
                           `if`("LPL" %in% input$segments,"#d35400","#bababa"),
                           `if`("Rural" %in% input$segments,"#2ecc71","#bababa"),
                           `if`("LSP/LPA" %in% input$segments,"#2980b9","#bababa"),
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
      #filter(type_de_logement %in% c("LSP/LPA","Rural","Location-Vente", "LPP","LPL"))%>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      group_by(Annee,Segment)%>%
      summarise(lanc=sum(Lancement))
  })
  
  
  
  
  daa2=reactive({
    livraison_wilayas%>%
      filter(type_de_logement %in% segments_reactive(),
             annee>= min(input$annees),annee<=max(input$annees),
             waw %in% wilaya_reactive()  ) %>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
      #filter(type_de_logement %in% c("LSP/LPA","Rural","Location-Vente", "LPP","LPL"))%>% #,waw %in% input$wilayas,annee>input$annee[1],annee<input$annee[2])%>%
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
    #data.frame(ns=c("Location-Vente","LPL","LPP","LSP/LPA","Rural","ACLS"),couleur=c("#f1c40f","#d35400","#2c3e50","#2980b9","#2ecc71","#800080"))
    
    data.frame(ns=c("ACLS","Location-Vente","LPL","LPP","LSP/LPA","Rural"),couleur=c("#800080","#f1c40f","#d35400","#2c3e50","#2980b9","#2ecc71"))
    
    
  })
  
  
  line_color1_lanc<-reactive({
    line_color0_lanc() %>% filter(ns %in% unique(daa2_lancement()$Segment)) 
    #%>% mutate(re=c(2,3,4,5,6,1)) %>% arrange(re) %>% select(couleur)
  })
  
  line_color0<-reactive({
    #data.frame(ns=c("Location-Vente","LPL","LPP","LSP/LPA","Rural","ACLS"),couleur=c("#f1c40f","#d35400","#2c3e50","#2980b9","#2ecc71","#800080"))
    
    data.frame(ns=c("ACLS","Location-Vente","LPL","LPP","LSP/LPA","Rural"),couleur=c("#800080","#f1c40f","#d35400","#2c3e50","#2980b9","#2ecc71"))
    
  })
  
  line_color1<-reactive({
    line_color0() %>% filter(ns %in% unique(daa2()$type_de_logement))
    
    #%>% mutate(re=c(2,3,4,5,6,1)) %>% arrange(re) %>% select(couleur)
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
                                 #               `if`("LSP/LPA" %in% input$segments,"#2ecc71"),
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
                                 #               `if`("LSP/LPA" %in% input$segments,"#2ecc71"),
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
  
  observe({
    updateNavbarPage(session,"main_navbar", selected = HTML('
          <span style="opacity:0" class="glyphicon glyphicon-home"></span>
                     '))
    
  })
 
  
}
#runApp(list(ui = ui, server = server),port=8180, launch.browser = TRUE)
shinyApp(ui = ui, server = server)