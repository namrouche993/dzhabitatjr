#setwd("C:/electron-quick-start-win32-ia32/resources/app")
#getwd()

.libPaths(paste0(getwd(),"/R-Portable-Win/library"))
.Library=paste0(getwd(),"/R-Portable-Win/library")

library(shiny)
library(tidyverse)
library(leaflet)
library(shinyjs)
#library(shinyalert)
library(geojsonio)
library(readxl)
library(reactable)
library(shinyBS)
library(leaflet.extras)
library(rmapshaper)


# wilayas58=data.frame(wilaya=c(
#             unique(livraison_wilayas$waw),
#             "49-Timimoun","50-Bordj Badji Mokhtar","51-Ouled Djellal","52-Béni Abbès","53-In Salah",
#             "54-In Guezzam","55-Touggourt","56-Djanet","57-El M'Ghair","58-El Meniaa"
#             ))


algeria580 <- geojsonio::geojson_read("geoData.geojson", what = "sp")
algeria58 <- rmapshaper::ms_simplify(algeria580, keep = 0.05, keep_shapes = TRUE)

wilayas58 <- read_excel("wilayas58.xlsx")

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
  setView(lng = 1.63333 , lat = 28.6167, zoom = 6)%>%
  #clearBounds()%>%
  
  addProviderTiles("OpenStreetMap.BZH",
                   options = leafletOptions(minZoom = 5, maxZoom = 10,dragging = TRUE)) %>%   #or we can use addProviderTiles(one_providers)
  setMapWidgetStyle(list(background= "#ffffff"))


ui <- fluidPage(
  
  tags$head(HTML('<style type="text/css">

#reactable_58wilayas {
padding-left:200px;
}


#sasa {
font-size:22px;
}

')),
  
  useShinyjs(),
  
  actionButton("preview", "Preview"),
  
  leafletOutput("mapsalgerie",height = 950),

  bsModal("modal_eqp", textOutput("sasa"), "preview", size = "large"
          ,reactableOutput("reactable_58wilayas"))
  
)

server <- function(input, output,session) {
   
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
                    #colorBin("Spectral",reverse=TRUE,bins = 10,wilayas58$field)(wilayas58$field)
                    colorBin("Spectral",bins = 10,wilayas58$Region)(wilayas58$Region)
                  
                  
                  
                  ,color ="black",
                  label =
                    sprintf('<strong style="font-size:18px;">%s</strong><br/>',
                            wilayas58$wilaya
                            
                            
                    ) %>% lapply(htmltools::HTML),
                  fillOpacity = 0.99,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "bottom",
                    offset = c(0,20)
                  ),
                  
                  highlight=highlightOptions(
                    weight=5,fillOpacity = 0.7,bringToFront=TRUE,fillColor = "red"
                  ),layerId = wilayas58$wilaya
      )
    
  })
  
}

shinyApp(ui, server)

