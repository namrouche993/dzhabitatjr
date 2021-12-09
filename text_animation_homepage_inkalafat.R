library(shiny)
library(shinyjs)

ui <- fluidPage(
  navbarPage("AZEAZ",
    tabPanel("acceuil",
             fluidRow(
               wellPanel(id="well_homepage",
                         style=
                           'min-height: 20px;
 padding: 19px;
 margin-bottom: -90px;
 border-radius: 4px;
 -webkit-box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
 box-shadow: inset 0 1px 1px rgb(0 0 0 / 5%);
 background-color: #fff;
 border: 0px',
                         includeHTML(paste0(getwd(),'/www/index2.html')),
                         tags$head(includeScript(paste0(getwd(),'/www/script2.js'))),
                         tags$head(includeCSS(paste0(getwd(),'/www/style2.css')))
               )
             )
             #,
             #textOutput('texta5')
             
             ),
    tabPanel("tr",
             textOutput('texta5')
             #,
             #htmlOutput("tree_file")
    )
  )

  
)

server <- function(input, output) {
  addResourcePath("pdf_files_DGCMR", paste0(getwd(),"/pdf_files_DGCMR"))

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
  
  
  output$texta5<-renderText({
    paste('azedsds88888888844444444')
  })
  
}

shinyApp(ui, server)