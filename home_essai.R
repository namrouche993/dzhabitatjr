library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  fluidRow(

    tags$body(tags$div(id='page',
             tags$div(class='container home-page',
                      tags$div(class='text-zone',
                               tags$h1(class='blast-root',HTML("Ministere de l'Habitat, <br/>de l'Urbanisme et de la Ville")),
                               tags$h2("DZ Habitat / by : namrouche993")
                      )
             )
    )),

#     tags$head(HTML("
# 
# <div id='page'>
# 		<div class='container home-page'>
# 			<div class='text-zone'>
# 				<h1 aria-label='Hi, I'm namrouche993@gmail.com ,data scientist.' class='blast-root'>
# 					Ministere de l'Habitat,
#                    <br>
#                      de l'Urbanisme<br>
#           et de la Ville.
# 				</h1>
# 				<h2>DZ Habitat / by : namrouche993</h2>
# 			</div>
# 		</div>
# 	</div>
# 
#          ")
              
              ),

    tags$head(tags$style(HTML('
  
  <style type="text/css">


 @import url("https://fonts.googleapis.com/css?family=Titillium+Web:300,400,600");
 @import url("https://fonts.googleapis.com/css?family=La+Belle+Aurore");
 @import url("https://fonts.googleapis.com/css?family=Exo:700,800");

#well22 {
 height:400px;
 width:300px;
}

#page {
    box-sizing: border-box;
    padding-top: 60px;
    padding-bottom: 0px;
}

.text-zone {
    transform: translateY(-50%);
    width: 100%;
    background-color: #0b2535;
    font-family: "Titillium Web", sans-serif;
    padding-left: 225px;
}

.text-zone a {
	 font-weight: 300;
}
 .home-page h1 {
    color: #fff;
    font-size: 53px;
    line-height: 48px;
    margin: 0;
    font-family: "Exo", sans-serif;
    font-weight: normal;
}
 .home-page h1::before {
	 font-family: "La Belle Aurore", cursive;
	 color: #10364e;
	 font-size: 18px;
	 position: absolute;
	 margin-top: -35px;
}
 .home-page h1::after {
	 font-family: "La Belle Aurore", cursive;
	 color: #10364e;
	 font-size: 18px;
	 position: absolute;
	 margin-top: 18px;
	 margin-left: 20px;
	 -webkit-animation: myanim2 1s 3s backwards;
	 animation: myanim2 1s 3s backwards;
}
 .home-page h2 {
	 color: #d8be68;
	 margin-top: 9px;
	 font-weight: 400;
	 font-size: 11px;
	 font-family: "Titillium Web", sans-serif;
	 letter-spacing: 3px;
	 -webkit-animation: myanim2 1s 2.5s backwards;
	 animation: myanim2 1s 2.5s backwards;
	 padding-bottom:2px;
}
 .home-page .flat-button {
	 color: #d8be68;
	 font-size: 11px;
	 letter-spacing: 3px;
	 font-family: "Open Sans", sans-serif;
	 text-decoration: none;
	 padding: 6px 10px;
	 border: 1px solid #d8be68;
	 margin-top: 40px;
	 float: left;
	 border-radius: 4px;
	 opacity: 0;
}
 .home-page .flat-button:hover {
	 background: #d8be68;
	 color: #0b2535;
}
 .home-page .blast {
	 opacity: 0;
	 display: inline-block;
	 -webkit-transition: all 0.3s ease-out;
	 transition: all 0.3s ease-out;
}
 .home-page .blast:hover {
	 color: #d8be68;
}
 



  
')
    ))
  )

server <- function(input, output) {
  output$texta5<-renderText({
    paste("aaaaaaaaaaaaaaaaa")
  })
}

shinyApp(ui, server)