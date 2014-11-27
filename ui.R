
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(leafletR)

shinyUI(navbarPage("AmuseDBExplorer", id="nav",
  
  tabPanel("Interactive map",
    div(class="outer",
               
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      )
    )       
  ),
  
  tabPanel("Data explorer",
    fluidRow(
           dataTableOutput("df.bioch")
    )
  )
))
