
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(leaflet))

shinyUI(navbarPage("AmuseDBExplorer", id="nav",
  
  tabPanel("Interactive map",
    div(class="outer",
               
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      #map <- leaflet(base.map="mqosm"),
      #includeHTML(map) # browseURL(map)
      #leafletMap("map", width="100%", height="100%",
      #           initialTileLayer = "http//otile4.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpg",
      #           #initialTileLayer = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png',
      #           initialTileLayerAttribution = HTML('© MapQuest, Map Data © OpenStreetMap contributors, ODbL'),
      #           #options=NULL
      #           options=list(
      #              center = c(37.45, -93.85),
      #              zoom = 4,
      #              maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
      #            )
      #)
      htmlOutput("mapp",inline=TRUE)),
      absolutePanel(top = 60, left = "auto", right = 20, bottom = "auto",
                  selectInput("mapPick", "Background Map",c("OpenStreetMap" = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", # works!
                                                            "MapQuestOSM" = "http://oatile3.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpg", # works!
                                                            "MapQuestOpen.Aerial"= "http://oatile3.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg"), # works!
                              selected = c("http://oatile3.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpg")) # default: MapQuestOSM
    )       
  ),
  
#   tabPanel("Bioch data explorer (4 plants)",
#     fluidRow(
#            dataTableOutput("df.bioch.4p")
#     )
#   ),
  
  tabPanel("Mucilage biochemical dataset",
    fluidPage(
      title="Search mucilage dataset",
      sidebarLayout(
        sidebarPanel(
          tags$strong("Filter accessions by AV number"),
          tags$textarea(id="select_av", rows=3, cols=40, "All"),
          conditionalPanel(
            'input.dataset === "raw"',
            #textInput("select_av",label = strong("Filter accessions by AV number"), value="All"),
            selectizeInput("show_mucilbiochcols", label = strong("Mucilage biochemical dataset"), 
                        choices = choices_mucilbiochcols, 
                        selected = choices_mucilbiochcols,
                        multiple = TRUE)
            ),
          conditionalPanel(
            'input.dataset === "mean"',
            selectInput("show_mucilbiochcols", label=strong("Mucilage biochemical dataset"), 
                           choices=choices_mucilbiochcols, 
                           selected=choices_mucilbiochcols,
                           multiple=TRUE)
          )
        ),
        mainPanel(
          tabsetPanel(
            id="dataset",
            tabPanel('raw', dataTableOutput("raw")),
            tabPanel('mean')
            )
          ),
        position = 'right'
        )
      )
    )
))
