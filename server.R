
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(leaflet))

shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################
  
  # Create the map
  map <- createLeafletMap(session, "map")
  output$mapp <- renderUI({
    input$mapPick
    isolate({
      leafletMap("map", "100%", "100%",
                 initialTileLayer = input$mapPick,
                 initialTileLayerAttribution = HTML('© MapQuest, Map Data © OpenStreetMap contributors, ODbL'),
                 options=list(center = center(),zoom = zoom()))
    })
  })
  zoom <- reactive({
    ifelse(is.null(input$map_zoom),3,input$map_zoom)
  })
  center <- reactive({
    if(is.null(input$map_bounds)) {
      c(45, 5)
    } else {
      map_bounds <- input$map_bounds
      c((map_bounds$north + map_bounds$south)/2.0,(map_bounds$east + map_bounds$west)/2.0)
    }
  })
  
  ## Bioch data Explorer ###########################################
  
#   ## 4 plants
#   output$df.bioch.4p <- renderDataTable({
#     db.bioch.4p
#   },
#   options = list(orderClasses = TRUE)
#   )

  ## 4 plants but no missing values
  output$df.bioch.4p.nomiss <- renderDataTable({
    db.bioch.4p.clean
  },
  options = list(orderClasses = TRUE)
  )

  # mandatory mucilage biochemical datasets columns
  mandatory_mucilbiochcols <- names(db.bioch.4p.clean)[1:4]  

  ## raw
  output$raw <- renderDataTable({
    ## filter accessions by AV number
    if ( input$select_av == "All" ||  input$select_av == "" || is.null(input$select_av) || is.na(input$select_av) ) {
      avs <- unique(db.bioch.4p.clean$AV)
    } else {
      avs <- as.numeric(unlist(strsplit(input$select_av, split="[\\s\\n]*", perl=TRUE)))
    }
    mucilbioch <- db.bioch.4p.clean %>%
      filter(
        AV %in% avs
        )
    
    ## filter mucilage datasets
    mucilbioch <- mucilbioch[, c(mandatory_mucilbiochcols, input$show_mucilbiochcols), drop=FALSE]

  },
  options = list(orderClasses = TRUE)
  )


})