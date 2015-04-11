
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
  
  ## Mucilage bioch data Explorer ###########################################
  
  ### dynamic sliders ###########################################
  
  #### Gal_A range slider
  output$dynamic_gala_slider <- renderUI({
    if (!"Gal_A" %in% input$show_mucilbiochcols)
      return()
    min_gala <- min(db.bioch.all.clean$Gal_A)
    max_gala <- max(db.bioch.all.clean$Gal_A)
    sliderInput("gala_range", strong("Gal_A range:"),
              min = min_gala, max = max_gala, value = c(min_gala, max_gala))
  })
  
  #### OsesNeutres range slider ###########################################
  output$dynamic_ozn_slider <- renderUI({
    if (!"OsesNeutres" %in% input$show_mucilbiochcols)
      return()
    min_ozn <- min(db.bioch.all.clean$OsesNeutres)
    max_ozn <- max(db.bioch.all.clean$OsesNeutres)
    sliderInput("ozn_range", strong("OsesNeutres range:"),
                min = min_ozn, max = max_ozn, value = c(min_ozn, max_ozn))
  })
  
  #### Molecular weight range slider ###########################################
  output$dynamic_mw_slider <- renderUI({
    if (!"MW" %in% input$show_mucilbiochcols)
      return()
    min_mw <- min(db.bioch.all.clean$MW)
    max_mw <- max(db.bioch.all.clean$MW)
    sliderInput("mw_range", strong("MW range:"),
                min = min_mw, max = max_mw, value = c(min_mw, max_mw))
  })
  
  #### Intrinsic viscosity range slider ###########################################
  output$dynamic_iv_slider <- renderUI({
    if (!"IV" %in% input$show_mucilbiochcols)
      return()
    min_iv <- min(db.bioch.all.clean$IV)
    max_iv <- max(db.bioch.all.clean$IV)
    sliderInput("iv_range", strong("IV range:"),
                min = min_iv, max = max_iv, value = c(min_iv, max_iv))
  })
  
  #### Giration radius range slider ###########################################
  output$dynamic_rg_slider <- renderUI({
    if (!"RG" %in% input$show_mucilbiochcols)
      return()
    min_rg <- min(db.bioch.all.clean$RG)
    max_rg <- max(db.bioch.all.clean$RG)
    sliderInput("rg_range", strong("RG range:"),
                min = min_rg, max = max_rg, value = c(min_rg, max_rg))
  })
  
  #### Hydrodynamic radius range slider ###########################################
  output$dynamic_rh_slider <- renderUI({
    if (!"RH" %in% input$show_mucilbiochcols)
      return()
    min_rh <- min(db.bioch.all.clean$RH)
    max_rh <- max(db.bioch.all.clean$RH)
    sliderInput("rh_range", strong("RH range:"),
                min = min_rh, max = max_rh, value = c(min_rh, max_rh))
  })
  
  ### Filtering mucilage datasets ###########################################
  
#   ## all plants
#   output$df.bioch.all <- renderDataTable({
#     db.bioch.all
#   },
#   options = list(orderClasses = TRUE)
#   )

#   ## all plants but no missing values
#   output$df.bioch.all.nomiss <- renderDataTable({
#     db.bioch.all.clean
#   },
#   options = list(orderClasses = TRUE)
#   )

  # mandatory mucilage biochemical datasets columns
  mandatory_mucilbiochcols <- names(db.bioch.all.clean)[1:4]  

  ### raw ###########################################
  output$raw <- renderDataTable({
    #### filter accessions by AV number ###########################################
    if ( input$select_av == "All" ||  input$select_av == "" || is.null(input$select_av) || is.na(input$select_av) ) {
      avs <- unique(db.bioch.all.clean$AV)
    } else {
      avs <- as.numeric(unlist(strsplit(input$select_av, split="[\\s\\n]*", perl=TRUE)))
    }
    mucilbioch <- db.bioch.all.clean %>%
      filter(
        AV %in% avs
        )
#     mucilbioch <- db.bioch.all.clean[which(db.bioch.all.clean$AV %in% avs),]

    #### filter mucilage datasets ###########################################
    mucilbioch <- mucilbioch[, c(mandatory_mucilbiochcols, input$show_mucilbiochcols), drop=FALSE]

    #### filter Gal_A range ###########################################
    if ("Gal_A" %in% input$show_mucilbiochcols) {
      # %>% function is bugged, generating:
      ## Error in eval(substitute(expr), envir, enclos) : 
      ##      incorrect length (0), expecting: 1272
      #       mucilbioch %>%
      #         filter(
      #           Gal_A >= input$gala_range[1] & Gal_A <= input$gala_range[2]
      #         )
      ## nor this syntax works
      #      filter(mucilbioch, Gal_A >= input$gala_range[1] & Gal_A <= input$gala_range[2])
      ## this classical one works
      mucilbioch <- mucilbioch[which(mucilbioch$Gal_A >= input$gala_range[1] & mucilbioch$Gal_A <= input$gala_range[2]), 1:ncol(mucilbioch)]
    } 

    #### filter OsesNeutres range ###########################################
    if ("OsesNeutres" %in% input$show_mucilbiochcols) {
      mucilbioch <- mucilbioch %>%
              filter(
                OsesNeutres >= input$ozn_range[1] & OsesNeutres <= input$ozn_range[2]
              )
    }

    #### filter Molecular weight range ###########################################
    if ("MW" %in% input$show_mucilbiochcols) {
      mucilbioch <- mucilbioch %>%
        filter(
          MW >= input$mw_range[1] & MW <= input$mw_range[2]
        )
    }

    #### filter Intrinsic viscosity range ###########################################
    if ("IV" %in% input$show_mucilbiochcols) {
      mucilbioch <- mucilbioch %>%
        filter(
          IV >= input$iv_range[1] & IV <= input$iv_range[2]
        )
    }

    #### filter Giration radius range ###########################################
    if ("RG" %in% input$show_mucilbiochcols) {
      mucilbioch <- mucilbioch %>%
        filter(
          RG >= input$rg_range[1] & RG <= input$rg_range[2]
        )
    }

    #### filter Hydrodynamic radius range ###########################################
    if ("RH" %in% input$show_mucilbiochcols) {
      mucilbioch <- mucilbioch %>%
        filter(
          RH >= input$rh_range[1] & RH <= input$rh_range[2]
        )
    }

    # return at last
    mucilbioch
  },
  options = list(orderClasses = TRUE)
  )

  ### mean ###########################################
  # TODO

})