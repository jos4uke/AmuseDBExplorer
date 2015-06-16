
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(ggplot2))

# FUNCTIONS
split_string <- function(string) {
  unlist(strsplit(string, split="[\\s\\n]+", perl=TRUE))
}

get_controls_av_name <- function() {
  uniq <- unique(db.bioch.controls %>%
            select(
                AV, NAME
              ))
  unlist(lapply(unique(db.bioch.controls$AV), function(c){
    paste(c, "(", uniq[uniq$AV == c, "NAME"], ")")
  }))
}

# SERVER
shinyServer(function(input, output, session) {

  ## Database description ###########################################
  output$desc <- renderUI({
    content <- as.character(tagList(
      tags$h4(strong("Raw/summary datasets:")),
      tags$strong(HTML(sprintf("- %s: %d",
                               "Total accessions (+ controls)", length(unique(db.bioch.all$AV))
      ))), tags$br(),
      tags$li(sprintf("%s: %d", 
                     "in complete datasets", length(unique(db.bioch.all.clean$AV))
      )),
      tags$li(sprintf("%s: %d (%s)", 
                     "in incomplete datasets (NA/ND values)", length(unique(db.bioch.incomplete$AV)), paste(unique(db.bioch.incomplete$AV), collapse=", ")
      )), tags$strong("- in summary dataset:"),
      tags$li(sprintf("%s: %d", 
                      "with 4 plants", length(dimnames(p4)[[1]])
      )),
      tags$li(sprintf("%s: %d (%s)", 
                      "without 4 plants", length(unique(db.bioch.no4p$AV)), paste(unique(db.bioch.no4p$AV), collapse=", ")
      )), tags$br(),
      tags$strong(HTML(sprintf("- %s: %d (%s)",
                               "Total cultures", length(unique(db.bioch.all$Culture)), paste(unique(db.bioch.all$Culture), collapse=", ")
      ))), tags$br(),
      tags$li(sprintf("%s: %d", 
                      "in complete datasets", length(unique(db.bioch.all.clean$Culture))
      )),
      tags$li(sprintf("%s: %d", 
                      "in incomplete datasets", length(unique(db.bioch.incomplete$Culture))
      )), tags$br(),
      tags$strong(HTML(sprintf("- %s: %d",
                               "Total seed pools", length(unique(db.bioch.all$SeedPool))
      ))), tags$br(),
      tags$li(sprintf("%s: %d", 
                     "in complete datasets", length(unique(db.bioch.all.clean$SeedPool))
      )),
      tags$li(sprintf("%s: %d", 
                     "in incomplete datasets", length(unique(db.bioch.incomplete$SeedPool))
      )), tags$br(),
      tags$strong(HTML(sprintf("- %s: %d",
                               "Total controls", length(unique(db.bioch.controls$AV))
      ))), tags$br(),
      tags$li(sprintf("%s: %s", 
                      'control_av (name)' , paste(get_controls_av_name(), collapse=", ")
      )),tags$br(),
      tags$strong(HTML(sprintf("- %s: %d",
                               "Soluble mucilage biochemical variables", length(choices_mucilbiochcols)
      ))), tags$br(),
      tags$li(sprintf("%s: %s", 
                      'list' , paste(names(choices_mucilbiochcols), collapse=", ")
      )),tags$br(),
      tags$h4(strong("Geoloc climate datasets:")), 
      tags$a(href="https://www.pik-potsdam.de/members/cramer/climate", "(Cramer&Leemans database, version 2.1)"), tags$br(),
      tags$strong(HTML(sprintf("- %s: %d",
                               "Total accessions", length(unique(db.climate.geoloc$AV))
      ))), tags$br(),
      tags$li(sprintf("%s: %d", 
                      "total accessions with geoloc data", length(acc_gps$AV)
      )),
      tags$li(sprintf("%s: %d (%s)", 
                      "total accessions without geoloc data", length(acc_wogps$AV), paste(acc_wogps$AV, collapse=", ")
      )), tags$br(),
      tags$strong(HTML(sprintf("- %s: %d",
                               "Climate datasets", length(names(choices_climatodatasets))
      ))), tags$br(),
      tags$li(sprintf("%s: %s", 
                      "list", paste(names(choices_climatodatasets), collapse=", ")
      ))
    ))
    
    # at last
    content
  })
  
  ## Interactive Map ###########################################
  
  ### map informations ###########################################
  output$mapdesc <- reactive({
    if (is.null(input$map_bounds))
      return(list())
    desc <- list(
      lat = mean(c(input$map_bounds$north, input$map_bounds$south)),
      lng = mean(c(input$map_bounds$east, input$map_bounds$west)),
      zoom = input$map_zoom,
      shownAccs = nrow(accessionsInBounds()),
      totalAccs = length(db.climate.geoloc$AV)
    )
    HTML(paste('The map is centered at ', strong(desc$lat), ' latitude, ', strong(desc$lng), 'longitude,',
               'with a zoom level of ', strong(desc$zoom), '.<br/>',
               strong(desc$shownAccs), 'out of ', strong(length(acc_gps$AV)), 'visible accessions are displayed. (over a total of', strong(desc$totalAccs), 'available accessions).'  ))
  })
  
  ### map legend ###########################################
  output$legend <- renderPlot({
    cgl <- db.climate.geoloc # copy data
    # rename column
    names(cgl)[names(cgl)=="GEOLOC_QUAL"]  <- "Geo-location quality"
    # sort columns
    cgl.sort <- within(cgl, 
                       `Geo-location quality` <- factor(`Geo-location quality`, 
                                          levels=names(sort(table(`Geo-location quality`), 
                                                            decreasing=TRUE))))
    # adapt colors
    qual_colors_2 <- qual_colors
    qual_colors_2[qual_colors_2=="green"] <- "darkgreen" # the green is too light
    
    hist <- ggplot(cgl.sort, aes(`Geo-location quality`, fill=`Geo-location quality`, ymax=max(..count..)+100)) + 
              geom_bar() +
              geom_bar(show_guide=FALSE) +
              scale_fill_manual(values=qual_colors_2) +
              stat_bin(geom="text", aes(label=..count..), vjust=-1, size=4) +
              ylim(0, 300) +  
              theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank()) +
              theme(legend.position = "bottom", legend.direction = "vertical")
    hist
  })
  
  ### input accessions AV numbers ###########################################
  input_avs_map <- reactive({
    y <- input$select_av_map
    
    if ( y == "All" ||  y == "" || is.null(y) || is.na(y) ) {
      return("All")
    }
    
    as.numeric(split_string(y))
  })
  
  observe({
    y <- input$select_av
    if ( !( y == "All" ||  y == "" || is.null(y) || is.na(y) ) ) {
      updateTextInput(session, "select_av_map", value = y)
    }
  })
  
  output$search_avs_map <- renderText({
    y <- input_avs_map()
    if ( y == "All" ||  y == "" || is.null(y) || is.na(y) ) {
      return()
    }
    return(y)
  })
  
  requested_accessions <- reactive({
    x <- input_avs_map()  
    
    # Requested accessions
    #if x !(All|""|NA|NULL) draw circles around markers
    if ( !( x == "All" ||  x == "" || is.null(x) || is.na(x) ) ) {
      # get accessions
      req_acc <- db.climate.geoloc %>%
        filter(
          AV %in% x            
        )
      return(req_acc)
    }
    return()
  })
  
  ### Create the map 
  map <- createLeafletMap(session, "map")
  
  output$mapp <- renderUI({
    input$mapPick
    isolate({
      leafletMap("map", "100%", 900,
                 initialTileLayer = input$mapPick,
                 initialTileLayerAttribution = HTML('© MapQuest, Map Data © OpenStreetMap contributors, ODbL'),
                 options=list(
                              center = center(),
                              zoom = zoom(),
                              maxBounds = list(list(-90, -180), list(90, 180))
                              ))
    })
  })
  
  zoom <- reactive({
    ifelse(is.null(input$map_zoom), 3, input$map_zoom)
  })
  
  center <- reactive({
    if(is.null(input$map_bounds)) {
      c(24, 28)
    } else {
      # Accessions in bound
      req_acc <- accessionsInBounds()
      if (nrow(req_acc) == 0)
        return()

      # fitsbound
      if ( length(req_acc$AV) == 1 ) {
        c(req_acc$LATITUDE, req_acc$LONGITUDE)
      } else if (length(req_acc$AV) > 0) {
        sw_margin <- 5
        ne_margin <- 5
        sw <- list(lat1=min(req_acc$LATITUDE), lng1=min(req_acc$LONGITUDE))
        ne <- list(lat2=max(req_acc$LATITUDE), lng2=max(req_acc$LONGITUDE))

        c(mean(sw$lat1,ne$lat2), mean(sw$lng1,ne$lng2))
      }
    }
  })
  
  ### Marks
  
  # A reactive expression that returns the set of accessions that are
  # in bounds right now
  accessionsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(db.climate.geoloc[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)  
    
    acc_in_bounds <- subset(db.climate.geoloc,
           LATITUDE >= latRng[1] & LATITUDE <= latRng[2] &
             LONGITUDE >= lngRng[1] & LONGITUDE <= lngRng[2])
    acc_not_in_bounds <- db.climate.geoloc %>%
                            filter(!(AV %in% acc_in_bounds$AV))
#     # not in bounds: 179 214 227 294 298
#     print(acc_not_in_bounds$AV)
    acc_in_bounds
  })
  
# session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
# integration; without it, the addCircle commands arrive in the browser
# before the map is created.
  session$onFlushed(once=TRUE, function() { 
    # clear the map and center on requested accessions zone
    clearAndMove <- observe({
      req_acc <- requested_accessions()
      if (is.null(req_acc))
        return()
      # TODO list:
      # is x all the accessions?
      ## yes: get accessions in bound and draw markers by geoloc_qual
      ## no: 
      ### get requested accessions coordinates and fits the new bounds: southwest=min(lat, lng)-sw_margins and northeast=max(lat, lng)+ne_margins
      ### highlight the requested accessions in yellow circles for example
      ### get accessions in bound in the new bounds and draw markers
      isolate({
        # Clear existing marks/circles/popups before drawing
        map$clearMarkers()
        map$clearShapes()
        map$clearPopups()
            
        # fitsbound
        if ( length(req_acc$AV) == 1 ) {
          #           map$setView(req_acc$LATITUDE, req_acc$LONGITUDE, 8, forceReset = TRUE)
          map$fitBounds(req_acc$LATITUDE-5, req_acc$LONGITUDE-5, req_acc$LATITUDE+5, req_acc$LONGITUDE+5)
        } else if (length(req_acc$AV) > 0) {
          sw_margin <- 5
          ne_margin <- 5
          sw <- list(lat1=min(req_acc$LATITUDE), lng1=min(req_acc$LONGITUDE))
          ne <- list(lat2=max(req_acc$LATITUDE), lng2=max(req_acc$LONGITUDE))
          # both methods prevent to zoom and browse the map
          map$fitBounds(sw$lat1-sw_margin, sw$lng1-sw_margin, ne$lat2+ne_margin, ne$lng2+ne_margin)
          # idem using setview: cannot zoom or browse map
          #         map$setView(mean(sw$lat1,ne$lat2), mean(sw$lng1,ne$lng2), zoom(), forceReset = FALSE)
        }
      })
    })
     
    # draw accessions
    drawAccessions <- observe({
      # Accessions in bound
      accessions <- accessionsInBounds()
      if (nrow(accessions) == 0)
        return()
      
      #       # Define colors: moved to global.R
      #       geoloc_qual <- unique(db.climate.geoloc$GEOLOC_QUAL)
      #       qual_colors <- c("green", "orange", "blue", "grey")
      #       names(qual_colors) <- geoloc_qual
      #       colors <- qual_colors[as.character(db.climate.geoloc$GEOLOC_QUAL)]
      acc_colors <- colors[match(accessions$AV, db.climate.geoloc$AV)]
      names(acc_colors) <- c() # reset names to get indexes back when add marks
      
      #       map$addCircleMarker(
      #         accessions$LATITUDE,
      #         accessions$LONGITUDE,
      #         rep(6, length(accessions)),
      #         accessions$AV,
      #         list(stroke=FALSE, fill=TRUE, fillOpacity=0.7),
      #         list(color=acc_colors)
      #       )
      
      # Draw in batches of 100; makes the app feel a bit more responsive
      chunksize <- 100
      for (from in seq.int(1, nrow(accessions), chunksize)) {
        to <- min(nrow(accessions), from + chunksize)
        accchunk <- accessions[from:to,]
        # Bug in Shiny causes this to error out when user closes browser
        # before we get here
        try(
          map$addCircleMarker(
            accchunk$LATITUDE,
            accchunk$LONGITUDE,
            rep(6, length(accchunk)),
            accchunk$AV,
            list(stroke=FALSE, fill=TRUE, fillOpacity=0.7, pointerEvents="mouseover"),
            list(color=acc_colors[from:to])
          )
        )
      }
      
      # draw requested accessions
      req_acc <- requested_accessions()
      if (is.null(req_acc))
        return()
      # highlight and popup
      map$addMarker(
        req_acc$LATITUDE, req_acc$LONGITUDE,
        req_acc$AV,
        list(riseOnHover=TRUE, fillOpacity=0.9),
        list(title=req_acc$AV, alt=req_acc$NAME)
      )
      # cannot display multiple popups at the same time (see js bindings src) => uses mouse events to display
      #         lapply(req_acc, function(x) {
      #           acc_content <- as.character(tagList(tags$strong(HTML(sprintf("%s (%s)", x[2], x[1]))),tags$br()))
      #           map$showPopup(x[6], x[7], acc_content, layerId = x[2], options=list())
      #         })
      #         acc_content <- as.character(tagList(tags$strong(HTML(sprintf("%s %s", req_acc$AV, req_acc$NAME))),tags$br()))
      #         map$showPopup(req_acc$LATITUDE, req_acc$LONGITUDE, req_acc$AV, req_acc$AV, options=list(keepInView=TRUE))
    })

    # TIL this is necessary in order to prevent the observer from
    # attempting to write to the websocket after the session is gone.
    session$onSessionEnded(clearAndMove$suspend)
    session$onSessionEnded(drawAccessions$suspend)
  })
  
  # Show a popup at the given location
  showAccPopup <- function(acc, lat, lng) {
    selectedAcc <- db.climate.geoloc[db.climate.geoloc$AV == acc,]
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("AV %s (%s)",
                               selectedAcc$AV, selectedAcc$NAME
      ))), tags$br(),
      sprintf("geoloc quality: %s", selectedAcc$GEOLOC_QUAL), tags$br()
    ))
    map$showPopup(lat, lng, content, acc)
  }
  
  # When mark is mouseover, show popup with accession infos: AV number, name
  mouseOverMark <- observe({
    map$clearPopups()
    event <- input$map_marker_mouseover
    if (is.null(event))
      return()
    
    isolate({
      showAccPopup(event$id, event$lat, event$lng)
    })
  })

  session$onSessionEnded(mouseOverMark$suspend)

  # GoToMap
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map$clearPopups()
      dist <- 0.5
      av <- input$goto$av
      lat <- input$goto$lat
      lng <- input$goto$lng
      showAccPopup(av, lat, lng)
      map$fitBounds(lat - dist, lng - dist,
                    lat + dist, lng + dist)
    })
  })

  ## Mucilage bioch data Explorer ###########################################
  
  ### input accessions AV numbers ###########################################
  input_avs <- reactive({
    x <- input$select_av

    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      return("All")
    }

    as.numeric(split_string(x))
  })
  
  observe({
    x <- input$select_av_map
    if ( !( x == "All" ||  x == "" || is.null(x) || is.na(x) ) )  {
      updateTextInput(session, "select_av", value = x)
    }
  })
  
  output$search_avs <- renderText({
    x <- input_avs()
    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      return()
    }
    return(x)
  })

  ### input accessions names ###########################################
  observe({
    x <- input$show_accessions_name
    
    if ( x == "All" || x == "" || is.null(x) || is.na(x) ) {
      return()
    }
    avsbynames <- unique(db.bioch.all.clean %>%
                    filter(NAME %in% x) %>%
                    select(AV)
                  )
    y <- isolate(input$select_av)
    stillSelectedAVS <- as.numeric(split_string(y))
    if ( !( stillSelectedAVS == "All" ||  stillSelectedAVS == "" || is.null(stillSelectedAVS) || is.na(stillSelectedAVS) ) ) {
#       selectedAVS <- stillSelectedAVS
      selectedAVS <- unique(c(stillSelectedAVS, unlist(avsbynames)))
    } else {
      selectedAVS <- avsbynames
    }
    
    # at last
    updateTextInput(session, "select_av", value = paste(selectedAVS, collapse=" "))
  })

  ### input accessions names on map ###########################################
  observe({
    x <- input$show_accessions_name_map
    
    if ( x == "All" || x == "" || is.null(x) || is.na(x) ) {
      return()
    }
    avsbynames <- unique(db.climate.geoloc %>%
                          filter(NAME %in% x) %>%
                          select(AV)
                        )
    
    y <- isolate(input$select_av_map)
    stillSelectedAVS <- as.numeric( split_string(y) )
    if ( !( stillSelectedAVS == "All" ||  stillSelectedAVS == "" || is.null(stillSelectedAVS) || is.na(stillSelectedAVS) ) ) {
      selectedAVS <- unique(c(stillSelectedAVS, unlist(avsbynames)))
    } else {
      selectedAVS <- avsbynames
    }
    
    # at last
    updateTextInput(session, "select_av_map", value = paste(selectedAVS, collapse=" "))
  })
  
  ### dynamic sliders ###########################################
  
  #### Gal_A range slider ###########################################
  output$dynamic_gala_slider <- renderUI({
    if (!"Gal_A" %in% input$show_mucilbiochcols)
      return()
    min_gala <- min(db.bioch.all.clean$Gal_A)-0.5
    max_gala <- max(db.bioch.all.clean$Gal_A)+0.5
    sliderInput("gala_range", strong("Gal_A range:"),
              min = min_gala, max = max_gala, value = c(min_gala, max_gala))
  })
  
  #### Gal_A mean range slider 
  output$dynamic_gala_mean_slider <- renderUI({
    if (!"Gal_A" %in% input$show_mucilbiochsummarycols)
      return()
    group_by_culture <- summaryByCulture()
    if (!is.null(group_by_culture)) {
      db.bioch.4p.summary <- group_by_culture
    }
    min_gala_mean <- min(db.bioch.4p.summary$Gal_A_mean)-0.5
    max_gala_mean <- max(db.bioch.4p.summary$Gal_A_mean)+0.5
    sliderInput("gala_mean_range", strong("Gal_A mean range:"),
                min = min_gala_mean, max = max_gala_mean, value = c(min_gala_mean, max_gala_mean))
  })
  
  #### OsesNeutres range slider ###########################################
  output$dynamic_ozn_slider <- renderUI({
    if (!"OsesNeutres" %in% input$show_mucilbiochcols)
      return()
    min_ozn <- min(db.bioch.all.clean$OsesNeutres)-0.5
    max_ozn <- max(db.bioch.all.clean$OsesNeutres)+0.5
    sliderInput("ozn_range", strong("OsesNeutres range:"),
                min = min_ozn, max = max_ozn, value = c(min_ozn, max_ozn))
  })
  
  #### OsesNeutres mean range slider
  output$dynamic_ozn_mean_slider <- renderUI({
    if (!"OsesNeutres" %in% input$show_mucilbiochsummarycols)
      return()
    group_by_culture <- summaryByCulture()
    if (!is.null(group_by_culture)) {
      db.bioch.4p.summary <- group_by_culture
    }
    min_ozn_mean <- min(db.bioch.4p.summary$OsesNeutres_mean)-0.5
    max_ozn_mean <- max(db.bioch.4p.summary$OsesNeutres_mean)+0.5
    sliderInput("ozn_mean_range", strong("OsesNeutres mean range:"),
                min = min_ozn_mean, max = max_ozn_mean, value = c(min_ozn_mean, max_ozn_mean))
  })
  
  #### Mean Molar Mass range slider ###########################################
  output$dynamic_mm_slider <- renderUI({
    if (!"MM" %in% input$show_mucilbiochcols)
      return()
    min_mm <- min(db.bioch.all.clean$MM, na.rm = TRUE)
    max_mm <- max(db.bioch.all.clean$MM, na.rm = TRUE)
    sliderInput("mm_range", strong("MM range:"),
                min = min_mm, max = max_mm, value = c(min_mm, max_mm))
  })
  
  #### Mean Molar Mass mean range slider
  output$dynamic_mm_mean_slider <- renderUI({
    if (!"MM" %in% input$show_mucilbiochsummarycols)
      return()
    group_by_culture <- summaryByCulture()
    if (!is.null(group_by_culture)) {
      db.bioch.4p.summary <- group_by_culture
    }
    min_mm_mean <- min(db.bioch.4p.summary$MM_mean)-0.5
    max_mm_mean <- max(db.bioch.4p.summary$MM_mean)+0.5
    sliderInput("mm_mean_range", strong("MM mean range:"),
                min = min_mm_mean, max = max_mm_mean, value = c(min_mm_mean, max_mm_mean))
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
  
  #### Intrinsic viscosity mean range slider
  output$dynamic_iv_mean_slider <- renderUI({
    if (!"IV" %in% input$show_mucilbiochsummarycols)
      return()
    group_by_culture <- summaryByCulture()
    if (!is.null(group_by_culture)) {
      db.bioch.4p.summary <- group_by_culture
    }
    min_iv_mean <- min(db.bioch.4p.summary$IV_mean)-0.5
    max_iv_mean <- max(db.bioch.4p.summary$IV_mean)+0.5
    sliderInput("iv_mean_range", strong("IV mean range:"),
                min = min_iv_mean, max = max_iv_mean, value = c(min_iv_mean, max_iv_mean))
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
  
  #### Giration radius range mean slider
  output$dynamic_rg_mean_slider <- renderUI({
    if (!"RG" %in% input$show_mucilbiochsummarycols)
      return()
    group_by_culture <- summaryByCulture()
    if (!is.null(group_by_culture)) {
      db.bioch.4p.summary <- group_by_culture
    }
    min_rg_mean <- min(db.bioch.4p.summary$RG_mean)-0.5
    max_rg_mean <- max(db.bioch.4p.summary$RG_mean)+0.5
    sliderInput("rg_mean_range", strong("RG mean range:"),
                min = min_rg_mean, max = max_rg_mean, value = c(min_rg_mean, max_rg_mean))
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
  
  #### Hydrodynamic radius mean range slider
  output$dynamic_rh_mean_slider <- renderUI({
    if (!"RH" %in% input$show_mucilbiochsummarycols)
      return()
    group_by_culture <- summaryByCulture()
    if (!is.null(group_by_culture)) {
      db.bioch.4p.summary <- group_by_culture
    }
    min_rh_mean <- min(db.bioch.4p.summary$RH_mean)-0.5
    max_rh_mean <- max(db.bioch.4p.summary$RH_mean)+0.5
    sliderInput("rh_mean_range", strong("RH mean range:"),
                min = min_rh_mean, max = max_rh_mean, value = c(min_rh_mean, max_rh_mean))
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
  mandatory_mucilbiochcols <- names(db.bioch.all.clean)[1:5]  

  ### raw ###########################################

  datasetRaw <- reactive({
    #### filter accessions by AV number ###########################################
    x <- input_avs()
    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      avs <- unique(db.bioch.all.clean$AV)
    } else {
      avs <- x
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
      #      mucilbioch <- mucilbioch %>%
      #        filter(
      #          Gal_A >= input$gala_range[1] & Gal_A <= input$gala_range[2]
      #        )
      ## nor this syntax works
      #      mucilbioch <- filter(mucilbioch, Gal_A >= input$gala_range[1] & Gal_A <= input$gala_range[2])
      ## this classical one works
      mucilbioch <- mucilbioch[which(mucilbioch$Gal_A >= input$gala_range[1] & mucilbioch$Gal_A <= input$gala_range[2]), 1:ncol(mucilbioch), drop=FALSE]      
    } 
    
    #### filter OsesNeutres range ###########################################
    if ("OsesNeutres" %in% input$show_mucilbiochcols) {
      mucilbioch <- mucilbioch %>%
        filter(
          OsesNeutres >= input$ozn_range[1] & OsesNeutres <= input$ozn_range[2]
        )
    }
    
    #### filter Mean Molar Mass range ###########################################
    if ("MM" %in% input$show_mucilbiochcols) {
      mucilbioch <- mucilbioch %>%
        filter(
          MM >= input$mm_range[1] & MM <= input$mm_range[2]
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
  })

  output$raw <- renderDataTable({
    datasetRaw()
  },
  options = list(orderClasses = TRUE)
  )

  output$downloadRawData <- downloadHandler(
    filename = function() { 
      paste('AMUSE_raw_dataset_', format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), '.zip', sep='') 
    },
    content = function(file) {
      print(file)
      setwd(tempdir())
      df <- datasetRaw()
      saveTime <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
      baseFile <- paste('AMUSE_raw_dataset_', saveTime, sep='')
      # process csv file
      csvFile <- paste(baseFile, '.csv', sep='')
      write.csv2(df, csvFile, row.names=FALSE)
      print(csvFile)
      
      # process metadata file
      metadataFile <- paste(baseFile, '_metadata.txt', sep='')
      sink(metadataFile)
      cat(paste("Dataset File: ", csvFile, "\n",sep=''))
      cat(paste("Metadata File: ", metadataFile, "\n",sep=''))
      cat(paste("Date: ", saveTime, "\n",sep=''))
      cat("AMUSE Raw Dataset\n")
      cat("DataTable dimensions: ")
      cat(dim(df), "\n")
      cat("Filters:\n")
      cat(paste("#AV numbers: ", length(unique(df$AV)), "\n", sep=''))
      if ("Gal_A" %in% input$show_mucilbiochcols) 
        cat(paste("#Gal_A range: ", input$gala_range[1], ':', input$gala_range[2], "\n", sep=''))
      if ("OsesNeutres" %in% input$show_mucilbiochcols)
        cat(paste("#OsesNeutres range: ", input$ozn_range[1], ":", input$ozn_range[2], "\n", sep=''))      
      if ("MM" %in% input$show_mucilbiochcols)
        cat(paste("#MM range: ", input$mm_range[1], ":", input$mm_range[2], "\n", sep=''))
      if ("IV" %in% input$show_mucilbiochcols)
        cat(paste("#IV range: ", input$iv_range[1], ":", input$iv_range[2], "\n", sep=''))
      if ("RG" %in% input$show_mucilbiochcols)
        cat(paste("#RG range: ", input$rg_range[1], ":", input$rg_range[2], "\n", sep=''))
      if ("RH" %in% input$show_mucilbiochcols)
        cat(paste("#RH range: ", input$rh_range[1], ":", input$rh_range[2], "\n", sep=''))
      cat("Contact: Joseph.Tran@versailles.inra.fr\n")
      sink()
      
      # zip
      zip(zipfile=file, files=c(csvFile, metadataFile))
    },
    contentType = "application/zip"
  )

  ### summary ###########################################
  summaryByCulture <- reactive({
    if (input$groupByCulture) {
      db.bioch.4p.summary <- db.bioch.all.clean %>%
        filter(AV %in% p4$AV) %>%
        select(NAME, AV, Culture, Gal_A, OsesNeutres, MM, IV, RG, RH) %>%
        group_by(NAME, AV, Culture) %>%
        summarise_each(funs(min, Q1, median, mean, Q3, max, IQR, sd))
      
      db.bioch.4p.summary
    }

  })

  datasetSummary <- reactive({
    mandatory_mucilbiochsummarycols <- 1:2
    
    #### group by culture ###########################################
    group_by_culture <- summaryByCulture()
    if ( !(is.null(group_by_culture)) ) {
      db.bioch.4p.summary <- group_by_culture
      mandatory_mucilbiochsummarycols <- 1:3
    }
      
    #### filter accessions by AV number ###########################################
    x <- input_avs()
    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      avs <- p4$AV
    } else {
      avs <- x
    }
    mucilbiochsummary <- db.bioch.4p.summary %>%
      filter(
        AV %in% avs
      )
    
    #### filter mucilage datasets ###########################################
    show_cols_idx <- unlist(vapply(input$show_mucilbiochsummarycols, function(d){
      grep(pattern=d, x=names(mucilbiochsummary))
    },FUN.VALUE=c(Min.=0, Q1=0, Median=0, Mean=0, Q3=0, Max.=0, IQR=0, sd=0)))
    mucilbiochsummary <- mucilbiochsummary[, c(mandatory_mucilbiochsummarycols, show_cols_idx), drop=FALSE]
    
    #### filter summary columns ###########################################
    show_summary_cols_idx <- unlist(lapply(input$show_summarycols, function(c){
      grep(pattern=c, x=names(mucilbiochsummary))
    }))    
    mucilbiochsummary <- mucilbiochsummary[, c(mandatory_mucilbiochsummarycols, show_summary_cols_idx), drop=FALSE]
    ##### reorder by dataset
    cols_by_dataset <- unlist(lapply(input$show_mucilbiochsummarycols, function(d){
      grep(pattern=d, x=names(mucilbiochsummary))
    }))
    mucilbiochsummary <- mucilbiochsummary[, c(mandatory_mucilbiochsummarycols, cols_by_dataset), drop=FALSE]

    ### filter on mean
    if ("_mean" %in% input$show_summarycols) {
      #### filter Gal_A range ###########################################
      if ("Gal_A" %in% input$show_mucilbiochsummarycols) {
        # %>% function is bugged, generating:
        ## Error in eval(substitute(expr), envir, enclos) : 
        ##      incorrect length (0), expecting: 1272
        #      mucilbiochsummary <- mucilbiochsummary %>%
        #        filter(
        #          Gal_A_mean >= input$gala_mean_range[1] & Gal_A_mean <= input$gala_mean_range[2]
        #        )
        ## nor this syntax works
        #      mucilbiochsummary <- filter(mucilbiochsummary, Gal_A_mean >= input$gala_mean_range[1] & Gal_A_mean <= input$gala_mean_range[2])
        ## this classical one works
        mucilbiochsummary <- mucilbiochsummary[which(mucilbiochsummary$Gal_A_mean >= input$gala_mean_range[1] & mucilbiochsummary$Gal_A_mean <= input$gala_mean_range[2]), 1:ncol(mucilbiochsummary), drop=FALSE]      
      }
      
      #### filter OsesNeutres mean range ###########################################
      if ("OsesNeutres" %in% input$show_mucilbiochsummarycols) {
        mucilbiochsummary <- mucilbiochsummary %>%
          filter(
            OsesNeutres_mean >= input$ozn_mean_range[1] & OsesNeutres_mean <= input$ozn_mean_range[2]
          )
      }
      
      #### filter Mean Molar Mass mean range ###########################################
      if ("MM" %in% input$show_mucilbiochsummarycols) {
        mucilbiochsummary <- mucilbiochsummary %>%
          filter(
            MM_mean >= input$mm_mean_range[1] & MM_mean <= input$mm_mean_range[2]
          )
      }    
      
      #### filter Intrinsic viscosity mean range ###########################################
      if ("IV" %in% input$show_mucilbiochsummarycols) {
        mucilbiochsummary <- mucilbiochsummary %>%
          filter(
            IV_mean >= input$iv_mean_range[1] & IV_mean <= input$iv_mean_range[2]
          )
      }
      
      #### filter Giration radius range ###########################################
      if ("RG" %in% input$show_mucilbiochsummarycols) {
        mucilbiochsummary <- mucilbiochsummary %>%
          filter(
            RG_mean >= input$rg_mean_range[1] & RG_mean <= input$rg_mean_range[2]
          )
      }
      
      #### filter Hydrodynamic radius mean range ###########################################
      if ("RH" %in% input$show_mucilbiochsummarycols) {
        mucilbiochsummary <- mucilbiochsummary %>%
          filter(
            RH_mean >= input$rh_mean_range[1] & RH_mean <= input$rh_mean_range[2]
          )
      }
    }
    
    # return at last
    mucilbiochsummary
  })

  output$summary <- renderDataTable({
    datasetSummary()
  },
  options = list(orderClasses = TRUE)
  )

  output$downloadSummaryData <- downloadHandler(
    filename = function() { 
      paste('AMUSE_summary_dataset_', format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), '.zip', sep='') 
    },
    content = function(file) {
      print(file)
      setwd(tempdir())
      df <- datasetSummary()
      saveTime <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
      baseFile <- paste('AMUSE_summary_dataset_', saveTime, sep='')
      # process csv file
      csvFile <- paste(baseFile, '.csv', sep='')
      write.csv2(df, csvFile, row.names=FALSE)
      print(csvFile)
      
      # process metadata file
      metadataFile <- paste(baseFile, '_metadata.txt', sep='')
      sink(metadataFile)
      cat(paste("Dataset File: ", csvFile, "\n",sep=''))
      cat(paste("Metadata File: ", metadataFile, "\n",sep=''))
      cat(paste("Date: ", saveTime, "\n",sep=''))
      cat("AMUSE summary Dataset\n")
      cat("DataTable dimensions: ")
      cat(dim(df), "\n")
      cat("Filters:\n")
      cat(paste("#AV numbers: ", length(unique(df$AV)), "\n", sep=''))
      if ("Gal_A" %in% input$show_mucilbiochsummarycols) 
        cat(paste("#Gal_A mean range: ", input$gala_mean_range[1], ':', input$gala_mean_range[2], "\n", sep=''))
      if ("OsesNeutres" %in% input$show_mucilbiochsummarycols)
        cat(paste("#OsesNeutres mean range: ", input$ozn_mean_range[1], ":", input$ozn_mean_range[2], "\n", sep=''))      
      if ("MM" %in% input$show_mucilbiochsummarycols)
        cat(paste("#MM mean range: ", input$mm_mean_range[1], ":", input$mm_mean_range[2], "\n", sep=''))
      if ("IV" %in% input$show_mucilbiochsummarycols)
        cat(paste("#IV mean range: ", input$iv_mean_range[1], ":", input$iv_mean_range[2], "\n", sep=''))
      if ("RG" %in% input$show_mucilbiochsummarycols)
        cat(paste("#RG mean range: ", input$rg_mean_range[1], ":", input$rg_mean_range[2], "\n", sep=''))
      if ("RH" %in% input$show_mucilbiochsummarycols)
        cat(paste("#RH mean range: ", input$rh_mean_range[1], ":", input$rh_mean_range[2], "\n", sep=''))
      cat("Contact: Joseph.Tran@versailles.inra.fr\n")
      sink()
      
      # zip
      zip(zipfile=file, files=c(csvFile, metadataFile))
    },
    contentType = "application/zip"
  )

  ### geoclimato ###########################################

  datasetClimate <- reactive({
    mandatory_geoloccols <- 1:8
    
    #### filter accessions by AV number ###########################################
    x <- input_avs()
    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      avs <- db.climate.geoloc$AV
    } else {
      avs <- x
    }
    
    #### append climate datasets ###########################################
    if (!is.null(input$show_climatodatasets)) {
      show_cols_idx <- unlist(vapply(input$show_climatodatasets, function(c){
        grep(pattern=paste(c,"_",sep=""), x=names(db.climate.all))
      },FUN.VALUE=c(Jan=0, Feb=0, Mar=0, Apr=0, May=0, Jun=0, Jul=0, Aug=0, Sep=0, Oct=0, Nov=0, Dec=0)))
      geoclimato <- db.climate.all %>%
        filter(AV %in% avs) %>%
        select(one_of(names(db.climate.all)[c(mandatory_geoloccols, show_cols_idx)]))
      #### only filter by accession on geoloc dataset
    } else {
      geoclimato <- db.climate.geoloc %>%
        filter(
          AV %in% avs
        )
    }

    #### filter accessions by geoloc qualities ###########################################
    if (!is.null(input$show_geolocquals)) {
      geoclimato <- geoclimato %>%
        filter(
          GEOLOC_QUAL %in% input$show_geolocquals
          )
    }
    
    #### filter latitude/longitude range ###########################################
    geoclimato <- geoclimato %>%
      filter(
        (LATITUDE >= input$lat_range[1] & LATITUDE <= input$lat_range[2]) | is.na(LATITUDE),
        (LONGITUDE >= input$long_range[1] & LONGITUDE <= input$long_range[2]) | is.na(LONGITUDE)
      ) 
    
    #### add gotomap ###########################################
    geoclimato <- geoclimato %>%
      mutate(GotoMap = paste('<a class="go-map" href="" data-lat="', LATITUDE, '" data-long="', LONGITUDE, '" data-av="', AV, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    
    # return at last
    geoclimato[,c(1:8, ncol(geoclimato), 9:ncol(geoclimato)-1)]
  })

  output$geoclimato <- renderDataTable({
    datasetClimate()
  },
  options = list(orderClasses = TRUE),
  escape = FALSE
  )

  output$downloadClimateData <- downloadHandler(
    filename = function() { 
      paste('AMUSE_climate_dataset_', format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), '.zip', sep='') 
    },
    content = function(file) {
      print(file)
      setwd(tempdir())
      df <- datasetClimate()
      saveTime <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
      baseFile <- paste('AMUSE_climate_dataset_', saveTime, sep='')
      # process csv file
      csvFile <- paste(baseFile, '.csv', sep='')
      write.csv2(df, csvFile, row.names=FALSE)
      print(csvFile)
      
      # process metadata file
      metadataFile <- paste(baseFile, '_metadata.txt', sep='')
      sink(metadataFile)
      cat(paste("Dataset File: ", csvFile, "\n",sep=''))
      cat(paste("Metadata File: ", metadataFile, "\n",sep=''))
      cat(paste("Date: ", saveTime, "\n",sep=''))
      cat("AMUSE climate Dataset\n")
      cat("DataTable dimensions: ")
      cat(dim(df), "\n")
      cat("Filters:\n")
      cat(paste("#AV numbers: ", length(unique(df$AV)), "\n", sep='')) 
      cat(paste("#LATITUDE range: ", input$lat_range[1], ':', input$lat_range[2], "\n", sep=''))
      cat(paste("#LONGITUDE range: ", input$long_range[1], ":", input$long_range[2], "\n", sep=''))      
      cat("Contact: Joseph.Tran@versailles.inra.fr\n")
      sink()
      
      # zip
      zip(zipfile=file, files=c(csvFile, metadataFile))
    },
    contentType = "application/zip"
  )

### incomplete ###########################################

#### Raw with less than 4 plants
  datasetRawlt4p <- reactive({
    #### filter accessions by AV number ###########################################
    x <- input_avs()
    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      avs <- unique(db.bioch.no4p$AV)
    } else {
      avs <- x
    }
    Rawlt4p <- db.bioch.no4p %>%
      filter(
        AV %in% avs
      )
    
    # return at last
    Rawlt4p
  })

  output$rlt4p <- renderDataTable({
    datasetRawlt4p()
  },
  options = list(orderClasses = TRUE)
  )

  output$downloadRawLessThan4PlantsData <- downloadHandler(
    filename = function() { 
      paste('AMUSE_raw_with_less_than_4_plants_dataset', format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), '.zip', sep='') 
    },
    content = function(file) {
      print(file)
      setwd(tempdir())
      df <- datasetRawlt4p()
      saveTime <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
      baseFile <- paste('AMUSE_raw_with_less_than_4_plants_dataset', saveTime, sep='')
      # process csv file
      csvFile <- paste(baseFile, '.csv', sep='')
      write.csv2(df, csvFile, row.names=FALSE)
      print(csvFile)
      
      # process metadata file
      metadataFile <- paste(baseFile, '_metadata.txt', sep='')
      sink(metadataFile)
      cat(paste("Dataset File: ", csvFile, "\n",sep=''))
      cat(paste("Metadata File: ", metadataFile, "\n",sep=''))
      cat(paste("Date: ", saveTime, "\n",sep=''))
      cat("AMUSE_raw_with_less_than_4_plants_dataset\n")
      cat("DataTable dimensions: ")
      cat(dim(df), "\n")
      cat("Contact: Joseph.Tran@versailles.inra.fr\n")
      sink()
      
      # zip
      zip(zipfile=file, files=c(csvFile, metadataFile))
    },
    contentType = "application/zip"
  )

#### Raw with NA/ND values
  datasetRawNand <- reactive({
    #### filter accessions by AV number ###########################################
    x <- input_avs()
    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      avs <- unique(db.bioch.incomplete$AV)
    } else {
      avs <- x
    }
    RawNand <- db.bioch.incomplete %>%
      filter(
        AV %in% avs
      )
    
    # return at last
    RawNand
  })
  
  output$rnand <- renderDataTable({
    datasetRawNand()
  },
  options = list(orderClasses = TRUE)
  )
  
  output$downloadRawNandData <- downloadHandler(
    filename = function() { 
      paste('AMUSE_raw_with_NA_ND_dataset', format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), '.zip', sep='') 
    },
    content = function(file) {
      print(file)
      setwd(tempdir())
      df <- datasetRawNand()
      saveTime <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
      baseFile <- paste('AMUSE_raw_with_NA_ND_dataset', saveTime, sep='')
      # process csv file
      csvFile <- paste(baseFile, '.csv', sep='')
      write.csv2(df, csvFile, row.names=FALSE)
      print(csvFile)
      
      # process metadata file
      metadataFile <- paste(baseFile, '_metadata.txt', sep='')
      sink(metadataFile)
      cat(paste("Dataset File: ", csvFile, "\n",sep=''))
      cat(paste("Metadata File: ", metadataFile, "\n",sep=''))
      cat(paste("Date: ", saveTime, "\n",sep=''))
      cat("AMUSE_raw_with_NA_ND_dataset\n")
      cat("DataTable dimensions: ")
      cat(dim(df), "\n")
      cat("Contact: Joseph.Tran@versailles.inra.fr\n")
      sink()
      
      # zip
      zip(zipfile=file, files=c(csvFile, metadataFile))
    },
    contentType = "application/zip"
  )

#### Geoclimato without gps coordinates
  datasetGeoclimatoNogps <- reactive({
    #### filter accessions by AV number ###########################################
    x <- input_avs()
    if ( x == "All" ||  x == "" || is.null(x) || is.na(x) ) {
      avs <- unique(acc_wogps$AV)
    } else {
      avs <- x
    }
    GeoclimatoNogps <- acc_wogps %>%
      filter(
        AV %in% avs
      )
    
    # return at last
    GeoclimatoNogps
  })

  output$gnogps <- renderDataTable({
    datasetGeoclimatoNogps()
  },
  options = list(orderClasses = TRUE)
  )
  
  output$downloadGeoclimatoNoGpsData <- downloadHandler(
    filename = function() { 
      paste('AMUSE_geoclimato_no_gps_dataset', format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss"), '.zip', sep='') 
    },
    content = function(file) {
      print(file)
      setwd(tempdir())
      df <- datasetGeoclimatoNogps()
      saveTime <- format(Sys.time(), "%Y-%m-%d_%Hh%Mm%Ss")
      baseFile <- paste('AMUSE_geoclimato_no_gps_dataset', saveTime, sep='')
      # process csv file
      csvFile <- paste(baseFile, '.csv', sep='')
      write.csv2(df, csvFile, row.names=FALSE)
      print(csvFile)
      
      # process metadata file
      metadataFile <- paste(baseFile, '_metadata.txt', sep='')
      sink(metadataFile)
      cat(paste("Dataset File: ", csvFile, "\n",sep=''))
      cat(paste("Metadata File: ", metadataFile, "\n",sep=''))
      cat(paste("Date: ", saveTime, "\n",sep=''))
      cat("AMUSE_geoclimato_no_gps_dataset\n")
      cat("DataTable dimensions: ")
      cat(dim(df), "\n")
      cat("Contact: Joseph.Tran@versailles.inra.fr\n")
      sink()
      
      # zip
      zip(zipfile=file, files=c(csvFile, metadataFile))
    },
    contentType = "application/zip"
  )
})