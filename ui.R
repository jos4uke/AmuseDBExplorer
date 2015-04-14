
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
                  selectInput("mapPick", strong("Background Map"),c("OpenStreetMap" = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", # works!
                                                            "MapQuestOSM" = "http://oatile3.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpg", # works!
                                                            "MapQuestOpen.Aerial"= "http://oatile3.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg"), # works!
                              selected = c("http://oatile3.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpg")) # default: MapQuestOSM
    ),
    
    # Shiny versions prior to 0.11 should use class="modal" instead.
    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 150, left = "auto", right = 25, bottom = "auto",
                  width = 330, height = "auto",
                  
                  h1("AmuseDB explorer"),
                  
                  textInput(inputId = "select_av_map", label = strong("Filter accessions by AV number"), value = "All")
    ),
    
    # cite
    tags$div(id="cite",
             strong('Data compiled for ', tags$em('ANR AMUSE project 2009-2012 (Helen North, IJPB)'), ' by Joseph Tran (IJPB).')
    )
  ),
  
#   tabPanel("Bioch data explorer (all plants)",
#     fluidRow(
#            dataTableOutput("df.bioch.all")
#     )
#   ),
  
  tabPanel("Search arabidopsis mucilage dataset",
    fluidPage(
      title="Search arabidopsis mucilage dataset",
      headerPanel("Search arabidopsis mucilage dataset"),
      tags$br(),
      tags$div("Raw, summary and geoloc climate datasets are available."),
      tags$br(),
      ### dataset #########################################
      fluidRow(
        column(4,
           selectInput("dataset", label = strong("Select dataset"), 
                       choices = list("Raw dataset" = "raw", "Summary dataset" = "summary", "Geo-climato dataset" = "geoclimato"), 
                       selected = list("Raw dataset" = "raw"))
               )
        ),
      ### description #########################################
      conditionalPanel(
        'input.dataset === "raw"',
        tags$div("Raw tab view provides mucilage dataset raw values: each table entry is related to one accession, one culture, one seed pool and one repetition number.")
      ),
      conditionalPanel(
        'input.dataset === "summary"',
        tags$div("Summary tab view provides descriptive statistics on mucilage dataset by accession. Important: note that only accessions with 4 plants are considered in the summary statistics.")
      ),
      conditionalPanel(
        'input.dataset === "geoclimato"',
        tags$div("Geoclimato tab view provides geo-localisation and climate datasets by accession."),
        tags$a(href="https://www.pik-potsdam.de/members/cramer/climate", "(Cramer&Leemans database, version 2.1)")
      ),  
      ### constant #########################################
      fluidRow(
        tags$br(),
        column(4,
          textInput(inputId = "select_av", label = strong("Filter accessions by AV number"), value = "All")
          ),
        column(4,
        tags$br(),
        tags$div("By default, all data is shown. Provide accession AV number to filter dataset. 
                 Multiple AV numbers is allowed separated by blank space or newline.")
        )
        ),
        
      ### raw conditional panel #########################################
      conditionalPanel(
        'input.dataset === "raw"',
        #### mucilage biochemical datasets
        fluidRow(
          tags$br(),
          column(4,
            selectizeInput("show_mucilbiochcols", label = strong("Select mucilage biochemical datasets"), 
                    choices = choices_mucilbiochcols, 
                    selected = choices_mucilbiochcols,
                    multiple = TRUE)
            ),
          column(4,
            tags$br(),
            tags$div("By default, all datasets are selected. Delete dataset in the list, or select dataset from the drop-down menu. 
                              Multiple choice is allowed.")
          )
          ),
        fluidRow(
          column(5,
                 tags$br(),
                 tags$div("Sliders allow you to filter dataset on the values range."),
                 tags$br()
                 )
          ),
        fluidRow(
          column(4,
            #### filering Gal_A dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochcols.indexOf("Gal_A") >= 0',
              uiOutput("dynamic_gala_slider")
              ),
            #### filtering Neutral oses dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochcols.indexOf("OsesNeutres") >= 0',
              uiOutput("dynamic_ozn_slider")
              )
            ),
          column(4,
            #### filtering Molecular weight dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochcols.indexOf("MW") >= 0',
              uiOutput("dynamic_mw_slider")
              ),
            #### filtering Intrinsic viscosity dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochcols.indexOf("IV") >= 0',
              uiOutput("dynamic_iv_slider")
              )
            ),
          column(4,
            #### filtering Giration radius dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochcols.indexOf("RG") >= 0',
              uiOutput("dynamic_rg_slider")
              ),
            #### filtering Hydrodynamic radius dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochcols.indexOf("RH") >= 0',
              uiOutput("dynamic_rh_slider")
              )
            )
          ),
        tags$hr(),
        tags$h2("Results"),
        wellPanel(
          tags$div("Download the filtered raw dataset results in csv format in zipped archive."),
          downloadButton('downloadRawData', 'Download zip file')
          ),
        tags$br(),
        dataTableOutput("raw")
        ),
      
      ### summary  #########################################
      conditionalPanel(
        'input.dataset === "summary"',
        #### mucilage biochemical datasets
        fluidRow(
          tags$br(),
          column(4,
            selectizeInput("show_mucilbiochsummarycols", label = strong("Mucilage biochemical dataset"), 
                       choices = choices_mucilbiochcols, 
                       selected = choices_mucilbiochcols,
                       multiple = TRUE)
            ),
          column(4,
                 tags$br(),
                 tags$div("By default, all datasets are selected. Delete dataset in the list, or select dataset from the drop-down menu. 
                          Multiple choice is allowed.")
                 )
          ),
        fluidRow(
          column(5,
                 tags$br(),
                 tags$div("Sliders allow you to filter dataset on the values range."),
                 tags$br()
          )
        ),
        fluidRow(
          column(4,
            #### filering Gal_A mean dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochsummarycols.indexOf("Gal_A") >= 0',
              uiOutput("dynamic_gala_mean_slider")
              ),
            #### filtering Neutral oses mean dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochsummarycols.indexOf("OsesNeutres") >= 0',
              uiOutput("dynamic_ozn_mean_slider")
            )
            ),
          column(4,
            #### filtering Molecular weight mean dataset #########################################
            conditionalPanel(
             'input.show_mucilbiochsummarycols.indexOf("MW") >= 0',
             uiOutput("dynamic_mw_mean_slider")
            ),
            #### filtering Intrinsic viscosity mean dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochsummarycols.indexOf("IV") >= 0',
              uiOutput("dynamic_iv_mean_slider")
              )
            ),
          column(4,
            #### filtering Giration radius mean dataset #########################################
            conditionalPanel(
             'input.show_mucilbiochsummarycols.indexOf("RG") >= 0',
             uiOutput("dynamic_rg_mean_slider")
            ),
            #### filtering Hydrodynamic radius mean dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochsummarycols.indexOf("RH") >= 0',
              uiOutput("dynamic_rh_mean_slider")
            )     
                 )
          ),
        tags$hr(),
        tags$h2("Results"),
        wellPanel(
          tags$div("Download the filtered summary dataset results in csv format in zipped archive."),
          downloadButton('downloadSummaryData', 'Download zip file')
        ),
        tags$br(),
        tags$p(strong("Help:")),
        helpText('_min, _Q1, _median, _mean, _Q3, _max, _IQR, _sd suffixes give respectively minimum, first quartile, median, mean, third quartile, maximum, interquartile range and standard deviation values for each dataset.'),
        dataTableOutput("summary")
        ),
      
      ### geoclimato  #########################################
      conditionalPanel(
        'input.dataset === "geoclimato"',
        #### geoclimato datasets
        fluidRow(
          tags$br(),
          column(4,
                 selectizeInput("show_climatodatasets", label = strong("Select climate datasets"), 
                                choices = choices_climatodatasets, 
                                selected = choices_climatodatasets,
                                multiple = TRUE)
          ),
          column(4,
                 tags$br(),
                 tags$div("By default, all datasets are selected. Delete dataset in the list, or select dataset from the drop-down menu. 
                          Multiple choice is allowed.")
          )
        ),
        #### gps coordinates filtering
        fluidRow(
          tags$br(),
          tags$div("Sliders allow you to filter dataset on the values range."),
          tags$br(),
          column(4,
                 wellPanel(
                   #### Latitude slider ###########################################
                     sliderInput("lat_range", strong("Latitude range:"),
                                 min = min_lat, max = max_lat, value = c(min_lat, max_lat))
                   )
                 ),
          column(4,
                 wellPanel(
                   #### Longitude slider ###########################################
                   sliderInput("long_range", strong("Longitude range:"),
                               min = min_long, max = max_long, value = c(min_long, max_long))
                 )
          )
          ),
        tags$hr(),
        tags$h2("Results"),
        tags$pre("Legend: 
  - 'MONTHLY HOURS OF SUNSHINE' = 'mhs',
  - 'MONTHLY PRECIPITATION' = 'mp',
  - 'MEAN MONTHLY NUMBER OF RAIN DAYS' = 'mmnrd',
  - 'MEAN MONTHLY TEMPERATURE' = 'mmt',
  - 'MEAN MONTHLY TEMP RANGE' = 'mmtr'"),
        wellPanel(
          tags$div("Download the filtered climate datasets results in csv format in zipped archive."),
          downloadButton('downloadClimateData', 'Download zip file')
        ),
        tags$br(),
        dataTableOutput("geoclimato")
        
        )
      )
    )
))
