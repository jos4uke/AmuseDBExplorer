
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(leaflet))

shinyUI(navbarPage(strong("AmuseDBExplorer"), id="nav",
  
  tabPanel("Interactive map",
      fluidPage(      
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),

        fluidRow(
          column(12,
                 uiOutput("mapp", inline=TRUE)
                 )
          ),
        absolutePanel(top = 75, left = "auto", right = 35, bottom = "auto",
                    selectInput("mapPick", strong("Background Map"),c("OpenStreetMap" = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", # works!
                                                              "MapQuestOSM" = "http://oatile3.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpg", # works!
                                                              "MapQuestOpen.Aerial"= "http://oatile3.mqcdn.com/tiles/1.0.0/sat/{z}/{x}/{y}.jpg"), # works!
                                selected = c("http://oatile3.mqcdn.com/tiles/1.0.0/map/{z}/{x}/{y}.jpg")) # default: MapQuestOSM
        ),
      
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", 
                    draggable = TRUE, top = 160, left = "auto", right = 40, bottom = "auto",
                    width = 330, height = "auto",
                    
                    h3("Search arabidopsis accessions"),
                    textInput(inputId = "select_av_map", label = strong("Fill in AV number(s)"), value = "All"),
                    tags$br(),
                    selectizeInput("show_accessions_name_map", label = strong("Select accessions by name"), 
                                   choices = choices_acc_names_map,
                                   selected = "All",
                                   multiple = TRUE),
                    hr(),
                    h4("Map information"),
                    htmlOutput(
                      outputId = 'mapdesc', inline=TRUE
                    ),
                    tags$br(),
                    h4("Legend"),
                    plotOutput(outputId = 'legend', width = 200, height = 200),
                    # conditional reset button
                    conditionalPanel(
                      '!!input.goto',
                      tags$br(),
                      actionButton("resetgomap", label = "Reset Go to map icon")
                      )
      ),
      
      # AMUSEDB explorer frontend title
      fluidRow(
        column(8, offset=3,
               h3('Frontend interface to', strong('AMUSEDB'), ': Arabidopsis Mucilage Natural Variability Database')
        )
      )
    ),
    # cite
    tags$br(),
    tags$div(
             strong('Data compiled for ', tags$em('AMUSE ANR project 2009-2012 (Helen North (IJPB/INRA Versailles) and Marie-Christine Ralet (BIA/INRA Nantes))'), ' by Joseph Tran (IJPB/INRA Versailles).')
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
                       choices = list("Raw dataset" = "raw", "Summary dataset" = "summary", "Geo-climato dataset" = "geoclimato", "Incomplete dataset" = "incomplete"), 
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
      conditionalPanel(
        'input.dataset === "incomplete"',
        tags$div("Incomplete tab view provides incomplete datasets (raw with less than 4 plants, with NA/ND values, geoclimato without gps coordinates).")
      ),
      ### constant #########################################
      #### search by AV number
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
      #### search by name
      fluidRow(
        tags$br(),
        column(4,
               selectizeInput("show_accessions_name", label = strong("Select accessions by name"), 
                              choices = choices_acc_names,
                              selected = "All",
                              multiple = TRUE)
               ),
        column(4,
               tags$br(),
               tags$div("You can search accession by name. Multiple choice is allowed.")
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
            #### filtering Mean Molar Mass dataset #########################################
            conditionalPanel(
              'input.show_mucilbiochcols.indexOf("MM") >= 0',
              uiOutput("dynamic_mm_slider")
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
          tags$br(),
          column(4,
                 checkboxInput("groupByCulture", label = strong("group by culture"), value = FALSE)
                 ),
          column(4,
                 tags$br(),
                 tags$div("By default, summary dataset is grouped by accession AV number/NAME. Checking the 'group by culture' will summary the dataset by accession AV number/NAME and culture.")
                 )
          ),
        fluidRow(
          tags$br(),
          column(4,
                 selectizeInput("show_summarycols", label = strong("Summary dataset columns"), 
                                choices = choices_summarycols, 
                                selected = choices_summarycols[c(4,8)],
                                multiple = TRUE)
          ),
          column(4,
                 tags$br(),
                 tags$div("By default, only mean and standard deviation columns are selected. You can add additional or remove available summary columns from the drop-down menu. 
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
        conditionalPanel('input.show_summarycols.indexOf("_mean") >= 0',
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
              #### filtering Mean Molar Mass mean dataset #########################################
              conditionalPanel(
               'input.show_mucilbiochsummarycols.indexOf("MM") >= 0',
               uiOutput("dynamic_mm_mean_slider")
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
        #### geoclimato datasets
        fluidRow(
          tags$br(),
          column(4,
                 selectizeInput("show_geolocquals", label = strong("Select geoloc qualities"), 
                                choices = choices_geoloc_qual, 
                                selected = choices_geoloc_qual,
                                multiple = TRUE)
          ),
          column(4,
                 tags$br(),
                 tags$div("By default, all geoloc qualities are selected. Delete item in the list, or select item from the drop-down menu. 
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
      ),
    
      ### incomplete  #########################################
      conditionalPanel(
        'input.dataset === "incomplete"',
        #### incomplete datasets
        fluidRow(
          tags$br(),
          column(4,
                 selectizeInput("show_incompletedatasets", label = strong("Select incomplete datasets"), 
                                choices = choices_incompletedatasets, 
                                selected = choices_incompletedatasets,
                                multiple = TRUE)
          ),
          column(4,
                 tags$br(),
                 tags$div("By default, all datasets are selected. Delete dataset in the list, or select dataset from the drop-down menu. 
                          Multiple choice is allowed.")
                 )
          ),
        tags$hr(),
        tags$h2("Results"),
        #### raw with less than 4 plants
        conditionalPanel('input.show_incompletedatasets.indexOf("rlt4p") >= 0',
          tags$h3("raw with less than 4 plants dataset"),
          wellPanel(
            tags$div("Download the raw dataset with less than 4 plants dataset results in csv format in zipped archive."),
            downloadButton('downloadRawLessThan4PlantsData', 'Download zip file')
          ),
          tags$br(),
          dataTableOutput("rlt4p")
        ),
        tags$br(),
        #### raw with NA/ND values
        conditionalPanel('input.show_incompletedatasets.indexOf("rnand") >= 0',
          tags$h3("raw with NA/ND values dataset"),
          wellPanel(
           tags$div("Download the raw dataset with NA/ND values dataset results in csv format in zipped archive."),
           downloadButton('downloadRawNandData', 'Download zip file')
          ),
          tags$br(),
          dataTableOutput("rnand")
        ),
        tags$br(),
        #### geoclimato without gps coordinates
        conditionalPanel('input.show_incompletedatasets.indexOf("gnogps") >= 0',
          tags$h3("geoclimato without gps coordinates dataset"),
          wellPanel(
           tags$div("Download the geoclimato dataset without gps coordinates dataset results in csv format in zipped archive."),
           downloadButton('downloadGeoclimatoNoGpsData', 'Download zip file')
          ),
          tags$br(),
          dataTableOutput("gnogps")
        ),
        tags$br()
        #### geoclimato with NA/ND values
        
      )
    ),

  tabPanel("Database information",
   fluidPage(
     # AMUSEDB infos
     fluidRow(
       column(8, offset=3,
              h3(strong('AMUSEDB'), ': Arabidopsis Mucilage Natural Variability Database'),
              htmlOutput(
                outputId = 'desc'
              )
       )
     )
   )
  )
))
