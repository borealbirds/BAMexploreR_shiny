tagList(
  navbarPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "lux"),
    id = 'tabs',
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/style.css", rel = "stylesheet"))
    ),
    title = HTML('<div style="margin-top: -10px;"><a href="https://borealbirds.ca/" target="_blank"><img src="bam.png" height="50"></a></div>'),
    windowTitle = "BAM National Model Explorer",
    tabPanel("Welcome", value = 'intro'),
    tabPanel("Explore the data", value = 'data'),
    tabPanel("Analyze", value = 'dist'),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://borealbirds.ca/" target="_blank">BAM Homepage</a>'),
               HTML('<a href="https://github.com/borealbirds/BAMexploreR_shiny" target="_blank">Github Page</a>'),
               HTML('<a href="https://github.com/borealbirds/BAMexploreR_shiny/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: bamp@ualberta.ca" target="_blank">Send Email</a>')),
    tabPanel(NULL, icon = icon("power-off"), value = "_stopapp"),
  ),
  tags$div(
    useShinyjs(),
    class = "container-fluid",
    conditionalPanel(
      condition="input.tabs == 'intro'",
      fluidRow(
        column(12,
          tags$img(src = "bird4.png",
                   height = "250px", width = "100%",
                   style = "display: block; margin: 0px auto -10px auto;"))),
      fluidRow(
        column(12, wellPanel(
          includeMarkdown("Rmd/text_intro_tab.Rmd")))
        )
      ),
      # OBTAIN Data ####
    conditionalPanel(
        condition="input.tabs == 'data'",
        fluidRow(
          column(3,
              tabsetPanel(
                   tabPanel("Component",
                            div(style = "color: white !important; font-size:20px; font-weight: bold; margin-top: 20px;", "Explore National Model output", class = "explore_module"),
                            exploreUI("explore_module")
                   ),
                   tabPanel(
                     'Module Guidance', icon = icon("circle-info"),
                     uiOutput('gtext_module')
                   )
                 )
        ),
        column(6,
               tabsetPanel(
                 id = 'main',
                 tabPanel(
                   'Map',
                   leaflet::leafletOutput("myMap", height = 700),
                 )
               )
        ),
        column(3,
               tabsetPanel(
                 tabPanel("Population density",
                          bandUI("explore_module"),
                          sppUI("explore_module"),
                          dwdUI("explore_module")
                          
                 ),
                 tabPanel("Land",
                          div("Graph or table", class = "moduleName")
                 ),
               )
        )
      )
    ),
    conditionalPanel(
          condition="input.tabs == 'dist'",
          column(4,
                 tabsetPanel(
                   tabPanel("Component",
                            div("Analyze the data", class = "moduleName"),
                            #help_comp_ui("distHelp"),
                            uiOutput("buffer_module"),
                   ),
                   tabPanel("Statistics table", tableOutput("buffStats"))
                 )
               )
        )
  )
)
