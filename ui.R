tagList(
  navbarPage(
    theme = bslib::bs_theme(version = 4, bootswatch = "lux"),
    id = 'tabs',
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/style_blanc.css", rel = "stylesheet")
                ),
      tags$div(
        style = "position: absolute; right: 20px; top: 10px;",
        actionButton(
          "reload_btn",
          label = "Reload",
          icon = icon("refresh"),
          style = "color: white; background-color: rgb(30, 80, 85); border: none; font-size: 16px; margin-top: 8px;"
        ),
        style = "position: absolute; right: 20px; top: 10px;" # adjust position
      )
    ),
    title = HTML('<div style="margin-top: -10px;"><a href="https://borealbirds.ca/" target="_blank"><img src="bam.png" height="50"></a></div>'),
    windowTitle = "BAM National Model Explorer",
    tabPanel("Welcome", value = 'intro'),
    tabPanel("Access the data", value = 'data'),
    tabPanel("Population distribution", value = 'popstats'),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://borealbirds.ca/" target="_blank">BAM Homepage</a>'),
               HTML('<a href="https://github.com/borealbirds/BAMexploreR_shiny" target="_blank">Github Page</a>'),
               HTML('<a href="https://github.com/borealbirds/BAMexploreR_shiny/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: bamp@ualberta.ca" target="_blank">Send Email</a>'))
  ),
  tags$div(
    useShinyjs(),
    #class = "container-fluid",
    conditionalPanel(
      condition="input.tabs == 'intro'",
      #fluidRow(
      #  column(12,
      #    tags$img(src = "bird5.png",
      #             height = "350px", width = "100%",
      #             style = "display: block; margin: 0px auto -10px auto;"))),
          # Wrapper div to control background color
      div(style = "background-color: white; width: 100vw; margin: 0; padding: 0; display: flex; justify-content: center;",
          tags$img(src = "bird.png", height = "350px",  style = "display: block; object-fit: contain; width: 100%; max-width: none;")),
      fluidRow(
        column(12,   div(id = "markdown-content",       # <--- this ID links to your CSS
          includeMarkdown("Rmd/text_intro_tab.md")))
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
                 tabPanel("Download",
                          bandUI("explore_module"),
                          sppUI("explore_module"),
                          dwdUI("explore_module")
                          
                 )
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
