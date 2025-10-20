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
    tabPanel("Predictors importance", value = 'pred'),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://borealbirds.ca/" target="_blank">BAM Homepage</a>'),
               HTML('<a href="https://github.com/borealbirds/BAMexploreR_shiny" target="_blank">Github Page</a>'),
               HTML('<a href="https://github.com/borealbirds/BAMexploreR_shiny/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: bamp@ualberta.ca" target="_blank">Send Email</a>'))
  ),
  tags$div(
    useShinyjs(),
    
    # Intro tab
    conditionalPanel(
      condition="input.tabs == 'intro'",
      div(style = "background-color: white; width: 100vw; margin: 0; padding: 0; display: flex; justify-content: center;",
          tags$img(src = "bird.png", height = "350px",  style = "display: block; object-fit: contain; width: 100%; max-width: none;")),
      fluidRow(
        column(12, div(id = "markdown-content", includeMarkdown("Rmd/text_intro_tab.md")))
      )
    ),
    
    # Main layout for data + popstats tabs
    conditionalPanel(
      condition="input.tabs == 'data' || input.tabs == 'popstats' || input.tabs == 'pred'",
      fluidRow(
        # Left panel changes depending on tab
        column(3,
               conditionalPanel(
                 condition="input.tabs == 'data'",
                 tabsetPanel(
                   tabPanel("Component",
                            div(style = "color: white !important; font-size:20px; font-weight: bold; margin-top: 20px;", 
                                "Explore National Model output", class = "explore_module"),
                            exploreUI("explore_module")
                   ),
                   tabPanel("Module Guidance", icon = icon("circle-info"), div(style = "color: white !important; font-size: 14px; font-family: 'Cormorant Garamond', serif;", includeMarkdown("./Rmd/gtext_data.Rmd")))
                 )
               ),
               conditionalPanel(
                 condition="input.tabs == 'popstats'",
                 tabsetPanel(
                   tabPanel("Component", popUI("pop_module")),
                   tabPanel("Module Guidance", icon = icon("circle-info"), div(style = "color: white !important; font-size: 14px; font-family: 'Cormorant Garamond', serif;", includeMarkdown("./Rmd/gtext_popstats.Rmd")))
                 )
               ),
               conditionalPanel(
                 condition="input.tabs == 'pred'",
                 tabsetPanel(
                   tabPanel("Component", predUI("pred_module")),
                   tabPanel("Module Guidance", icon = icon("circle-info"), div(style = "color: white !important; font-size: 14px; font-family: 'Cormorant Garamond', serif;", includeMarkdown("./Rmd/gtext_pred.Rmd")))
                 )
               )
        ),
        # Map always visible in the middle
        column(6,
               conditionalPanel(
                 condition="input.tabs == 'data' || input.tabs == 'popstats'" ,
                 tabsetPanel(id ="centerPanel",
                   tabPanel("MapView", 
                            leafletOutput("myMap", height = 700) %>% withSpinner(),
                            #popTable("pop_module")
                            conditionalPanel(
                              condition = "input.tabs == 'popstats'",
                              popTable("pop_module")
                            )
                   ),
                   tabPanel("Species occurrence",
                            conditionalPanel(
                              condition = "input.tabs == 'popstats' && input['pop_module-popAnalysis'] == 'popArea'",
                              popOccUI("pop_module")
                            )
                   ) 
                 )
               ),
               conditionalPanel(
                 condition="input.tabs == 'pred'" , 
                 barchartUI("pred_module")
               )
        ),
        column(3,
               #uiOutput("rightPanel")
               conditionalPanel(
                 condition="input.tabs == 'data'" , 
                 bandUI("explore_module"),
                 sppUI("explore_module"),
                 dwdUI("explore_module")
               ),
               conditionalPanel(
                 condition="input.tabs == 'popstats'" , 
                 popSppUI("pop_module"),
                 popDwdUI("pop_module")
               ),
               conditionalPanel(
                 condition="input.tabs == 'pred'" , 
                 axisUI("pred_module"),
                 predDwdUI("pred_module")
               )
               
        )
      )
    )
  )
)
