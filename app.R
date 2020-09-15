# Social media campaigns R app

# loading ----
library(shiny)
library(shinydashboard)
## Load libraries on shiny server
if (FALSE) {
  library(shinycssloaders)
  #library(shinyBS)
  #library(shinyjs)
  library(patchwork)
  library(plotly)
  library(xlsx)
  library(config)
  #library(RMySQL)
  #library(png)
  #library(keyring)
  #library(shiny)
  library(dplyr)
  library(DT)
  library(data.table)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
  library(reshape2)
  library(ggpubr)
  library(grid)
  library(gtable)
  library(rsconnect)
  library(DBI)
  library(devtools)
}
for (packages in c(
  'shiny',
  'dplyr',
  'DT',
  'DBI',
  'data.table',
  'RMySQL',
  #'RMariaDB',
  'datasets',
  'shinydashboard',
  'ggplot2',
  'tidyr',
  'devtools',
  'patchwork',
  'shinycssloaders',
  #'timevis',
  'gridExtra',
  'ggpubr',
  'gtable',
  'plotly',
  'devtools',
  'reshape2',
  'grid',
  "xlsx"
))
for (package in packages) {
  if (!require(package, character.only = T, quietly = T)) {
    #install.packages(package, repos = "http://cran.us.r-project.org")
    library(package, character.only = T)
  }
}
# Call functions
source('calculations.R', local = TRUE)

# Retrieve datasets and plots
final_df <- populate_dataframe()
platform_list <- unique(final_df$platform)
new_platform_list <- c('All Platforms', platform_list)
months_list <- unique(final_df$reported_month)
audience_list <- unique(final_df$audience)
new_audience_list <- c('All Audiences', audience_list)
cities_list <- unique(final_df$city)
new_cities_list <- c('All Cities', cities_list)

# app.R ----
# header ----
header <- dashboardHeader(# Application title
  title = "Social Media Analytics")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard")
    ),
    menuItem(
      "Platform View",
      tabName = "platform",
      icon = icon("badge-ad")
    ),
    menuItem(
      "Audience View",
      tabName = "audience",
      icon = icon("people")
    ),
    menuItem(
      "City View",
      tabName = "city",
      icon = icon("house-door")
    )
  )
)
# body ----
body <- dashboardBody( tabItems(
    tabItem(
      tabName = 'dashboard', # ----
        tags$h1(tags$b("Summary")),
        br(),
      valueBoxOutput("media", width = 3),
      box(title= 'MEDIA TYPES', background='black', width=3,color='black', tableOutput('media_table')),
      #infoBoxOutput('media2'),
      valueBoxOutput('platforms', width = 3),
      box(title= 'PLATFORMS', background='black',color='black', width=3,tableOutput('platform_table')),
      #infoBoxOutput('platforms2'),
      valueBoxOutput("audiences", width = 3),
      box(title= 'AUDIENCES', background='black',color='black', width=3,tableOutput('audience_table')),
      #infoBoxOutput('audiences2'),
      valueBoxOutput('cities', width = 3),
      box(title= 'CITIES', background='black',color='black', width=3,tableOutput('cities_table')),
      #infoBoxOutput('cities2'),
      valueBoxOutput('entries', width = 2),
      box(background='black',color='black', width=3,downloadButton("dataset","Download Dataset")),
      fluidRow(downloadButton("dataset","Download Dataset"))
    ),
    tabItem(
      tabName = 'platform',
      tags$h1(tags$b("By Platform")),
      selectInput(
        "platform_choice",
        h3("Select platform"),
        choices = new_platform_list,
        selected = 1
      ),
     uiOutput('platform_plots')
    ), 
    tabItem(
      tabName = 'audience',
      tags$h1(tags$b("By Audience")),
      selectInput(
        "audience_choice",
        h3("Select Audience"),
        choices = new_audience_list,
        selected = 1
      ),
      uiOutput('audience_plots')
      # conditionalPanel(condition = 'input.audience_choice!=NULL',
      #                  uiOutput('audience_plots'))
    ),
    tabItem(
      tabName = 'city',
      tags$h1(tags$b("By City")),
        selectInput(
          "city_choice",
          h3("Select City"),
          choices = new_cities_list,
          selected = 1
        ),
      uiOutput('city_plots'),tags$h3('mierda')
      #   conditionalPanel(condition = 'input$city_choice!=NULL',
      #                    uiOutput('city_plots'))
    )
  )
)

  # server----
  server <- function(input, output) {
    # Dashboard ----
      # valueBoxes
    output$platforms <- renderValueBox({
      valueBox(
        value = length(unique(final_df$platform)),
        subtitle = "platforms",
        color = 'maroon',
        icon = icon("database")
      )
    })
    
    
    output$platforms2<-renderInfoBox({infoBox('Platforms',
                                              data.frame(platform_list),
                                              icon=icon('list'),
                                              color='purple', 
                                              fill=TRUE)})
    output$media2<-renderInfoBox({infoBox('Media Types',
                                              unique(final_df$medium),
                                              icon=icon('list'),
                                              color='blue', 
                                              fill=TRUE)})
    output$audiences2<-renderInfoBox({infoBox('Audiences',
                                          audience_list,
                                          icon=icon('list'),
                                          color='green', 
                                          fill=TRUE)})
    output$cities2<-renderInfoBox({infoBox('Cities',
                                              cities_list,
                                              icon=icon('list'),
                                              color='red', 
                                              fill=TRUE)})
    output$media <- renderValueBox({
      valueBox(
        value = 2,
        subtitle = "media types",
        color = 'olive',
        icon = icon("database")
      )
    })
    output$audiences <- renderValueBox({
      valueBox(
        value = length(unique(final_df$audience)),
        subtitle = "audiences",
        color = 'orange',
        icon = icon("database")
      )
    })
    output$cities <- renderValueBox({
      valueBox(
        value = length(unique(final_df$city)),
        subtitle = "cities",
        color = 'purple',
        icon = icon("database")
      )
    })
    output$entries <- renderValueBox({
      valueBox(
        value = nrow(final_df),
        subtitle = "datapoints",
        color = 'maroon',
        icon = icon("database")
      )
    })
      # tables
    output$platform_table<-renderTable(data.frame(platform_list),
                                       rownames = FALSE,
                                       colnames = FALSE,
                                       bordered=FALSE,
                                       align='l',
                                       spacing='xs')
    output$audience_table<-renderTable(data.frame(audience_list),
                                       rownames = FALSE,
                                       colnames = FALSE,
                                       bordered=FALSE)
    output$cities_table<-renderTable(data.frame(cities_list),
                                     rownames = FALSE,
                                     colnames = FALSE,
                                     bordered=FALSE)
    output$media_table<-renderTable(data.frame(unique(final_df$medium)),
                                    rownames = FALSE,
                                    colnames = FALSE,
                                    bordered=FALSE)
    output$dataset <- downloadHandler(
      filename = function() {
        paste0('Social_Media_Analytics_downloaded_', Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(final_df, file)
      }
    )

    # platform View ----
    #selectedPlatform <- reactive({input$platform_choice })
    observeEvent(input$platform_choice, {
      if(!is.null(input$platform_choice)){
        platform_outputted <-analyze_platform(final_df, input$platform_choice)
        print(platform_outputted[[1]])
        print(input$platform_choice)
        numPlots<-length(platform_outputted)
        print(numPlots)
        output$platform_plots <- renderUI({
          plot_list <- list()
          plot_list <-
            lapply(1:numPlots, function(i) {
              plotname <- paste("plot",'platform', i, sep="")

              plotlyOutput(plotname, height = 280, width = 250)
            })
        })
        for (i in 1:length(platform_outputted)){
          local({
            my_i <- i
            plotname <- paste("plot",'platform', my_i, sep="")
            output[[plotname]] <- renderPlotly(platform_outputted[[i]])
            })
          }
        }
      })
    # audience View ---
    #selectedaudience <- reactive({input$audience_choice })
    observeEvent(input$audience_choice, {
      if(!is.null(input$audience_choice)){
        audience_outputted <-analyze_audience(final_df, input$audience_choice)
        #print(audience_outputted[[1]])
        print(input$audience_choice)
        numPlots2<-length(audience_outputted)
        print(numPlots2)
        output$audience_plots <- renderUI({
          plot_list2 <- list()
          plot_list2 <-
            lapply(1:numPlots2, function(i) {
              plotname2 <- paste("plot",'audience', i, sep="")
              plotlyOutput(plotname2, height = 280, width = 250)
            })
        })
        for (i in 1:numPlots2){
          local({
            my_i <- i
            plotname2 <- paste("plot",'audience', my_i, sep="")
            output[[plotname2]] <- renderPlotly(audience_outputted[[i]])
          })
        }
      }
    })
    
    # city View ---
    selectedCity <- reactive({input$city_choice })
    observeEvent(input$city_choice, {
      if(!is.null(input$city_choice)){
        city_outputted <-analyze_city(final_df, input$city_choice)
        #print(city_outputted[[1]])
        print(input$city_choice)
        numPlots3<-length(city_outputted)
        print(numPlots3)
        output$city_plots <- renderUI({
          plot_list3 <- list()
          plot_list3 <-
            lapply(1:numPlots3, function(i) {
              plotname3 <- paste("plot",'city', i, sep="")

              plotlyOutput(plotname3, height = 280)
            })
        })
        for (i in 1:numPlots3){
          local({
            my_i <- i
            plotname3 <- paste("plot",'city', my_i, sep="")
            output[[plotname3]] <- renderPlotly(city_outputted[[i]])
          })
        }
      }
    })
}
ui <- dashboardPage(title='Social Media Analytics', skin= 'black', header, sidebar, body)
# Run the application
shinyApp(ui = ui, server = server)
  