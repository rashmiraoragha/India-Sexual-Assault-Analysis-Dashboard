library('shiny')
library('shinydashboard')
library('ggplot2')
library('data.table')
library('dplyr')
source('server.R')

shinyUI(
  
  dashboardPage(
    
    skin = "blue",
    title = "India Sexual Assault - Analysis Dashboard (by Rashmi)",
    
    dashboardHeader(
      title = "Analysis Dashboard"
    ),
    
    dashboardSidebar(
      
      sidebarMenu(
        
        # Display Sidebar Labels
        
        menuItem("Summary", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Detailed Analysis", tabName = "analysisViz", icon = icon("bar-chart"))
      )
    ),
    
    dashboardBody(
      
      tabItems(
        
        # Display Dashboard
        
        tabItem(tabName = "dashboard",
                
                h2("India Sexual Assault - Analysis Dashboard (by Rashmi)"),
                br(),
                h4("This dashboard presents analysis of documented sexual assaults that have taken place in India from 2001 to 2013."),
                br(),
                h4("The objective of the dashboard is to analyze the trend of sexual assaults in India, victim groups of these assaults, and the usual assailants who carry out these henious crimes. By doing quantifiable analysis on sexual assaults in India, this dashboard aims to bring social awareness about sexual assaults in India."),
                br(),
                h4("All data sources used for this dashboard have been taken from Government of India website:", 
                   a("http://data.gov.in"), 
                   ". You can find the GitHub repository for this project here: ",
                   a("https://github.com/rashmiraoragha"),
                   ". Please feel free to fork and contribute!"),
                
                br(),
                
                h2("Key Metrics"),
                
                fluidRow(
                  
                  br(),
                  
                  valueBoxOutput("progressBox1"),
                  
                  valueBoxOutput("progressBox2"),
                  
                  valueBoxOutput("progressBox3")
                ),
                
                # h2("Street Information"),
                
                fluidRow(
                  
                  br(),
                  
                  valueBoxOutput("progressBox4"),
                  
                  valueBoxOutput("progressBox5"),
                  
                  valueBoxOutput("progressBox6")
                )
                
        ),
        
        tabItem(tabName = "analysisViz",
                
                # Display Pothole Graphs and Widgets
                
                fluidRow(
                  
                  
                  
                  box(status = "warning",
                      
                      selectInput("uiStates", 
                                  h3("Select State"), 
                                  choices = states,
                                  selected = 'All'
                      ),
                      
                      br(),
                      width = 4
                  ),
                  box(status = "warning",
                      selectInput("uiYears", 
                                  h3("Select Year"), 
                                  choices = years,
                                  selected = 'All'
                      ),
                      br(),
                      width = 4
                  ),
                  box(status = "warning",
                      selectInput("uiCrimes", 
                                  h3("Select Crime Type"), 
                                  choices = crimeTypes,
                                  selected = 'All'
                      ),
                      br(),
                      width = 4
                  )
                  
                ),
                
                fluidRow(
                  box(status = "warning",
                      plotOutput(outputId = "barPlot"),
                      width = 6
                  ),
                  box(status = "warning",
                      plotOutput(outputId = "piePlot"),
                      width = 6
                  )
                )
        )
      )
    )
  )
)