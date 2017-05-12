
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library( shinydashboard )
library( plotly )

header  <- dashboardHeader( 
  title = "AppAgile meets Twitter",
  titleWidth = "250"
)

sidebar <- dashboardSidebar(
  textInput( "termId", label = "Search term", value = "IoT" ),
  numericInput( "numberId", label = "Number of tweets", value = 200, min = 50, 
                max = 3200, step = 100 ),
  actionButton( "searchId", label = "Search" )
)

body    <- dashboardBody( 
  # tags$head(
  #   tags$link( rel = "stylesheet", type = "text/css", href = "custom.css" )
  # ),
  fluidRow(
    infoBoxOutput( "tweetsBox" ),
    infoBoxOutput( "toptermBox" )
  ),
  fluidRow(
    column( width = 6,
            box( 
              width = NULL,
              title = "Top Terms",
              collapsible = TRUE,
              plotlyOutput( "topterms" )
            ) ),
    column( width = 6,
            box( 
              width = NULL,
              collapsible = TRUE,
              title = "Term Cloud",
              plotOutput( "termcloud" )
            ) )
  )
)

dashboardPage( skin = "black", header, sidebar, body )
