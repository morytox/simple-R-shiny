
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library( shinydashboard )
library( wordcloud )
source( "helperFunctions.R" )

function( input, output, session ) 
{
  oauth_tw_app()
  # AppAgile colors
  magenta    <- "#E80071"
  gray       <- "#D6D6D6"
  darkgray   <- "#6A6A6A"
  appagile   <- c( gray, darkgray, "black", magenta ) 
  
  # search twitter via API and transform to term document matrix
  tdm <- eventReactive( input$searchId, 
                               {
                                 twDataFrame <- search_twitter( input$termId, n = input$numberId, 
                                                                lang = "en" )
                                 twCorpus    <- transform_tweets( twDataFrame, lang = "en" )
                                 TermDocumentMatrix( twCorpus, 
                                                                    control = list( 
                                                                      wordLengths = c( 1, Inf ) ) )
                             } )
                         
  
  output$tweetsBox <- renderInfoBox(
    {
      infoBox( input$numberId, "Tweets", icon = icon( "twitter" ), color = "black", width = 4 )
    }
  )
  output$toptermBox <- renderInfoBox(
    {
      topterms <- find_freq_terms( tdm(), lowfreq = 15 )
      topterms <- topterms[topterms$term != "iot", ]
      topterms <- topterms[order( -topterms$freq ), ]
      infoBox( topterms$term[1], "Top term", color = "maroon", icon = icon( "trophy" ),
                width = 4 )
    }
  )
  
  output$topterms <- renderPlotly( 
    {
      topterms <- find_freq_terms( tdm(), lowfreq = 15 )
      topterms <- topterms[topterms$term != "iot", ]
      plot_ly( x = topterms$freq, y = droplevels( topterms$term ), type = "bar", orientation = "h", 
               color = I( magenta ) ) %>% 
        config( displaylogo = FALSE ) %>%
        layout( xaxis = list( title = "Freq" ), 
                yaxis = list( title = "Term" ), margin = list( l = 150 ) )
      
      # ggplot( topterms, aes( x = term, y = freq ) ) + geom_bar( stat = "identity" ) +
      #   xlab( "Terms" ) + ylab( "Freq" ) + coord_flip() + 
      #   theme( axis.text = element_text( size = 8 ) )
    })
  output$termcloud <- renderPlot(
    {
      topterms <- find_freq_terms( tdm(), lowfreq = 1 )
      topterms <- topterms[topterms$term != "iot", ]
      wordcloud( topterms$term, topterms$freq, random.order = FALSE,
                 colors = appagile, max.words = 200 )
    }
  )
}