#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$scatterPlot <- renderPlot({
      

        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      ggplot(data = pingouins,
             aes(x = aile_lng_mm,
                 y = masse_g)) +
        geom_point() +
        get(gsub("\\(\\)","",input$theme))() +
        theme(panel.background = element_rect(fill = input$panel.col,
                                              colour = input$panel.col),
              plot.background = element_rect(fill = input$plot.col,
                                             colour = input$plot.col))

    })

})
