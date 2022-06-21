# Load packages ----

library(palmerpenguins)
library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  d1 <- palmerpenguins::penguins

    output$p1.points <- renderPlot({
        
        ggplot(data = d1,
               aes(x = bill_length_mm, y = bill_depth_mm)) +
          geom_point(aes(colour = species),
                     shape = as.numeric(input$shape),
                     size = as.numeric(input$size),
                     alpha = as.numeric(input$alpha)) +
          scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
          labs(title = "Penguin bill dimensions",
               subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station",
               x = "Bill length (mm)",
               y = "Bill depth (mm)") +
          get(input$theme)()
    })
    
    output$p1.theme <- renderPlot({
      
      ggplot(data = d1,
             aes(x = bill_length_mm, y = bill_depth_mm)) +
        geom_point(aes(colour = species),
                   shape = as.numeric(input$shape),
                   size = as.numeric(input$size),
                   alpha = as.numeric(input$alpha)) +
        scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
        labs(title = "Penguin bill dimensions",
             subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station",
             x = "Bill length (mm)",
             y = "Bill depth (mm)") +
        get(input$theme)()
    })
    
    output$p1.panel <- renderPlot({
      
      rgb.col <- c(input$red, input$green, input$blue)
      
      ggplot(data = d1,
             aes(x = bill_length_mm, y = bill_depth_mm)) +
        geom_point(aes(colour = species),
                   shape = as.numeric(input$shape),
                   size = as.numeric(input$size),
                   alpha = as.numeric(input$alpha)) +
        scale_colour_manual(values = c("darkorange", "purple", "cyan4")) +
        labs(title = "Penguin bill dimensions",
             subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station",
             x = "Bill length (mm)",
             y = "Bill depth (mm)") +
        get(input$theme)() +
        theme(
          panel.background = element_rect(
            fill = rgb(red = input$red, green = input$green, blue = input$blue, maxColorValue = 255)))
    })
    
    

})
