# Load packages ----

library(shiny)
library(tidyverse)

# Import dataset ----

pingouins <- read_csv("https://raw.githubusercontent.com/codons-blog/C-04-VisualisationDonnees/main/data/pingouins.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Palmer Penguins",
               windowTitle = "ggplot2 Shiny App"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "theme", 
                        label = "Choisir le theme:", 
                        choices = c("theme_gray()", "theme_bw()", "theme_linedraw()",
                                    "theme_light()", "theme_dark()", "theme_minimal()",
                                    "theme_classic()", "theme_void()"), 
                        selected = "theme_gray()"),
            selectInput(inputId = "panel.col",
                        label = "Panel col:",
                        choices = c("blue", "red"),
                        selected = "blue"),
            selectInput(inputId = "plot.col",
                        label = "Plot col:",
                        choices = c("blue", "red"),
                        selected = "blue")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("scatterPlot")
        )
    )
))
