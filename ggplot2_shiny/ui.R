# Load packages ----

library(palmerpenguins)
library(shiny)
library(tidyverse)


# Define UI ----

navbarPage("Paramétrer ggplot2",
           tabPanel("geom_point()",
                    sidebarLayout(
                      sidebarPanel(
                        width = 2,
                        selectInput(
                          inputId = "shape",
                          label = "shape",
                          choices = 0L:25L,
                          selected = "19"),
                        sliderInput(
                          inputId = "size",
                          label = "size",
                          min = 1,
                          max = 5, value = 2, step = 0.5, ticks = FALSE),
                        sliderInput(
                          inputId = "alpha",
                          label = "alpha",
                          min = 0,
                          max = 1, value = 1, step = 0.1, ticks = FALSE)
                      ),
                      mainPanel(
                        width = 10,
                        plotOutput("p1.points", height = "750px")
                      )
                    )
           ),
           tabPanel("theme",
                    sidebarLayout(
                      sidebarPanel(
                        width = 2,
                        selectInput(
                          inputId = "theme",
                          label = "theme",
                          choices = c("theme_grey",
                                      "theme_bw",
                                      "theme_linedraw",
                                      "theme_light",
                                      "theme_dark",
                                      "theme_minimal",
                                      "theme_classic",
                                      "theme_void"),
                          selected = "theme_grey")
                      ),
                      mainPanel(
                        width = 10,
                        plotOutput("p1.theme", height = "750px")
                      )
                    )
                    ),
           tabPanel("panel",
                    sidebarLayout(
                      sidebarPanel(
                        width = 2,
                        h4("background colour"),
                        numericInput(
                          inputId = "red",
                          label = "rouge", 
                          value = 235, min = 0, max = 255, step = 1),
                        numericInput(
                          inputId = "green",
                          label = "vert",
                          value = 235, min = 0, max = 255, step = 1),
                        numericInput(
                          inputId = "blue",
                          label = "bleu",
                          value = 235, min = 0, max = 255, step = 1)
                      ),
                      mainPanel(
                        width = 10,
                        plotOutput("p1.panel", height = "750px")
                      )
                    )
           ),
           )