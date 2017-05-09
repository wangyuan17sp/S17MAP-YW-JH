library(shinydashboard)
library(ggplot2)
library(shiny)
library(dplyr)


dashboardPage(skin = "green",
              dashboardHeader(
                title = "Shapesplosion: two sample t-test",
                titleWidth = 350),
              
              dashboardSidebar(
                fileInput('datafile', 'Choose CSV file',
                          accept=c('text/csv', 'text/comma-separated-values,text/plain')),
                menuItemOutput("menu"),
                htmlOutput("text")
              ),
              
              dashboardBody(
                tabItems(
                  tabItem(tabName = "item1",
                          fluidRow(
                            box(title = "Boxplot of gender",
                                width = 8,
                                solidHeader = TRUE,
                                plotOutput("plot1", height = 500),
                                status = "success"),
                            valueBoxOutput("pval1"),
                            valueBoxOutput("num1")
                          )
                  ),
                  tabItem(tabName = "item2",
                          fluidRow(
                            box(title = "Boxplot of gender",
                                width = 6,
                                solidHeader = TRUE,
                                plotOutput("plot2", height = 500),
                                status = "success"),
                            valueBoxOutput("pval2"),
                            valueBoxOutput("num2"),
                            box(title = "Histogram of pvalues",
                                width = 6,
                                solidHeader = TRUE,
                                plotOutput("p_dist", height = 256),
                                status = "primary")
                          )
                  )
                )
              )
)



