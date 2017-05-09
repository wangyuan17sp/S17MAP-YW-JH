library(shinydashboard)
library(ggplot2)

dashboardPage(
  dashboardHeader(title = "Visualizing two-sample t-test", titleWidth = 280),
  dashboardSidebar(
    sidebarMenuOutput("menu"),
    htmlOutput("text")
  ),
  
  
  dashboardBody(
    fluidRow(
      box(
        title = "Populations Distribution", 
        width = 6,
        status = "primary",
        plotOutput("distPlot1", height = 300)
      ),
      
      box(
        title = "Samples Distribution", 
        width = 6,
        status = "primary",
        plotOutput("sample_dist", height = 300)
      )),
    
    fluidRow(
      tabBox(
        title = "",
        selected = "Mean Differences",
        tabPanel("Mean Differences",
                 plotOutput("mean_diff1", height = 250)),
        tabPanel("t-statistics",
                 plotOutput("mean_diff2", height = 250))
      ),
      
      box(
        title = "Histogram of p-values",
        width = 6,
        status = "primary",
        plotOutput("mean_diff3", height = 250)
      )
    )
  )
)



