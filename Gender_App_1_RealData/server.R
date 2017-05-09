convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}
#http://stackoverflow.com/questions/31794702/r-shiny-dashboard-tabitems-not-apparing

function(input, output) {
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  output$menu <- renderMenu({
    data <- filedata()
    if (is.null(data)) {
      return(NULL)
    } else {
      sidebarMenu(
        convertMenuItem(menuItem("Group ID", tabName = "item1", icon = icon("dashboard"),
                                 selectizeInput(inputId = "group", 
                                                label = "Group ID:", 
                                                choices = unique(as.character(data$groupID)), 
                                                selected = unique(as.character(data$groupID))[1], 
                                                multiple = TRUE)
        ), tabName = "item1"),
        convertMenuItem(menuItem("Random", tabName = "item2", 
                                 icon = icon("cog", lib = "glyphicon"),
                                 menuSubItem(
                                   icon = NULL,
                                   sliderInput("slider", "Sample size:", 5, 516, 30)
                                 ),
                                 menuSubItem(
                                   icon = NULL,
                                   sliderInput("trial", "Number of trials:", 20, 5000, 1000)
                                 ),
                                 menuSubItem(
                                   icon = NULL,
                                   numericInput("alpha", "Alpha Level",
                                                0.05,
                                                min = 0, max = 1)
                                 ),
                                 menuSubItem(
                                   icon = NULL,
                                   actionButton("action", "Run")
                                 )),
                        tabName = "item2")
      )
    }
  })
  
  output$text <- renderText({"
    <div style='padding: 0.5em 1em 0.5em 1em'>
    <b>Yuan Wang<b>
    </div>

    <div style='padding: 0.5em 1em 0.5em 1em'>
    <a>Grinnell College<a>
    </div>
  "})
  
  output$plot1 <- renderPlot({
    data <- filedata()
    if (is.null(data)) {
      return(NULL)
    } else {
      data1 <- data[data$groupID %in% input$group,]
      data1$gender <- as.factor(data1$gender)
      TimeUsedSec <- data1$TimeUsedSec
      
      ggplot(data = data1, aes(x = gender, y = TimeUsedSec)) +
        geom_boxplot() +
        aes(colour = gender) +
        theme(legend.position="right") +
        stat_summary(fun.y = mean, geom = "point", pch = 8, cex = 3) +
       scale_colour_discrete(name="Gender",labels=c("Female", "Male"))+
        scale_x_discrete(labels=c("Female", "Male"))
    }
  })
  
  output$pval1 <- renderValueBox({
    data <- filedata()
    if (is.null(data)) {
      valueBox("N/A", "P-value", color = "yellow")
    } else {
      data1 <- data[data$groupID %in% input$group,]
      female <- data1[data1$gender == 0,]$TimeUsedSec
      male <- data1[data1$gender == 1,]$TimeUsedSec
      
      p <- round(t.test(female, male)$p.value, digits = 3)
      
      if (p < 0.1) {
        valueBox(p, "P-value", icon = icon("thumbs-up", lib = "glyphicon"), color = "red")
      } else {
        valueBox(p, "P-value", color = "yellow")
      }
    }
  })
  
  output$num1 <- renderValueBox({
    data <- filedata()
    if (is.null(data)) {
      valueBox("N/A", "Sample size", color = "blue")
    } else {
      data1 <- data[data$groupID %in% input$group,]
      
      n <- dim(data1)[1]
      valueBox(n, "Sample size", color = "blue")
    }
  })
  

    data2 <- reactive({
      data <- filedata()
      if (is.null(data)) {
        return(NULL)
      } else {
        data2 <- sample_n(data, input$slider)
        return(data2)
      }
    })
  
  output$plot2 <- renderPlot({
    data2 <- data2()
    if (is.null(data2)) {
      return(NULL)
    } else {
      data2$gender <- as.factor(data2$gender)
      TimeUsedSec <- data2$TimeUsedSec
      
      ggplot(data = data2, aes(x = gender, y = TimeUsedSec)) +
        geom_boxplot() +
        aes(colour = gender) +
        theme(legend.position="right") +
        stat_summary(fun.y = mean, geom = "point", pch = 8, cex = 3) +
      scale_colour_discrete(name="Gender",labels=c("Female", "Male")) +
      scale_x_discrete(labels=c("Female", "Male"))
      
    }
  })
  
  output$pval2 <- renderValueBox({
    data2 <- data2()
    if (is.null(data2)) {
      valueBox("N/A", "P-value", color = "yellow")
    } else {
      female <- data2[data2$gender == 0,]$TimeUsedSec
      male <- data2[data2$gender == 1,]$TimeUsedSec
      if (length(female) <= 1 || length(male) <= 1) {
        valueBox("N/A", "P-value", icon = icon("list"), color = "black")
      } else {
        
        p <- round(t.test(female, male)$p.value, digits = 3)
        
        if (p < 0.1) {
          valueBox(p, "P-value", icon = icon("thumbs-up", lib = "glyphicon"), color = "red")
        } else {
          valueBox(p, "P-value", color = "yellow")
        }
      }
    }
  })
  
  output$num2 <- renderValueBox({
    data2 <- data2()
    if (is.null(data2)) {
      valueBox("N/A", "Sample size", color = "blue")
    } else {
      n <- dim(data2)[1]
      valueBox(n, "Sample size", color = "blue")
    }
  })
  
  d <- eventReactive(input$action, {
    data <- filedata()
    if (is.null(data)) {
      return(NULL)
    } else {
      d <- vector()
      for (i in 1:input$trial) {
        data3 <- data[sample(nrow(data), input$slider), ]
        #sample_n(data, input$slider)
        female <- data3[data3$gender == 0,]$TimeUsedSec
        male <- data3[data3$gender == 1,]$TimeUsedSec
        if (length(female) <= 1 || length(male) <= 1) { }
        else {
          mean_diff <- round(mean(female) - mean(male), digits = 5)
          ttest <- t.test(male, female, paired = FALSE, conf.level = 1 - input$alpha)
          tscore <- round(ttest$statistic, digits = 5)
          pval <- round(ttest$p.value, digits = 5)
          d <- cbind(d, c(mean_diff, tscore, pval))
        }
        
      }
      return(d)
    }
  })
  
  output$p_dist <- renderPlot({
    data <- filedata()
    if (is.null(data)) {
      return(NULL)
    } else {
      d <- vector()
      d <- cbind(d, d())
      h <- hist(d[3,], col= "palegreen", pch = 20, 
                breaks = 80, plot = FALSE)
      cuts <- cut(h$breaks, c(-Inf, input$alpha, Inf))
      plot(h, xlab = "", ylab = "", main = paste("alpha = ", input$alpha), 
           col = c("red", "palegreen")[cuts])
    }
  })
}