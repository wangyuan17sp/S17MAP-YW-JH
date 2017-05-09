function(input, output) {
  hx1 <- reactive({
    switch(input$population1,
           "normal" = {rnorm(5000, input$popu_mean1, input$sd1)}, 
           "skewed" = {rchisq(5000, df = input$sd1, ncp = input$popu_mean1)},
           "uniform" = {runif(5000, min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1)})
  })
  hx2 <- reactive({
    switch(input$population2,
           "normal" = {rnorm(5000, input$popu_mean2, input$sd2)},
           "skewed" = {rchisq(5000, df = input$sd2, ncp = input$popu_mean2)},
           "uniform" = {runif(5000, min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2)})
  })
  
  std1 <- reactive({
    switch(input$population1,
           "normal" = {input$sd1},
           "skewed" = {sqrt(input$sd1 * 2)},
           "uniform" = {sqrt(1/12) * input$sd1 * 2})
  })
  std2 <- reactive({
    switch(input$population2,
           "normal" = {input$sd2},
           "skewed" = {sqrt(input$sd2 * 2)},
           "uniform" = {sqrt(1/12) * input$sd2 * 2})
  })
  
  
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Population 1", icon = NULL,
               menuSubItem(
                 icon = NULL,
                 selectInput(inputId='population1', 
                             label = 'population1',
                             choice = c('normal','skewed','uniform'))),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'popu_mean1',
                              label = 'Population mean/ncp', 
                              0,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'size1',
                              label = 'Sample size',
                              20,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'sd1',
                              label = 'Std. deviation/df',
                              10,
                              min = -100, max = 100))
      ),
      menuItem("Population 2", icon = NULL,
               menuSubItem(
                 icon = NULL,
                 selectInput(inputId='population2', 
                             label = 'population2',
                             choice = c('normal','skewed','uniform'))),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'popu_mean2',
                              label = 'Population mean/ncp', 
                              0,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'size2',
                              label = 'Sample size',
                              20,
                              min = -100, max = 100)),
               menuSubItem(
                 icon = NULL,
                 numericInput(inputId = 'sd2',
                              label = 'Std. deviation/df',
                              10,
                              min = -100, max = 100))
      ),
      sliderInput("slider", "Number of trials:", 20, 5000, 1000),
      actionButton("action", "Run"),
      menuItem("Preferences", icon = NULL,
               menuSubItem(
                 icon = NULL,
                 sliderInput("slider1", "Number of breaks:", 5, 150, 50)
               ),
               menuSubItem(
                 icon = NULL,
                 numericInput("alpha", "Alpha Level",
                              0.05,
                              min = 0, max = 1)
               )
      ))
  })
  
  output$text <- renderText({"
    <div style='padding: 0.5em 1em 0.5em 1em'>
    <b>Yuan Wang<b>
    </div>
    
    <div style='padding: 0.5em 1em 0.5em 1em'>
    <a>Grinnell College<a>
    </div>
    "})
  
  output$distPlot1 <- renderPlot({
    low_range = min(input$popu_mean1-4*input$sd1, input$popu_mean2-4*input$sd2)
    high_range = max(input$popu_mean1+4*input$sd1, input$popu_mean2+4*input$sd2)
    height1 = max(0.4/input$sd1, 0.4/input$sd2)
    height2 = max(0.1*input$sd1, 0.1*input$sd2)
    half_width =  max(4*input$sd2, 4*input$sd1)
    
    
    switch(input$population1,
           "normal" = {
             g <- ggplot(data.frame(x = c(low_range, high_range)), aes(x)) + 
               stat_function(fun = dnorm, args = list(mean = input$popu_mean1, sd = input$sd1), 
                             colour = "red")
           },
           
           "skewed" = {
             g <- ggplot(data.frame(x = c(low_range, high_range)), aes(x)) +
               stat_function(fun = dchisq, args = list(df = input$sd1, ncp = input$popu_mean1),
                             colour = "red")
           },
           
           "uniform" = {
             g <- ggplot(data.frame(x = c(low_range, high_range)), aes(x)) +
               stat_function(fun = dunif, 
                             args = list(min = input$popu_mean1 - input$sd1, max = input$popu_mean1 + input$sd1),
                             colour = "red")
           })
    
    switch(input$population2,
           "normal" = {
             g + stat_function(fun = dnorm, args = list(mean = input$popu_mean2, sd = input$sd2),
                               colour = "blue")
           },
           
           "skewed" = {
             g + stat_function(fun = dchisq, args = list(df = input$sd2, ncp = input$popu_mean2), 
                               colour = "blue")
           },
           
           "uniform" = {
             g + stat_function(fun = dunif, 
                               args = list(min = input$popu_mean2 - input$sd2, max = input$popu_mean2 + input$sd2),
                               colour = "blue")
           })
  })
  
  data <- eventReactive(input$action, {
    d <- vector()
    for (i in 1:input$slider) {
      sp1 <- sample(hx1(), input$size1)
      sp2 <- sample(hx2(), input$size2)
      mean_diff <- round(mean(sp1) - mean(sp2), digits = 5)
      ttest <- t.test(sp1, sp2, paired = FALSE, conf.level = 1 - input$alpha)
      tscore <- round(ttest$statistic, digits = 5)
      pval <- round(ttest$p.value, digits = 5)
      reject <- FALSE
      if (pval < input$alpha) {reject <- TRUE}
      d <- cbind(d, c(mean_diff, tscore, pval, reject))
    }
    return(d)
  })
  
  output$sample_dist <- renderPlot({
    height1 <- max(0.5 * input$size1, 0.5/input$size2)
    data <- vector()
    sp1 <- sample(hx1(), input$size1)
    sp2 <- sample(hx2(), input$size2)
    samp1 <- data.frame(title = sp1)
    samp2 <- data.frame(title = sp2)
    samp1$pop <- "1"
    samp2$pop <- "2"
    h <- rbind(samp1, samp2)
    ggplot(h, aes(x = title, fill = pop)) + geom_dotplot(binwidth=1, alpha = 0.7) + ylim(0, height1)
  })
  
  t_dist <- reactive({
    qt(c(input$alpha / 2, 1 - input$alpha / 2), df = input$size1 + input$size2 - 2)
  })
  
  output$mean_diff1 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    t_dist <- t_dist()
    sd1 <- std1()
    sd2 <- std2()
    
    denominator <- sqrt(sd1*sd1/input$size1 + sd2*sd2/input$size2)
    
    h <- hist(data[1,], col = "plum", pch = 16, 
              breaks = input$slider1,
              plot = FALSE)
    cuts <- cut(h$breaks, c(-Inf, t_dist[1] * denominator, t_dist[2] * denominator, Inf))
    plot(h, xlab = "", ylab = "", main = paste("Mean Difference = ", input$popu_mean1 - input$popu_mean2, ", red = reject"),
         col = c("red", "plum", "red")[cuts])
    #abline(v = input$popu_mean1 - input$popu_mean2, lwd = 2, col = "red")
    
  })
  
  output$mean_diff2 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    t_dist <- t_dist()
    
    h <- hist(data[2,], col = "wheat1", pch = 16, 
              breaks = input$slider1, plot = FALSE) 
    cuts <- cut(h$breaks, c(-Inf, t_dist[1], t_dist[2], Inf))
    plot(h, xlab = "", ylab = "", main = "red = reject", 
         col = c("red", "wheat1", "red")[cuts])
    #abline(v = c(t_dist[1], t_dist[2]), lwd = c(2, 2), col = c("red", "red"))
  })
  
  output$mean_diff3 <- renderPlot({
    data <- vector()
    data <- cbind(data, data())
    
    reject <- length(which(data == TRUE))
    power <- 0
    type1error <- 0
  
    if (abs(input$popu_mean1 - input$popu_mean2) > 0) {
      power <- round(reject / input$slider, digits = 5)
      h <- hist(data[3,], col= "palegreen", pch = 20, 
                breaks = input$slider1, plot = FALSE)
      cuts <- cut(h$breaks, c(-Inf, input$alpha, Inf))
      plot(h, xlab = "", ylab = "", main = paste("alpha = ", input$alpha, ", power = ", power), 
           col = c("red", "palegreen")[cuts])
      }
    else {
      type1error <- round(reject / input$slider, digits = 5)
      h <- hist(data[3,], col= "palegreen", pch = 20, 
                breaks = input$slider1, plot = FALSE)
      cuts <- cut(h$breaks, c(-Inf, input$alpha, Inf))
      plot(h, xlab = "", ylab = "", main = paste("alpha = ", input$alpha, ", Type I Error = ", type1error), 
           col = c("red", "palegreen")[cuts])}
    
    
    #abline(v = input$alpha, lwd = 2, col = "red")
  })
  }
