rm(list = ls())
library(shiny)
library(shinythemes)
library(ggthemes)
library(tidyverse)
library(magrittr)
library(stringi)
library(janitor)
library(broom)

ui <- fluidPage(
  # Setup title part ---------------------------
  titlePanel(
    windowTitle = "Exploring test statistics", #this appears in the browser tab
    title = h1("Stats spin-off: exploring test statistics",
            h4("by Jens Roeser (Nottingham Trent University)"),
            h4("email: jens.roeser@ntu.ac.uk"))),
  
  # Themes changes app layout
  theme = shinytheme("united"),
  #  shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  navbarPage("test-stats app",
             # Inputs
             tabPanel("t-value",
               sidebarPanel( 
                 sliderInput("effect", label = "Mean difference", 
                             min = -100, max = 100, value = 0),
                 sliderInput("sd", label = "SD", 
                             min = 1, max = 100, value = 50),
                 sliderInput("n", label = "Sample size", 
                             min = 10, max = 500, step = 10, value = 250)
                 ),
               mainPanel(plotOutput("tplot")),
            ),
            tabPanel("ANOVA",
                     sidebarPanel(
                       sliderInput("effect1", label = "Mean difference (a vs b)", 
                                   min = -100, max = 100, value = 0),
                       sliderInput("effect2", label = "Mean difference (b vs c)", 
                                   min = -100, max = 100, value = 0),
                       sliderInput("sd_aov", label = "SD", 
                                   min = 1, max = 100, value = 50),
                       sliderInput("n_aov", label = "Sample size",
                                  min = 9, max = 500, sep = 9, value = 250)                
                     ),
                     mainPanel(plotOutput("fplot")),
            ),
            tabPanel("Correlation",
                     sidebarPanel(
                       sliderInput("cor", label = "Pearson's r", 
                                   min = -.99, max = .99, value = 0),
                       sliderInput("sdcor", label = "SD", 
                                   min = 1, max = 100, value = 50),
                       sliderInput("ncor", label = "Sample size", 
                                   min = 10, max = 500, value = 250),
                     ),
                     
                     mainPanel(plotOutput("corplot")),
            ),
            tabPanel("Linear regression",
                     sidebarPanel( 
                       sliderInput("alpha", label = "Intercept", 
                                   min = 70, max = 80, value = 75),
                       sliderInput("beta", label = "Slope", 
                                   min = -5, max = 5, value = 0),
                       sliderInput("lmsd", label = "SD", 
                                   min = 1, max = 10, value = 5),
                       sliderInput("nlm", label = "Sample size", 
                                   min = 10, max = 500, value = 250),
                       checkboxInput("show_rl", 
                                     "Show line of best fit", 
                                     value = FALSE )
                       ),
                     mainPanel(
                        plotOutput("lmplot"),
                      
                        verbatimTextOutput("lmout")
#                        box(title = "Coefficients table"
#                            , status = "primary", solidHeader = F
#                            , collapsible = T, width = 12
#                            , column(12, align="center", tableOutput('lmsummary')))
          )), 
            tabPanel("Daisie",
                     mainPanel( imageOutput("photo")))
            )
)

 

server <- function(input, output, session) {
  
  output$tplot <- renderPlot({
    
    effect <- input$effect
    mean <- 100
    mean1 <- mean - effect/2
    mean2 <- mean + effect/2
    sd <- input$sd
    N <- input$n
    ya <- rnorm(N/2, mean = mean1, sd = sd)
    yb <- rnorm(N/2, mean = mean2, sd = sd)
    
    groups <- c("group a", "group b")
    
    tdata <- tibble(y = c(ya, yb),
           group = c(rep(groups[1], N/2),rep(groups[2], N/2)))
    
    tres <- psyntur::t_test(y ~ group, data = tdata)
    df <- tres$parameter #%>% pull()
    tvalue <- round(-tres$statistic,2)
    pvalue <- round(tres$`p.value`,3)
    pvalue <- ifelse(pvalue < 0.001, "<0.001", 
                     ifelse(pvalue < 0.01, "<0.01", 
                            ifelse(pvalue < 0.05, "<0.05", 
                                   paste0("=",pvalue))))
    
    ggplot(tdata, aes(y = y, x = group)) +
      geom_boxplot(width = .5, outlier.shape = NA) +
      geom_jitter(size = 1, alpha = .5, shape = 21, width = .25) +
      theme_classic(base_size = 16) +
      scale_y_continuous(limits = c(0, 220)) +
      labs(y = "Dependent variable", x = "Independent variable",
           caption = bquote(italic("t")*"("*.(df)*")"*"="*.(tvalue)*","~italic("p")*.(pvalue))) +
      theme(plot.caption = element_text(size = 16))
#           title = bquote(italic("t")*"("*.(N - 2)*")="*.(tvalue)*", "*italic("p")*.(pvalue)))
#    annotate("label", x = Inf, y = Inf,
 #            label = ), 
  #           parse = T, size = 6, vjust=1, hjust=1) 
    

  }, height = 380, width = 520)
  

  output$fplot <- renderPlot({

    effect1 <- input$effect1
    effect2 <- input$effect2
    mean <- 200
    mean1 <- mean - effect1/2
    mean2 <- mean 
    mean3 <- mean2 + effect2/2
    
    sd <- input$sd_aov
    N <-  input$n_aov
    ya <- rnorm(N/3, mean = mean1, sd = sd)
    yb <- rnorm(N/3, mean = mean2, sd = sd)
    yc <- rnorm(N/3, mean = mean3, sd = sd)
    
    groups <- c("group a", "group b", "group c")
    
    fdata <- tibble(y = c(ya, yb, yc),
                   group = c(rep(groups[1], N/3),rep(groups[2], N/3),rep(groups[3], N/3)))
    
    
    aov <- summary(aov(y ~ group, fdata))
    fvalue <- round(aov[[1]]$`F value`[1],2)
    pvalue <- round(aov[[1]]$`Pr(>F)`[1],3)  
    dfs <- aov[[1]]$Df
    
    pvalue <- ifelse(pvalue < 0.001, "<0.001", 
                     ifelse(pvalue < 0.01, "<0.01", 
                            ifelse(pvalue < 0.05, "<0.05", 
                                   paste0("=",pvalue))))
    
    ggplot(fdata, aes(y = y, x = group)) +
      geom_boxplot(width = .5, outlier.shape = NA) +
      geom_jitter(size = 1, alpha = .5, shape = 21, width = .25) +
      theme_classic(base_size = 16) +
      scale_y_continuous(limits = c(0, 400)) +
      labs(y = "Dependent variable", x = "Independent variable",
           caption = bquote(italic("F")*"("*.(dfs[1])*", "*.(dfs[2])*")="*.(fvalue)*", "*italic("p")*.(pvalue))) + 
      theme(plot.caption = element_text(size = 16))
    
    
  }, height = 450, width = 520)
  
  output$corplot <- renderPlot({
    N <- input$ncor
    cor <- input$cor
    sd <- input$sdcor
    cors <- faux::rnorm_multi(N, 2, 100, sd, cor, varnames = c("x", "y"))
    pvalue <- round(cor.test(cors$x, cors$y, method = "pearson")$p.value,3)
    pvalue <- ifelse(pvalue < 0.001, "<0.001", 
                     ifelse(pvalue < 0.01, "<0.01", 
                            ifelse(pvalue < 0.05, "<0.05", 
                                   paste0("=",pvalue))))
    
    ggplot(cors, aes(x = x, y = y)) +
      geom_point(size = 1, alpha = .5, shape = 21) +
      theme_classic(base_size = 16) +
      labs(caption = bquote(italic("r")*"="*.(round(cor,2))*", "*italic("p")*.(pvalue))) +
      scale_y_continuous(limits = c(0, 200)) +
      scale_x_continuous(limits = c(0, 200)) +
      theme(plot.caption = element_text(size = 16))
    
    
  }, height = 380, width = 520)
  
  lmdata <- reactive({
    beta <- input$beta
    alpha <- input$alpha
    sd <- input$lmsd
    
    N <- input$nlm
    x <- runif(N, min = 0, max = 5)
    e <- rnorm(N, sd = sd)
    y <- beta * x + alpha + e
    
    data <- tibble(x = x, y = y)
    
  })
  
  output$lmplot <- renderPlot({

    # Add r2, and model summary
    m <- lm(y ~ x, lmdata())
    r2 <- round(summary(m)$r.squared,3)

    gg <- ggplot(data = lmdata(), aes(x=x, y=y)) +
      geom_point(size = 1, alpha = .5, shape = 21) +
      theme_classic(base_size = 16) +
      labs(caption = bquote(italic("R")^2==.(r2))) +
      scale_y_continuous(limits = c(50, 100)) +
      scale_x_continuous(limits = c(0, 5)) +
      theme(plot.caption = element_text(size = 16))
    
    if(input$show_rl){
      gg + geom_smooth(method = "lm", se = F, colour = "forestgreen") 
    } 
    else{gg}
    
  }, height = 380, width = 520)
  
  output$lmsummary <- renderTable({
    m <- lm(y ~ x, data = lmdata())

    tidy(m) %>% as.data.frame() %>%
      mutate(`p.value`= ifelse(`p.value` < 0.001, "<0.001", 
                           ifelse(`p.value` < 0.01, "<0.01", 
                              ifelse(`p.value` < 0.05, "<0.05", round(`p.value`,3)))) ) %>%
      rename(predictor = term,
             se = `std.error`,
             `t-value` = statistic,
             `p-value` = `p.value`) %>%
      mutate(across(where(is.double), round, 2))
    })

  
  output$lmout <- renderPrint({ 
    data <- lmdata()
    m <- lm(y ~ x, data = data)
    summary(m)$coef %>% as.data.frame() %>% mutate(across(where(is.numeric), round, 3))
  })
  
  output$photo <- renderImage({
    list(
      src = file.path("pics/daisie.jpeg"),
      contentType = "image/jpeg",
      width = 500,
      height = 650
    )
  }, deleteFile = FALSE)
      
  
}

# Create app
shinyApp(ui = ui, server = server)

