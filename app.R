#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(stargazer)
library(RColorBrewer)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(MatchIt)
library(marginaleffects)
library(caret)
library(cobalt)
library(ggnewscale)

fig.width <- 600
fig.height <- 350

ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      .tab-pane {
        margin-top: 5px;
        margin-bottom: 5px;
      }
      
      button {
        margin-top: 5px;
        margin-bottom: 5px;
      }"))
  ),
  
  navbarPage("Mediation and Causal Discovery",
             
             tabPanel("Our Story",
                      titlePanel("Assessing new textbooks"),
                      div(p("A large school district has replaced the reading textbooks in some of its first grade classrooms.")),
                      div(p("Some of the schools used the new textbooks, some didn't. The textbooks weren't given randomly to different teachers but it still seems like it might be a quasi-experiment.")),
                      div(HTML("<br/>")),
                      div(strong("Question:"), em("Do the new textbooks improve the reading scores on a standardized test?")),
                      div(HTML("<br/>")),
                      div(p("The data has 5 fields:")),
                      div(strong("prek:"), span("whether this school has a Prekindergarten program")),
                      div(strong("new_books:"), span("whether this classroom used the new textbooks")),
                      div(strong("class_size:"), span("how many students are in the classroom")),
                      div(strong("teacher_exp:"), span("how many years experience the teacher has")),
                      div(strong("scores:"), span("the average score on the reading test for the classroom"))
             ),
             
             tabPanel("View data",
                      DT::dataTableOutput("mytable")
             ),
             
             tabPanel("Explore",
                      plotOutput("scores_eda", width=fig.width, height=fig.height),
                      plotOutput("scores_books_eda", width=fig.width, height=fig.height),
                      plotOutput("scores_prek_eda", width=fig.width, height=fig.height),
                      plotOutput("scores_class_size", width=fig.width, height=fig.height),
                      plotOutput("scores_teacher_exp", width=fig.width, height=fig.height)
             ),
             
             tabPanel("Means",
                      div(p("Let's calculate the mean (aka average) of classrooms with new books and those without.")),
                      
                      actionButton("calcMeanBtn", "Calculate Mean"),
                      div(HTML("<br/>")),
                      verbatimTextOutput("means"),
                      div(HTML("<br/>")),
                      plotOutput("plot_mean", width=fig.width, height=fig.height),
                      verbatimTextOutput("ttest"),
                      div(HTML("<br/>")),
                      plotOutput("plot_ttest", width=fig.width, height=fig.height),
             ),
             
             tabPanel("Regression",
                      div(p("Enter a formula to run a regression on our data. To start, do 'scores ~ new_books'")),
                      textInput("regression", "Regression formula", ""),
                      verbatimTextOutput("summary"),
                      
                      plotOutput("plot1", width=fig.width, height=fig.height),
                      plotOutput("plot2", width=fig.width, height=fig.height),
                      plotOutput("plot3", width=fig.width, height=fig.height),
                      plotOutput("plot4", width=fig.width, height=fig.height)
             ),
             
             tabPanel("Class Sizes",
                      div(p("Let's calculate whether there's a difference between classes with books and without. We'll just do this by taking the mean of both groups.")),
                      div(HTML("<br/>")),
                      actionButton("calcCSMean", "Calculate Mean"),
                      div(HTML("<br/>")),
                      verbatimTextOutput("cs_means"),
                      div(HTML("<br/>")),
                      plotOutput("plot_cs_mean", width=fig.width, height=fig.height),
             ),
             
             tabPanel("Discovery",
                      div(HTML("What does a Causal Discovery Algorithm say about it? We can use <a href='https://rdrr.io/cran/pcalg/man/LINGAM.html'>\"cool\" algorithms</a> to do our Causal Inference for us (though we should always double-check them).")),
                      img(src='LiNGAM_DAG.png', width=(fig.width * 1.2), height=(fig.height * 1.2)),
                      div(a())
             )
  )
)

server <- function(input, output) {
  
  reading_scores <- readRDS("scores")
  reading_scores$scores = round(reading_scores$scores, digits = 3)
  
  output$mytable <- DT::renderDataTable(reading_scores)
  
  ################################################################################
  # plot EDA
  output$scores_eda <- renderPlot({ggplot(reading_scores, aes(x = scores)) +
      geom_histogram(fill="#00AFBB") +
      ggtitle("Histogram of all scores")})
  
  output$scores_books_eda <- renderPlot({ggplot(reading_scores, aes(x = scores)) +
      geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                     position = "identity", bins = 20, alpha = 0.2) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      scale_fill_manual(values = c("#00AFBB", "#E7B800"))  +
      ggtitle("Histogram of scores by books")})
  
  output$scores_prek_eda <- renderPlot({ggplot(reading_scores, aes(x = scores)) +
      geom_histogram(aes(color = as.factor(prek), fill = as.factor(prek)), 
                     position = "identity", bins = 20, alpha = 0.2) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      scale_fill_manual(values = c("#00AFBB", "#E7B800"))  +
      ggtitle("Histogram of scores by Pre-K")})
  
  output$scores_class_size <- renderPlot({
    ggplot(reading_scores, aes(y = scores, x=class_size)) + geom_point()
  })
  
  output$scores_teacher_exp <- renderPlot({
    ggplot(reading_scores, aes(y = scores, x=teacher_exp)) + geom_point()
  })
  
  ################################################################################
  # fitting regressions
  ################################################################################
  
  parse_lm <- function(formula, fit){
    
    if(formula == "" || grepl("~", formula) == FALSE) {
      return()
    }
    
    output$plot1 <<- NULL
    output$plot2 <<- NULL
    output$plot3 <<- NULL
    
    first_split <- unlist(strsplit(formula, "[~]"))
    dependent <- str_trim(first_split[1])
    independent <- unlist(lapply(strsplit(first_split[2], "[+]"), str_trim))
    
    for( i in 1:length(independent))
    {
        whichplot <- paste("plot", toString(i), sep="")
        if(independent[i] == "prek")
        {
          output[[whichplot]] <- renderPlot({
            ggplot(reading_scores, aes(x = .data[[dependent]])) +
              geom_histogram(aes(color = as.factor(prek), fill = as.factor(prek)), 
                             position = "identity", bins = 20, alpha = 0.2) +
              scale_color_manual(values = c("#00AFBB", "#E7B800")) +
              scale_fill_manual(values = c("#00AFBB", "#E7B800"))
          })
        }
        if(independent[i] == "new_books")
        {
          output[[whichplot]] <- renderPlot({
            ggplot(reading_scores, aes(x = .data[[dependent]])) +
              geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                             position = "identity", bins = 20, alpha = 0.2) +
              scale_color_manual(values = c("#00AFBB", "#E7B800")) +
              scale_fill_manual(values = c("#00AFBB", "#E7B800"))
          })
        }
        if(independent[i] == "class_size")
        {
          
          if(dependent == "new_books")
          {
            output[[whichplot]] <- renderPlot({
              ggplot(reading_scores, aes(x = class_size)) +
                geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                               position = "identity", bins = 20, alpha = 0.2) +
                scale_color_manual(values = c("#00AFBB", "#E7B800")) +
                scale_fill_manual(values = c("#00AFBB", "#E7B800"))
            })
          }
          else if(grepl( "new_books", formula) == TRUE)
          {
            output[[whichplot]] <- renderPlot({
              ggplot(reading_scores, aes(y = scores, x=class_size)) +
                geom_point(aes(color = as.factor(new_books)), alpha=0.4) +
                scale_color_manual(values = c("#00AFBB", "#E7B800")) +
                scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
                geom_smooth(method = 'lm', se = FALSE, formula = y ~ x, colour = "#E7B800", data=reading_scores[reading_scores$new_books == 1,]) +
                geom_smooth(method = 'lm', se = FALSE, formula = y ~ x, colour = "#00AFBB", data=reading_scores[reading_scores$new_books == 0,])
              
            })
          }
          else
          {
            output[[whichplot]] <- renderPlot({
              ggplot(reading_scores, aes(y = scores, x=class_size)) +
                geom_point(, alpha=0.4) +
                geom_smooth(method = 'lm', se = FALSE, formula = y ~ x, colour = "#E7B800", data=reading_scores)
            })
              
          }
        }
        if(independent[i] == "teacher_exp")
        {
          output[[whichplot]] <- renderPlot({
            ggplot(reading_scores, aes(x = .data[[dependent]], y=teacher_exp)) +
              geom_point(aes(color = as.factor(new_books))) +
              scale_color_manual(values = c("#00AFBB", "#E7B800")) +
              scale_fill_manual(values = c("#00AFBB", "#E7B800"))
          })
        }
    }
  }
  
  fit.regression <- reactive({
 
    tryCatch({
      
      formula <- unlist(strsplit(input$regression, "[~]"))
      dependent <- str_trim(formula[1])
      
      fit <- NULL
      
      print(dependent)
      
      if(dependent == "class_size" || dependent == "scores") {
        fit <- lm(input$regression, data = reading_scores)
      }
      
      if(dependent == "new_books" || dependent == "prek") {
        print("running glm")
        fit <- glm(input$regression, family=binomial(link = "probit"), data = reading_scores)
      }
      
      return(fit)
    }, warning = function(w) {
      return(NULL)
    }, error = function(e) {
      return(NULL)
    })
    
  })
  
  ################################################################################
  # observe events
  ################################################################################
  
  observeEvent(input$calcMeanBtn, {
    
    m <- mean(reading_scores[reading_scores$new_books == 1,]$scores) - mean(reading_scores[reading_scores$new_books == 0,]$scores)
    output$means <- renderPrint(paste0(" The difference between classrooms with new books vs those without is ", m))
    
    t <- t.test(scores ~ new_books, data=reading_scores)
    output$ttest <- renderPrint(t)

    output$plot_mean <- renderPlot({ggplot(reading_scores, aes(as.factor(new_books), colour=as.factor(new_books), scores)) + geom_boxplot()})
    output$plot_ttest <- renderPlot({ggplot(reading_scores, aes(scores, colour=as.factor(new_books), group=as.factor(new_books))) +
        geom_density(adjust=2)
    })
  })
  
  observeEvent(input$calcCSMean, {
    t <- t.test(reading_scores[reading_scores$new_books == 1,]$class_size, reading_scores[reading_scores$new_books == 0,]$class_size)
    output$cs_means = renderPrint(t)
    
    output$plot_cs_mean <- renderPlot({
      ggplot(reading_scores, aes(x = class_size)) +
        geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                       position = "identity", bins = 20, alpha = 0.2) +
        scale_color_manual(values = c("#00AFBB", "#E7B800")) +
        scale_fill_manual(values = c("#00AFBB", "#E7B800"))
    })
  })
  
  observeEvent(input$regression, {
    f <- fit.regression()
    if(!is.null(f)) {
      print("rendering summary")
      output$summary <- renderPrint({
          summary(f)
      })
      
      # break apart lm statement and graph each component hist for prek/new_book and scatter for class_size/teach_exp
      parse_lm(input$regression, f)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)