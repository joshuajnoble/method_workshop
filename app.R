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
library(lmtest)
library(sandwich)

fig.width <- 800
fig.height <- 450

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
                      div(p("Some of the schools used the new textbooks, some didn't. The textbooks weren't given randomly to different teachers but we still have a test and a control so this has a quasi-experimental set-up.")),
                      div(HTML("<br/>")),
                      div(h4("Question:", em("Do the new books improve the reading test scores?"))),
                      div(HTML("<br/>")),
                      div(p("The data has 5 fields:")),
                      div(strong("prek:"), span("whether this school has a Prekindergarten program")),
                      div(strong("new_books:"), span("whether this classroom used the new textbooks")),
                      div(strong("class_size:"), span("how many students are in the classroom")),
                      div(strong("teacher_exp:"), span("how many years experience the teacher has")),
                      div(strong("scores:"), span("the average score on the reading test for the classroom"))
             ),
             
             tabPanel("View data",
                      DT::dataTableOutput("all_data_view")
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
                      textOutput("means"),
                      div(HTML("<br/>")),
                      plotOutput("plot_mean", width=fig.width, height=fig.height),
                      htmlOutput("ttest"),
                      div(HTML("<br/>")),
                      plotOutput("plot_ttest", width=fig.width, height=fig.height),
             ),
             
             tabPanel("Regression",
                      div(p("Enter a formula to run a regression on our data. To start, do 'scores ~ new_books'")),
                      textInput("regression", "Regression formula", ""),
                      div(HTML("<br/>")),
                      DT::dataTableOutput("lm_summary"),
                      div(HTML("<br/>")),
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
                      htmlOutput("cs_means"),
                      div(HTML("<br/>")),
                      plotOutput("plot_cs_mean", width=fig.width, height=fig.height),
                      plotOutput("plot_cs_ttest", width=fig.width, height=fig.height)
             ),
             
             tabPanel("Discovery",
                      div(HTML("What does a Causal Discovery Algorithm say about it? We can use <a href='https://rdrr.io/cran/pcalg/man/LINGAM.html'>\"cool\" algorithms</a> to do our Causal Inference for us (though we should always double-check them).")),
                      img(src='DAG.png', width=(fig.width * 1.2)),
                      div(a())
             )
  )
)

clean_ttest_output <- function(t){

  output <- paste0(
    "<strong>", names(t$estimate)[1], " </strong>: ", round(t$estimate[[1]], digits = 3), " <br/> ",
    "<strong>", names(t$estimate)[2], " </strong>: ", round(t$estimate[[2]], digits = 3), " <br/> ",
    "<strong>P-value</strong> (e.g. likelihood these groups are the same): ", round(t$p.value, digits = 3), " <br/> "
  )
  return(output)
}

clean_lm_output <- function(coef_test_output){
  
  cdf <- as.data.frame(coef_test_output[,])
  output <- data.frame(
                       "Parameter Name" = row.names(cdf)[2:nrow(cdf)], 
                       "Estimate" = round(cdf[2:nrow(cdf), 1], digits = 4), 
                       "Standard Error" = round(cdf[2:nrow(cdf), 2], digits = 4), 
                       "P-value" = round(cdf[2:nrow(cdf), 4], digits = 4)
                       )
  return(output)
}

server <- function(input, output) {
  
  reading_scores <- readRDS("scores")
  reading_scores$scores = round(reading_scores$scores, digits = 3)
  
  output$all_data_view <- DT::renderDataTable(reading_scores)
  
  ################################################################################
  # plot EDA
  output$scores_eda <- renderPlot({ggplot(reading_scores, aes(x = scores)) +
      geom_histogram(fill="#00AFBB", bins = 20) +
      xlab("Average Score for Classroom") + theme_light(base_size = 16) + 
      ggtitle("Histogram of all scores")})
  
  output$scores_books_eda <- renderPlot({
      ggplot(reading_scores, aes(x = scores, color=as.factor(new_books), fill = as.factor(new_books))) +
      geom_histogram(position = "identity", bins = 20, alpha = 0.2) +
      scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes')) +
      scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'), guide = "none") +
      labs(fill = "New books?")+ theme_light(base_size = 16) +
      theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
      xlab("Average Score for Classroom") +
      ggtitle("Histogram of scores by books")
    })
  
  output$scores_prek_eda <- renderPlot({
      ggplot(reading_scores, aes(x = scores)) +
      geom_histogram(aes(fill = as.factor(prek), color=factor(prek)), position = "identity", bins = 20, alpha = 0.2) +
      scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'))  +
      scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'), guide = "none") +
      labs(fill = "School has PreK?")+ theme_light(base_size = 16) +
      xlab("Average Score for Classroom") +
      theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
      ggtitle("Histogram of scores by Pre-K")
  })
  
  output$scores_class_size <- renderPlot({
    ggplot(reading_scores, aes(y = scores, x=class_size)) + geom_point() +
      ylab("Average Score for Classroom")+ theme_light(base_size = 16) + xlab("Students in Classroom") +
      ggtitle("Scores by Kids in Classroom")
  })
  
  output$scores_teacher_exp <- renderPlot({
    ggplot(reading_scores, aes(y = scores, x=teacher_exp)) + geom_point() +
      ylab("Average Score for Classroom")+ theme_light(base_size = 16) + xlab("Years of Teacher Experience") +
      ggtitle("Scores by Teacher Experience")
  })
  
  ################################################################################
  # fitting regressions
  ################################################################################
  
  graph_lm <- function(formula, fit){
    
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
              labs(fill = "School has PreK?") +
              theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
              geom_histogram(aes(color = as.factor(prek), fill = as.factor(prek)), 
                             position = "identity", bins = 20, alpha = 0.2) +
              scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'), guide = "none") +
              scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'))+ theme_light(base_size = 16)
          })
        }
        if(independent[i] == "new_books")
        {
          output[[whichplot]] <- renderPlot({
            ggplot(reading_scores, aes(x = .data[[dependent]])) +
              geom_histogram(aes(color = as.factor(new_books), fill = as.factor(new_books)), 
                             position = "identity", bins = 20, alpha = 0.2) +
              labs(fill = "New books?") +
              theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
              scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'), guide = "none") +
              scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'))+ theme_light(base_size = 16)
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
                labs(fill = "New books?") +
                theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
                scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'), guide = "none") +
                scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'))+ theme_light(base_size = 16)
            })
          }
          else if(grepl( "new_books", formula) == TRUE)
          {
            output[[whichplot]] <- renderPlot({
              ggplot(reading_scores, aes(y = scores, x=class_size)) +
                geom_point(aes(color = as.factor(new_books)), alpha=0.4) +
                labs(fill = "New books?", color="New books?") +
                theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
                scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes')) +
                scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'))+ theme_light(base_size = 16)
              
            })
          }
          else
          {
            output[[whichplot]] <- renderPlot({
              ggplot(reading_scores, aes(y = scores, x=class_size)) +
                geom_point(alpha=0.4) +
                theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
                geom_smooth(method = 'lm', se = FALSE, formula = y ~ x, colour = "#E7B800", data=reading_scores) + 
                theme_light(base_size = 16)
            })
              
          }
        }
        if(independent[i] == "teacher_exp")
        {
          output[[whichplot]] <- renderPlot({
            ggplot(reading_scores, aes(x = .data[[dependent]], y=teacher_exp)) +
              geom_boxplot(aes(fill = as.factor(new_books), colour = as.factor(new_books))) +
              labs(fill = "New books?") +
              theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
              ggtitle("Boxplot of New Books by Teacher Experience") + 
              scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes')) +
              scale_fill_manual(values = c("#00AFBB33", "#E7B80033"), guide = "none") + theme_light(base_size = 16)
          })
        }
    }
  }
  
  fit_regression <- reactive({
 
    tryCatch({
      
      formula <- unlist(strsplit(input$regression, "[~]"))
      dependent <- str_trim(formula[1])
      
      fit <- NULL
      
      if(dependent == "class_size" || dependent == "scores") {
        fit <- lm(input$regression, data = reading_scores)
      }
      
      if(dependent == "new_books" || dependent == "prek") {
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
  
  # overall mean
  observeEvent(input$calcMeanBtn, {
    
    m <- mean(reading_scores[reading_scores$new_books == 1,]$scores) - mean(reading_scores[reading_scores$new_books == 0,]$scores)
    output$means <- renderPrint(paste0(" The difference between classrooms with new books vs those without is ", round(m, digits = 4)))
    
    t <- t.test(scores ~ new_books, data=reading_scores)
    output$ttest <- renderUI(HTML(clean_ttest_output(t)))

    output$plot_mean <- renderPlot({
      ggplot() + 
        geom_boxplot(reading_scores, mapping = aes(as.factor(new_books), fill=as.factor(new_books), colour=as.factor(new_books), scores)) +
        ggtitle("Boxplot of Scores by New Books") + 
        scale_color_manual(values = c("#00AFBB", "#E7B800"), guide = "none") +
        scale_fill_manual(values = c("#00AFBB33", "#E7B80033"), labels=c('No', 'Yes')) +
        labs(fill = "New books?")+ theme_light(base_size = 16)
    })
    
    output$plot_ttest <- renderPlot({
      ggplot(reading_scores, aes(scores, colour=as.factor(new_books), fill=as.factor(new_books), group=as.factor(new_books))) +
        geom_density(adjust=2) + 
        ggtitle("Density Plot of Scores by New Books") + 
        scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'), guide = "none") +
        scale_fill_manual(values = c("#00AFBB33", "#E7B80033")) +
        labs(fill = "New books?")+ theme_light(base_size = 16)
    })
  })
  
  # class size mean
  observeEvent(input$calcCSMean, {
    t <- t.test(reading_scores[reading_scores$new_books == 1,]$class_size, reading_scores[reading_scores$new_books == 0,]$class_size)
    output$cs_means = renderUI(HTML(clean_ttest_output(t)))
    
    output$plot_cs_mean <- renderPlot({
      ggplot(reading_scores, aes(x = class_size)) +
        geom_histogram(aes(fill = as.factor(new_books), color=factor(new_books)), position = "identity", bins = 20, alpha = 0.2) +
        scale_fill_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'))  +
        scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes'), guide = "none") +
        labs(fill = "New Books?") +
        xlab("Average Score for Classroom") +
        theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
        ggtitle("Histogram of class_size by New Books")+ theme_light(base_size = 16)
    })
    
    output$plot_cs_ttest <- renderPlot({ggplot(reading_scores, aes(as.factor(new_books), fill=as.factor(new_books), colour=as.factor(new_books), class_size)) + geom_boxplot() +
      labs(fill = "New books?") +
        theme(legend.title = element_text(size = 12), legend.text=element_text(size=12)) + 
        ggtitle("Boxplot of Teacher Experience by New Books") + 
        scale_color_manual(values = c("#00AFBB", "#E7B800"), labels=c('No', 'Yes')) +
        scale_fill_manual(values = c("#00AFBB33", "#E7B80033"), guide = "none")+ theme_light(base_size = 16)
      })
  })
  
  observeEvent(input$regression, {
    f <- fit_regression()
    
    if(!is.null(f)) {
      print("rendering summary")
      
      coef_output <- coeftest(f, vcov = vcovHC)
      
      output$lm_summary <- DT::renderDataTable(clean_lm_output(coef_output), options = list(dom = 't'))
      
      # break apart lm statement and graph each component hist for prek/new_book and scatter for class_size/teach_exp
      graph_lm(input$regression, f)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)