#' @title Naive Bayes Classifier
#'
#' @description Predicts any variable in any categorical dataset for given values of predictor variables.If a dataset contains 4 variables, then any variable can be predicted based on the values of the other three variables given by the user. The user can upload their own datasets and select what variable they want to predict.A handsontable is provided to enter the predictor values and also accuracy of the prediction is also shown.
#'
#' @return NULL
#'
#' @examples
#' if(interactive()){KCSNBShiny()}
#'
#' @export

KCSNBShiny<-function()
{
  requireNamespace("shiny")
  requireNamespace("rhandsontable")
  requireNamespace("dplyr")
  requireNamespace("caret")
  requireNamespace("e1071")

  ui <- fluidPage(
    titlePanel(title = "Naive Bayes Classifier"),
    tags$h4("This is a Naive Bayes Classifier which is used for categorical datasets(Ex:Titanic dataset)"),
    sidebarLayout(
      sidebarPanel(
        fileInput(inputId = "file",label = "Select file",multiple = FALSE),
        sliderInput(inputId = "slider",label = "what is the proportion of the training dataset?",min = 0.1,max = 0.9,value = 0.6,step = 0.1),
        uiOutput("selectinput")

      ),
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Apriori Probabilities",verbatimTextOutput("Apriori")),
          tabPanel("Data",tableOutput("Data")),
          tabPanel(title = "Accuracy",
                   tags$h4("Accuracy:"),
                   verbatimTextOutput("Accuracy")),
          tabPanel(title="Prediction",
                   tags$h2("Please edit the predictor values in the table given below"),
                   tags$br(),
                   rHandsontableOutput("table"),
                   tags$br(),
                   tags$h3("The prediction is: "),
                   verbatimTextOutput("answer"))

        )
      )
    ))


  server <- function(input, output, session) {

    data<-reactive({
      file1<-input$file
      if(is.null(file1)){return()}
      read.table(file1$datapath,sep=",",header=TRUE)
    })

    output$Data<-renderTable({
      data()
    })

    TitanicNB<-reactive({
      data1<-data()
      rownames(data1)<-1:nrow(data1)
      train.index<-sample(rownames(data1),dim(data1)[1]*input$slider)
      train.df<-data1[train.index, ]
      test.index<-setdiff(rownames(data1),train.index)
      test.df<-data1[test.index, ]
      naiveBayes(get(input$var) ~ .,data=train.df)
    })

    output$Apriori<-renderPrint({
      TitanicNB()
    })

    output$Accuracy<-renderPrint({
      data1<-data()
      rownames(data1)<-1:nrow(data1)
      train.index<-sample(rownames(data1),dim(data1)[1]*input$slider)
      train.df<-data1[train.index, ]
      test.index<-setdiff(rownames(data1),train.index)
      test.df<-data1[test.index, ]
      pred.prob<-predict(TitanicNB(), newdata = test.df, type="raw")
      pred.class<-predict(TitanicNB(),newdata = test.df)
      mod<-select(test.df,input$var)
      df<-data.frame(Actual=mod,Predicted=pred.class)
      confusionMatrix(df[ ,1],df[ ,2])
    })


    output$selectinput<-renderUI({
      if(is.null(data())){return()}
      else
        selectInput(inputId = "var",label = "select the variable to be predicted",multiple = F,choices = colnames(data()))
    })

    output$table<-renderRHandsontable({
      data1<-data()
      ds<-select(data1,c(-input$var))
      rhandsontable(ds[1, ])
    })

    output$answer<-renderPrint({
      a<-as.data.frame(input$table$data)
      k<-colnames(data())!=input$var
      colnames(a)<-colnames(data()[k])
      pred.prob<-predict(TitanicNB(), newdata = a, type="raw")
      pred.class<-predict(TitanicNB(),newdata = a)
      print(pred.prob)
      print(pred.class)
    })

    output$final<-renderPrint({
      frame<-data.frame(input$text1,input$text2,input$text3,input$text4)
      colnames(frame)<-colnames(data())
      pred.prob<-predict(TitanicNB(), newdata = frame, type="raw")
      pred.class<-predict(TitanicNB(),newdata = frame)
      print(pred.class)
      print(pred.prob)
    })


  }

  shinyApp(ui, server)
}
