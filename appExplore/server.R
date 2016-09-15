library(shiny)
library(ggplot2)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {

  ### Dynamic UI
    output$columns = renderUI({
      mydata = get(input$data)
      selectInput('col', 'Columns', names(mydata), selected = "v_1025")

    })
  ###
    output$bycolumns = renderUI({
      mydata = get(input$data)
      selectInput('bycol', 'By Columns', names(mydata), selected = "keshel")
    })
  ###


    type <- eventReactive(input$action,{
      input$type
    })

    data <- eventReactive(input$action,{
      data <- get(input$data)
      # data <- subset(data, select = c(input$col, input$bycol))
    })


    inputcol <- eventReactive(input$action,{
      inputcol <- input$col
      # data <- subset(data, select = c(input$col, input$bycol))
    })

    inputbycol <- eventReactive(input$action,{
      inputbycol <- input$bycol
      # data <- subset(data, select = c(input$col, input$bycol))
    })

    observeEvent(input$action,{
      data <- data()
      inputcol <- inputcol()
      val <- data[[inputcol]]
      class(val)
      if (is.numeric(val)){
        minval = floor(min(val, na.rm=TRUE))
        maxval = floor(max(val, na.rm=TRUE))
      } else {
        minval = 0
        maxval = 100
      }
      # Control the value, min, max, and step.
      updateSliderInput(session, "receive", value = c(minval, maxval),
                        min = minval, max = maxval)
    })


  output$distPlot <- renderPlot({
    data=data()
    inputcol <- inputcol()
    inputbycol <- inputbycol()

    if (type()==FALSE){
      data <- subset(data, input$receive[1] <= get(inputcol) & get(inputcol) <= input$receive[2])
      ggplot(data, aes( x = get(inputcol))) +
      geom_histogram(fill = "blue", color = "black", bins = input$bins) +
      coord_cartesian(xlim = input$receive)+
        labs(x =inputcol)
      } else {
        ggplot(data, aes(x=factor(get(inputcol)))) + stat_count(fill = "blue") +
          labs(x =inputcol)
        }
  })
#
#   output$boxPlot <- renderPlot({
#     data=get(input$data)
#     print(class(data))
#     x<-data[[input$col]]
#     # draw the histogram with the specified number of bins
#     boxplot(x,col="yellow",main=paste("Boxplot of", input$col))
#   })

  output$spline <- renderPlot({
    data=data()
    inputcol <- inputcol()
    inputbycol <- inputbycol()

    if (type()==FALSE){
      data <- subset(data, input$receive[1] <= get(input$col) & get(input$col) <= input$receive[2])
    ggplot(data,aes(x = get(inputcol), y = get(inputbycol)))+
      geom_smooth()+
    coord_cartesian(xlim = input$receive) +
        labs(x =inputcol, y = paste0("Mean", inputbycol))
    }
      # } else {

    #   temp <- group_by_(data, input$col) %>% rename_(varname = input$bycol)
    #   temp %>% summarise(mean = mean(varname)) %>%
    #     ggplot(.,aes(x = get(input$col), y = mean)) +
    #     geom_bar(stat = "identity", fill = "red", colour = "black") +
    #     labs(x =input$col, y = paste0("Mean", input$bycol))
    # }
  })

  output$summary <- renderTable({
    data=data()
    inputcol <- inputcol()
    inputbycol <- inputbycol()

    if (type()==FALSE){
      data <- subset(data, input$receive[1] <= get(inputcol) & get(inputcol) <= input$receive[2])

      summarise_each_(data,funs( Mean= mean(.,na.rm=TRUE)
                                ,Min=min(.,na.rm=TRUE)
                                ,P5=quantile(.,0.05,na.rm=TRUE)
                                ,P25=quantile(.,0.25,na.rm=TRUE)
                                ,Median=median(.,na.rm=TRUE)
                                ,P75=quantile(.,0.75,na.rm=TRUE)
                                ,Max=max(.,na.rm=TRUE)
                                ,N =n()
                                ),inputcol)

    } else {
      # by(data[[input$col]],summary(data[[input$bycol]]))
      temp <- group_by_(data, inputcol) %>% rename_(varname = inputbycol)
      temp %>%
        summarise(N = n(), Mean_By_Col = mean(varname,na.rm=TRUE), Var_By_Col = var(varname, na.rm=TRUE)) %>%
        mutate(Percent = N*100/sum(N, na.rm=TRUE))

    }

  })



})