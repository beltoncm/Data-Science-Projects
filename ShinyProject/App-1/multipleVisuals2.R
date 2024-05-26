library(shiny)
library(ggplot2)
u<- shinyUI(fluidPage(
  titlePanel("title panel"),
  
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("do2", "Make 3 plots", value = T)
                )
                ,
                mainPanel("main panel",
                         fluidRow(
                            splitLayout(style = "border: 1px solid silver:", cellWidths = c(300,200,100), 
                                       plotOutput("plotgraph1"), 
                                        plotOutput("plotgraph2"),
                                       plotOutput("plotgraph3")
                            )
                          )
                )
  )
)
)
s <- shinyServer(function(input, output){
  #set.seed(1234)
  
  x    <- faithful$waiting
  
  pt1 <- qplot(x,fill=I("red"),binwidth=0.2,title="plotgraph1")
  
  pt3 <- qplot(x,fill=I("blue"),binwidth=0.2,title="plotgraph3")
  pt2 <- reactive({
    input$do2
    if (input$do2){
      return(qplot(x,fill=I("blue"),binwidth=0.2,title="plotgraph2"))
    } else {
      return(NULL)
    }
  })
  output$plotgraph1 = renderPlot({pt1})
  output$plotgraph2 = renderPlot({pt2()})
  output$plotgraph3 = renderPlot({pt3}
  )
})

shinyApp(u,s)
