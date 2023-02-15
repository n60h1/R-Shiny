
library(shiny)
library(Matrix) 
library(arules)
library(arulesViz)
library(rJava)
library(rCBA)
library(RCurl)
library(rio)


#pioneer10<-read.csv(text=getURL("https://github.com/n60h1/R-Shiny/raw/main/pioneer10_1.csv"), sep='\t')

#pioneer10<-import(text=getURL("https://github.com/n60h1/R-Shiny/raw/main/pioneer10.csv"))

#pioneer10<-read.csv(text=getURL("https://github.com/n60h1/R-Shiny/raw/main/pioneer10test.csv"))

pioneer10<-import("D:/pioneer10.csv")





ui <- fluidPage(
  
  
  titlePanel("Pioneer 10 Apriori Association Rules"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      sliderInput(inputId = "minlift",
                  label = "Minimum lift:",
                  min = 2,
                  max = 5,
                  value = 2.5,
                  step = 0.1),
      
      sliderInput(inputId = "minsup",
                  label = "Minimum support:",
                  min = 0.00018,
                  max = 0.001,
                  value = 0.001,
                  step = 0.00001),
      
      sliderInput(inputId = "mincon",
                  label = "Minimum confidence:",
                  min = 0.01,
                  max = 0.4,
                  value = 0.1,
                  step = 0.01),
      
      actionButton("submitbutton", "Submit", class = "btn btn-primary")
      
    ),
    
    
    mainPanel(
      
      
      plotOutput(outputId = "distPlot")
      
    )
  )
)


server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    
    
    
    ptest=pioneer10
    
    i=nrow(pioneer10)
    while(i>0){
      if(ptest[i,2]==-1e31){
        ptest <- ptest[-i, ]
        
      }
      i<-i-1
    }
    
    ap1=apriori(data=ptest,parameter = list(supp = input$minsup,conf = input$mincon,minlen = 2))
    


    subap1<-subset(ap1,lift>input$minlift)

    
    plot(subap1)
    
  })
  
}


shinyApp(ui = ui, server = server)
