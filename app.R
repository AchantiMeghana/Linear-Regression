
library(shiny)
library(vroom)
library(rmarkdown)

# Define UI for application 
ui <- fluidPage(
  
#Title
  titlePanel(strong("Linear Regression")),
  
  fluidRow(
    column(3,

#Upload dataset
           fileInput("upload","Upload your dataset (.csv or .tsv )",multiple= FALSE,
                     accept = c(".csv",".tsv")),
                      
# Select independent variable x           
           selectInput("x","Choose the independent variable",
                       choices = NULL),

# Select independent variable y          
           selectInput("y","Choose the dependent variable",
                       choices = NULL),
           
#Choose no. of rows of table to be displayed
           sliderInput("n","Choose the number of rows of data to be displayed",
                       min= 0, max =0 ,step = 1,value =0)
    ),

     column(9,
#Plot output
           plotOutput("regression_plot")
           )),

  fluidRow(
    column(2,
  # Download reports        
   downloadButton("download","Get your report!", icon = shiny::icon("download")) 
              )),

#display columns of dataset
  fluidRow(
    column(9,
        tableOutput("table")   
      )),


)   

# Define server logic 
server <- function(input, output,session) {
 
#storing dataset in reactive variable
  data <- reactive({
    req(input$upload,cancelOutput = TRUE) #program doesn't run if condition is false
    ext <- tools::file_ext(input$upload$name)
    switch (ext,
            tsv = vroom::vroom(input$upload$datapath,delim ="\t"),
            csv  = vroom::vroom(input$upload$datapath,delim =","),
            validate("Upload a .csv or .tsv file"))}) 

 #Update slider range
    observeEvent(data(),{
      updateSliderInput(inputId = "n",max = nrow(data()))
    }) 

#Displaying dataset    
  observeEvent(data(),{
    output$table <- renderTable(head(data(),input$n))
})
  
  #Update choices in widgets for x,y
  observeEvent(data(),{
    updateSelectInput(inputId = "x",choices = colnames(data()))
  })
  observeEvent(data(),{
    updateSelectInput(inputId = "y",choices = colnames(data()))
  })
  
  #Plotting the linear regression graph
  output$regression_plot <- renderPlot({
    plot(as.numeric(unlist(data()[,input$x]))
         ,as.numeric(unlist(data()[,input$y]))
         ,xlab = input$x, ylab = input$y,col = "darkred")
    abline(lm(as.numeric(unlist(data()[,input$y]))
              ~as.numeric(unlist(data()[,input$x]))))
  })

  # Store summary of regression
result <- reactive({
  req(input$upload)
  lmdata <- lm(as.numeric(unlist(data()[,input$y]))
                ~as.numeric(unlist(data()[,input$x])))
  summary(lmdata)
})

#Downloading the report
output$download <- downloadHandler(
   filename = "report.html",
  content = function(file) {
   
    #Using a temporary directory
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    # Setting up parameters to pass to Rmd document
    params <- result()
    
  #Knitting the document
    rmarkdown::render(tempReport, output_file = file,
                      params = params)}
)
  

  

}

# Run the application 
shinyApp(ui = ui, server = server)
