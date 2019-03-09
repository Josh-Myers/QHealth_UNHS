library(shiny)

# load model and set intercept value
model = readRDS("model.rds")
intercept = -1.564
  
# define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("QHealth UNHS"),
   
   sidebarLayout(
      sidebarPanel(
         checkboxInput("sex_f",
                     "Female gender:"),
         checkboxInput("atsi_y",
                       "Indigenous:"),
         checkboxInput("ref_bilat",
                       "Bilateral refer:"),
         checkboxInput("cranio_y",
                       "Craniofacial abnormalities:"),
         checkboxInput("famHx_y",
                       "Family history:"),
         checkboxInput("syndrome_y",
                       "Syndrome:"), 
         actionButton('do', "Go")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         textOutput("prediction")
      )
   )
)

# Define server logic 
server <- function(input, output) {
  
  data = eventReactive(input$do, {
    sex_f = ifelse(input$sex_f, 1, 0)
    atsi_y = ifelse(input$atsi_y, 1, 0)
    ref_bilat = ifelse(input$ref_bilat, 1, 0)
    cranio_y = ifelse(input$cranio_y, 1, 0)
    famHx_y = ifelse(input$famHx_y, 1, 0)
    syndrome_y = ifelse(input$syndrome_y, 1, 0)
    data.frame(intercept, sex_f, atsi_y, ref_bilat, cranio_y, famHx_y, syndrome_y)
  })

   output$prediction <- renderText({
     pred = predict(model, newdata = data(), type = 'response')
     pred
     
        })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

