library(shiny)

# load model and set intercept value
model = readRDS("model.rds")
intercept = -1.564
  
# define UI
ui <- fluidPage(
   
   # Application title
   headerPanel("QHealth UNHS"),
   tabsetPanel(
     tabPanel("Results", fluid = TRUE,
          sidebarLayout(
            sidebarPanel(
                  h5("Select all that are true"),
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
                           ), # close sidebarPanel
              mainPanel(
                br(),
                  textOutput("prediction")
              ) # close mainPanel
          ) # close sidebarLayout
     ), # close tabpanel
  
          tabPanel("About", fluid = TRUE,
                  br(),
                  p('This app predicts the likely hearing outcome for a baby who referred on the newborn hearing screen based on subject characteristics 
                     and risk factors'),
                  p('To use the app select all characteristics that are true. For example, for a female, indigenous baby with family history of hearing loss, 
                    check the "Female gender", "Indigenous", and "Family history" boxes then press the "Go" button'),
                  p('The model calculates the risk that this infant will be diagnosed with a permanent hearing loss. Risk is a proportion between 0 and 1,
                    0 meaning that the outcome is very unlikley and 1 meaning that it is very likely.'),
                  p('Article by Driscoll et al. (2019) bla bla bla'),
                  p('note: the model cannot make predictions for infants with: bacterial meningitis, hyperbilirubinaemia, perinatal infection, severe asphyxia, 
                    and professional concern risk factors due to small numbers of babies with these risk factors')
                  ) # close tabPanel
          ) # close tabset panel
     ) # close fluid page

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
      paste("The risk of hearing loss for this child is", round(pred, 2))
        })

}

# Run the application 
shinyApp(ui = ui, server = server)

