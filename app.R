library(shiny)

# load model and set intercept value
model = readRDS("model.rds")
intercept = -2.030

# define UI
ui <- fluidPage(
   
   # Application title
   headerPanel("Newborn hearing screening risk calculator"),
   tabsetPanel(
     tabPanel("Risk calculator", fluid = TRUE,
          sidebarLayout(
            sidebarPanel(
                  h5("Select all that are true"),
                  checkboxInput("sex_f",
                     "Female gender"),
                  checkboxInput("atsi_y",
                       "Indigenous"),
                  checkboxInput("ref_bilat",
                       "Bilateral refer"),
                  checkboxInput("cranio_y",
                       "Craniofacial abnormalities"),
                  checkboxInput("famHx_y",
                       "Family history"),
                  checkboxInput("syndrome_y",
                       "Syndrome"), 
                  actionButton('do', "Go"),
                  br()
                           ), # close sidebarPanel
              mainPanel(
                br(),
                textOutput("prediction"),
                br()
              ) # close mainPanel
          ) # close sidebarLayout
     ), # close tabpanel
     
          tabPanel("About", fluid = TRUE,
                  br(),
                  p('This app predicts the likely hearing outcome for a baby who referred on QHealth’s Healthy Hearing Newborn Hearing Screen,  
                    based on subject characteristics and reported risk factors.'),
                  p('To use the app, select all characteristics that are true. For example, for a female, indigenous baby with family history of hearing loss, 
                    check the "Female gender", "Indigenous", and "Family history" boxes then press the "Go" button'),
                  p('The risk that this child will go on to be diagnosed with permanent hearing loss is then calculated, given the selected risk factors. 
                    Risk is a proportion between 0 and 1,
                    0 meaning that the outcome is very unlikley and 1 meaning that it is almost certain. Average risk for a male child is 0.12, meaning that
                    for every 100 males who refer from the screen, 12 will be diagnosed with a permanent hearing loss (1.2 for every 10, or 12%). You can 
                    calculate this by deselecting all factors and pressing "Go". Average risk for females is slightly higher (0.15), meaning that for females, 15% of
                    referrals will go on to be diagnosed with permanent hearing loss. 
                      If we use the calculator 
                    with our initial example: a female indigenous baby with family history of hearing loss, we can see the risk is 0.37, quite a lot higher than the 
                    average risk for females (0.15). This baby
                    has almost 40% risk of being diagnosed with permanent hearing loss.'),
                  p('The risk calculator cannot make predictions for infants with: bacterial meningitis, hyperbilirubinaemia, perinatal infection, severe asphyxia, 
                    prolonged ventilation 
                    or professional concern risk factors due to small numbers of babies with these risk factors'),
                  p('For further details, see the pubilcation by Fitzgibbons, J., Driscoll, C., Beswick, R., & Myers, J. xxxxxxxxxxxx (Submitted to xxxxxxxxxxx).')
                  
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

