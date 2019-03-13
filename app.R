library(shiny)

# load model and set intercept value
model = readRDS("model.rds")
intercept = -1.564

# descriptions
Bacterial_meng = "Infants with bacterial meningitis are approximately 3 times more likely to have permanent hearing loss than without this risk factor."
Cranio = "Infants with craniofacial abnormalities are approximately 7 times more likely to have permanent hearing loss than without this risk factor."
Family_hx = "Infants with family history of hearing loss are approximately 4 times more likely to have permanent hearing loss than without this risk factor."
Hyperbili = "Infants with hyperbilirubinaemia are approximately 2 times more likely to have permanent hearing loss than without this risk factor."
Perinatal_inf = "Infants with perinatal infection are approximately 5 times more likely to have permanent hearing loss than without this risk factor."
Prof_conc = "Infants with professional concern are approximately 3 times more likely to have permanent hearing loss than without this risk factor."
Prol_vent = "Infants with prolonged ventilation are approximately 2 times more likely to have permanent hearing loss than without this risk factor."
Bilat_ref = "Infants who refer in both ears are approximately 4 times more likely to have permanent hearing loss than unilateral."
Sev_asph = "Infants with severe asphyxia are approximately 4 times more likely to have permanent hearing loss than without this risk factor."
Syndrome = "Infants with a syndrome are approximately 4 times more likely to have permanent hearing loss than without this risk factor."
  
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
     
     tabPanel("Odds Ratios", fluid = TRUE,
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "rf", label = "Select risk factor", 
                              choices = c("Bacterial meningitis", "Bilateral refer", "Craniofacial abnormalities", "Family history", 
                                          "Hyperbilirubinaemia", "Perinatal infection", "Professional concern", 
                                          "Prolonged ventilation", "Severe asphyxia", "Syndrome")),
                  actionButton('go', "Go"),
                  br()
                ), # close sidebarPanel
                mainPanel(
                  br(),
                  textOutput("odds"),
                  p(),
                  textOutput("description"),
                  br()
                ) # close mainPanel
              ) # close sidebarLayout
     ), # close tabpanel
  
          tabPanel("About", fluid = TRUE,
                  br(),
                  p('This app predicts the likely hearing outcome for a baby who referred on QHealth’s Healthy Hearing Newborn Hearing Screen,  
                    based on subject characteristics and reported risk factors.'),
                  p('To use the "Risk calculator" tab, select all characteristics that are true. For example, for a female, indigenous baby with family history of hearing loss, 
                    check the "Female gender", "Indigenous", and "Family history" boxes then press the "Go" button'),
                  p('The risk that this child will go on to be diagnosed with permanent hearing loss is then calculated, given the selected risk factors. 
                    Risk is a proportion between 0 and 1,
                    0 meaning that the outcome is very unlikley and 1 meaning that it is almost certain. Average risk for a male child is 0.17, meaning that
                    for every 100 males who refer from the screen, 17 will be diagnosed with a permanent hearing loss (or 1.7 for every 10). You can 
                    calculate this by deselecting all factors and pressing "Go". Average risk for females is slightly higher (0.19), meaning that for females, for 
                    every 10 referrals, approximately 2 (1.9) will go on to be diagnosed with permanent hearing loss. 
                      If we use the calculator 
                    with our first example: a female indigenous baby with family history of hearing loss, we can see the risk is 0.41, quite a lot higher than the 
                    average risk for females (0.19). This baby
                    has approximately 40% risk of being diagnosed with permanent hearing loss.'),
                  p('The risk calculator cannot make predictions for infants with: bacterial meningitis, hyperbilirubinaemia, perinatal infection, severe asphyxia, 
                    and professional concern risk factors due to small numbers of babies with these risk factors'),
                  p('In this case, you can use the "Odds ratios" tab, which calculates the relative odds ratio for a child with a certain risk factor.'),
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

   
   odds = eventReactive(input$go, {
   switch(input$rf, 
          "Bacterial meningitis" = 2.55,
          "Craniofacial abnormalities" = 7.27,
          "Family history" = 4.07,
          "Hyperbilirubinaemia" = 2.23,
          "Perinatal infection" = 4.77,
          "Professional concern" = 2.83,
          "Prolonged ventilation" = 1.82,
          "Bilateral refer" = 3.8,
          "Severe asphyxia" = 3.75,
          "Syndrome" = 4.16
   )
   })
     
   description = eventReactive(input$go, { 
     switch(input$rf,
            "Bacterial meningitis" = Bacterial_meng,
            "Craniofacial abnormalities" = Cranio,
            "Family history" = Family_hx,
            "Hyperbilirubinaemia" = Hyperbili,
            "Perinatal infection" = Perinatal_inf,
            "Professional concern" = Prof_conc,
            "Prolonged ventilation" = Prol_vent,
            "Bilateral refer" = Bilat_ref,
            "Severe asphyxia" = Sev_asph,
            "Syndrome" = Syndrome
            )
   })
   
   output$odds = renderText({
     odds = odds()
     paste("Odds ratio = ", odds)
   })
     
   output$description = renderText({
     description = description()
     print(description)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

