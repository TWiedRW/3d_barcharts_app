
library(shiny)


##### UI Pages #####

##### Informed consent page #####
informed_consent <- fluidPage(
  theme = shinythemes::shinytheme('cerulean'),
  sidebarLayout(
    sidebarPanel(
      radioButtons("statStudent", "Are you currently enrolled in STAT 218?",
                   c("Yes" = "yes",
                     "No" = "no"), selected = ''),
      uiOutput("informed_consent"),
      uiOutput("informed_consent_button")
    
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.statStudent == 'yes'",
        includeHTML("Informed Consent/graphics-consent-218.html")
      ),
      conditionalPanel(
        condition = "input.statStudent == 'no'",
        includeHTML("Informed Consent/graphics-consent-dept.html")
      )
    )
  )
)


##### Demographics page #####
demographic <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2('Demographic Information'),
      p('Please select the demographic information that best describes you.'),
      radioButtons('age', 'Age', 
                  c("Under 19", "19-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "Over 60", "Prefer not to answer"),
                  selected = ''),
      radioButtons('gender', 'Gender Identity',
                   c('Male', 'Female', 'Variant/Nonconforming', 'Prefer not to answer'),
                   selected = ''),
      radioButtons('education', 'Highest Education Level',
                   choices = c("High School or Less",
                               "Some Undergraduate Courses",
                               "Undergraduate Degree",
                               "Some Graduate Courses",
                               "Graduate Degree",
                               "Prefer not to answer"),
                   selected = '')
    ),
    mainPanel(
      h2('Participant Identifier'),
      p('Your session identifier is below. You do not need to save this value.'),
      textOutput('sessionInfo'),
      p('\n'),
      p('To ensure that we have another unique identifier for you, please answer the following question.'),
      textInput('subjectZoo', 'What is your favorite zoo animal?'),
      uiOutput('demographic')
    )
  )
)


##### Instructions #####

instructions <- fluidPage(

      h2('Instructions'),
      p('Thank you for participating in our experiment on perceptural judgments in different graphical mediums.',
        'In this survey, you will see a series of graphs. Each graph will have 10 bars; two of the bars will be identified with a circle and a triangle.',
        'First, we will ask you to identify which bar (circle or triangle) is smaller.',
        'Then, we will ask you to estimate the size of the smaller bar if the larger bar is 100 units tall.'),
      p('Before you start, please enter your kit number in the box below.',
        'This number is located on the index card inside the bag of 3D-printed graphs.',
        'If you do not have a bag of 3D-printed graphs, please check the "Online Participant" checkbox to verify that you are not an in-person participant.'),
      
      selectInput('kitNumber', 'Kit Number', choices = c('',1:21), selected = 'Oh hello'),
      checkboxInput('onlineParticipant', 'Online Participant: Please only check this box if you do not have access to the 3D printed bar charts.', value = FALSE),
      uiOutput('instructions')
      

)


# In this survey, a series of graphs will be given to you. Each graph will have 10 bars; two of the bars will be identified with a circle and a triangle. First, we will ask you to identify which bar (circle or triangle) is smaller. Then, we will ask you to estimate the size of the smaller bar if the larger bar is 100 units tall.
# 
# In some cases, the graphs will be displayed on screen. In other cases, we will ask you to get one of the graphs from a ziploc bag you should have received before starting this study. For these 3D-printed graphs, we will ask you to tell us which graph you selected using the ID code engraved on the bottom of the chart.
# 
# The next screen will display a few sample graphs. The answers for each of these graphs are located underneath the sample.

##### UI Logic #####
ui <- navbarPage(
  "Graphics Experiment",
  id = 'navbar',
  tabPanel("Informed Consent", informed_consent),
  tabPanel("Demographics", demographic),
  tabPanel("Instructions", instructions),
  collapsible = TRUE
)

##### Server Logic #####
server <- function(input, output, session) {

  trial_data <- reactiveValues(
    sessionID = NULL,
    appStartTime = NULL
  )
  
  
  
  
  
  output$sessionInfo <- renderText({
    as.character(session$token)
  })
  
  output
  
  ##### Informed Consent Logic #####
  
  output$informed_consent <- renderUI({
    if(req(input$statStudent) != ""){
      list(radioButtons("consent", "I have read the informed consent document and agree to participate in this experiment",
                   c("I agree. You may save my data" = TRUE,
                     "I do not agree. Please do not save my data" = FALSE), selected = ''))
    }
  })
  
  output$informed_consent_button <- renderUI({
    validate(need(req(input$consent) != '', 'Please select an option to continue'))
    list(actionButton("startExperiment", "Next page"))

  })
  
  
  
  
  observeEvent(input$startExperiment, {
    
    #Move from informed consent to demographic page
    updateNavbarPage(session, inputId = "navbar", "Demographics")
    
    
      
  })

  
  ##### Demographics Logic #####
  
  output$demographic <- renderUI({
    validate(need(input$age != "", 'Please enter your age to continue'))
    validate(need(input$gender != "", 'Please enter your gender to continue'))
    validate(need(input$education != "", 'Please enter your education to continue'))
    validate(need(input$subjectZoo != "", 'Please enter your favorite zoo animal to continue'))
    actionButton("startPractice", "Start Practice")
  })
  
  ##### Instructions Logic #####
  
  #Update kit value to 'Online' if online participant is checked
  observeEvent(input$onlineParticipant, {
    if(input$onlineParticipant == TRUE){
      updateSelectInput(session, "kitNumber", choices = c('Online'))
    } else {
      updateSelectInput(session, "kitNumber", choices = c('',1:21),
                        selected = '')
    }
  })
  
  output$instructions <- renderUI({
    validate(need(input$kitNumber %in% c(1:21) | input$onlineParticipant == T, 'Please enter your kit number to continue'))
    list(actionButton("startPractice", "Start Practice"))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
