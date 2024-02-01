
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


##### Practice page #####




##### UI Logic #####
ui <- navbarPage(
  "Graphics Experiment",
  id = 'navbar',
  tabPanel("Informed Consent", informed_consent),
  tabPanel("Demographics", demographic),
  # header = ,
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
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
