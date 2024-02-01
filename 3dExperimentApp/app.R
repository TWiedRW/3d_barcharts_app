
library(shiny)
source('R/plot-code.R')
graphData <- read.csv('Data/graphData.csv')
printedKitInfo <- read.csv('Data/printedKitInfo.csv')
practiceData <- read.csv('Data/practiceData.csv')


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


##### Practice #####

practice <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  sidebarLayout(
    sidebarPanel(
      h2('Practice'),
      radioButtons('practiceGraphs', 'Which marked bar is smaller?',
                   c('Circle', 'Triangle'), selected = ''),
      sliderInput('practiceEstimate', 'If the larger marked bar is 100 units tall, how tall is the smaller marked bar?',
                  min = 0, max = 100, value = 50, step = 0.1, ticks = F),
      actionButton('submitPractice', 'Submit')
      
    ),
    mainPanel(
      uiOutput('practiceGraphUI')
    )
  )
)


##### UI Logic #####
ui <- navbarPage(
  "Graphics Experiment",
  id = 'navbar',
  tabPanel("Informed Consent", informed_consent),
  tabPanel("Demographics", demographic),
  tabPanel("Instructions", instructions),
  tabPanel("Practice", practice),
  collapsible = TRUE
)

##### Server Logic #####
server <- function(input, output, session) {

  trial_data <- reactiveValues(
    sessionID = NULL,
    appStartTime = NULL,
    practice_trial = 1,
    practice_state = 'watch',
    practice_data = NULL,
    practice_data_solution = NULL,
    practice_data_estimate = NULL
  )
  
  
  
  
  
  output$sessionInfo <- renderText({
    as.character(session$token)
  })
  
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
                        selected = )
    }
  })
  
  output$instructions <- renderUI({
    validate(need(input$kitNumber %in% c(1:21) | input$onlineParticipant == T, 'Please enter your kit number to continue'))
    list(p('On the next page, you will be presented with a series of three practice graphs.',
           'Your responses will not be saved, but you will be able to see the correct answers after you submit your responses.'),
      actionButton("startPractice", "Start Practice"))
  })
  
  
  
  ##### Practice Logic #####
  
  
  #Generate practice graph
  output$practiceGraph <- renderPlot({
    trial_data$practice_data <- filter(practiceData, practiceID == min(trial_data$practice_trial,3))
    
    pracGraph <- Bar2D(trial_data$practice_data,
          shape_order = 1)
    
    
    trial_data$practice_data_estimate <- trial_data$practice_data %>% 
      mutate(Height = ifelse(is.na(Identifier), NA, Height),
             Height.save = Height,
             Height.save = ifelse(is.na(Height.save), 0, Height.save),
             HeightLabel = 100*Height/max(Height, na.rm = T),
             Height = ifelse(Height == min(Height, na.rm = T), input$practiceEstimate, NA),
             Label = ifelse(is.na(Height), NA, paste0('Your estimate: ', round(input$practiceEstimate,1))),
             Height = ifelse(is.na(Height), 0, Height),
             Height = Height * max(Height.save, na.rm = T)/100,
             GroupOrderLabel = ifelse(is.na(Label), NA, GroupOrder))
    
    
    
    if(trial_data$practice_state == 'watch'){
      pracGraph
    } else {
      #
      pracGraph + 
        geom_bar(mapping = aes(x = trial_data$practice_data_estimate$GroupOrder, 
                               y = trial_data$practice_data_estimate$Height), 
                 stat = 'identity', fill = 'red', alpha = 1/2,
                 width = 0.8, na.rm = T) + 
        geom_label(mapping = aes(x = trial_data$practice_data_estimate$GroupOrder, 
                                y = 100, 
                                label = trial_data$practice_data_estimate$Label),
                   na.rm = T) + 
        geom_segment(mapping = aes(x = trial_data$practice_data_estimate$GroupOrderLabel, 
                                   xend = trial_data$practice_data_estimate$GroupOrderLabel, 
                                   y = trial_data$practice_data_estimate$Height, yend = 95), 
                     color = 'red', alpha = 1/2, na.rm = T) 
    }

    
  })
  
  
  output$practiceGraphUI <- renderUI({
    list(plotOutput('practiceGraph'))
  })
  
  
  


  #
  observeEvent(input$submitPractice, {
    if(trial_data$practice_trial == 3 & trial_data$practice_state == 'learn'){
      updateNavbarPage(session, inputId = "navbar", "Demographics")
      trial_data$practice_trial <- 1
      
    } else{
      trial_data$practice_trial <- trial_data$practice_trial + 1
      shinyjs::disable('estimatePractice')
      shinyjs::hide('submitPractice')
      trial_data$practice_state <- 'learn'
    }
  })
  
  observeEvent(input$practiceNext, {
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
