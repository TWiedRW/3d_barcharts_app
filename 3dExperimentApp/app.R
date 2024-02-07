
library(shiny)
library(shinyjs)
library(rgl)
library(RSQLite)
source('R/plot-code.R')
graphData <- read.csv('Data/graphData.csv')
printedKitInfo <- read.csv('Data/printedKitInfo.csv')
practiceData <- read.csv('Data/practiceData.csv')
source('R/create_lineup.R')
source('R/generate_code.R')
options(rgl.printRglwidget = TRUE)

stl_files <- list.files('Plots/stl_files', pattern = '.stl', full.names = T)

##### UI Pages #####

##### Informed consent page #####
informed_consent <- fluidPage(
  theme = shinythemes::shinytheme('cerulean'),
  sidebarLayout(
    sidebarPanel(
      radioButtons("statStudent", "Are you currently enrolled in STAT 218?",
                   c("Yes" = TRUE,
                     "No" = FALSE), selected = ''),
      uiOutput("informed_consent"),
      uiOutput("informed_consent_button")
    
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.statStudent == 'TRUE'",
        includeHTML("Informed Consent/graphics-consent-218.html")
      ),
      conditionalPanel(
        condition = "input.statStudent == 'FALSE'",
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
      selectizeInput('age', 'Age', 
                  c('',"Under 19", "19-25", "26-30", "31-35", "36-40", "41-45", "46-50", "51-55", "56-60", "Over 60", "Prefer not to answer"),
                  selected = NULL),
      selectizeInput('gender', 'Gender Identity',
                   c('','Male', 'Female', 'Variant/Nonconforming', 'Prefer not to answer'),
                   selected = NULL),
      selectizeInput('education', 'Highest Education Level',
                   choices = c("","High School or Less",
                               "Some Undergraduate Courses",
                               "Undergraduate Degree",
                               "Some Graduate Courses",
                               "Graduate Degree",
                               "Prefer not to answer"),
                   selected = NULL)
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
      p('On the next page, you will be presented with a series of three practice graphs.',
        'Your responses will not be saved, but you will be able to see the correct answers after you submit your responses.'),
      actionButton('startPractice', 'Start Practice')
      
      

)


##### Practice #####

practice <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      uiOutput('practiceGraphTitle'),
      radioButtons('practiceGraphs', 'Which marked bar is smaller?',
                   c('Circle', 'Triangle'), selected = ''),
      sliderInput('practiceEstimate', 'If the larger marked bar is 100 units tall, how tall is the smaller marked bar?',
                  min = 0, max = 100, value = 50, step = 0.1, ticks = F),
      actionButton('submitPractice', 'Submit'),
      uiOutput('practiceSolutions')
      
    ),
    mainPanel(
      uiOutput('practiceGraphUI'),
      conditionalPanel(
        condition = "input.subjectZoo == 'devmode'",
        tableOutput('practiceTable')
      )
    )
  )
)

##### Setup #####

setup <- fluidPage(
  h2('Welcome to the Graphics Experiment'),
  p('Before you start, please enter your kit number in the box below.',
    'This number is located on the index card inside the bag of 3D-printed graphs.',
    'If you do not have a bag of 3D-printed graphs, please check the "Online Participant" checkbox to verify that you are not an in-person participant.'),
  selectInput('kitNumber', 'Kit Number', choices = c('',1:21), selected = ''),
  checkboxInput('onlineParticipant', 'Online Participant: Please only check this box if you do not have access to the 3D printed bar charts.', value = FALSE),
  uiOutput('setup')
)


##### Experiment page #####

experiment <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      uiOutput('expGraphTitle'),
      uiOutput('expPrintedLabel'),
      radioButtons('expGraphs', 'Which marked bar is smaller?',
                   c('Circle', 'Triangle'), selected = ''),
      sliderInput('expEstimate', 'If the larger marked bar is 100 units tall, how tall is the smaller marked bar?',
                  min = 0, max = 100, value = 50, step = 0.1, ticks = F),
      conditionalPanel(
        condition = 'input.consent == "FALSE"',
        helpText('Demo Mode: Because you did not agree to participate in the experiment, your data will not be saved.')
      ),
      actionButton('submitExp', 'Submit')
      
    ),
    mainPanel(
      uiOutput('expGraphDisplay'),
      conditionalPanel(
        condition = "input.subjectZoo == 'devmode'",
        uiOutput('expDevStuff')
      )
    )
  )
)




##### Exit #####
exit <- fluidPage(
  h2('Thank you'),
  conditionalPanel(
    condition = "input.consent == 'TRUE'",
    p('Thank you for participating in our experiment.',
      'Your response had been submitted.',
      'Please return the graphs to the kit bag and close out of this page.'),
  ),
  
  
  conditionalPanel(
    condition = "input.consent == 'FALSE'",
    p('Thank you for participating in our experiment.',
      'Please return the graphs to the kit bag and close out of this page.'),
  ),
  conditionalPanel(
    condition = "input.statStudent == 'TRUE'",
    h4('Completion code'),
    textOutput('completionCode'),
    helpText("Save this code and submit it to Canvas to complete your Stat 218 assignment. You will not have access to this code when you close the app.")
  ),
  p(),
  actionButton('reset', 'New Submission')
)




##### UI Logic #####
ui <- navbarPage(
  "Graphics Experiment",
  id = 'navbar',
  tabPanel("Informed Consent", informed_consent),
  tabPanel("Demographics", demographic),
  tabPanel("Instructions", instructions),
  tabPanel("Practice", practice),
  tabPanel("Setup", setup),
  tabPanel("Experiment", experiment),
  tabPanel("Finishing Up", exit),
  collapsible = TRUE
)

##### Server Logic #####
server <- function(input, output, session) {

  trial_data <- reactiveValues(
    sessionID = session$token,              #Unique shiny token
    appStartTime = Sys.time(),              #Time app started
    practice_trial = 1,                     #Current practice trial
    practice_state = 'watch',               #State of practice trial
    practice_data = NULL,                   #Data for current practice trial
    practice_start_time = NULL,             #Starting time for practice trials
    practice_end_time = NULL,               #Ending time for practice trials
    practice_data_smaller = NULL,           #Correct qualitative answer for current practice trial
    practice_data_solution = NULL,          #Correct quantitative answer for current practice trial
    practice_data_estimate = NULL,          #Participant estimate for current practice trial
    exp_kit = NULL,                         #Kit number (dbl)
    exp_lineup = NULL,                      #Lineup of trials (df)
    exp_trial = NULL,                       #Current trial number (dbl)
    exp_trial_data = NULL,                  #Data for current trial (df),
    exp_plot_type = NULL,                   #Type of plot for current trial (2dd, 3dd, 3dp, 3ds)
    graphTitle = NULL,                      #Title of current graph
    exp_start_time = NULL,                  #Time the experiment was started
    exp_plot_start_time = NULL,             #Time the current plot was displayed
    exp_plot_end_time = NULL,               #Time the current plot was submitted
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
    
    list(actionButton("submitConsent", "Next page"))

  })

  observeEvent(input$submitConsent, {
    
    #Move from informed consent to demographic page
    updateNavbarPage(session, inputId = "navbar", "Demographics")

  })

  
  ##### Demographics Logic #####
  
  output$demographic <- renderUI({
    validate(need(input$age != "", 'Please select your age to continue'))
    validate(need(input$gender != "", 'Please select your gender to continue'))
    validate(need(input$education != "", 'Please select your education to continue'))
    validate(need(input$subjectZoo != "", 'Please enter your favorite zoo animal to continue'))
    
    list(
      conditionalPanel(
        condition = "input.consent == 'FALSE'",
        helpText('Demo Mode: Because you did not agree to participate in the experiment, your data will not be saved.')
      ), 
      actionButton("toInstructions", "Next Page")
    )
    
    
  })
  
  observeEvent(input$toInstructions, {
    updateNavbarPage(session, inputId = "navbar", "Instructions")
  })
  
  ##### Instructions Logic #####
  
  #Update kit value to 'Online' if online participant is checked
  observeEvent(input$startPractice, {
    updateNavbarPage(session, inputId = "navbar", "Practice")
    trial_data$practice_start_time <- Sys.time()
  })
  
  
  ##### Practice Logic #####
  
  
  #Generate practice graph and solution
  output$practiceGraph <- renderPlot({
    trial_data$practice_data <- filter(practiceData, practiceID == min(trial_data$practice_trial,3))
    
    pracGraph <- Bar2D(trial_data$practice_data,
          shape_order = 1)
    
    trial_data$practice_data_estimate <- trial_data$practice_data %>% 
      mutate(Height = ifelse(is.na(Identifier), NA, Height),
             Height.save = Height,
             Height.save = ifelse(is.na(Height.save), 0, Height.save),
             Height.save2 = Height,
             HeightLabel = 100*Height/max(Height, na.rm = T),
             Height = ifelse(Height == min(Height, na.rm = T), input$practiceEstimate, NA),
             Label = ifelse(is.na(Height), NA, paste0('Your estimate: ', round(input$practiceEstimate,1))),
             Height = ifelse(is.na(Height), 0, Height),
             Height = Height * max(Height.save, na.rm = T)/100,
             GroupOrderLabel = ifelse(is.na(Label), NA, GroupOrder),
             Smaller = ifelse(IDchr == '▲', 'Triangle', 
                              ifelse(IDchr == '●', 'Circle', NA)),
             Smaller = ifelse(!is.na(GroupOrderLabel), Smaller, NA))
    
    
    
    if(trial_data$practice_state == 'watch'){
      pracGraph
    } else {
      #Add estimated bar to graph
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
  
  
  # Render plot
  output$practiceGraphUI <- renderUI({
    list(plotOutput('practiceGraph', width = '16.175cm', height = '9.5cm'))
  })
  
  #Practice solution slider
  output$practiceSolutions <- renderUI({
    if(trial_data$practice_state == 'learn'){
      list(h2('Solution'), 
           disabled(radioButtons('practiceSolutionRadio', 'Correct Answer',
                   c('Circle', 'Triangle'), selected = '')),
           disabled(sliderInput('practiceSolutionSlider', 'Correct Answer',
                       min = 0, max = 100, value = 100*min(trial_data$practice_data_estimate$Height.save2, na.rm = T)/max(trial_data$practice_data_estimate$Height.save2, na.rm = T), 
                       step = 0.1, ticks = F)),
           helpText('You can adjust the estimate slider to compare your estimate to the correct answer.'),
           actionButton('practiceNext', 'Next'))
    }
  })
  
  output$practiceTable <- renderTable({
    trial_data$practice_data_estimate
  })
  
  #Practice graph title
  output$practiceGraphTitle <- renderUI({
    list(h2(paste('Graph', as.character(trial_data$practice_trial), 'of 3')))
  })
  
  output$practiceSolutionText <- renderText({
    as.character(unique(trial_data$practice_data_estimate$Smaller))
  })
  
  #Button to submit practice estimate
  observeEvent(input$submitPractice, {
    trial_data$practice_state <- 'learn'
    hide('submitPractice')
    updateRadioButtons(session, "practiceSolutionRadio", selected = as.character(trial_data$practice_data_estimate$Smaller[!is.na(trial_data$practice_data_estimate$Smaller)]))
    message(paste('Participant completed practice graph', trial_data$practice_trial))
  })

  #Button to continue to next practice graph
  observeEvent(input$practiceNext, {
    if(trial_data$practice_trial == 3 & trial_data$practice_state == 'learn'){
      trial_data$practice_trial <- 1
      updateNavbarPage(session, inputId = "navbar", "Setup")
      practice_end_time <- Sys.time()
    } else{
      trial_data$practice_trial <- trial_data$practice_trial + 1
    }
    
    trial_data$practice_state <- 'watch'
    shinyjs::show('submitPractice')
    updateSliderInput(session, "practiceEstimate", value = 50)
  })
  
  
  ##### Setup Logic #####
  
  #Update kit value to 'Online' if online participant is checked
  observeEvent(input$onlineParticipant, {
    if(input$onlineParticipant == TRUE){
      updateSelectInput(session, "kitNumber", choices = c('Online'))
    } else {
      updateSelectInput(session, "kitNumber", choices = c('',1:21),
                        selected = )
    }
  })
  
  output$setup <- renderUI({
    validate(need(input$kitNumber %in% c(1:21) | input$onlineParticipant == T, 'Please enter your kit number to continue'))
    list(
      actionButton("beginExp", "Begin Experiment"))
  })
  
  observeEvent(input$beginExp, {
    trial_data$exp_kit <- input$kitNumber
    trial_data$exp_lineup <- create_lineup(trial_data$exp_kit)
    trial_data$exp_trial <- 1
    trial_data$exp_start_time <- Sys.time()
    trial_data$exp_plot_start_time <- Sys.time()
    updateNavbarPage(session, inputId = "navbar", "Experiment")
  })
  
  ##### Experiment Logic #####
  
  #Update graph title
  output$expGraphTitle <- renderUI({
    trial_data$graphTitle <- paste('Graph', as.character(trial_data$exp_trial), 
                        'of', as.character(nrow(trial_data$exp_lineup)))
    list(h2(trial_data$graphTitle))
  })
  
  output$expPrintedLabel <- renderUI({
    
    validate(need(trial_data$exp_plot_type == '3dPrint', ''))
    selection_options <- trial_data$exp_lineup %>% 
      filter(plot == '3dPrint')
    
    
    if(trial_data$exp_plot_type == '3dPrint'){
      list(selectInput('expPrintedLabel', 'Identify the label on the bottom of the 3D printed graph.',
                       choices = c('-- Select ID --', sort(selection_options$kitMarking), 'Other')),
           conditionalPanel(
             'input.expPrintedLabel=="Other"',
             textInput('expPrintedLabelCorrecter', 'If the identifier on the bottom of the plot does not match any of the available options, please enter the identifier here: ')
           ))
    }
  })
  
  
  #Button to submit experiment estimate
  observeEvent(input$submitExp, {
    
    try(close3d())

    message(sprintf('Participant completed experiment graph %d (%s)', 
                    trial_data$exp_trial,
                    trial_data$exp_plot_type))
    trial_data$exp_trial_end_time <- Sys.time()
    trial_data$exp_trial <- trial_data$exp_trial + 1
    updateSliderInput(session, "expEstimate", value = 50)
    trial_data$exp_plot_type <- 'Refresh'
    if(trial_data$exp_trial > nrow(trial_data$exp_lineup)){
      updateNavbarPage(session, inputId = "navbar", "Finishing Up")
      trial_data$exp_trial <- NULL
    }
    try(close3d())
  })
  

  
  #Generate 2d plots
  output$bar2d <- renderPlot({
    validate(need(as.character(trial_data$exp_trial_data$plot) == '2dDigital', 'Error: Wrong plot type for this graph'))
    Bar2D(trial_data$exp_trial_data,
          shape_order = 1)
  })
  
  
  
  #Generate 3d digital plots
  output$bar3dd <- renderRglwidget({
    try(close3d())
    validate(need(as.character(trial_data$exp_trial_data$plot) == '3dDigital', 'Error: Wrong plot type for this graph'))
    open3d()
    Bar3D(stl_files[trial_data$exp_trial],
          color = 'blue')
    rglwidget()
    
  })
  
  
  
  
  #Generate 3d printed plots
  output$bar3dp <- renderPlot({
    validate(need(as.character(trial_data$exp_trial_data$plot) == '3dPrint', 'Error: Wrong plot type for this graph'))
    ggplot(mapping = aes(x = 0, y = 0)) + 
      geom_text(label = 'Please select a 3D printed graph from your kit.',
                 size = 8) + 
      theme_void()
  })
  
  
  #Generate 3d static plots
  output$bar3ds <- renderPlot({
    validate(need(as.character(trial_data$exp_trial_data$plot) == '3dStatic', 'Error: Wrong plot type for this graph'))
    Bar2D(trial_data$exp_trial_data,
          shape_order = 1)
  })
  
  
  
  
  #---------------------------------------------------------------------------#
  
  #Display experiment plot
  output$expGraphDisplay <- renderUI({
  
    #Check that all conditions are met
    validate(need(!is.null(trial_data$exp_kit), 'Error: No kit number selected'),
             need(!is.null(trial_data$exp_lineup), 'Error: Trial lineup not created'),
             need(!is.null(trial_data$exp_trial), 'Error: Trial number not set'))
    
    # try(close3d())
    
    #Subset lineup to current trial
    trial_data$exp_trial_data <- filter(trial_data$exp_lineup, 
                                        trial_order == trial_data$exp_trial) %>% 
      left_join(graphData, by = 'fileID', relationship = 'many-to-many')
    
    #Lineup plot type
    trial_data$exp_plot_type <- unique(as.character(trial_data$exp_trial_data$plot))
    
    #Render plot based on plot type
    list(switch (as.character(unique(trial_data$exp_trial_data$plot)),
            '2dDigital' = plotOutput('bar2d', width = '14.175cm', height = '9.5cm'),
            '3dPrint' = plotOutput('bar3dp', width = '16.175cm', height = '9.5cm'),
            '3dStatic'  = plotOutput('bar3ds', width = '16.175cm', height = '9.5cm'),
            '3dDigital' = rglwidgetOutput('bar3dd')
    ))
    
    
  })
  
  
  output$devTrialData <- renderPrint({
    print(list(
      exp_kit = trial_data$exp_kit,             
      exp_lineup = trial_data$exp_lineup,          
      exp_trial = trial_data$exp_trial,           
      exp_trial_data = trial_data$exp_trial_data,      
      exp_plot_type = trial_data$exp_plot_type
    ))
  })
  
  output$expDevStuff <- renderUI({
    list(
      h2('Developer tools'),
      verbatimTextOutput('devTrialData')
    )
  })
  
  
  #-------------------------------------------------------------------------#
  
  
  ##### Exit Logic #####
  
  output$completionCode <- renderText({
    generate_code(NULL)
  })
  
  observeEvent(input$reset, {
    shinyjs::refresh()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
