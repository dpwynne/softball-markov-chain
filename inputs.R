library(shiny)
library(shinyjs)

## We can ask them to rename a file player_stats.csv and overwrite the old player_stats.csv
# in the Data folder before running
# Either keep the upload button or just write one line of code here to import player_stats.csv

# Define UI for application that draws a histogram
ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("files"),

  
sidebarPanel(width = 12,
useShinyjs(),
id = "panel",
numericInput("num", "Please Insert Current Number of Outs", value = 0, min = 0, max = 3),

# Ask Bret how this should be reworded to reflect current player at bat and subsequent batters
h3("You will now insert the batting order using player number"),
br(),

fluidRow(
  column(3,
selectInput("Number1", "Insert Current batter", selected = 0, Softball_data1$NO.)),
  column(3,
selectInput("Number2", "Insert Batter 2", Softball_data1$NO.)),
  column(3,
selectInput("Number3", "Insert Batter 3", Softball_data1$NO.)),
  column(3,
selectInput("Number4", "Insert Batter 4", Softball_data1$NO.)),
  column(3,
selectInput("Number5", "Insert Batter 5", Softball_data1$NO.)),
  column(3,
selectInput("Number6", "Insert Batter 6", Softball_data1$NO.)),
  column(3,
selectInput("Number7", "Insert Batter 7", Softball_data1$NO.)),
  column(3,
selectInput("Number8", "Insert Batter 8", Softball_data1$NO.)),
  column(3,
selectInput("Number9", "Insert Batter 9", Softball_data1$NO.))
),

plotOutput("run_distribution"),

tableOutput("result"),

br(),

sliderInput("success", "Probability of Success", value = 50, min = 0, max = 100),
),

# Ask Bret if these instructions make sense
actionButton("resetAll", "Clear", class = "btn-danger"),
actionButton("done", "Run Model", class = "btn-lg btn-success"),

)

# Define server logic
server = function(input, output) {
  
  ## pull the batter numbers
  X1 <- reactive(input$Number1) # may need to fix this
  # however X1 works, do the same thing for X2-X9
  
  # fix the U0 matrix so it's not 1 in [1,1] but reflects current inning state
  U0_nosteal <- # some function of reactive(input$outs)
    # for now, assume "no steal" - runner on 1st, input$outs outs state
  U0_steal <- # function of reactive(input$success), reactive(input$outs)
    # assume "steal" to runner on 2nd, X outs with prob. p and no on, X+1 outs with prob. 1-p
  
  # simulate inning - remember to fix Simulated_inning to include U0 as input instead of hard-coding
  # either source the code or copy the code at the beginning of the file
  simulate_nosteal <- Simulated_inning(Softball_data1, X1, X2, X3, X4, X5, X6, X7, X8, X9, U0_nosteal)
  simulated_steal <- Simulated_inning(Softball_data1, X1, X2, X3, X4, X5, X6, X7, X8, X9, U0_steal)

  # create a data frame that will make it easy to ggplot with a facet by situation
  simulated_all <- c(simulate_nosteal, simulated_steal)
  inning <- data.frame(runs = rep(seq(0, 20), 2),
                             situation= rep(c("No Steal Attempt", "Steal Attempt"), each = 21),
                             prob = simulated_all)
  
  output$run_distribution <- renderPlot({
    # make a ggplot faceted by situation
  })
  
  # summarize distribution of runs scored in each situation
  # May have to round things if this doesn't automatically round when output
  runs <- inning %>% group_by(situation) %>% summarize(
    `Expected Runs` = sum(runs*prob),
    `Std. Dev. of Runs` = sqrt(sum((runs - `Expected Runs`)^2*prob)) # confirm this is the 335 formula for sd of discrete RV
  )
  
  # output summary of distribution  
  output$result <- renderTable({
   runs # this may be the wrong way to render the table 
    #paste("Lineup:", input$"Number1")
  })
  
  # reset the inputs - Clear button
  observeEvent(input$resetAll, {
    reset("panel")
  })
  
  # Add an observeEvent that runs the whole model after clicking Run Model
  
}

# Run the application 
shinyApp(ui = ui, server = server)