library(shiny)


# Define UI for application that draws a histogram
ui <- fluidPage(
  fileInput("upload", NULL, buttonLabel = "Upload...", multiple = TRUE),
  tableOutput("files"),

  
sidebarPanel(width = 12,
useShinyjs(),
id = "panel",
numericInput("num", "Please Insert Current Number of Outs", value = 0, min = 0, max = 3),

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

textOutput("result"),

br(),

sliderInput("success", "Probability of Success", value = 50, min = 0, max = 100),
),
actionButton("resetAll", "Clear", class = "btn-danger"),
actionButton("done", "Click When Done", class = "btn-lg btn-success"),

)
# Define server logic required to draw a histogram
server = function(input, output) {
  output$result <- renderText({
    paste("Lineup:", input$"Number1")
  })
  observeEvent(input$resetAll, {
    reset("panel")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)