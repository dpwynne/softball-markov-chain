library(dplyr)
library(tidyverse)
library(shinyjs)
library(ggplot2)
library(shiny)
library(scales)

# Download softball dataset
Softball_data1 <- readxl::read_excel("Data/Softball_data1.xlsx")
Softball_data1 <- Softball_data1 %>%
  mutate(NO. = suppressWarnings(as.numeric(NO.))) %>%
  filter(!is.na(NO.), AB > 0) %>% arrange(desc(H))
softball_input <- Softball_data1$NO.
names(softball_input) <- paste(Softball_data1$NO., 
                               Softball_data1$NAME, sep = ": ")

# RA: the matrix of runs scored on a transition with no outs
RA <- matrix(1:64, nrow = 8, ncol = 8)
RA <- cbind(c(1,2,2,2,3,3,3,4),c(0,1,1,1,2,2,2,3),c(0,1,1,1,2,2,2,3),c(0,1,1,1,2,2,2,3),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,0,0,0,1))

# RB: the matrix of runs scored on a transition resulting in one more out
RB <- matrix(1:64, nrow = 8, ncol = 8)
RB <- cbind(c(0,1,1,1,2,2,2,3),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,1,1,1,2),c(0,0,0,0,0,0,0,1),c(0,0,0,0,0,0,0,1),c(0,0,0,0,0,0,0,1),c(0,0,0,0,0,0,0,0))

# R0: a matrix of 0 runs scored
R0 <- matrix(0, nrow = 8, ncol = 8)

# Rout - matrix of inning end, no runs scored
Rout <- matrix(0, nrow = 8, ncol = 1)

# 25 x 25 R matrix - hard-coded based on number of runs scored in each transition
R_matrix <- rbind(
  cbind(RA, RB, R0, Rout),
  cbind(R0, RA, RB, Rout),
  cbind(R0, R0, RA, Rout),
  matrix(0, nrow = 1, ncol = 25)
)


# U0 - 25x21 matrix with first entry 1
U0 <- matrix(data = 0L, ncol = 25, nrow = 21)
U0[1,1] <- 1


Un_plus_one <- function(Un, P, R){
  
  
  # Un: the 21 x 25 Un matrix from the previous iteration
  # The j^th row of Un indicates that (j-1) runs have scored this inning
  # P: the 25 x 25 transition matrix for the batter
  # R: the 25 x 25 "runs" matrix indicating how many runs scored on a transition
  
  # First we subset the P matrix into P0, P1, P2, P3, P4
  # where Pk is a matrix with the entry of P if the corresponding entry of R is k and 0 otherwise
  P0 <- P * (R == 0)
  P1 <- P * (R == 1)
  P2 <- P * (R == 2)
  P3 <- P * (R == 3)
  P4 <- P * (R == 4)
  
  # Now we create the U_(n+1) matrix
  
  Unplus <- matrix(0, nrow = 21, ncol = 25)
  
  # The first 4 rows of the matrix have only partial sums
  
  Unplus[1,] <- Un[1,] %*% P0
  Unplus[2,] <- Un[2,] %*% P0 + Un[1,] %*% P1
  Unplus[3,] <- Un[3,] %*% P0 + Un[2,] %*% P1 + Un[1,] %*% P2
  Unplus[4,] <- Un[4,] %*% P0 + Un[3,] %*% P1 + Un[2,] %*% P2 + Un[1,] %*% P3
  
  # The last 17 rows of the matrix should have full sums
  
  for (j in 5:21){
    Unplus[j,] <- Un[j,] %*% P0 + Un[j-1, ] %*% P1 + Un[j-2, ] %*% P2 + Un[j-3, ] %*% P3 + Un[j-4, ] %*% P4
  }
  
  return(Unplus)
  
  
}

# U0 - 25x21 matrix with first entry 1
U0 <- matrix(data = 0L, ncol = 25, nrow = 21)
U0[1,1] <- 1










# Define UI for application that draws a histogram
ui <- fluidPage(sidebarPanel(width = 12,
                             useShinyjs(),
                             id = "panel",
                             numericInput("outs", "Please Insert Current Number of Outs", value = 0, min = 0, max = 3),
                             
                             # Ask Bret how this should be reworded to reflect current player at bat and subsequent batters
                             h3("You will now insert the batting order using player number"),
                             br(),
                             
                             fluidRow(
                               column(3,
                                      selectInput("Number1", "Current batter", selected = softball_input[1], softball_input)),
                               column(3,
                                      selectInput("Number2", "On-deck batter", selected = softball_input[2], softball_input)),
                               column(3,
                                      selectInput("Number3", "Inning Batter 3", selected = softball_input[3], softball_input)),
                               column(3,
                                      selectInput("Number4", "Inning Batter 4", selected = softball_input[4], softball_input)),
                               column(3,
                                      selectInput("Number5", "Inning Batter 5", selected = softball_input[5], softball_input)),
                               column(3,
                                      selectInput("Number6", "Inning Batter 6", selected = softball_input[6], softball_input)),
                               column(3,
                                      selectInput("Number7", "Inning Batter 7", selected = softball_input[7], softball_input)),
                               column(3,
                                      selectInput("Number8", "Inning Batter 8", selected = softball_input[8], softball_input)),
                               column(3,
                                      selectInput("Number9", "Inning Batter 9", selected = softball_input[9], softball_input))
                             ),
                             
                             
                             br(),
                             
                             sliderInput("success", "Probability of Success", value = 50, min = 0, max = 100),
                             
                             br(), 
                             
                             actionButton("resetAll", "Clear", class = "btn-danger"),
                             
                             
                             
                             br(), 
                             br(),
                             
                             plotOutput("run_distribution"),
                             br(),
                             tableOutput("result"),
),

# Ask Bret if these instructions make sense


)

# Define server logic
server = function(input, output) {
  
  Simulated_inning <- function(Y,U0,P1,P2,P3,P4,P5,P6,P7,P8,P9){
    # Function to calculate different player's U matrices.
    # Y: a data frame containing the number of at-bats, hits, doubles, triples, home runs and walks for each player.
    # P1-P9: integers - the player number to compute the U matrix for.
    
    
    
    # Create a 25 x 25 x 9 array for the player transition matrices
    # Check here to make sure that each 3rd dimension (25 x 25 matrix) has the right numbers
    P <- array(c(
      player_transition_matrix(P1, Y),
      player_transition_matrix(P2, Y),
      player_transition_matrix(P3, Y),
      player_transition_matrix(P4, Y),
      player_transition_matrix(P5, Y),
      player_transition_matrix(P6, Y),
      player_transition_matrix(P7, Y),
      player_transition_matrix(P8, Y),
      player_transition_matrix(P9, Y)
    ),
    dim = c(25, 25, 9))  
    
    
    # flip between U0 and U1 - U0 = Un and U1 = UnPlusOne
    U1 <- U0
    
    # for iteration
    i <- 0
    
    # while loop to reiterate the function until the 25th column = 1.
    # while Un after batter has less than 99.9% chance of being end of inning 
    # U0 is the inning state at start of the at-bat
    # U1 is the inning state at the end of the at-bat
    while(sum(U1[,25]) < 0.999){
      player_num <- (i %% 9) + 1 # get the next player in the lineup, when i = 0 it's P1
      U1 <- Un_plus_one(U0, P[,,player_num], R_matrix) # get the updated Un_plus_one
      U0 <- U1 # set the new Un_plus_one to the new Un
      i <- i+1 # increment i
    }
    
    # R_matrix: The 25 x 25 "runs" matrix indicating how many runs scored on a transition
    
    return(U1[,25])
    
  }
  
  player_transition_matrix <- function(X,Y){
    # computes the P transition matrix for a given player
    # P is used in the Un_plus_one function
    # X: an integer - the player number to compute the matrix for
    # Y: a data frame containing the number of at-bats, hits, doubles, triples, home runs and walks for each player
    
    # Set up the P matrix
    # right now we have to keep it as a data frame - will debut to make sure it works with just matrix
    df <- matrix(data = 0L, ncol = 25, nrow = 25)
    
    # Filter the data frame to get just the player number
    Y2 <- Y %>% dplyr::filter(NO. == X)
    
    # Convert everything to integers and replace - with 0
    Y2[Y2 == "-"] <- "0"
    Y2$HR <- as.integer(Y2$HR)
    Y2$AB <- as.integer(Y2$AB)
    Y2$H <- as.integer(Y2$H)
    Y2$"2B" <- as.integer(Y2$"2B")
    Y2$"3B" <- as.integer(Y2$"3B")
    Y2$BB <- as.integer(Y2$BB)
    
    # Check to make sure at-bats is positive
    # There's a better way to do this, but we'll just hard-code the stop for now
    if(Y2$AB <= 0){
      stop("Player has no at-bats")
    }
    
    # compute the probabilities of each event
    PA <- Y2$AB + Y2$BB # number of plate appearance including walks
    p_HR <- Y2$HR/PA # probability of HR on plate appearance
    p_SO <- ((Y2$H-(Y2$"2B"+Y2$"3B"+Y2$HR))/PA) + (Y2$BB/PA) # prob of single OR walk
    p_1B <- ((Y2$H-(Y2$"2B"+Y2$"3B"+Y2$HR))/PA) # prob of single
    p_2B <- Y2$"2B"/PA # prob of double
    p_3B <- Y2$"3B"/PA # prob of triple
    p_BB <- Y2$BB/PA # prob of walk
    P_OUT <- 1 - p_SO - p_2B - p_3B - p_HR  # prob of out
    
    # end of inning state
    df[25,25] <- 1
    
    # A matrix - plays that do not increase the outs
    df[1:8,1] <- p_HR
    df[1,2] <- df[2,5] <- p_SO
    df[3,2] <- df[4,2] <- df[7,2] <- df[5,5] <- df[6,5] <- df[8,5] <- p_1B
    df[1,3] <- df[3,3] <- df[4,3] <- df[7,3] <- df[2,7] <- df[5,7] <- df[6,7] <- df[8,7] <- p_2B
    df[1:8,4] <- p_3B
    df[3,5] <- df[4,6] <- df[5:8,8] <- p_BB
    
    # A matrix again
    df[9:16,9] <- p_HR
    df[9,10] <- df[10,13] <- p_SO
    df[11,10] <- df[12,10] <- df[15,10] <- df[13,13] <- df[14,13] <- df[16,13] <- p_1B
    df[9,11] <- df[11,11] <- df[12,11] <- df[15,11] <- df[10,15] <- df[13,15] <- df[14,15] <- df[16,15] <- p_2B
    df[9:16,12] <- p_3B
    df[11,13] <- df[12,14] <- df[13:16,16] <- p_BB
    
    # A matrix again
    df[17:24,17] <- p_HR
    df[17,18] <- df[18,21] <- p_SO
    df[19,18] <- df[20,18] <- df[23,18] <- df[21,21] <- df[22,21] <- df[24,21] <- p_1B
    df[17,19] <- df[19,19] <- df[20,19] <- df[23,19] <- df[18,23] <- df[21,23] <- df[22,23] <- df[24,23] <- p_2B
    df[17:24,20] <- p_3B
    df[19,21] <- df[20,22] <- df[21:24,24] <- p_BB
    
    # B matrix
    df[1,9] <- df[2,10] <- df[3,11] <- df[4,12] <- df[5,13] <- df[6,14] <- df[7,15] <- df[8,16] <- df[9,17] <- df[10,18] <- df[11,19] <- df[12,20] <- df[13,21] <- df[14,22] <- df[15,23] <- df[16,24] <- P_OUT
    
    # F matrix
    df[17:24,25] <- P_OUT
    
    # output the final matrix
    df
    
  }
  
  
  # fix the U0 matrix so it's not 1 in [1,1] but reflects current inning state
  
  # for now, assume "no steal" - runner on 1st, input$outs outs state
  # assume "steal" to runner on 2nd, X outs with prob. p and no on, X+1 outs with prob. 1-p
  
  # simulate inning - remember to fix Simulated_inning to include U0 as input instead of hard-coding
  # either source the code or copy the code at the beginning of the file
  simulate_nosteal <- reactive({
    U0_nosteal <- matrix(data = 0L, ncol = 25, nrow = 21)
    U0_nosteal[1,2+8*input$outs] <- 1# some function of reactive(input$outs)
    Simulated_inning(Softball_data1,U0_nosteal,input$Number1,input$Number2,input$Number3,
                     input$Number4,input$Number5,input$Number6,input$Number7,input$Number8,input$Number9)
  })
  
  simulated_steal <- reactive({
    U0_steal <- matrix(data = 0L, ncol = 25, nrow = 21)
    U0_steal[1,3+8*input$outs] <- input$success/100# function of reactive(input$success), reactive(input$outs)
    U0_steal[1,1+8*(input$outs + 1)] <- 1 - input$success/100
    
    Simulated_inning(Softball_data1,U0_steal,input$Number1,input$Number2,input$Number3,
                     input$Number4,input$Number5,input$Number6,input$Number7,input$Number8,input$Number9)
  })
  
  # create a data frame that will make it easy to ggplot with a facet by situation
  inning <- reactive({
    data.frame(runs = rep(seq(0, 20), 2),
               Situation= rep(c("No Steal Attempt", "Steal Attempt"), each = 21),
               prob = c(simulate_nosteal(), simulated_steal())
    )
  })
  #  inning <-   
  output$run_distribution <- renderPlot({
    # make a ggplot faceted by situation
    ggplot(inning() %>% filter(prob > 0.001), 
           mapping = aes(x = runs, y = prob, fill = Situation)) + geom_bar(stat = "identity") +
      geom_text(aes(x = runs, y = prob, 
                    label = paste0(round(100*prob,1), "%")), 
                vjust = -0.5) +
      scale_y_continuous(expand = c(0.1, 0)) +
      scale_x_continuous(breaks = seq(0, 20)) +
      facet_grid(Situation~.) +
      scale_color_manual(values = c("red","blue"))+
      theme(strip.text = element_text(
        size = 12, color = "black"))+
      scale_y_continuous(labels=percent)+
      expand_limits(y = 1)+
      theme(axis.title.y=element_blank())
    
    
    
    
  })
  
  
  # summarize distribution of runs scored in each situation
  # May have to round things if this doesn't automatically round when output
  #runs <- 
  
  #output summary of distribution
  output$result <- renderTable({
    inning() %>% group_by(Situation) %>% summarize(
      `Expected Runs` = sum(runs*prob),
      `Std. Dev. of Runs` = sqrt(sum((runs - `Expected Runs`)^2*prob)) # confirm this is the 335 formula for sd of discrete RV
    )
    #runs # this may be the wrong way to render the table
    #paste("Lineup:", input$"Number1")
  })
  
  # reset the inputs - Clear button
  observeEvent(input$resetAll, {
    reset("panel")
  })
  
  observeEvent(input$outs, {
    number <- input$outs  # value 2
    max_number <- 3
    if(number > max_number) {
      ## Conditions
      number <- max_number
    }
    output$outs <- renderText({outs})
  })
  # Add an observeEvent that runs the whole model after clicking Run Model
  
}

# Run the application 
shinyApp(ui = ui, server = server)