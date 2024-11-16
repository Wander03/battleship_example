server <- function(input, output, session) {
  
  # Reactive value to store the results of simulations (all simulations)
  simResults <- reactiveVal(list())
  
  # Function to validate inputs
  validateInputs <- function() {
    if (input$numShips > input$gridSize^2) {
      showNotification("Error: Number of ships exceeds total grid cells!", type = "error", duration = 5)
      return(FALSE)
    }
    TRUE
  }
  
  # Function to generate grid and simulate game
  simulateGame <- function() {
    gridSize <- input$gridSize
    numShips <- input$numShips
    
    # Place ships randomly on the grid (ships cannot be in the same cell)
    totalCells <- gridSize^2
    shipCells <- sample(totalCells, numShips)
    
    # Initialize the grid and shots
    shots <- integer(0)
    hits <- 0
    
    # Check which distribution to use
    if (input$distributionType == "neg_hypergeo") {
      # Negative Hypergeometric Distribution
      while (hits < numShips) {
        shot <- sample(totalCells, 1)
        
        # Add the shot only if it's not already in the list of shots
        if (!(shot %in% shots)) {
          shots <- c(shots, shot)  # Add the shot to the list of shots
          if (shot %in% shipCells) {
            hits <- hits + 1  # Increment hit count if the shot hits a ship
          }
        }
      }
      
    } else if (input$distributionType == "neg_binomial") {
      shipCellsHit <- shipCells
      
      # Negative Binomial Distribution
      while (hits < numShips) {
        shot <- sample(totalCells, 1)
        
        # Add the shot
        shots <- c(shots, shot)  # Add the shot to the list of shots
        if (shot %in% shipCellsHit) {
          hits <- hits + 1  # Increment hit count if the shot hits a ship
          shipCellsHit <- shipCellsHit[shipCellsHit != shot]
        }
      }
    }
    
    # Store the results of this simulation
    list(
      totalShots = length(shots), 
      shots = shots, 
      shipCells = shipCells
    )
  }
  
  # Run multiple simulations
  runSimulations <- function(numSimulations) {
    results <- list()
    for (i in 1:numSimulations) {
      results[[i]] <- simulateGame()
    }
    results
  }
  
  # Render grid plot
  output$gridPlot <- renderPlot({
    state <- simResults()
    if (length(state) == 0) return(NULL)  # If no data to plot
    
    gridSize <- input$gridSize
    # Get the latest simulation shots and ship locations (most recent simulation)
    latestResult <- state[[length(state)]]
    shots <- latestResult$shots
    shipCells <- latestResult$shipCells
    
    # Create an empty plot to represent the grid
    plot(1:gridSize, 1:gridSize, type = "n", xlab = "Columns", ylab = "Rows", 
         main = paste0("Battleship Simulation (Total Shots = ", length(shots), ")"))
    rect(rep(0, gridSize + 1), 0:gridSize, rep(gridSize + 1, gridSize + 1), gridSize, border = "gray")
    
    # Mark ship locations
    for (cell in shipCells) {
      x <- (cell - 1) %% gridSize + 1
      y <- gridSize - (cell - 1) %/% gridSize
      rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5, col = "dodgerblue3", border = NA)
    }
    
    # Mark shots
    for (shot in shots) {
      x <- (shot - 1) %% gridSize + 1
      y <- gridSize - (shot - 1) %/% gridSize
      points(x, y, pch = 16, col = "firebrick3")
    }
    
    legend("topright", legend = c("Ship", "Shot"), fill = c("dodgerblue3", "firebrick3"), border = NA)
  })
  
  # Render histogram of shots required for each simulation
  output$shotHistogram <- renderPlot({
    results <- simResults()
    if (length(results) == 0) return(NULL)  # No data to plot
    
    # Create a histogram of the total shots across simulations
    totalShots <- sapply(results, function(res) res$totalShots)
    hist(totalShots, 
         main = paste0("Distribution of Shots Required to Sink All Ships (n = ", length(results), ")"),
         xlab = "Number of Shots", 
         ylab = "Frequency", 
         col = "cornflowerblue", 
         border = "white")
  })
  
  # Start the simulation when the button is clicked
  observeEvent(input$simulate, {
    if (!validateInputs()) return()  # Check inputs before running simulations
    newResults <- runSimulations(input$numSimulations)
    currentResults <- simResults()
    simResults(c(currentResults, newResults))  # Append new results to existing ones
  })
  
  # Clear simulation results whenever the "Clear Results" button is clicked
  observeEvent(input$clear, {
    simResults(list())  # Clear all simulation results
  })
}