server <- function(input, output, session) {
  
  # Reactive values for simulation and play mode
  simResults <- reactiveVal(list())
  playerResults <- reactiveVal(list())  # Store player results
  gameState <- reactiveVal(list(grid = NULL, ships = NULL))  # For interactive play mode
  
  # Function to validate simulation inputs
  validateInputs <- function() {
    if (input$numShips < 1 || input$gridSize < 1 || input$numSimulations < 1) {
      showNotification("Error: Are you trying to break the app!?! Keep the all your numbers positive!", type = "error", duration = 5)
      return(FALSE)
    } else if (input$numShips > input$gridSize^2) {
      showNotification("Error: Number of ships exceeds total grid cells!", type = "error", duration = 5)
      return(FALSE)
    } else if (input$gridSize > 20) {
      showNotification("Error: Please keep grid size 20 or below!", type = "error", duration = 5)
      return(FALSE)
    } else if (input$numSimulations > 10000) {
      showNotification("Error: Please keep simulation count below 10,000!", type = "error", duration = 5)
      return(FALSE)
    }
    TRUE
  }
  
  # Function to simulate a single game
  simulateGame <- function() {
    gridSize <- input$gridSize
    numShips <- input$numShips
    totalCells <- gridSize^2
    shipCells <- sample(totalCells, numShips)
    shots <- integer(0)
    hits <- 0
    first_hit <- NULL
    f_hit <- FALSE
    
    if (input$distributionType == "neg_hypergeo") {
      while (hits < numShips) {
        shot <- sample(totalCells, 1)
        if (!(shot %in% shots)) {
          shots <- c(shots, shot)
          if (shot %in% shipCells) {
            if (!f_hit) {
              first_hit <- length(shots)
              f_hit <- TRUE
            }
            hits <- hits + 1
          }
        }
      }
    } else if (input$distributionType == "neg_binomial") {
      remainingShips <- shipCells
      while (hits < numShips) {
        shot <- sample(totalCells, 1)
        shots <- c(shots, shot)
        if (shot %in% remainingShips) {
          if (!f_hit) {
            first_hit <- length(shots)
            f_hit <- TRUE
          }
          hits <- hits + 1
          remainingShips <- remainingShips[remainingShips != shot]
        }
      }
    }
    
    list(
      totalShots = length(shots), 
      shots = shots, 
      shipCells = shipCells, 
      first_hit = first_hit
    )
  }
  
  # Function to run multiple simulations
  runSimulations <- function(numSimulations) {
    results <- list()
    for (i in 1:numSimulations) {
      results[[i]] <- simulateGame()
    }
    results
  }
  
  # Function to reset the game state for play mode
  resetGame <- function() {
    gridSize <- input$gridSize
    numShips <- input$numShips
    totalCells <- gridSize^2
    shipCells <- sample(totalCells, numShips)
    
    gameState(list(
      gridSize = gridSize,
      numShips = numShips,
      shipCells = shipCells,
      shots = integer(0),
      hits = 0,
      shotsToFirstHit = NULL,
      gameOver = FALSE
    ))
  }
  
  # Observe toggle for play mode
  observeEvent(input$modeToggle, {
    if (input$modeToggle == "play") {
      resetGame()
    } else {
      gameState(NULL)
    }
  })
  
  # Handle user clicks in play mode
  observeEvent(input$grid_click, {
    state <- gameState()
    if (is.null(state) || state$gameOver || input$modeToggle != "play") return()
    
    gridSize <- state$gridSize
    x <- floor(input$grid_click$x + 0.5)
    y <- gridSize - floor(input$grid_click$y + 0.5) + 1
    cell <- (y - 1) * gridSize + x
    
    if (cell %in% state$shots) {
      showNotification("Cell already fired at!", type = "message", duration = 2)
      return()
    }
    
    state$shots <- c(state$shots, cell)
    
    if (cell %in% state$shipCells && is.null(state$shotsToFirstHit)) {
      state$shotsToFirstHit <- length(state$shots)
      showNotification(paste("First hit! It took", state$shotsToFirstHit, "shots."), type = "warning", duration = 2)
    }
    
    if (cell %in% state$shipCells) {
      state$hits <- state$hits + 1
      if (state$hits == state$numShips) {
        state$gameOver <- TRUE
        showNotification("You win! All ships sunk!", type = "warning", duration = 5)
        
        # Store player's result after winning
        playerResults(list(totalShots = length(state$shots), shots = state$shots, shotsToFirstHit = state$shotsToFirstHit))
        
      } else {
        showNotification("Hit!", type = "warning", duration = 2)
      }
    } else {
      showNotification("Miss!", type = "message", duration = 2)
    }
    
    gameState(state)
  })
  
  # Render the grid plot
  output$gridPlot <- renderPlot({
    if (input$modeToggle == "play") {
      state <- gameState()
      if (is.null(state)) return(NULL)
      
      gridSize <- state$gridSize
      shipCells <- state$shipCells
      shots <- state$shots
      
      plot(1:gridSize, 1:gridSize, type = "n", xlab = "Columns", ylab = "Rows", 
           main = if (state$gameOver) "Game Over!" else "Play Battleship",
           xlim = c(0.5, gridSize + 0.5), ylim = c(0.5, gridSize + 0.5))
      rect(rep(0, gridSize + 1), 0:gridSize, rep(gridSize + 1, gridSize + 1), gridSize, border = "gray")
      
      for (cell in shipCells) {
        x <- (cell - 1) %% gridSize + 1
        y <- gridSize - (cell - 1) %/% gridSize
        rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5, col = if (state$gameOver) "dodgerblue3" else NA, border = NA)
      }
      
      for (shot in shots) {
        x <- (shot - 1) %% gridSize + 1
        y <- gridSize - (shot - 1) %/% gridSize
        points(x, y, pch = 16, col = if (shot %in% shipCells) "green3" else "firebrick3")
      }
      
    } else {
      state <- simResults()
      if (length(state) == 0) return(NULL)
      
      gridSize <- input$gridSize
      latestResult <- state[[length(state)]]
      shots <- latestResult$shots
      shipCells <- latestResult$shipCells
      
      plot(1:gridSize, 1:gridSize, type = "n", xlab = "Columns", ylab = "Rows", 
           main = paste0("Battleship Simulation (Total Shots = ", length(shots), ")"))
      rect(rep(0, gridSize + 1), 0:gridSize, rep(gridSize + 1, gridSize + 1), gridSize, border = "gray")
      
      for (cell in shipCells) {
        x <- (cell - 1) %% gridSize + 1
        y <- gridSize - (cell - 1) %/% gridSize
        rect(x - 0.5, y - 0.5, x + 0.5, y + 0.5, col = "dodgerblue3", border = NA)
      }
      
      for (shot in shots) {
        x <- (shot - 1) %% gridSize + 1
        y <- gridSize - (shot - 1) %/% gridSize
        points(x, y, pch = 16, col = "firebrick3")
      }
      
      legend("topright", legend = c("Ship", "Shot"), fill = c("dodgerblue3", "firebrick3"), border = NA)
    }
  })
  
  # Render histogram for simulation results
  output$shotHistogram <- renderPlot({
    results <- simResults()
    playerResult <- playerResults()
    
    # Check if results exist and are not empty
    if (is.null(results) || length(results) == 0) {
      plot.new()
      text(0.5, 0.5, "No simulation data available.\nRun simulations to view results.", cex = 1.2, col = "darkred")
      return(NULL)
    }
    
    if (input$plotToggle == "totalShots") {
      totalShots <- sapply(results, function(res) res$totalShots)
      avgShots <- mean(totalShots)
      
      hist(totalShots, 
           main = paste0("Distribution of Total Shots (n = ", length(results), ")"),
           xlab = "Number of Shots", 
           ylab = "Frequency", 
           col = "cornflowerblue", 
           border = "white")
      abline(v = avgShots, col = "lightcoral", lwd = 2, lty = 2)
      legend("topright", legend = paste("Average:", round(avgShots, 2)), 
             col = "lightcoral", lty = 2, bty = "n")
      
      # Add player result if available
      if (length(playerResult) > 0) {
        abline(v = playerResult$totalShots, col = "darkgreen", lwd = 2, lty = 2)
        legend("topleft", legend = c("Your Shots"), col = "darkgreen", lwd = 2, lty = 2)
      }
      
    } else {
      shotsToFirstHit <- sapply(results, function(res) res$first_hit)
      
      # Check if shotsToFirstHit has valid data
      if (all(is.na(shotsToFirstHit))) {
        plot.new()
        text(0.5, 0.5, "No valid data for 'Shots to First Hit'.", cex = 1.2, col = "darkred")
        return(NULL)
      }
      
      avgFirstHit <- mean(shotsToFirstHit, na.rm = TRUE)
      
      hist(shotsToFirstHit, 
           main = paste0("Distribution of Shots to First Hit (n = ", length(results), ")"),
           xlab = "Shots to First Hit", 
           ylab = "Frequency", 
           col = "lightcoral", 
           border = "white")
      abline(v = avgFirstHit, col = "cornflowerblue", lwd = 2, lty = 2)
      legend("topright", legend = paste("Average:", round(avgFirstHit, 2)), 
             col = "cornflowerblue", lty = 2, bty = "n")
      
      # Add player result if available
      if (length(playerResult) > 0) {
        abline(v = playerResult$shotsToFirstHit, col = "darkgreen", lwd = 2, lty = 2)
        legend("topleft", legend = c("Your First Shot"), col = "darkgreen", lwd = 2, lty = 2)
      }
    }
  })
  
  
  
  # Render simulation table
  output$simulationTable <- DT::renderDataTable({
    results <- simResults()
    if (length(results) == 0) return(NULL)
    
    data.frame(
      Simulation = seq_along(results),
      TotalShots = sapply(results, function(res) res$totalShots),
      ShotsToFirstHit = sapply(results, function(res) res$first_hit)
    )
  })
  
  # Observe simulation button
  observeEvent(input$simulate, {
    if (!validateInputs()) return()
    newResults <- runSimulations(input$numSimulations)
    currentResults <- simResults()
    simResults(c(currentResults, newResults))
  })
  
  # Observe clear button
  observeEvent(input$clear, {
    simResults(list())
    gameState(NULL)
  })
  
  # Observe reset board button
  observeEvent(input$resetBoard, {
    if (!validateInputs()) return()
    resetGame()
  })
  
  # Clear data when distribution type changes
  observeEvent(input$distributionType, {
    simResults(list())
    showNotification("Data cleared due to change in distribution type.", type = "message", duration = 3)
  })
}
