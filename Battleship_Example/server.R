server <- function(input, output, session) {
  
  simResults <- reactiveVal(list())
  
  validateInputs <- function() {
    if (input$numShips > input$gridSize^2) {
      showNotification("Error: Number of ships exceeds total grid cells!", type = "error", duration = 5)
      return(FALSE)
    }
    TRUE
  }
  
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
  
  runSimulations <- function(numSimulations) {
    results <- list()
    for (i in 1:numSimulations) {
      results[[i]] <- simulateGame()
    }
    results
  }
  
  output$gridPlot <- renderPlot({
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
  })
  
  output$shotHistogram <- renderPlot({
    results <- simResults()
    if (length(results) == 0) return(NULL)
    
    if (input$plotToggle == "totalShots") {
      totalShots <- sapply(results, function(res) res$totalShots)
      avgShots <- mean(totalShots)  # Calculate the average
      
      hist(totalShots, 
           main = paste0("Distribution of Total Shots (n = ", length(results), ")"),
           xlab = "Number of Shots", 
           ylab = "Frequency", 
           col = "cornflowerblue", 
           border = "white")
      abline(v = avgShots, col = "lightcoral", lwd = 2, lty = 2)  # Add vertical line for average
      legend("topright", legend = paste("Average:", round(avgShots, 2)), 
             col = "lightcoral", lty = 2, bty = "n")
      
    } else {
      shotsToFirstHit <- sapply(results, function(res) res$first_hit)
      avgFirstHit <- mean(shotsToFirstHit)  # Calculate the average
      
      hist(shotsToFirstHit, 
           main = paste0("Distribution of Shots to First Hit (n = ", length(results), ")"),
           xlab = "Shots to First Hit", 
           ylab = "Frequency", 
           col = "lightcoral", 
           border = "white")
      abline(v = avgFirstHit, col = "cornflowerblue", lwd = 2, lty = 2)  # Add vertical line for average
      legend("topright", legend = paste("Average:", round(avgFirstHit, 2)), 
             col = "cornflowerblue", lty = 2, bty = "n")
    }
  })
  
  
  output$simulationTable <- DT::renderDataTable({
    results <- simResults()
    if (length(results) == 0) return(NULL)
    
    data.frame(
      Simulation = seq_along(results),
      TotalShots = sapply(results, function(res) res$totalShots),
      ShotsToFirstHit = sapply(results, function(res) res$first_hit)
    )
  })
  
  observeEvent(input$simulate, {
    if (!validateInputs()) return()
    newResults <- runSimulations(input$numSimulations)
    currentResults <- simResults()
    simResults(c(currentResults, newResults))
  })
  
  observeEvent(input$clear, {
    simResults(list())
  })
  
  # Clear data when distribution type changes
  observeEvent(input$distributionType, {
    simResults(list())
    showNotification("Data cleared due to change in distribution type.", type = "message", duration = 3)
  })
}
