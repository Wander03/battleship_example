ui <- fluidPage(
  titlePanel("Battleship Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("gridSize", "Grid Size (N x N):", value = 5, min = 2, max = 20),
      numericInput("numShips", "Number of Ships:", value = 2, min = 1, max = 10),
      
      selectInput("distributionType", "Choose Distribution Type:",
                  choices = c("Negative Hypergeometric" = "neg_hypergeo", 
                              "Negative Binomial" = "neg_binomial")),
      
      numericInput("numSimulations", "Number of Simulations to Run:", value = 1, min = 1, max = 100),
      
      actionButton("simulate", "Run Simulation(s)"),
      actionButton("clear", "Clear Results")
    ),
    
    mainPanel(
      plotOutput("gridPlot", height = 400, width = 600),
      plotOutput("shotHistogram", height = 400, width = 1000)
    )
  )
)