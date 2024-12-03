ui <- fluidPage(
  titlePanel("Battleship Simulation & Game"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("gridSize", "Grid Size (N x N):", value = 5, min = 2, max = 20),
      numericInput("numShips", "Number of Ships:", value = 2, min = 1, max = 10),
      
      selectInput("distributionType", "Choose Distribution Type:",
                  choices = c("Negative Hypergeometric" = "neg_hypergeo", 
                              "Negative Binomial" = "neg_binomial")),
      
      numericInput("numSimulations", "Number of Simulations to Run:", value = 1, min = 1, max = 100),
      
      actionButton("simulate", "Run Simulation(s)"),
      actionButton("clear", "Clear Results"),
      
      radioButtons("plotToggle", "Choose Plot to Display:",
                   choices = c("Total Shots" = "totalShots", 
                               "Shots to First Hit" = "shotsToFirstHit"),
                   inline = TRUE),
      
      radioButtons("modeToggle", "Choose Mode:",
                   choices = c("Simulation" = "simulate", "Play Game" = "play"),
                   inline = TRUE),
      
      checkboxInput("showTable", "Show Table", value = FALSE),
      
      conditionalPanel(
        condition = "input.modeToggle == 'play'",
        actionButton("resetBoard", "Reset Board")
      ),
      
      # Spacer and "Created by" link
      tags$div(
        style = "margin-top: 20px; text-align: left; font-size: 12px;",
        "Created by ",
        tags$a(href = "https://github.com/Wander03/battleship_example", 
               target = "_blank", "Andrew Kerr",
               style = "text-decoration: none; color: blue;")
      )
    ),
    
    mainPanel(
      plotOutput("gridPlot", height = 400, width = 600, click = "grid_click"),
      conditionalPanel(
        condition = "input.showTable == true",
        dataTableOutput("simulationTable")
      ),
      plotOutput("shotHistogram", height = 400, width = 1000)
    )
  )
)
