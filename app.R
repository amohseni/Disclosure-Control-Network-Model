###############################################################
# Shiny App: Selective Disclosure & Perceptions of Polarization
###############################################################

# Install required libraries
# install.packages(
#   c(
#     "rstudioapi",
#     "shiny",
#     "shinydashboard",
#     "igraph",
#     "dplyr",
#     "ggplot2",
#     "tidyr",
#     "plotly",
#     "DT",
#     "shinyjs",
#     "future",
#     "promises",
#     "shinyBS",
#     "RColorBrewer",
#     "visNetwork"
#   )
# )

# Libraries & source
library(shinydashboard)
library(shiny)
library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(plotly)
library(DT)
library(shinyjs)
library(shinyBS)
library(visNetwork)
library(future)
library(promises)
library(RColorBrewer)


# Set working directory to current file's folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Source the model functions (assuming the model code is in "Disclosure-Control-Network-Simulation.R")
source("Disclosure-Control-Network-Simulation.R")

###############################################################
# Helper Functions for UI Elements
###############################################################

## Robust blue palette centered on #337ab7
make_blues <- function(n) {
  n <- as.integer(n)
  if (is.na(n) || n < 1)
    return("#337ab7")
  if (n == 1)
    return("#337ab7")
  # Brewer provides up to 9; extend if needed
  base_n <- min(max(n, 3), 9)
  brewer_colors <- brewer.pal(base_n, "Blues")
  grDevices::colorRampPalette(brewer_colors)(n)
}

# Define progress bar function for UI
progressBar <- function(id,
                        value = 0,
                        label = NULL,
                        color = "primary",
                        display_pct = TRUE) {
  value <- max(0, min(100, value))
  label <- if (!is.null(label))
    label
  else if (display_pct)
    paste0(value, "%")
  else
    ""
  
  div(
    class = "progress",
    div(
      class = paste0("progress-bar bg-", color),
      id = id,
      role = "progressbar",
      style = paste0("width: ", value, "%;"),
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      label
    )
  )
}

# Helper function to get current progress value
getCurrentProgressValue <- function(session, id) {
  progress_str <- session$sendCustomMessage("shinyjs-getValueFromEl",
                                            list(id = id, prop = "aria-valuenow"))
  if (is.null(progress_str) || progress_str == "")
    return(0)
  as.numeric(progress_str)
}

# Function to update progress bar
updateProgressBar <- function(session,
                              id,
                              value = NULL,
                              label = NULL,
                              color = NULL) {
  message <- list(id = id)
  
  if (!is.null(value)) {
    value <- max(0, min(100, value))
    message$style <- paste0("width: ", value, "%;")
    message$`aria-valuenow` <- value
    if (is.null(label))
      message$text <- paste0(value, "%")
  }
  
  if (!is.null(label)) {
    message$text <- label
  }
  
  if (!is.null(color)) {
    message$class <- list(remove = "bg-.*", add = paste0("bg-", color))
  }
  
  session$sendInputMessage(id, message)
}

###############################################################
# UI DEFINITION
###############################################################

ui <- dashboardPage(
  dashboardHeader(
    title = HTML(
      "Selective Self-Disclosure & Perceived Polarization <br> in Social Networks"
    ),
    titleWidth = 600
  ),
  
  dashboardSidebar(width = 600, fluidRow(
    # Left column: Navigation, Basic parameters, Decay Factor, and Run Button
    column(
      6,
      id = "sidebar",
      style = "padding: 10px;",
      sidebarMenu(
        id = "tabs",
        style = "font-size: 16px;",
        menuItem("Results", tabName = "results", icon = icon("chart-bar")),
        menuItem(
          "Network Visualization",
          tabName = "network",
          icon = icon("project-diagram")
        ),
        menuItem(
          "Parameter Sweep",
          tabName = "sweep",
          icon = icon("tasks")
        ),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
      ),
      actionButton(
        "run",
        "Run Simulation",
        icon = icon("play"),
        style = "width: 80%; box-sizing: border-box; color: #fff; background-color: #337ab7; width: 80%; margin: auto; display: block;"
      ),
      sliderInput(
        "N",
        "Number of Agents (N):",
        min = 1,
        max = 100,
        value = 40
      ),
      helpText("Number of agents in the network."),
      sliderInput(
        "L",
        "Length of Type Vector (L):",
        min = 1,
        max = 10,
        value = 3
      ),
      helpText("Number of traits per agent."),
      sliderInput(
        "T",
        "Number of Rounds (T):",
        min = 1,
        max = 100,
        value = 20
      ),
      helpText("Total number of simulation rounds."),
      sliderInput(
        "delta",
        "Influence Decay Factor (δ):",
        min = 0.1,
        max = 1.0,
        value = 0.5,
        step = 0.05
      ),
      helpText("Determines how rapidly influence decays with network distance.")
    ),
    # Right column: Network and simulation parameters (excluding decay factor and run button)
    column(
      6,
      style = "padding-left: 5px;",
      selectInput(
        "network_type",
        "Network Type:",
        choices = c(
          "Erdős-Rényi" = "ER",
          "Watts-Strogatz" = "WS",
          "Barabási-Albert" = "BA"
        )
      ),
      helpText("Select the network generation algorithm."),
      
      conditionalPanel(
        condition = "input.network_type == 'ER'",
        sliderInput(
          "p_er",
          "Edge Probability (p):",
          min = 0.01,
          max = 0.5,
          value = 0.1,
          step = 0.01
        )
      ),
      helpText("Probability of edge creation in an ER network."),
      
      conditionalPanel(
        condition = "input.network_type == 'WS'",
        sliderInput(
          "k_ws",
          "Initial Neighbors (k):",
          min = 2,
          max = 10,
          value = 4,
          step = 1
        ),
        helpText("Initial number of neighbors in a WS network."),
        sliderInput(
          "p_ws",
          "Rewiring Probability (p):",
          min = 0.01,
          max = 0.5,
          value = 0.1,
          step = 0.01
        ),
        helpText("Probability of rewiring connections in a WS network.")
      ),
      
      conditionalPanel(
        condition = "input.network_type == 'BA'",
        sliderInput(
          "m_ba",
          "Edges per New Node (m):",
          min = 1,
          max = 10,
          value = 3,
          step = 1
        ),
        helpText("Number of edges to attach from each new node in a BA network.")
      ),
      
      selectInput(
        "init_type",
        "Type Vector Initialization:",
        choices = c("Random" = "random", "Polarized" = "polarized")
      ),
      helpText("Choose how agent traits are initialized."),
      
      conditionalPanel(
        condition = "input.init_type == 'polarized'",
        sliderInput(
          "b",
          "Bias Parameter (b):",
          min = 0.5,
          max = 0.99,
          value = 0.7,
          step = 0.01
        ),
        helpText("Controls the degree of polarization.")
      ),
      selectInput(
        "model_version",
        "Network Version:",
        choices = c("Static" = "static", "Dynamic" = "dynamic")
      ),
      helpText(
        "Choose whether the network remains static or evolves dynamically."
      ),
      
      selectInput(
        "disclosure_type",
        "Disclosure Type:",
        choices = c("Selective" = "selective", "Global" = "global")
      ),
      helpText("Determine if disclosures are made selectively or globally."),
      
      conditionalPanel(
        condition = "input.disclosure_type == 'selective'",
        uiOutput("s_ui"),
        helpText(
          "Selective Disclosure Size: Number of agents that reveal their traits."
        )
      )
    )
  )),
  dashboardBody(
    shinyjs::useShinyjs(),
    # Initialize shinyjs here
    includeCSS("www/style.css"),
    tabItems(
      tabItem(
        tabName = "results",
        tabBox(
          title = "Results",
          width = 12,
          tabPanel("Time Series", fluidRow(
            box(
              title = "Visualization Controls",
              width = 4,
              selectInput(
                "plot_type",
                "Plot Type:",
                choices = c("Similarity", "Polarization", "Network Metrics", "Mean Welfare"),
                selected = "Similarity"
              ),
              
              downloadButton("download_results", "Download Results"),
              br(),
              div(style = "margin-top: 20px;", h5("Status Log"), verbatimTextOutput("status_log"))
            ),
            box(
              title = "Time Series Plot",
              width = 8,
              plotlyOutput("time_series_plotly", height = "400px")
            )
          )),
          tabPanel("Detailed Results", fluidRow(
            tabBox(
              title = NULL,
              width = 12,
              tabPanel("Summary Statistics", tableOutput("summary_stats")),
              tabPanel(
                "Agent Disclosures",
                plotOutput("agent_disclosure_plot", height = "300px"),
                dataTableOutput("agent_disclosure_table")
              ),
              tabPanel(
                "Trait Disclosures",
                plotOutput("trait_disclosure_plot", height = "300px"),
                dataTableOutput("trait_disclosure_table")
              ),
              tabPanel("Raw Data", dataTableOutput("results_table"))
            )
          ))
        )
      ),
      tabItem(tabName = "network", fluidRow(
        box(
          title = "Controls (Static Plot)",
          width = 3,
          selectInput("network_round", "Round:", choices = ""),
          selectInput(
            "color_by",
            "Color Nodes By:",
            choices = c("Average Similarity", "Type Disclosure Rate", "Degree")
          ),
          sliderInput(
            "node_size",
            "Node Size:",
            min = 1,
            max = 10,
            value = 3
          ),
          checkboxInput("show_labels", "Show Node Labels", FALSE)
        ),
        box(
          title = "Network Graph (Static)",
          width = 9,
          plotOutput("network_plot", height = "600px")
        )
      ), br(), fluidRow(
        box(
          title = "Animation Controls",
          width = 3,
          # A separate slider for the animated version
          sliderInput(
            "anim_round",
            "Animation Round:",
            min = 1,
            max = 1,
            value = 1
          ),
          actionButton("play_btn", "Play"),
          actionButton("pause_btn", "Pause"),
          numericInput("play_speed", "Speed (ms):", value = 500, min = 100),
          helpText("Lower = faster animation")
        ),
        box(
          title = "Animated Network (visNetwork)",
          width = 9,
          visNetworkOutput("network_vis", height = "600px")
        )
      )),
      tabItem(tabName = "about", box(
        title = "About",
        width = 12,
        h3(
          HTML(
            "Selective Disclosure <br> & Perceived Polarization <br> on Social Networks"
          )
        ),
        p(
          "Social media platforms host public forums where individuals make declarations at an unprecedented scale, often with little control over their audience. In this paper, we develop a formal model in which agents, each endowed with a binary type vector, strive to be perceived as similar to others. Agents selectively disclose partial information about their type vectors---either to the entire network or to a chosen subset---with evaluations weighted by social proximity. We explore how these disclosure strategies influence perceived similarity among immediate contacts and across the broader network, and how they may contribute to perceptions regarding polarization. We compare the effect in static networks with those in dynamic networks, where agents form or sever ties based on perceived similarity."
        ),
        p(
          "This model simulates how changes in control over self-disclosure affects perceived similarity and polarization in social networks."
        )
      )),
      tabItem(tabName = "sweep", tabBox(
        width = 12,
        # ------------------ SWEEP INPUT TAB ------------------ #
        tabPanel(
          "Sweep Input",
          fluidRow(column(
            12,
            br(),
            div(
              style = "text-align: center;",
              actionButton(
                "run_sweep",
                "Run Parameter Sweep",
                icon = icon("play"),
                style = "width: 180pt; box-sizing: border-box; color: #fff; background-color: #337ab7; margin: auto; display: block;"
              )
            ),
            br(),
            div(
              style = "text-align: center;",
              helpText("Select parameter ranges and discrete options for the sweep."),
            ),
            div(
              style = "display: flex; flex-direction: column; align-items: center;",
              numericInput(
                "num_runs",
                "Number of Runs per Combination:",
                value = 10,
                min = 1
              )
            )
          )),
          br(),
          fluidRow(
            column(
              6,
              h6("Number of Agents (N):"),
              fluidRow(
                column(4, numericInput("n_min", "Min N:", 10, min = 1)),
                column(4, numericInput("n_max", "Max N:", 100, min = 1)),
                column(4, numericInput("n_step", "Increment:", 10, min = 1))
              ),
              
              h6("Length of Type Vector (L):"),
              fluidRow(
                column(4, numericInput("l_min", "Min L:", 1, min = 1)),
                column(4, numericInput("l_max", "Max L:", 10, min = 1)),
                column(4, numericInput("l_step", "Increment:", 1, min = 1))
              ),
              
              h6("Number of Rounds (T):"),
              fluidRow(
                column(4, numericInput("t_min", "Min T:", 10, min = 1)),
                column(4, numericInput("t_max", "Max T:", 50, min = 1)),
                column(4, numericInput("t_step", "Increment:", 10, min = 1))
              )
            ),
            
            column(
              6,
              checkboxGroupInput(
                "network_type_sweep",
                "Network Type(s):",
                choices = c("Erdős-Rényi", "Watts-Strogatz", "Barabási-Albert"),
                selected = c("Erdős-Rényi")
              ),
              checkboxGroupInput(
                "model_version_sweep",
                "Model Version:",
                choices = c("static", "dynamic"),
                selected = "static"
              ),
              checkboxGroupInput(
                "disclosure_type_sweep",
                "Disclosure Type:",
                choices = c("selective", "global"),
                selected = "selective"
              )
            )
          )
        ),
        # ------------------ SWEEP OUTPUT TAB ------------------ #
        tabPanel("Sweep Output", fluidRow(column(
          12, div(style = "text-align: center;", downloadButton("download_sweep", "Download Results"), ), br()
        )), fluidRow(
          column(
            12,
            plotOutput("sweep_plot", height = "400px"),
            br(),
            dataTableOutput("sweep_table")
          )
        ))
      ))
    )
  )
)

###############################################################
# SERVER LOGIC
###############################################################

server <- function(input, output, session) {
  # Reactive values to store simulation results and state
  values <- reactiveValues(
    sim_results = NULL,
    processed_results = NULL,
    sweep_results = NULL,
    status_log = character(0),
    is_running = FALSE,
    network_states = list()
  )
  
  # Helper function to get network parameter based on selected model
  get_network_param <- function() {
    if (input$network_type == "ER") {
      return(input$p_er)
    } else if (input$network_type == "WS") {
      return(input$p_ws)
    } else if (input$network_type == "BA") {
      return(input$m_ba)
    }
  }
  
  # Helper function to get initial k parameter for WS model
  get_k_param <- function() {
    if (input$network_type == "WS") {
      return(input$k_ws)
    } else {
      return(4)  # Default value for other models
    }
  }
  
  # Function to collect all parameters from UI inputs
  get_params <- function() {
    # Base parameters
    params <- list(
      N = input$N,
      L = input$L,
      T = input$T,
      network_type = input$network_type,
      init_type = input$init_type,
      model_version = input$model_version,
      disclosure_type = input$disclosure_type,
      delta = input$delta
    )
    
    # Network-specific parameters
    params$p <- get_network_param()
    params$k <- get_k_param()
    
    # Conditional parameters
    if (input$init_type == "polarized") {
      params$b <- input$b
    } else {
      params$b <- 0.7  # Default value
    }
    
    if (input$disclosure_type == "selective") {
      params$s <- input$s
    } else {
      params$s <- input$N  # Default value for global disclosure
    }
    
    return(params)
  }
  
  # Update parameter display
  output$current_params <- renderPrint({
    params <- get_params()
    cat("Network Size (N): ", params$N, "\n")
    cat("Type Vector Length (L): ", params$L, "\n")
    cat("Rounds (T): ", params$T, "\n")
    cat("Network Type: ", params$network_type, "\n")
    cat("Initialization: ", params$init_type, "\n")
    cat("Network Version: ", params$model_version, "\n")
    cat("Disclosure Type: ", params$disclosure_type, "\n")
    cat("Influence Decay (δ): ", params$delta, "\n")
    
    if (params$network_type == "ER") {
      cat("Edge Probability (p): ", params$p, "\n")
    } else if (params$network_type == "WS") {
      cat("Initial Neighbors (k): ", params$k, "\n")
      cat("Rewiring Probability (p): ", params$p, "\n")
    } else if (params$network_type == "BA") {
      cat("Edges per New Node (m): ", params$p, "\n")
    }
    
    if (params$init_type == "polarized") {
      cat("Bias Parameter (b): ", params$b, "\n")
    }
    
    if (params$disclosure_type == "selective") {
      cat("Selective Disclore Size (s): ", params$s, "\n")
    }
  })
  
  # Update summary statistics display
  output$summary_stats <- renderTable({
    req(values$processed_results)
    
    final_round <- nrow(values$processed_results$time_series)
    last_metrics <- values$processed_results$time_series[final_round, ]
    
    disclosure_rate <- values$processed_results$params$disclosure_metrics$disclosure_rate
    if (length(disclosure_rate) == 0)
      disclosure_rate <- NA
    
    data.frame(
      Metric = c(
        "Perceived Similarity (Neighbor)",
        "Perceived Similarity (Global)",
        "Perceived Similarity Gap",
        "Objective Similarity (Neighbor)",
        "Objective Similarity (Global)",
        "Objective Similarity Gap",
        "Revealed Similarity (Neighbor)",
        "Revealed Similarity (Global)",
        "Revealed Similarity Gap",
        "Polarization (Variance)",
        "Clustering Coefficient",
        "Modularity",
        "Mean Welfare",
        "Gini Coefficient",
        "Disclosure Rate"
      ),
      Value = c(
        last_metrics$perceived_neighbor_similarity,
        last_metrics$perceived_all_similarity,
        last_metrics$perceived_similarity_gap,
        last_metrics$objective_neighbor_similarity,
        last_metrics$objective_all_similarity,
        last_metrics$objective_similarity_gap,
        last_metrics$revealed_neighbor_similarity,
        last_metrics$revealed_all_similarity,
        last_metrics$revealed_similarity_gap,
        last_metrics$variance,
        last_metrics$clustering,
        last_metrics$modularity,
        last_metrics$mean_welfare,
        last_metrics$gini,
        disclosure_rate
      )
    )
  })
  
  # Log messages
  add_log <- function(message) {
    values$status_log <- c(paste(format(Sys.time(), "%H:%M:%S"), "-", message), values$status_log)
    if (length(values$status_log) > 100) {
      values$status_log <- values$status_log[1:100]
    }
  }
  
  # Display log
  output$status_log <- renderPrint({
    cat(paste(values$status_log, collapse = "\n"))
  })
  
  # Dynamically update UI to set max subset size to population max
  output$s_ui <- renderUI({
    sliderInput(
      "s",
      "Selective Disclosure Size (s):",
      min = 1,
      max = input$N,
      value = min(10, input$N),
      step = 1
    )
  })
  
  # Run simulation when button is clicked (either one)
  observeEvent(c(input$run, input$run2), {
    req(!values$is_running)  # Only run if not already running
    
    values$is_running <- TRUE
    add_log("Starting simulation...")
    
    # Get parameters
    params <- get_params()
    print(params)
    
    # Initialize progress bar
    updateProgressBar(session, "progress", value = 0)
    
    # Run simulation in a separate R process to avoid blocking the UI
    future::plan(future::multisession)
    
    future_promise <- future::future({
      # Run simulation
      sim_results <- run_simulation(params)
      processed_results <- process_simulation_results(sim_results)
      
      # Return both raw and processed results
      list(sim_results = sim_results, processed_results = processed_results)
    }, seed = TRUE) # Set RNG seed
    
    # Handle the promise when it completes
    promises::then(
      future_promise,
      onFulfilled = function(result) {
        values$sim_results <- result$sim_results
        values$processed_results <- result$processed_results
        
        # For simplicity, the final network is used for each round,
        # but in a real app you'd store the actual state at each round.
        values$network_states <- lapply(1:params$T, function(t) {
          if (t == 1) {
            return(values$sim_results$final_network)
          } else {
            return(values$sim_results$final_network)
          }
        })
        
        # Update round selector for network visualization
        updateSelectInput(session, "network_round", choices = setNames(1:params$T, paste("Round", 1:params$T)))
        
        # --- NEW CODE: Prepare data for visNetwork animation ---
        # (1) Compute a single layout from the first round's graph
        g_first <- values$network_states[[1]]
        coords <- layout_with_fr(g_first)
        coords <- coords * 200  # scaling to look nice in visNetwork
        
        # (2) We'll store node/edge data frames for each round in these:
        values$vis_nodes <- vector("list", params$T)
        values$vis_edges <- vector("list", params$T)
        
        # Because your example reuses the final network for each round,
        # we'll do the same for demonstration:
        for (r in seq_len(params$T)) {
          g_r <- values$network_states[[r]]
          
          # Create a data.frame for node positions (fixed)
          node_positions <- data.frame(id = 1:vcount(g_r),
                                       x = coords[, 1],
                                       y = coords[, 2])
          
          # Minimal color assignment here; you can override in renderVisNetwork
          # For demonstration, let's color all nodes the same:
          node_positions$color <- "lightblue"
          node_positions$label <- paste("Node", node_positions$id)
          node_positions$size  <- 20
          
          # Build edges from igraph
          edge_df <- get.data.frame(g_r, "edges")
          if (nrow(edge_df) > 0) {
            colnames(edge_df)[1:2] <- c("from", "to")
          } else {
            edge_df <- data.frame(from = integer(0), to = integer(0))
          }
          edge_df$color <- "gray"
          edge_df$width <- 2
          
          values$vis_nodes[[r]] <- node_positions
          values$vis_edges[[r]] <- edge_df
        }
        
        # (3) Set up the anim_round slider range
        updateSliderInput(
          session,
          "anim_round",
          min = 1,
          max = params$T,
          value = 1
        )
        # --- End NEW CODE ---
        
        add_log("Simulation completed successfully!")
        values$is_running <- FALSE
        updateProgressBar(session, "progress", value = 100)
      },
      onRejected = function(error) {
        add_log(paste("Error in simulation:", error$message))
        values$is_running <- FALSE
        updateProgressBar(session, "progress", value = 0)
      }
    )
    
  })
  
  # Generate time series plots
  create_plot <- function() {
    req(values$processed_results)
    
    df <- values$processed_results$time_series
    plot_type <- input$plot_type
    
    if (plot_type == "Similarity") {
      p <- ggplot(df, aes(x = round)) +
        geom_line(
          aes(y = perceived_neighbor_similarity, color = "Perceived (Neighbor)"),
          linewidth = 1
        ) +
        geom_line(aes(y = perceived_all_similarity, color = "Perceived (Global)"),
                  linewidth = 1) +
        geom_line(
          aes(y = objective_neighbor_similarity, color = "Objective (Neighbor)"),
          linewidth = 1
        ) +
        geom_line(aes(y = objective_all_similarity, color = "Objective (Global)"),
                  linewidth = 1) +
        geom_line(
          aes(y = revealed_neighbor_similarity, color = "Revealed (Neighbor)"),
          linewidth = 1,
          linetype = "dashed"
        ) +
        geom_line(
          aes(y = revealed_all_similarity, color = "Revealed (Global)"),
          size = 1,
          linetype = "dashed"
        ) +
        labs(
          title = "Similarity Measures Over Time",
          x = "Round",
          y = "Similarity",
          color = "Measure"
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Paired")
      
    } else if (plot_type == "Polarization") {
      p <- ggplot(df, aes(x = round)) +
        geom_line(aes(y = variance, color = "Variance"), size = 1) +
        geom_line(aes(y = bimodality, color = "Bimodality"), size = 1) +
        labs(
          title = "Polarization Measures Over Time",
          x = "Round",
          y = "Value",
          color = "Measure"
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Set2")
      
    } else if (plot_type == "Network Metrics") {
      p <- ggplot(df, aes(x = round)) +
        geom_line(aes(y = clustering, color = "Clustering"), size = 1) +
        geom_line(aes(y = modularity, color = "Modularity"), size = 1) +
        labs(
          title = "Network Metrics Over Time",
          x = "Round",
          y = "Value",
          color = "Measure"
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Set3")
      
    } else if (plot_type == "Welfare") {
      p <- ggplot(df, aes(x = round)) +
        geom_line(aes(y = mean_welfare, color = "Total Welfare"), size = 1) +
        geom_line(aes(y = gini * 100, color = "Gini Coefficient"), size = 1) +
        labs(
          title = "Welfare Measures Over Time",
          x = "Round",
          y = "Value",
          color = "Measure"
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Dark2") +
        scale_y_continuous(sec.axis = sec_axis( ~ . / 100, name = "Gini Coefficient"))
    }
    
    return(p)
  }
  
  # Regular plot output
  output$time_series_plot <- renderPlot({
    create_plot()
  })
  
  # Plotly interactive plot
  output$time_series_plotly <- renderPlotly({
    ggplotly(create_plot())
  })
  
  # Agent disclosure plot
  output$agent_disclosure_plot <- renderPlot({
    req(values$processed_results)
    # now a long-form df with agent, trait, n_disclosed
    df <- values$processed_results$agent_disclosures
    L  <- values$processed_results$params$L
    
    ggplot(df, aes(
      x = factor(agent),
      y = n_disclosed,
      fill = factor(trait)
    )) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual("Trait", values = make_blues(L)) +
      labs(title = "Disclosures by Agent & Trait", x = "Agent ID", y = "Number of Disclosures") +
      theme_minimal()
  })
  
  # Agent disclosure table
  output$agent_disclosure_table <- renderDataTable({
    req(values$processed_results)
    DT::datatable(
      values$processed_results$agent_disclosures,
      options = list(
        scrollY = "300px",
        paging = FALSE,
        scrollCollapse = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Trait disclosure plot
  output$trait_disclosure_plot <- renderPlot({
    req(values$processed_results)
    df <- values$processed_results$trait_disclosures
    L  <- values$processed_results$params$L
    
    ggplot(df, aes(
      x = factor(trait),
      y = disclosure_rate,
      fill = factor(trait)
    )) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = make_blues(L), guide = "none") +
      labs(title = "Disclosure Rate by Trait", x = "Trait Number", y = "Disclosure Rate") +
      theme_minimal()
  })
  
  # Trait disclosure table
  output$trait_disclosure_table <- renderDataTable({
    req(values$processed_results)
    DT::datatable(
      values$processed_results$trait_disclosures,
      options = list(
        scrollY = "300px",
        paging = FALSE,
        scrollCollapse = TRUE
      ),
      rownames = FALSE
    )
  })
  
  
  # Raw results table
  output$results_table <- renderDataTable({
    req(values$processed_results)
    DT::datatable(
      values$processed_results$time_series,
      options = list(
        scrollY = "550px",
        paging = FALSE,
        scrollCollapse = TRUE,
        scrollX = TRUE
      )
    )
  })
  
  # Network visualization (STATIC)
  output$network_plot <- renderPlot({
    req(values$sim_results, input$network_round)
    
    round_idx <- as.numeric(input$network_round)
    graph <- values$network_states[[round_idx]]
    
    # Get similarity matrix for the selected round
    perceived_similarity_matrix <- matrix(0, nrow = vcount(graph), ncol = vcount(graph))
    
    if (round_idx == values$processed_results$params$T) {
      perceived_similarity_matrix <- values$sim_results$rounds[[round_idx]]$similarity$perceived_similarity_matrix
    } else {
      perceived_similarity_matrix <- values$sim_results$rounds[[round_idx]]$similarity$perceived_similarity_matrix
    }
    
    node_color_values <- numeric(vcount(graph))
    
    if (input$color_by == "Average Similarity") {
      if (!is.null(perceived_similarity_matrix) &&
          is.matrix(perceived_similarity_matrix) &&
          nrow(perceived_similarity_matrix) == vcount(graph) &&
          ncol(perceived_similarity_matrix) == vcount(graph)) {
        node_color_values <- rowMeans(perceived_similarity_matrix, na.rm = TRUE)
      } else {
        node_color_values <- rep(0, vcount(graph))
        warning("Invalid or missing perceived similarity matrix; using zeros.")
      }
      
    } else if (input$color_by == "Type Disclosure Rate") {
      node_color_values <- values$processed_results$agent_disclosures$disclosures
      
    } else if (input$color_by == "Degree") {
      node_color_values <- degree(graph)
    }
    
    if (length(node_color_values) > 0 &&
        diff(range(node_color_values)) > 0) {
      node_colors <- (node_color_values - min(node_color_values)) /
        (max(node_color_values) - min(node_color_values))
    } else {
      node_colors <- rep(0.5, vcount(graph))
    }
    
    mapped_colors <- heat.colors(100)[round(node_colors * 99) + 1]
    
    layout <- layout_with_fr(graph)
    node_size <- input$node_size
    
    if (input$show_labels) {
      plot(
        graph,
        layout = layout,
        vertex.color = mapped_colors,
        vertex.size = node_size * 3,
        vertex.label = 1:vcount(graph),
        vertex.label.cex = 0.8,
        edge.arrow.size = 0.5,
        main = paste("Network at Round", round_idx)
      )
    } else {
      plot(
        graph,
        layout = layout,
        vertex.color = mapped_colors,
        vertex.size = node_size * 3,
        vertex.label = NA,
        edge.arrow.size = 0.5,
        main = paste("Network at Round", round_idx)
      )
    }
    
    legend_title <- switch(
      input$color_by,
      "Average Similarity" = "Avg. Similarity",
      "Type Disclosure Rate" = "Disclosures",
      "Degree" = "Degree"
    )
    
    legend_values <- seq(min(node_color_values),
                         max(node_color_values),
                         length.out = 5)
    legend_colors <- heat.colors(5)
    
    legend(
      "topright",
      legend = round(legend_values, 2),
      fill = legend_colors,
      title = legend_title
    )
  })
  
  # --- NEW CODE: Add reactiveVal for play/pause state ---
  values$is_playing <- reactiveVal(FALSE)
  
  observeEvent(input$play_btn, {
    values$is_playing(TRUE)
  })
  
  observeEvent(input$pause_btn, {
    values$is_playing(FALSE)
  })
  
  # This observer increments anim_round while playing
  observe({
    if (values$is_playing()) {
      invalidateLater(input$play_speed, session)
      new_val <- input$anim_round + 1
      if (!is.null(values$vis_nodes)) {
        if (new_val <= length(values$vis_nodes)) {
          updateSliderInput(session, "anim_round", value = new_val)
        } else {
          # Reached the final round
          values$is_playing(FALSE)
        }
      }
    }
  })
  # --- End NEW CODE ---
  
  # --- NEW CODE: visNetwork output for animation ---
  output$network_vis <- renderVisNetwork({
    req(values$vis_nodes, values$vis_edges, input$anim_round)
    
    r <- input$anim_round
    if (r < 1 || r > length(values$vis_nodes)) {
      # Safety check
      return(visNetwork(nodes = data.frame(), edges = data.frame()))
    }
    
    node_data <- values$vis_nodes[[r]]
    edge_data <- values$vis_edges[[r]]
    
    # OPTIONAL: If you want the color_by logic to match your static plot,
    # you could recalc color here. But for now we'll show the stored color.
    
    visNetwork(
      nodes = node_data,
      edges = edge_data,
      width = "100%",
      height = "600px"
    ) %>%
      visNodes(fixed = TRUE) %>%  # fix positions
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = TRUE) %>%
      visInteraction(dragNodes = FALSE,
                     dragView = TRUE,
                     zoomView = TRUE)
  })
  # --- End NEW CODE ---
  
  # Parameter sweep functionality
  observeEvent(input$run_sweep, {
    req(!values$is_running)
    
    values$is_running <- TRUE
    add_log("Starting parameter sweep...")
    
    # Get base parameters
    base_params <- get_params()
    
    # Create parameter grid based on selected parameters to sweep
    param_grid <- list()
    
    if ("network_type" %in% input$sweep_params) {
      param_grid$network_type <- c("ER", "WS", "BA")
    }
    
    if ("model_version" %in% input$sweep_params) {
      param_grid$model_version <- c("static", "dynamic")
    }
    
    if ("disclosure_type" %in% input$sweep_params) {
      param_grid$disclosure_type <- c("selective", "global")
    }
    
    if ("delta" %in% input$sweep_params) {
      param_grid$delta <- c(0.2, 0.5, 0.8)
    }
    
    if ("b" %in% input$sweep_params) {
      param_grid$b <- c(0.6, 0.75, 0.9)
    }
    
    # Check if any parameters selected
    if (length(param_grid) == 0) {
      add_log("Error: No parameters selected for sweep")
      values$is_running <- FALSE
      return()
    }
    
    # Set up progress updates
    updateProgressBar(session, "progress", value = 0)
    
    # Run parameter sweep in background
    future::plan(future::multisession)
    
    future_promise <- future::future({
      # Reduce number of rounds for sweep to make it faster
      base_params$T <- min(base_params$T, 10)
      
      # Run sweep
      sweep_results <- run_parameter_sweep(base_params, param_grid, num_runs = input$num_runs)
      return(sweep_results)
    })
    
    promises::then(
      future_promise,
      onFulfilled = function(result) {
        values$sweep_results <- result
        add_log("Parameter sweep completed successfully!")
        values$is_running <- FALSE
        updateProgressBar(session, "progress", value = 100)
      },
      onRejected = function(error) {
        add_log(paste("Error in parameter sweep:", error$message))
        values$is_running <- FALSE
        updateProgressBar(session, "progress", value = 0)
      }
    )
  })
  
  # Sweep results visualization
  output$sweep_plot <- renderPlot({
    req(values$sweep_results)
    
    param_cols <- names(values$sweep_results)[!(
      names(values$sweep_results) %in%
        c(
          "run",
          "final_neighbor_similarity",
          "final_all_similarity",
          "final_similarity_gap",
          "final_variance",
          "final_bimodality",
          "final_clustering",
          "final_modularity",
          "final_welfare",
          "final_gini",
          "disclosure_rate"
        )
    )]
    
    if (length(param_cols) >= 2) {
      x_param <- param_cols[1]
      color_param <- param_cols[2]
    } else if (length(param_cols) == 1) {
      x_param <- param_cols[1]
      color_param <- "run"
    } else {
      return(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No parameter sweep data available"
          ) +
          theme_void()
      )
    }
    
    plot_data <- values$sweep_results
    
    p <- ggplot(
      plot_data,
      aes_string(
        x = x_param,
        y = "final_similarity_gap",
        color = color_param,
        group = color_param
      )
    ) +
      stat_summary(fun = mean, geom = "line") +
      stat_summary(fun = mean,
                   geom = "point",
                   size = 3) +
      stat_summary(
        fun.data = function(x) {
          data.frame(ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
        },
        geom = "errorbar",
        width = 0.2
      ) +
      facet_wrap( ~ "Similarity Gap") +
      labs(title = "Parameter Sweep Results",
           x = gsub("_", " ", toupper(x_param)),
           y = "Value") +
      theme_minimal() +
      theme(legend.title = element_text(face = "bold"))
    
    return(p)
  })
  
  # Sweep results table
  output$sweep_table <- renderDataTable({
    req(values$sweep_results)
    
    sweep_summary <- values$sweep_results %>%
      group_by_at(vars(-run, -starts_with("final_"), -disclosure_rate)) %>%
      summarize(
        neighbor_similarity = mean(final_neighbor_similarity),
        all_similarity = mean(final_all_similarity),
        similarity_gap = mean(final_similarity_gap),
        polarization = mean(final_variance),
        clustering = mean(final_clustering),
        modularity = mean(final_modularity),
        welfare = mean(final_welfare),
        gini = mean(final_gini),
        disclosure_rate = mean(disclosure_rate),
        .groups = "drop"
      )
    
    sweep_summary
  })
  
  # Download handlers
  output$download_results <- downloadHandler(
    filename = function() {
      paste("simulation_results_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      req(values$processed_results)
      write.csv(values$processed_results$time_series, file, row.names = FALSE)
    }
  )
  
  output$download_sweep <- downloadHandler(
    filename = function() {
      paste("sweep_results_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv",
            sep = "")
    },
    content = function(file) {
      req(values$sweep_results)
      write.csv(values$sweep_results, file, row.names = FALSE)
    }
  )
  
  # Automatically run the simulation once the UI is fully loaded
  session$onFlushed(function() {
    if (!isolate(values$is_running)) {
      shinyjs::delay(1000, shinyjs::click("run"))
    }
  }, once = TRUE)
  
}

###############################################################
# RUN SHINY APP
###############################################################
shinyApp(ui = ui, server = server)
