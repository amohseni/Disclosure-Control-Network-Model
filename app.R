###############################################################
# Shiny App: Selective Disclosure & Perceptions of Polarization
###############################################################

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
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
source("Disclosure-Control-Network-Simulation.R", local = TRUE)


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
  
  dashboardSidebar(width = 600, div(style = "padding:15px;", fluidRow(
    ## ---------- LEFT 6 columns ----------
    column(
      6,
      id    = "sidebar",
      style = "padding: 10px;",
      
      sidebarMenu(
        id    = "tabs",
        style = "font-size: 16px;",
        menuItem(
          "Simulation",
          tabName = "results",
          icon = icon("chart-bar")
        ),
        menuItem(
          "Parameter Sweep",
          tabName = "sweep",
          icon = icon("tasks")
        ),
        menuItem("About", tabName = "about", icon = icon("info-circle"))
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
        max = 1,
        value = 0.5,
        step = 0.05
      ),
      helpText("Rate influence decays with distance.")
    ),
    
    ## ---------- RIGHT 6 columns ----------
    column(
      6,
      style = "padding-left: 5px;",
      
      sliderInput(
        "disclosure_pct",
        "Disclosure Size (% of Population):",
        min = 0,
        max = 100,
        value = 50,
        step = 1
      ),
      helpText("Determines the size of agent disclosures"),
      
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
        ),
        helpText("Chance that any two nodes are connected.")
      ),
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
        helpText("Number of nearest neighbors each node starts with."),
        sliderInput(
          "p_ws",
          "Rewiring Probability (p):",
          min = 0.01,
          max = 0.5,
          value = 0.1,
          step = 0.01
        ),
        helpText("Probability that each edge is rewired to a random node.")
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
        helpText("Number of links a new node creates when it joins the network.")
      ),
      
      selectInput(
        "init_type",
        "Type Vector Initialization:",
        choices = c("Random" = "random", "Polarized" = "polarized")
      ),
      helpText("Choose whether traits are random or correlated in two groups."),
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
        helpText("Sets the strength of correlation of traits within each group.")
      ),
      
      selectInput(
        "model_version",
        "Network Version:",
        choices = c("Static" = "static", "Dynamic" = "dynamic")
      ),
      helpText(
        "Choose whether the network remains static or evolves dynamically."
      )
    )
  ))),
  dashboardBody(
    shinyjs::useShinyjs(),
    # Initialize shinyjs here
    includeCSS("www/style.css"),
    tabItems(
      tabItem(
        tabName = "results",
        tabBox(
          title = "Simulation",
          width = 12,
          tabPanel("Simulation Plot", fluidRow(
            box(
              title = "Controls",
              width = 3,
              selectInput(
                "plot_type",
                "Plot Type:",
                choices = c("Similarity", "Polarization", "Network Metrics", "Welfare"),
                selected = "Similarity"
              ),
              br(),
              actionButton(
                "run",
                "Run Simulation",
                icon  = icon("play"),
                style = "width: 80%; box-sizing: border-box; color: #fff;
                            background-color: #337ab7; margin: auto; display: block;"
              ),
              br(),
              downloadButton("download_results", "Download Results", style = "width: 80%; box-sizing: border-box; margin: auto; display: block;"),
              br(),
              div(style = "margin-top: 20px;", h6("Status Log")),
              div(style = "height: 275px; overflow: auto; padding-top: 10px", verbatimTextOutput("status_log_sim"))
            ),
            box(
              title = "Summary Statistics",
              width = 3,
              tableOutput("summary_stats")
            ),
            box(
              title = NULL,
              width = 6,
              plotlyOutput("time_series_plotly", height = "400px")
            )
          )),
          tabPanel("Network Visualization", fluidRow(
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
          tabPanel("Detailed Results", fluidRow(
            tabBox(
              title = NULL,
              width = 12,
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
      tabItem(tabName = "about", 
              box(
        title = "About",
        width = 12,
        style  = "padding: 30px; font-size:12pt;",
        h3(
          HTML(
            "Selective Disclosure & Perceived Polarization on Social Networks"
          )
        ),
        br(),
        p(
          "Social media platforms host public forums where individuals make declarations at an unprecedented scale, often with little control over their audience."
        ),
        p("We develop a formal model in which agents, each endowed with a binary type vector, strive to be perceived as similar to others. Agents selectively disclose partial information about their type vectors---either to the entire network or to a chosen subset---with evaluations weighted by social proximity. We explore how these disclosure strategies influence perceived similarity among immediate contacts and across the broader network, and how they may contribute to perceptions regarding polarization. We compare the effect in static networks with those in dynamic networks, where agents form or sever ties based on perceived similarity."),
        p(
          "This model simulates how changes in control over self-disclosure affects perceived similarity and polarization in social networks."
        )
      )),
      tabItem(tabName = "sweep", tabBox(
        width = 12,
        # ------------------ SWEEP INPUT TAB ------------------ #
        tabPanel("Sweep Input", fluidRow(
          # ------------------ SWEEP INPUT CONTROLS ------------------ #
          column(
            4,
            br(),
            div(
              style = "text-align: center;",
              actionButton(
                "run_sweep",
                "Run Parameter Sweep",
                icon = icon("play"),
                style = "width: 180pt; box-sizing: border-box; color: #fff; background-color: #337ab7; margin: auto; display: block;"
              ),
              br(),
              div(
                style = "text-align: center;",
                downloadButton("download_sweep", "Download Results", style = "width: 180pt; box-sizing: border-box; margin: auto; display: block;"),
              ),
              br(),
              div(
                style = "margin-top: 20px;",
                h6("Status Log"),
                div(style = "height: 275px; overflow: auto; padding-top: 10px", verbatimTextOutput("status_log"))
              )
            ),
            br(),
          ),
          # ------------------ SWEEP INPUT VARS ------------------ #
          column(
            4,
            h6("Number of Runs per Combination:"),
            numericInput(
              "num_runs",
              label = NULL,
              value = 1,
              min = 1
            ),
            h6("Number of Agents (N):"),
            fluidRow(
              column(3, numericInput("n_min", "Min N:", 10, min = 1)),
              column(3, numericInput("n_max", "Max N:", 50, min = 1)),
              column(3, numericInput("n_step", "Increment:", 20, min = 1))
            ),
            
            h6("Length of Type Vector (L):"),
            fluidRow(
              column(3, numericInput("l_min", "Min L:", 1, min = 1)),
              column(3, numericInput("l_max", "Max L:", 5, min = 1)),
              column(3, numericInput("l_step", "Increment:", 2, min = 1))
            ),
            
            h6("Number of Rounds (T):"),
            fluidRow(
              column(3, numericInput("t_min", "Min T:", 10, min = 1)),
              column(3, numericInput("t_max", "Max T:", 50, min = 1)),
              column(3, numericInput("t_step", "Increment:", 20, min = 1))
            )
          ),
          column(
            4,
            h6("Decay Factor (δ):"),
            fluidRow(
              column(
                3,
                numericInput(
                  "delta_min",
                  "Min δ:",
                  value = 0.0,
                  min   = 0.0,
                  max   = 1.0,
                  step  = 0.01
                )
              ),
              column(
                3,
                numericInput(
                  "delta_max",
                  "Max δ:",
                  value = 1.0,
                  min   = 0.0,
                  max   = 1.0,
                  step  = 0.01
                )
              ),
              column(
                3,
                numericInput(
                  "delta_step",
                  "Increment:",
                  value = 0.5,
                  min   = 0.01,
                  max   = 1.0,
                  step  = 0.01
                )
              )
            ),
            h6("Disclosure Size (%):"),
            fluidRow(
              column(
                3,
                numericInput(
                  "disclosure_pct_min",
                  "Min %:",
                  value = 0,
                  min   = 0,
                  max   = 100
                )
              ),
              column(
                3,
                numericInput(
                  "disclosure_pct_max",
                  "Max %:",
                  value = 100,
                  min   = 0,
                  max   = 100
                )
              ),
              column(
                3,
                numericInput(
                  "disclosure_pct_step",
                  "Increment:",
                  value = 50,
                  min   = 1,
                  max   = 100
                )
              )
            ),
            checkboxGroupInput(
              "network_type_sweep",
              "Network Type(s):",
              choices = c("Erdős-Rényi", "Watts-Strogatz", "Barabási-Albert"),
              selected = c("Erdős-Rényi")
            ),
            checkboxGroupInput(
              "model_version_sweep",
              "Network Version:",
              choices = c("static", "dynamic"),
              selected = "static"
            )
          )
        )),
        tabPanel("Trends", fluidRow(
          column(
            3,
            selectInput(
              "trend_x",
              "X-axis parameter:",
              choices = list(
                "Number of agents (N)" = "N",
                "Type-vector length (L)" = "L",
                "Number of rounds (T)" = "T",
                "Disclosure size (s)" = "s",
                "Decay factor (δ)" = "delta"
              )
            ),
            uiOutput("trend_range_ui"),
            radioButtons(
              "trend_group",
              "Metric group:",
              choices = list("Similarity metrics" = "similarity", "Other metrics" = "other")
            )
          ),
          column(9, plotlyOutput("trend_plotly", height = "600px"))
        )),
        tabPanel("Comparisons", fluidRow(
          column(
            3,
            checkboxGroupInput(
              "violin_metrics",
              "Select outcome measures:",
              choices = c(
                "final_perceived_neighbor_similarity",
                "final_perceived_all_similarity",
                "final_perceived_similarity_gap",
                "final_objective_neighbor_similarity",
                "final_objective_all_similarity",
                "final_objective_similarity_gap",
                "final_revealed_neighbor_similarity",
                "final_revealed_all_similarity",
                "final_revealed_similarity_gap",
                "final_variance",
                "final_bimodality",
                "final_clustering",
                "final_modularity",
                "final_mean_welfare",
                "final_gini"
              ),
              selected = NULL
            )
          ), column(9, plotlyOutput("violin_plotly", height = "1200px"))
        )),
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
    params <- list(
      N            = input$N,
      L            = input$L,
      T            = input$T,
      network_type = input$network_type,
      init_type    = input$init_type,
      model_version = input$model_version,
      delta        = input$delta,
      p            = get_network_param(),
      k            = get_k_param()
    )
    # Polarization bias
    if (input$init_type == "polarized") {
      params$b <- input$b
    } else {
      params$b <- 0.7
    }
    # Compute s from disclosure_pct
    pct <- input$disclosure_pct / 100
    s_count <- ceiling(pct * params$N)
    params$s <- max(1, min(params$N, s_count))
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
    
  })
  
  # Update summary statistics display
  output$summary_stats <- renderTable({
    req(values$processed_results)
    
    last <- tail(values$processed_results$time_series, 1)
    
    disclosure_rate <- values$processed_results$params$disclosure_metrics$disclosure_rate
    if (length(disclosure_rate) == 0)
      disclosure_rate <- NA
    
    data.frame(
      Metric = c(
        "Perceived Neighbor Similarity",
        "Perceived Global Similarity",
        "Revealed Neighbor Similarity",
        "Revealed Global Similarity",
        "Objective Neighbor Similarity",
        "Objective Global Similarity",
        "Perceived Similarity Gap",
        "Revealed Similarity Gap",
        "Objective Similarity Gap",
        "Mean Welfare",
        "Disclosure Rate"
      ),
      Value = c(
        last$perceived_neighbor_similarity        %||% NA,
        last$perceived_all_similarity             %||% NA,
        last$objective_neighbor_similarity        %||% NA,
        last$objective_all_similarity             %||% NA,
        last$revealed_neighbor_similarity         %||% NA,
        last$revealed_all_similarity              %||% NA,
        last$perceived_similarity_gap             %||% NA,
        last$revealed_similarity_gap              %||% NA,
        last$objective_similarity_gap             %||% NA,
        last$mean_welfare                         %||% NA,
        disclosure_rate                           %||% NA
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
  
  # Display logs
  output$status_log <- renderPrint({
    cat(paste(values$status_log, collapse = "\n"))
  })
  output$status_log_sim <- renderPrint({
    cat(paste(values$status_log, collapse = "\n"))
  })
  
  # Trend-range slider
  output$trend_range_ui <- renderUI({
    req(input$trend_x)
    rng <- switch(
      input$trend_x,
      N = seq(input$n_min, input$n_max, by = input$n_step),
      L = seq(input$l_min, input$l_max, by = input$l_step),
      T = seq(input$t_min, input$t_max, by = input$t_step),
      s = seq(1, input$n_max, by = 1)
    )
    sliderInput(
      "trend_range",
      NULL,
      min = min(rng),
      max = max(rng),
      value = c(min(rng), max(rng)),
      step = unique(diff(rng))
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
        
        add_log("Simulation completed successfully!")
        values$is_running <- FALSE
      },
      onRejected = function(error) {
        add_log(paste("Error in simulation:", error$message))
        values$is_running <- FALSE
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
  
  # --- reactiveVal for play/pause state ---
  values$is_playing <- reactiveVal(FALSE)
  
  observeEvent(input$play_btn, {
    ## if we're at (or past) the last frame, jump back to 1
    if (!is.null(values$vis_nodes) &&
        input$anim_round >= length(values$vis_nodes)) {
      updateSliderInput(session, "anim_round", value = 1)
    }
    values$is_playing(TRUE) # now begin (or resume) the loop
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
  
  # --- visNetwork output for animation ---
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
  
  
  
  
  ### Parameter sweep functionality
  observeEvent(input$run_sweep, {
    req(!values$is_running)
    values$is_running <- TRUE
    add_log("🔔 run_sweep pressed!")
    
    # Map the network types to codes
    net_map <- c(
      "Erdős-Rényi"     = "ER",
      "Watts-Strogatz"  = "WS",
      "Barabási-Albert" = "BA"
    )
    
    # Build disclosure percentage grid
    disc_seq <- seq(input$disclosure_pct_min,
                    input$disclosure_pct_max,
                    by = input$disclosure_pct_step)
    # Snapshot inputs
    num_runs_local    <- input$num_runs
    base_params_local <- get_params()
    param_grid_local  <- list(
      N               = seq(input$n_min, input$n_max, by = input$n_step),
      L               = seq(input$l_min, input$l_max, by = input$l_step),
      T               = seq(input$t_min, input$t_max, by = input$t_step),
      network_type    = net_map[input$network_type_sweep],
      model_version   = input$model_version_sweep,
      disclosure_pct = disc_seq
    )
    
    # Build and replicate parameter combinations
    combos <- expand.grid(param_grid_local, stringsAsFactors = FALSE)
    combos <- combos[rep(seq_len(nrow(combos)), each = num_runs_local), , drop = FALSE]
    combos$run_id <- rep(seq_len(num_runs_local), length.out = nrow(combos))
    total_iters <- nrow(combos)
    
    # Convert percentage to count for each combination
    combos$s <- ceiling((combos$disclosure_pct / 100) * combos$N)
    combos$s <- pmin(combos$s, combos$N)
    
    # Run sweep with progress bar
    withProgress(message = "Running parameter sweep...", value = 0, {
      results_list <- vector("list", total_iters)
      
      for (i in seq_len(total_iters)) {
        # Merge base params with this combination
        params <- base_params_local
        for (nm in names(param_grid_local))
          params[[nm]] <- combos[i, nm]
        
        # Perform simulation and extract final metrics
        sim_res <- run_simulation(params)
        ts <- process_simulation_results(sim_res)$time_series
        final <- ts[nrow(ts), , drop = FALSE]
        
        # Store metrics
        results_list[[i]] <- tibble(
          network_type  = params$network_type,
          model_version = params$model_version,
          delta         = params$delta,
          b             = params$b,
          run_id        = combos$run_id[i],
          final_perceived_neighbor_similarity = final$perceived_neighbor_similarity,
          final_perceived_all_similarity      = final$perceived_all_similarity,
          final_perceived_similarity_gap      = final$perceived_similarity_gap,
          final_objective_neighbor_similarity = final$objective_neighbor_similarity,
          final_objective_all_similarity      = final$objective_all_similarity,
          final_objective_similarity_gap      = final$objective_similarity_gap,
          final_revealed_neighbor_similarity  = final$revealed_neighbor_similarity,
          final_revealed_all_similarity       = final$revealed_all_similarity,
          final_revealed_similarity_gap       = final$revealed_similarity_gap,
          final_variance                      = final$variance,
          final_bimodality                    = final$bimodality,
          final_clustering                    = final$clustering,
          final_modularity                    = final$modularity,
          final_mean_welfare                  = final$mean_welfare,
          final_gini                          = final$gini
        )
        
        #NEW: increment progress
        incProgress(1 / total_iters, detail = paste0("Run ", i, " of ", total_iters))
      }
    })
    
    # Assign results and reset state
    values$sweep_results <- dplyr::bind_rows(results_list)
    add_log("Parameter sweep completed successfully!")
    values$is_running <- FALSE
    updateProgressBar(session, "progress", value = 100)
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
      paste0("parameter_sweep_", Sys.Date(), ".csv")
    },
    contentType = "text/csv",
    # ← add this line
    content = function(file) {
      cat("👉 download_sweep content() is running\n")
      # Wait until the sweep has run
      req(values$sweep_results)
      # Write the raw data frame to a real CSV
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

# Open app in browser
options(shiny.launch.browser = TRUE)
shinyApp(ui = ui, server = server)
