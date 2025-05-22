###############################################################
# UI Definition
###############################################################

ui <- dashboardPage(
  dashboardHeader(
    title = HTML("Selective Disclosure & Perceived Polarization on Networks"),
    titleWidth = 600
  ),
  ## ---------- DASHBOARD SIDEBAR ----------
  dashboardSidebar(width = 200, div(style = "padding:15px;", fluidRow(
    column(
      12,
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
      )
    ),
  ))),
  ## ---------- DASHBOARD BODY ----------
  dashboardBody(
    shinyjs::useShinyjs(),
    includeCSS("www/style.css"),
    tabItems(
      tabItem(
        ## ---------- SIMULATION PAGE ----------
        tabName = "results",
        tabBox(
          title = NULL,
          width = 12,
          ## ---------- SIMULATION INPUTS PANEL ----------
          tabPanel("Simulation Inputs", fluidRow(
            ## ---------- SIMULATION INPUTS PANEL - LEFT COLUMN ----------
            box(
              title = "Controls",
              width = 3,
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
            ## ---------- SIMULATION INPUTS PANEL - MIDDLE COLUMN ----------
            column(
              3,
              style = "padding-left: 5px;",
              sliderInput(
                "N",
                "Number of Agents (N):",
                min = 1,
                max = 100,
                value = 40
              ),
              helpText("Number of agents in the network."),
              hr(style = "margin-top: 20px; margin-bottom: 20px;"),
              sliderInput(
                "L",
                "Length of Type Vector (L):",
                min = 1,
                max = 10,
                value = 3
              ),
              helpText("Number of traits per agent."),
              hr(style = "margin-top: 20px; margin-bottom: 20px;"),
              sliderInput(
                "T",
                "Number of Rounds (T):",
                min = 1,
                max = 100,
                value = 20
              ),
              helpText("Total number of simulation rounds."),
              hr(style = "margin-top: 20px; margin-bottom: 20px;"),
              sliderInput(
                "delta",
                "Influence Decay Factor (δ):",
                min = 0.1,
                max = 1,
                value = 0.5,
                step = 0.05
              ),
              helpText("Rate influence decays with distance."),
              hr(style = "margin-top: 20px; margin-bottom: 20px;")
            ),
            ## ---------- SIMULATION INPUTS PANEL - RIGHT COLUMN ----------
            column(
              3,
              style = "padding-left: 10px;",
              sliderInput(
                "disclosure_pct",
                "Disclosure Size (% of Population):",
                min = 0,
                max = 100,
                value = 50,
                step = 1
              ),
              helpText("Determines the size of agent disclosures"),
              hr(style = "margin-top: 20px; margin-bottom: 20px;"),
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
                  min = 0.0,
                  max = 1,
                  value = 0.1,
                  step = 0.1
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
              hr(style = "margin-top: 20px; margin-bottom: 20px;"),
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
              hr(style = "margin-top: 20px; margin-bottom: 20px;"),
              selectInput(
                "model_version",
                "Network Version:",
                choices = c("Static" = "static", "Dynamic" = "dynamic")
              ),
              helpText(
                "Choose whether the network remains static or evolves dynamically."
              ),
              hr(style = "margin-top: 20px; margin-bottom: 20px;")
            )
          )),
          ## ---------- TIME SERIES PLOT PANEL ----------
          tabPanel("Time Series Plot", fluidRow(
            box(
              title = "Summary Statistics",
              width = 3,
              tableOutput("summary_stats")
            ),
            box(
              title = NULL,
              width = 6,
              selectInput(
                width = 150,
                "plot_type",
                "Plot Type:",
                choices = c("Similarity", "Polarization", "Network Metrics", "Welfare"),
                selected = "Similarity"
              ),
              plotlyOutput("time_series_plotly", height = "400px")
            )
          )),
          ## ---------- NETWORK ANIMATION PANEL ----------
          tabPanel("Network Animation", fluidRow(
            box(
              title = "Animation Controls",
              width = 3,
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
              title = "Network Animation",
              width = 6,
              visNetworkOutput("network_vis", height = "600px")
            )
          )),
          ## ---------- DETAILED RESULTS PANEL ----------
          tabPanel("Detailed Results", fluidRow(
            tabBox(
              title = NULL,
              width = 9,
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
      ## ---------- ABOUT PAGE ----------
      tabItem(
        tabName = "about",
        box(
          title = NULL,
          width = 6,
          style  = "padding: 30px; font-size:12pt;",
          h3(
            HTML("Selective Disclosure & Perceived Polarization on Networks")
          ),
          br(),
          p(
            "Social media platforms host public forums where individuals make declarations at an unprecedented scale, often with little control over their audience."
          ),
          p(
            "We develop a formal model in which agents, each endowed with a binary type vector, strive to be perceived as similar to others. Agents selectively disclose partial information about their type vectors---either to the entire network or to a chosen subset---with evaluations weighted by social proximity. We explore how these disclosure strategies influence perceived similarity among immediate contacts and across the broader network, and how they may contribute to perceptions regarding polarization. We compare the effect in static networks with those in dynamic networks, where agents form or sever ties based on perceived similarity."
          ),
          p(
            "This model simulates how changes in control over self-disclosure affects perceived similarity and polarization in social networks."
          )
        )
      ),
      ## ---------- PARAMETER SWEEP PAGE ----------
      tabItem(tabName = "sweep", tabBox(
        width = 12,
        ## ---------- SWEEP INPUTS PANEL ----------
        tabPanel("Sweep Inputs", fluidRow(
          ## ---------- SWEEP INPUTS PANEL - LEFT COLUMN ----------
          column(
            3,
            box(
              title = "Controls",
              width = 12,
              div(
                style = "text-align: left;",
                radioButtons(
                  "sweep_preset",
                  label = "Sweep Preset:",
                  choices = c("Minimal Test", "Full Test", "Custom"),
                  selected = "Minimal Test",
                  inline = TRUE
                ),
                br(),
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
                  style = "margin-top: 10px;",
                  h6("Status Log"),
                  div(style = "height: 275px; overflow: auto; padding-top: 10px", verbatimTextOutput("status_log"))
                )
              )
            )
          ), 
          ## ---------- SWEEP INPUTS PANEL - MIDDLE COLUMN ----------
          column(
            3,
            style = "padding-left: 5px;",
            box(
              title = "Sweep Parameters",
              width = 12,
              div(
                hr(style = "margin-top: 0px; margin-bottom: 20px;"),
                h6("Number of Runs per Combination:"),
                numericInput(
                  "num_runs",
                  label = NULL,
                  value = 1,
                  min = 1
                ),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                h6("Number of Agents (N):"),
                fluidRow(
                  column(4, numericInput("n_min", "Min N:", 3, min = 1)),
                  column(4, numericInput("n_max", "Max N:", 7, min = 1)),
                  column(4, numericInput("n_step", "Increment:", 2, min = 1))
                ),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                h6("Length of Type Vector (L):"),
                fluidRow(
                  column(4, numericInput("l_min", "Min L:", 1, min = 1)),
                  column(4, numericInput("l_max", "Max L:", 3, min = 1)),
                  column(4, numericInput("l_step", "Increment:", 1, min = 1))
                ),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                h6("Number of Rounds (T):"),
                fluidRow(
                  column(4, numericInput("t_min", "Min T:", 1, min = 1)),
                  column(4, numericInput("t_max", "Max T:", 3, min = 1)),
                  column(4, numericInput("t_step", "Increment:", 2, min = 1))
                ),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                h6("Type Vector Initialization:", style = "margin-bottom: 15px;"),
                div(
                  style = "margin-bottom: 15px;",
                  checkboxGroupInput(
                    "init_type_sweep",
                    label = NULL,
                    choices = c("Random" = "random", "Polarized" = "polarized"),
                    selected = "random"
                  )
                ),
                
                conditionalPanel(
                  condition = "input.init_type_sweep.includes('polarized')",
                  br(),
                  h6("Bias Parameter (b):"),
                  fluidRow(
                    column(
                      4,
                      numericInput(
                        "b_min",
                        "Min b:",
                        0.7,
                        min = 0.5,
                        max = 0.99,
                        step = 0.01
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        "b_max",
                        "Max b:",
                        0.7,
                        min = 0.5,
                        max = 0.99,
                        step = 0.01
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        "b_step",
                        "Increment:",
                        0.1,
                        min = 0.01,
                        max = 0.2,
                        step = 0.01
                      )
                    )
                  )
                ),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                h6("Decay Factor (δ):"),
                fluidRow(
                  column(
                    4,
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
                    4,
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
                    4,
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
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                h6("Disclosure Size (%):"),
                fluidRow(
                  column(
                    4,
                    numericInput(
                      "disclosure_pct_min",
                      "Min %:",
                      value = 0,
                      min   = 0,
                      max   = 100
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "disclosure_pct_max",
                      "Max %:",
                      value = 100,
                      min   = 0,
                      max   = 100
                    )
                  ),
                  column(
                    4,
                    numericInput(
                      "disclosure_pct_step",
                      "Increment:",
                      value = 50,
                      min   = 1,
                      max   = 100
                    )
                  )
                )
              )
            )
          ), 
          ## ---------- SWEEP INPUTS PANEL - RIGHT COLUMN ----------
          column(
            3,
            style = "padding-left: 10px;",
            box(
              title = "Network Parameters",
              width = 12,
              div(
                hr(style = "margin-top: 0px; margin-bottom: 20px;"),
                checkboxGroupInput(
                  "network_type_sweep",
                  "Network Type(s):",
                  choices = c("Erdős-Rényi", "Watts-Strogatz", "Barabási-Albert"),
                  selected = c("Erdős-Rényi")
                ),
                
                conditionalPanel(
                  condition = "input.network_type_sweep.includes('Erdős-Rényi')",
                  br(),
                  h6("Erdős-Rényi Edge Probability (p):"),
                  fluidRow(
                    column(
                      4,
                      numericInput(
                        "p_er_min",
                        "Min p:",
                        0.2,
                        min = 0.0,
                        max = 1,
                        step = 0.2
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        "p_er_max",
                        "Max p:",
                        0.4,
                        min = 0.0,
                        max = 1,
                        step = 0.2
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        "p_er_step",
                        "Increment:",
                        0.2,
                        min = 0.0,
                        max = 0.4,
                        step = 0.2
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.network_type_sweep.includes('Watts-Strogatz')",
                  br(),
                  h6("Watts-Strogatz Initial Neighbors (k):"),
                  fluidRow(
                    column(4, numericInput(
                      "k_ws_min", "Min k:", 2, min = 2, max = 10
                    )),
                    column(4, numericInput(
                      "k_ws_max", "Max k:", 4, min = 2, max = 10
                    )),
                    column(4, numericInput(
                      "k_ws_step", "Increment:", 2, min = 1, max = 4
                    ))
                  ),
                  h6("Watts-Strogatz Rewiring Probability (p):"),
                  fluidRow(
                    column(
                      4,
                      numericInput(
                        "p_ws_min",
                        "Min p:",
                        0.1,
                        min = 0.01,
                        max = 0.5,
                        step = 0.01
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        "p_ws_max",
                        "Max p:",
                        0.3,
                        min = 0.01,
                        max = 0.5,
                        step = 0.01
                      )
                    ),
                    column(
                      4,
                      numericInput(
                        "p_ws_step",
                        "Increment:",
                        0.2,
                        min = 0.01,
                        max = 0.2,
                        step = 0.01
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "input.network_type_sweep.includes('Barabási-Albert')",
                  br(),
                  h6("Barabási-Albert Edges per New Node (m):"),
                  fluidRow(
                    column(4, numericInput(
                      "m_ba_min", "Min m:", 1, min = 1, max = 10
                    )),
                    column(4, numericInput(
                      "m_ba_max", "Max m:", 3, min = 1, max = 10
                    )),
                    column(4, numericInput(
                      "m_ba_step", "Increment:", 2, min = 1, max = 5
                    ))
                  )
                ),
                
                hr(),
                div(
                  style = "margin-bottom: 15px;",
                  checkboxGroupInput(
                    "model_version_sweep",
                    "Network Version:",
                    choices = c("Static" = "static", "Dynamic" = "dynamic"),
                    selected = c("static", "dynamic")
                  )
                )
              )
            )
          )
        )),
        ## ---------- TRENDS PANEL ----------
        tabPanel("Trends", fluidRow(
          column(
            3,
            box(
              title = "Trends",
              width = 12,
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
            br(),
            radioButtons(
              "trend_group",
              "Metric group:",
              choices = list("Similarity metrics" = "similarity", "Other metrics" = "other")
            )
          )),
          column(
            6,
            div(
              style = "position: relative; min-height: 600px; height: 600px;",
              div(
                id = "no_data_warning_trend",
                style = "text-align: center; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); padding: 30px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6; width: 80%;",
                icon("info-circle", class = "fa-2x", style = "color: #17a2b8;"),
                h5("No parameter sweep data available", style = "font-size: 16px;"),
                h5("Run a parameter sweep to visualize the resulting data.", style = "font-size: 16px;")
              ),
              div(
                id = "trend_plot_container",
                style = "display: none; min-height: 950px; padding-bottom: 50px;",
                plotlyOutput("trend_plotly", height = "900px")
              )
            )
          )
        )),
        ## ---------- COMPARISONS PANEL ----------        
        tabPanel("Comparisons", fluidRow(column(
          3,
          box(
            title = "Comparisons",
            width = 12,
            checkboxGroupInput(
              "violin_metrics",
              "Select measures to compare:",
              choices = c(
                "Perceived Neighbor Similarity" = "final_perceived_neighbor_similarity",
                "Perceived Global Similarity" = "final_perceived_all_similarity",
                "Perceived Similarity Gap" = "final_perceived_similarity_gap",
                "Objective Neighbor Similarity" = "final_objective_neighbor_similarity",
                "Objective Global Similarity" = "final_objective_all_similarity",
                "Objective Similarity Gap" = "final_objective_similarity_gap",
                "Revealed Neighbor Similarity" = "final_revealed_neighbor_similarity",
                "Revealed Global Similarity" = "final_revealed_all_similarity",
                "Revealed Similarity Gap" = "final_revealed_similarity_gap",
                "Variance" = "final_variance",
                "Bimodality" = "final_bimodality",
                "Clustering Coefficient" = "final_clustering",
                "Modularity" = "final_modularity",
                "Mean Welfare" = "final_mean_welfare",
                "Gini Coefficient" = "final_gini"
              )
            ),
            hr(),
            helpText(
              HTML(
                "<strong>Tip:</strong> Select 1-3 metrics for more readable plots"
              )
            )
          )
        ), column(
          6,
          div(
            style = "position: relative; min-height: 600px; height: 600px;",
            div(
              id = "no_data_warning_violin",
              style = "text-align: center; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); padding: 30px; background-color: #f8f9fa; border-radius: 5px; border: 1px solid #dee2e6; width: 80%;",
              icon("info-circle", class = "fa-2x", style = "color: #17a2b8;"),
              h5("No parameter sweep data available", style = "font-size: 16px;"),
              h5("Run a parameter sweep to visualize the resulting data.", style = "font-size: 16px;")
            ),
            div(
              id = "violin_plot_container",
              style = "display: none; min-height: 1250px; padding-bottom: 50px;",
              plotlyOutput("violin_plotly", height = "1200px")
            )
          )
        ))),
      ))
    )
  )
)
## ---------- EOD ----------