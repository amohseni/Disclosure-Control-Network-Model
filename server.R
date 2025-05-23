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
  
  # Initialize UI state based on data availability
  observe({
    if (is.null(values$sweep_results)) {
      # No data - show warnings, hide plots
      runjs(
        "
        $('#no_data_warning_trend').show();
        $('#no_data_warning_violin').show();
        $('#trend_plot_container').hide();
        $('#violin_plot_container').hide();
      "
      )
    } else {
      # Data available - hide warnings, show plots
      runjs(
        "
        $('#no_data_warning_trend').hide();
        $('#no_data_warning_violin').hide();
        $('#trend_plot_container').show();
        $('#violin_plot_container').show();
      "
      )
    }
  })
  
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
      return(4) # Default value for other models
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
    # Informational text about how x-axis ranges work
    req(input$trend_x)
    x_param <- input$trend_x
    
    # Get current min, max, step based on selected parameter
    x_min <- switch(
      x_param,
      "N" = input$n_min,
      "L" = input$l_min,
      "T" = input$t_min,
      "s" = input$disclosure_pct_min,
      "delta" = input$delta_min
    )
    x_max <- switch(
      x_param,
      "N" = input$n_max,
      "L" = input$l_max,
      "T" = input$t_max,
      "s" = input$disclosure_pct_max,
      "delta" = input$delta_max
    )
    x_step <- switch(
      x_param,
      "N" = input$n_step,
      "L" = input$l_step,
      "T" = input$t_step,
      "s" = input$disclosure_pct_step,
      "delta" = input$delta_step
    )
    
    div(
      style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
      p(strong("X-axis Settings:")),
      p(paste("Min:", x_min)),
      p(paste("Max:", x_max)),
      p(paste("Step:", x_step)),
      p("(Adjust in Sweep Input tab)")
    )
  })
  
  ## ---- Give the violin plot a default selection ---------------------------
  updateCheckboxGroupInput(
    session,
    "violin_metrics",
    selected = c(
      "final_perceived_neighbor_similarity",
      "final_perceived_all_similarity",
      "final_perceived_similarity_gap"
    )
  )
  
  # Run simulation when button is clicked (either one)
  observeEvent(c(input$run, input$run2), {
    req(!values$is_running)  # Only run if not already running
    
    values$is_running <- TRUE
    add_log("Starting simulation...")
    
    # Get parameters
    params <- get_params()
    print(params)
    
    # Run simulation with a progress bar
    withProgress(message = "Running simulation...", value = 0, {
      # Run simulation in a separate R process to avoid blocking the UI
      future::plan(future::multisession)
      
      future_promise <- future::future({
        # Run simulation
        sim_results <- run_simulation(params)
        processed_results <- process_simulation_results(sim_results)
        
        # Return both raw and processed results
        list(sim_results = sim_results, processed_results = processed_results)
      }, seed = TRUE) # Set RNG seed
      
      # Set up observer to periodically check if the future is done
      progress_observer <- observe({
        invalidateLater(100, session) # Check progress every 100ms
        
        # If the future is resolved, we're done
        if (future::resolved(future_promise)) {
          progress_observer$destroy()
          # Set progress to 100% when done
          setProgress(1, detail = "Finalizing results...")
        } else {
          # While the simulation is running, we can't get true progress
          # So we'll pulse the progress bar instead
          setProgress(value = NULL, message = "Running simulation...", detail = paste("Round", params$T))
        }
      })
    })
    
    # Handle the promise when it completes
    promises::then(
      future_promise,
      onFulfilled = function(result) {
        values$sim_results <- result$sim_results
        values$processed_results <- result$processed_results
        
        # Use the actual network state at each round from network_history
        values$network_states <- values$sim_results$network_history
        
        
        # --- NEW CODE: Prepare data for visNetwork animation ---
        # (1) Compute a single layout from the first round's graph
        g_first <- values$network_states[[1]]
        coords <- layout_with_fr(g_first)
        coords <- coords * 200  # scaling to look nice in visNetwork
        
        # (2) We'll store node/edge data frames for each round in these:
        values$vis_nodes <- vector("list", params$T)
        values$vis_edges <- vector("list", params$T)
        
        # Use the network state from each round:
        for (r in seq_len(params$T)) {
          g_r <- values$network_states[[r]]
          
          # Create a data.frame for node positions (fixed)
          node_positions <- data.frame(id = 1:vcount(g_r),
                                       x = coords[, 1],
                                       y = coords[, 2])
          
          # Color nodes based on their perceived similarity in this round
          # Calculate average perceived similarity for each node
          if (length(values$sim_results$rounds) >= r) {
            perceived_similarity_matrix <- values$sim_results$rounds[[r]]$similarity$similarity_matrices$perceived
            avg_similarities <- rowMeans(perceived_similarity_matrix, na.rm = TRUE)
            
            # Normalize for color mapping (0 to 1 scale)
            similarity_normalized <- (avg_similarities - min(avg_similarities)) / 
              (max(avg_similarities) - min(avg_similarities) + 1e-10) # Add small epsilon to avoid division by zero
            
            # Generate colors from blue to red
            node_colors <- colorRampPalette(c("blue", "lightblue", "lightpink", "red"))(100)
            color_idx <- ceiling(similarity_normalized * 99) + 1 # Scale to 1-100
            node_positions$color <- node_colors[color_idx]
            node_positions$similarity <- avg_similarities
            node_positions$title <- paste0("Node ", node_positions$id, "<br>Sim: ", round(avg_similarities, 3))
          } else {
            # Fallback if we don't have similarity data
            node_positions$color <- "lightblue"
            node_positions$title <- paste("Node", node_positions$id)
          }
          
          node_positions$label <- paste("", node_positions$id) # Show just the ID number
          node_positions$size <- 20
          
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
    
    # Set theme with increased title margin and right legend
    theme_set(theme_minimal() +
                theme(
                  plot.title = element_text(margin = margin(b = 20)),
                  legend.position = "right",
                  legend.title = element_text(face = "bold", family = "Roboto", size = 12),
                  legend.text = element_text(family = "Roboto", size = 10),
                  legend.box.background = element_rect(color = "#E5E5E5", fill = "white")
                ))
    
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
        scale_y_continuous(sec.axis = sec_axis(~ . / 100, name = "Gini Coefficient"))
    }
    
    return(p)
  }
  
  # Regular plot output
  output$time_series_plot <- renderPlot({
    create_plot()
  })
  
  # Plotly interactive plot
  output$time_series_plotly <- renderPlotly({
    ggplotly(create_plot()) %>%
      layout(
        font = list(family = "Roboto"),
        margin = list(t = 80, b = 100, r = 120), # Add right margin for legend
        # Increase top and bottom margins significantly
        title = list(
          y = 1,
          # Position title at top
          pad = list(t = 25, b = 25)  # Add padding above and below title
        ),
        legend = list(
          y = 0.98,
          x = 1.05,
          xanchor = "left",
          yanchor = "top",
          font = list(family = "Roboto", size = 12),
          borderwidth = 0,
          bgcolor = "rgba(255, 255, 255, 0.7)"
        )
      )
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
    
    # Create network visualization
    vis <- visNetwork(
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
                     zoomView = TRUE) %>%
      visLegend(addNodes = list(
        list(label = "Low Similarity", shape = "dot", color = "blue", size = 10),
        list(label = "Medium Similarity", shape = "dot", color = "lightblue", size = 10),
        list(label = "High Similarity", shape = "dot", color = "lightpink", size = 10),
        list(label = "Very High Similarity", shape = "dot", color = "red", size = 10)
      ), useGroups = FALSE) %>%
      visLayout(randomSeed = 123) # Use consistent layout across frames
    
    # Add title showing the current round
    vis <- vis %>%
      htmlwidgets::onRender(paste0("
        function(el, x) {
          // Add a title to the network visualization
          var titleDiv = document.createElement('div');
          titleDiv.style.textAlign = 'center';
          titleDiv.style.fontWeight = 'bold';
          titleDiv.style.marginBottom = '10px';
          titleDiv.innerHTML = 'Network at Round ", r, "';
          el.insertBefore(titleDiv, el.firstChild);
        }
      "))
    
    vis
  })
  
  ### Parameter sweep functionality
  observeEvent(input$run_sweep, {
    req(!values$is_running)
    values$is_running <- TRUE
    add_log("Starting parameter sweep...")
    
    # Map the network types to codes
    net_map <- c(
      "Erdős-Rényi"     = "ER",
      "Watts-Strogatz"  = "WS",
      "Barabási-Albert" = "BA"
    )
    
    # Build parameter sequences
    disc_seq <- seq(input$disclosure_pct_min,
                    input$disclosure_pct_max,
                    by = input$disclosure_pct_step)
    
    delta_seq <- seq(input$delta_min, input$delta_max, by = input$delta_step)
    
    # Type initialization - handle both random and polarized
    init_type_seq <- input$init_type_sweep
    
    # Bias parameter for polarized initialization (only used if "polarized" is selected)
    b_seq <- NULL
    if ("polarized" %in% init_type_seq) {
      b_seq <- seq(input$b_min, input$b_max, by = input$b_step)
      add_log(paste("Bias values:", paste(b_seq, collapse = ", ")))
    } else {
      # If only random, we still need a value for b in the grid
      b_seq <- 0.7 # Default value, won't be used
    }
    
    # Network-specific parameters
    # For each network type, we'll add its specific parameters if that type is selected
    
    # Snapshot inputs
    num_runs_local    <- input$num_runs
    base_params_local <- get_params()
    param_grid_local  <- list(
      N               = seq(input$n_min, input$n_max, by = input$n_step),
      L               = seq(input$l_min, input$l_max, by = input$l_step),
      T               = seq(input$t_min, input$t_max, by = input$t_step),
      delta           = delta_seq,
      network_type    = net_map[input$network_type_sweep],
      model_version   = input$model_version_sweep,
      disclosure_pct  = disc_seq,
      init_type       = init_type_seq
    )
    
    # Add b parameter only if polarized is selected
    if ("polarized" %in% init_type_seq) {
      param_grid_local$b <- b_seq
    }
    
    # Build and replicate parameter combinations
    combos <- expand.grid(param_grid_local, stringsAsFactors = FALSE)
    combos <- combos[rep(seq_len(nrow(combos)), each = num_runs_local), , drop = FALSE]
    combos$run_id <- rep(seq_len(num_runs_local), length.out = nrow(combos))
    total_iters <- nrow(combos)
    
    # Convert percentage to count for each combination
    combos$s <- ceiling((combos$disclosure_pct / 100) * combos$N)
    # s=0 is now properly handled in the simulation code
    combos$s <- pmin(combos$s, combos$N)
    
    # Run sweep with progress bar
    withProgress(message = "Running parameter sweep...", value = 0, {
      results_list <- vector("list", total_iters)
      
      for (i in seq_len(total_iters)) {
        # Merge base params with this combination
        params <- base_params_local
        for (nm in names(param_grid_local))
          params[[nm]] <- combos[i, nm]
        
        # Make sure delta is explicitly set (important!)
        params$delta <- combos[i, "delta"]
        
        # Perform simulation and extract final metrics
        sim_res <- run_simulation(params)
        ts <- process_simulation_results(sim_res)$time_series
        final <- ts[nrow(ts), , drop = FALSE]
        
        # Store metrics
        results_list[[i]] <- tibble(
          ## ---------- design parameters ----------
          N            = params$N,
          L            = params$L,
          T            = params$T,
          s            = params$s,
          # Store the original percentage from the parameter grid
          # (not the recalculated one that might be different for s=1)
          disclosure_pct = combos[i, "disclosure_pct"],
          delta        = params$delta,
          network_type = params$network_type,
          model_version = params$model_version,
          b            = params$b,
          run_id       = combos$run_id[i],
          
          ## ---------- outcome metrics ------------
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
    
    # Show plot containers and hide warning messages using direct JavaScript
    runjs(
      "
      // Hide warning messages
      $('#no_data_warning_trend').hide();
      $('#no_data_warning_violin').hide();

      // Show plot containers
      $('#trend_plot_container').show();
      $('#violin_plot_container').show();
    "
    )
    
    updateProgressBar(session, "progress", value = 100)
  })
  
  # -------------------------------------------------------------------
  #  Preset loader for the sweep-input pane
  # -------------------------------------------------------------------
  observeEvent(input$sweep_preset, ignoreInit = TRUE, {
    if (input$sweep_preset == "Minimal Test") {
      ## ---- MINIMAL --------------------------------------------------
      updateNumericInput(session, "num_runs", value = 1)
      
      updateNumericInput(session, "n_min", value = 3)
      updateNumericInput(session, "n_max", value = 7)
      updateNumericInput(session, "n_step", value = 2)
      
      updateNumericInput(session, "l_min", value = 1)
      updateNumericInput(session, "l_max", value = 3)
      updateNumericInput(session, "l_step", value = 1)
      
      updateNumericInput(session, "t_min", value = 1)
      updateNumericInput(session, "t_max", value = 3)
      updateNumericInput(session, "t_step", value = 2)
      
      updateNumericInput(session, "delta_min", value = 0.0)
      updateNumericInput(session, "delta_max", value = 1.0)
      updateNumericInput(session, "delta_step", value = 0.5)
      
      updateNumericInput(session, "disclosure_pct_min", value = 0)
      updateNumericInput(session, "disclosure_pct_max", value = 100)
      updateNumericInput(session, "disclosure_pct_step", value = 50)
      
      # Update network types
      updateCheckboxGroupInput(session, "network_type_sweep", selected = "Erdős-Rényi")
      
      # Include both static and dynamic networks
      updateCheckboxGroupInput(session,
                               "model_version_sweep",
                               selected = c("static", "dynamic"))
      
      # Network type specific parameters - Minimal test
      updateNumericInput(session, "p_er_min", value = 0.1)
      updateNumericInput(session, "p_er_max", value = 0.3)
      updateNumericInput(session, "p_er_step", value = 0.2)
      
      updateNumericInput(session, "k_ws_min", value = 2)
      updateNumericInput(session, "k_ws_max", value = 4)
      updateNumericInput(session, "k_ws_step", value = 2)
      
      updateNumericInput(session, "p_ws_min", value = 0.1)
      updateNumericInput(session, "p_ws_max", value = 0.3)
      updateNumericInput(session, "p_ws_step", value = 0.2)
      
      updateNumericInput(session, "m_ba_min", value = 1)
      updateNumericInput(session, "m_ba_max", value = 3)
      updateNumericInput(session, "m_ba_step", value = 2)
      
      # Type initialization
      updateCheckboxGroupInput(session, "init_type_sweep", selected = "random")
      updateNumericInput(session, "b_min", value = 0.7)
      updateNumericInput(session, "b_max", value = 0.7)
      updateNumericInput(session, "b_step", value = 0.1)
      
    } else if (input$sweep_preset == "Full Test") {
      ## ---- FULL -----------------------------------------------------
      updateNumericInput(session, "num_runs", value = 20) # Reduced from 1000 for practicality
      
      updateNumericInput(session, "n_min", value = 10)
      updateNumericInput(session, "n_max", value = 100)
      updateNumericInput(session, "n_step", value = 30)
      
      updateNumericInput(session, "l_min", value = 1)
      updateNumericInput(session, "l_max", value = 10)
      updateNumericInput(session, "l_step", value = 3)
      
      updateNumericInput(session, "t_min", value = 10)
      updateNumericInput(session, "t_max", value = 100)
      updateNumericInput(session, "t_step", value = 30)
      
      updateNumericInput(session, "delta_min", value = 0.0)
      updateNumericInput(session, "delta_max", value = 1.0)
      updateNumericInput(session, "delta_step", value = 0.25)
      
      updateNumericInput(session, "disclosure_pct_min", value = 0)
      updateNumericInput(session, "disclosure_pct_max", value = 100)
      updateNumericInput(session, "disclosure_pct_step", value = 25)
      
      # Update all network types
      updateCheckboxGroupInput(
        session,
        "network_type_sweep",
        selected = c("Erdős-Rényi", "Watts-Strogatz", "Barabási-Albert")
      )
      
      # Include both static and dynamic networks
      updateCheckboxGroupInput(session,
                               "model_version_sweep",
                               selected = c("static", "dynamic"))
      
      # Network type specific parameters - Full test
      updateNumericInput(session, "p_er_min", value = 0.05)
      updateNumericInput(session, "p_er_max", value = 0.45)
      updateNumericInput(session, "p_er_step", value = 0.1)
      
      updateNumericInput(session, "k_ws_min", value = 2)
      updateNumericInput(session, "k_ws_max", value = 8)
      updateNumericInput(session, "k_ws_step", value = 2)
      
      updateNumericInput(session, "p_ws_min", value = 0.05)
      updateNumericInput(session, "p_ws_max", value = 0.45)
      updateNumericInput(session, "p_ws_step", value = 0.1)
      
      updateNumericInput(session, "m_ba_min", value = 1)
      updateNumericInput(session, "m_ba_max", value = 5)
      updateNumericInput(session, "m_ba_step", value = 1)
      
      # Type initialization
      updateCheckboxGroupInput(session,
                               "init_type_sweep",
                               selected = c("random", "polarized"))
      updateNumericInput(session, "b_min", value = 0.5)
      updateNumericInput(session, "b_max", value = 0.9)
      updateNumericInput(session, "b_step", value = 0.1)
      
    } else {
      ## ---- CUSTOM (restore your original defaults) -----------------
      updateNumericInput(session, "num_runs", value = 5)
      
      updateNumericInput(session, "n_min", value = 10)
      updateNumericInput(session, "n_max", value = 50)
      updateNumericInput(session, "n_step", value = 20)
      
      updateNumericInput(session, "l_min", value = 1)
      updateNumericInput(session, "l_max", value = 5)
      updateNumericInput(session, "l_step", value = 2)
      
      updateNumericInput(session, "t_min", value = 10)
      updateNumericInput(session, "t_max", value = 50)
      updateNumericInput(session, "t_step", value = 20)
      
      updateNumericInput(session, "delta_min", value = 0.0)
      updateNumericInput(session, "delta_max", value = 1.0)
      updateNumericInput(session, "delta_step", value = 0.5)
      
      updateNumericInput(session, "disclosure_pct_min", value = 0)
      updateNumericInput(session, "disclosure_pct_max", value = 100)
      updateNumericInput(session, "disclosure_pct_step", value = 50)
      
      # Update network types
      updateCheckboxGroupInput(session,
                               "network_type_sweep",
                               selected = c("Erdős-Rényi"))
      
      # Include both static and dynamic networks
      updateCheckboxGroupInput(session,
                               "model_version_sweep",
                               selected = c("static", "dynamic"))
      
      # Network type specific parameters - Custom defaults
      updateNumericInput(session, "p_er_min", value = 0.1)
      updateNumericInput(session, "p_er_max", value = 0.4)
      updateNumericInput(session, "p_er_step", value = 0.1)
      
      updateNumericInput(session, "k_ws_min", value = 2)
      updateNumericInput(session, "k_ws_max", value = 6)
      updateNumericInput(session, "k_ws_step", value = 2)
      
      updateNumericInput(session, "p_ws_min", value = 0.1)
      updateNumericInput(session, "p_ws_max", value = 0.4)
      updateNumericInput(session, "p_ws_step", value = 0.1)
      
      updateNumericInput(session, "m_ba_min", value = 1)
      updateNumericInput(session, "m_ba_max", value = 4)
      updateNumericInput(session, "m_ba_step", value = 1)
      
      # Type initialization
      updateCheckboxGroupInput(session, "init_type_sweep", selected = "random")
      updateNumericInput(session, "b_min", value = 0.7)
      updateNumericInput(session, "b_max", value = 0.7)
      updateNumericInput(session, "b_step", value = 0.1)
    }
  })
  
  # Violin plot visualization
  output$violin_plotly <- renderPlotly({
    req(values$sweep_results, input$violin_metrics)
    
    # Set theme with increased title margin
    theme_set(theme_minimal() +
                theme(plot.title = element_text(margin = margin(b = 20))))
    
    
    # Skip if no metrics selected
    if (length(input$violin_metrics) == 0) {
      return(NULL)
    }
    
    # Create mapping from technical names to display names
    metric_labels <- c(
      "final_perceived_neighbor_similarity" = "Perceived Neighbor Similarity",
      "final_perceived_all_similarity" = "Perceived Global Similarity",
      "final_perceived_similarity_gap" = "Perceived Similarity Gap",
      "final_objective_neighbor_similarity" = "Objective Neighbor Similarity",
      "final_objective_all_similarity" = "Objective Global Similarity",
      "final_objective_similarity_gap" = "Objective Similarity Gap",
      "final_revealed_neighbor_similarity" = "Revealed Neighbor Similarity",
      "final_revealed_all_similarity" = "Revealed Global Similarity",
      "final_revealed_similarity_gap" = "Revealed Similarity Gap",
      "final_variance" = "Variance",
      "final_bimodality" = "Bimodality",
      "final_clustering" = "Clustering Coefficient",
      "final_modularity" = "Modularity",
      "final_mean_welfare" = "Mean Welfare",
      "final_gini" = "Gini Coefficient"
    )
    
    # Better model version labels
    model_labels <- c("static" = "Static Network", "dynamic" = "Dynamic Network")
    
    # Get selected metrics
    selected_cols <- names(input$violin_metrics)
    
    # Handle case where names may be NULL (which can happen when using the values directly)
    if (is.null(selected_cols) || length(selected_cols) == 0) {
      # This means we need to use the values directly
      selected_cols <- input$violin_metrics
      print("No names found, using values directly")
    }
    
    # Filter and reshape data for plotting
    # First verify if all selected metrics exist in the data
    missing_metrics <- setdiff(selected_cols, colnames(values$sweep_results))
    if (length(missing_metrics) > 0) {
      print(paste(
        "WARNING: Missing metrics in data:",
        paste(missing_metrics, collapse = ", ")
      ))
      # Use only metrics that exist in the data
      available_metrics <- intersect(selected_cols, colnames(values$sweep_results))
      if (length(available_metrics) == 0) {
        # Return empty plot if no valid metrics
        return(ggplotly(
          ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = "No valid metrics found in data. Check column names."
            ) +
            theme_void()
        ))
      }
    } else {
      available_metrics <- selected_cols
    }
    
    plot_data <- values$sweep_results %>%
      select(model_version, all_of(available_metrics)) %>%
      pivot_longer(
        cols = all_of(available_metrics),
        names_to = "metric",
        values_to = "value"
      )
    
    # Check for NA values that could cause issues
    na_count <- sum(is.na(plot_data$value))
    if (na_count > 0) {
      # Remove NA values to prevent plotting errors
      plot_data <- plot_data %>% filter(!is.na(value))
    }
    
    # Add display names - make sure to handle potentially missing values in metric_labels
    plot_data <- plot_data %>%
      mutate(
        metric_label = factor(metric_labels[metric], levels = metric_labels[available_metrics]),
        model_version_label = factor(model_labels[model_version], levels = model_labels)
      )
    
    # Define a consistent color palette for network types - matching the trends plot colors
    # Using the same hues as in the trends plot (blues for perceived, reds for revealed)
    network_colors <- c(
      "Static Network" = "#0077BB",
      # Darker blue - same as "Revealed Neighbor Similarity"
      "Dynamic Network" = "#CC3311"  # Darker red - same as "Perceived Neighbor Similarity"
    )
    
    
    # Check if we have data to plot
    if (nrow(plot_data) == 0) {
      return(ggplotly(
        ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = "No data available for selected metrics"
          ) +
          theme_void()
      ) %>%
        layout(font = list(family = "Roboto")))
    }
    
    # Check if model_version_label is missing or has invalid values
    if (!"model_version_label" %in% names(plot_data) ||
        all(is.na(plot_data$model_version_label))) {
      # If model_version is missing, create a simplified plot without it
      p <- ggplot(plot_data, aes(x = 1, y = value)) +
        geom_violin(alpha = 0.7,
                    trim = FALSE,
                    fill = "#4E79A7") +
        geom_boxplot(
          width = 0.1,
          fill = "white",
          alpha = 0.5,
          position = position_identity()
        ) +
        facet_wrap( ~ metric_label, scales = "free_y", ncol = 1) +
        labs(x = NULL, y = "Value", title = "Comparison of Outcome Measures") +
        theme_minimal() +
        theme(
          strip.background = element_rect(fill = "#EBF5FB", color = NA),
          strip.text = element_text(face = "bold", size = 12, color = "black"),
          axis.title = element_text(color = "black"),
          axis.text.x = element_blank(),
          # Hide x-axis labels
          axis.ticks.x = element_blank(),
          # Hide x-axis ticks
          panel.spacing = unit(1.5, "lines"),
          plot.title = element_text(hjust = 0.5, family = "Roboto", color = "black")
        )
    } else {
      # Normal plot with network type - fixed boxplot positioning
      p <- ggplot(plot_data,
                  aes(x = model_version_label, y = value, fill = model_version_label)) +
        geom_violin(alpha = 0.7, trim = FALSE) +
        # Use position = "identity" to center boxplots within violins
        geom_boxplot(
          width = 0.1,
          fill = "white",
          alpha = 0.5,
          position = position_identity()
        ) +
        facet_wrap( ~ metric_label, scales = "free_y", ncol = 1) +
        scale_fill_manual(values = network_colors) +
        labs(
          x = NULL,
          y = "Value",
          title = "Comparison of Outcome Measures by Network Type",
          fill = "Network Type"
        ) +
        theme_minimal() +
        theme(
          strip.background = element_rect(fill = "#EBF5FB", color = NA),
          strip.text = element_text(face = "bold", size = 12, color = "black"),
          axis.title = element_text(color = "black"),
          legend.position = "right",
          panel.spacing = unit(1.5, "lines"),
          plot.title = element_text(hjust = 0.5, family = "Roboto", color = "black")
        )
    }
    
    # Use tryCatch to handle potential errors during plotly conversion
    result <- tryCatch({
      # Convert to plotly for interactivity, using simple tooltips to avoid errors
      ggplotly(p, tooltip = "y") %>%
        layout(
          boxmode = "group",
          height = 250 * length(available_metrics),
          font = list(family = "Roboto"),
          margin = list(t = 80, b = 100, r = 120),
          legend = list(
            y = 0.98,
            x = 1.05,
            xanchor = "left",
            yanchor = "top",
            font = list(family = "Roboto", size = 12),
            borderwidth = 0,
            bgcolor = "rgba(255, 255, 255, 0.7)"
          ),
          title = list(
            y = 1,
            # Position title at top
            pad = list(t = 25, b = 25)  # Add padding above and below title
          )
        )
    }, error = function(e) {
      # If there's an error, return a simple error message plot
      print(paste("Error in ggplotly conversion:", e$message))
      ggplotly(ggplot() +
                 annotate(
                   "text",
                   x = 0.5,
                   y = 0.5,
                   label = paste("Error creating plot:", e$message)
                 ) +
                 theme_void()) %>%
        layout(font = list(family = "Roboto"))
    })
    
    return(result)
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
  
  # ---- trend plot  -----------------------------------------------------------
  output$trend_plotly <- renderPlotly({
    req(values$sweep_results, input$trend_group)
    
    # Separate normal metrics from gap metrics
    normal_metrics <- c(
      "final_perceived_neighbor_similarity",
      "final_perceived_all_similarity",
      "final_objective_neighbor_similarity",
      "final_objective_all_similarity",
      "final_revealed_neighbor_similarity",
      "final_revealed_all_similarity"
    )
    
    gap_metrics <- c(
      "final_perceived_similarity_gap",
      "final_objective_similarity_gap",
      "final_revealed_similarity_gap"
    )
    
    # Combined for backwards compatibility
    similarity_metrics <- c(normal_metrics, gap_metrics)
    
    # Get x-axis parameter information from inputs
    x_param <- input$trend_x
    x_min <- switch(
      x_param,
      "N" = input$n_min,
      "L" = input$l_min,
      "T" = input$t_min,
      "s" = input$disclosure_pct_min,
      "delta" = input$delta_min
    )
    x_max <- switch(
      x_param,
      "N" = input$n_max,
      "L" = input$l_max,
      "T" = input$t_max,
      "s" = input$disclosure_pct_max,
      "delta" = input$delta_max
    )
    x_step <- switch(
      x_param,
      "N" = input$n_step,
      "L" = input$l_step,
      "T" = input$t_step,
      "s" = input$disclosure_pct_step,
      "delta" = input$delta_step
    )
    
    # Create sequence for x-axis breaks
    x_breaks <- seq(x_min, x_max, by = x_step)
    
    # Create a mapping of metrics to readable labels
    metric_labels <- c(
      "final_perceived_neighbor_similarity" = "Perceived Neighbor Similarity",
      "final_perceived_all_similarity" = "Perceived Global Similarity",
      "final_perceived_similarity_gap" = "Perceived Similarity Gap",
      "final_objective_neighbor_similarity" = "Objective Neighbor Similarity",
      "final_objective_all_similarity" = "Objective Global Similarity",
      "final_objective_similarity_gap" = "Objective Similarity Gap",
      "final_revealed_neighbor_similarity" = "Revealed Neighbor Similarity",
      "final_revealed_all_similarity" = "Revealed Global Similarity",
      "final_revealed_similarity_gap" = "Revealed Similarity Gap",
      "final_variance" = "Variance",
      "final_bimodality" = "Bimodality",
      "final_clustering" = "Clustering Coefficient",
      "final_modularity" = "Modularity",
      "final_mean_welfare" = "Mean Welfare",
      "final_gini" = "Gini Coefficient"
    )
    
    # Map UI parameter name to data column name
    data_param <- switch(
      x_param,
      "N" = "N",
      "L" = "L",
      "T" = "T",
      "s" = ifelse(
        "disclosure_pct" %in% names(values$sweep_results),
        "disclosure_pct",
        # Use disclosure_pct if it exists
        ifelse(
          "s" %in% names(values$sweep_results),
          "s",
          # Otherwise use s as fallback
          NA
        )
      ),
      # NA if neither exists
      "delta" = "delta",
      NA
    )
    
    # Check if parameter exists in data
    if (is.na(data_param) ||
        !data_param %in% names(values$sweep_results)) {
      # Create a safe plot with message
      p <- ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = paste("Parameter", x_param, "not found in sweep results data.")
        ) +
        theme_void()
      return(ggplotly(p))
    }
    
    # Continue with filtering and plotting
    df <- values$sweep_results %>%
      pivot_longer(
        cols      = starts_with("final_"),
        names_to  = "metric",
        values_to = "value"
      ) %>%
      filter(
        (
          input$trend_group == "similarity" &
            metric %in% similarity_metrics
        ) |
          (
            input$trend_group == "other" & !(metric %in% similarity_metrics)
          )
      )
    
    # Check if we can safely filter by parameter
    if (data_param %in% names(df)) {
      # If using disclosure_pct or s, try to be more flexible with matching
      if ((data_param == "disclosure_pct" ||
           data_param == "s") &&
          length(df[[data_param]][df[[data_param]] %in% x_breaks]) == 0) {
        # Try to find closest values
        all_values <- sort(unique(df[[data_param]]))
        
        # Calculate actual values in the data that are closest to our breaks
        closest_values <- sapply(x_breaks, function(x) {
          all_values[which.min(abs(all_values - x))]
        })
        
        # Filter to these closest values
        df <- df %>%
          filter(.data[[data_param]] %in% closest_values) %>%
          # Add readable metric labels
          mutate(
            metric_label = factor(metric_labels[metric], levels = metric_labels),
            x = .data[[data_param]] # Ensure x is properly set for grouping
          )
      } else {
        # Normal exact matching
        df <- df %>%
          filter(.data[[data_param]] %in% x_breaks) %>%
          # Add readable metric labels
          mutate(
            metric_label = factor(metric_labels[metric], levels = metric_labels),
            x = .data[[data_param]] # Ensure x is properly set for grouping
          )
      }
    } else {
      df <- df %>% mutate(
        metric_label = factor(metric_labels[metric], levels = metric_labels),
        x = 1 # Fallback value for grouping
      )
    }
    
    # Make sure there's at least one model_version value
    if (!"model_version" %in% names(df) ||
        length(unique(df$model_version)) == 0) {
      df$model_version <- "static"
    }
    
    # Summarize data if we have valid records
    if (nrow(df) > 0) {
      summary_df <- df %>%
        group_by(x, model_version, metric, metric_label) %>%
        summarise(mean = mean(value), .groups = "drop")
    } else {
      # Create empty summary with same structure
      summary_df <- data.frame(
        x = numeric(0),
        model_version = character(0),
        metric = character(0),
        metric_label = character(0),
        mean = numeric(0)
      )
    }
    
    # Improved x-axis label
    x_axis_label <- switch(
      x_param,
      "N" = "Number of Agents (N)",
      "L" = "Type Vector Length (L)",
      "T" = "Number of Rounds (T)",
      "s" = "Disclosure Size (%)",
      "delta" = "Influence Decay Factor (δ)",
      x_param
    )
    
    # For non-similarity metrics, create a 2x2 grid similar to similarity metrics
    if (input$trend_group != "similarity") {
      # Create a message plot if no data
      if (nrow(summary_df) == 0) {
        p <- ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = paste("No data available for", x_axis_label)
          ) +
          theme_void() +
          theme(text = element_text(family = "Roboto"))
        return(ggplotly(p))
      }
      
      # Split data for static and dynamic networks
      static_data <- summary_df %>% filter(model_version == "static")
      dynamic_data <- summary_df %>% filter(model_version == "dynamic")
      
      # Define colors for each metric - use consistent color scheme
      metric_count <- length(unique(summary_df$metric_label))
      metric_colors <- colorRampPalette(c("#0077BB", "#CC3311", "#555555", 
                                       "#BBDEFB", "#FFAEA5", "#CCCCCC", 
                                       "#33BBEE", "#EE7733", "#999999"))(metric_count)
      names(metric_colors) <- unique(summary_df$metric_label)
      
      # Define line types
      line_types <- rep("solid", length(metric_colors))
      names(line_types) <- names(metric_colors)
      
      # A list to track which metrics have been added to the legend
      added_to_legend <- list()
      
      # Create Static Network plot
      static_plot <- plotly::plot_ly() %>%
        plotly::layout(
          title = "Static Network",
          xaxis = list(title = x_axis_label),
          yaxis = list(
            title = "Mean Value",
            range = c(regular_min, regular_max)
          )
        )
      
      # Create Dynamic Network plot  
      dynamic_plot <- plotly::plot_ly() %>%
        plotly::layout(
          title = "Dynamic Network",
          xaxis = list(title = x_axis_label),
          yaxis = list(
            title = "",  # Remove title
            showticklabels = FALSE,  # Hide tick labels
            range = c(regular_min, regular_max)  # Keep range for consistency
          )
        )
      
      # Add traces for static networks
      if (nrow(static_data) > 0) {
        for (m in unique(static_data$metric_label)) {
          df <- static_data %>% filter(metric_label == m)
          if (nrow(df) > 0) {
            # First time this metric appears, show in legend
            showInLegend <- !m %in% names(added_to_legend)
            if (showInLegend)
              added_to_legend[[m]] <- TRUE
            
            static_plot <- static_plot %>%
              plotly::add_trace(
                data = df,
                x = ~ x,
                y = ~ mean,
                type = 'scatter',
                mode = 'lines',
                name = m,
                legendgroup = m,
                line = list(
                  width = 2,
                  color = metric_colors[m]
                ),
                showlegend = showInLegend
              )
          }
        }
      }
      
      # Add traces for dynamic networks
      if (nrow(dynamic_data) > 0) {
        for (m in unique(dynamic_data$metric_label)) {
          df <- dynamic_data %>% filter(metric_label == m)
          if (nrow(df) > 0) {
            # Only show legend if we haven't seen this metric before
            showInLegend <- !m %in% names(added_to_legend)
            if (showInLegend)
              added_to_legend[[m]] <- TRUE
            
            dynamic_plot <- dynamic_plot %>%
              plotly::add_trace(
                data = df,
                x = ~ x,
                y = ~ mean,
                type = 'scatter',
                mode = 'lines',
                name = m,
                legendgroup = m,
                line = list(
                  width = 2,
                  color = metric_colors[m]
                ),
                showlegend = showInLegend
              )
          }
        }
      }
      
      # Create placeholder plots to maintain 2x2 grid
      empty_plot <- plotly::plot_ly() %>%
        plotly::layout(
          showlegend = FALSE,
          xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
          yaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE)
        )
      
      # Combine the plots into a 2x2 grid like similarity metrics
      return(
        plotly::subplot(
          static_plot,
          dynamic_plot,
          empty_plot,
          empty_plot,
          nrows = 2,
          shareX = TRUE,
          shareY = TRUE,  # Share y-axis scale between plots in the same row
          margin = 0.02,  # Reduce margin between subplots
          titleX = TRUE,
          titleY = TRUE
        ) %>% plotly::layout(
          font = list(family = "Roboto"),
          height = 900,
          showlegend = TRUE,
          uniformtext = list(minsize = 10, mode = 'show'),
          margin = list(t = 100, b = 100, r = 120),
          title = list(
            text = paste("Parameter Trends by", x_axis_label),
            y = 0.98
          ),
          legend = list(
            y = 0.98,
            x = 1.05,
            xanchor = "left",
            yanchor = "top",
            font = list(family = "Roboto", size = 12),
            borderwidth = 0,
            bgcolor = "rgba(255, 255, 255, 0.7)",
            tracegroupgap = 5
          ),
          annotations = list(
            # Column titles
            list(
              text = "Static Network",
              x = 0.15,
              y = 1.05,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 16, family = "Roboto", color = "black")
            ),
            list(
              text = "Dynamic Network",
              x = 0.85,
              y = 1.05,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              font = list(size = 16, family = "Roboto", color = "black")
            )
          )
        )
      )
    }
    
    # For similarity metrics, create a 2x2 grid
    # This is a clean implementation based on simple_trends_solution.R
    
    # Filter data for each subplot
    static_regular_data <- summary_df %>%
      filter(model_version == "static" & metric %in% normal_metrics)
    
    dynamic_regular_data <- summary_df %>%
      filter(model_version == "dynamic" &
               metric %in% normal_metrics)
    
    static_gap_data <- summary_df %>%
      filter(model_version == "static" & metric %in% gap_metrics)
    
    dynamic_gap_data <- summary_df %>%
      filter(model_version == "dynamic" & metric %in% gap_metrics)
    
    # Calculate consistent y-axis ranges for regular metrics
    regular_min <- min(c(
      min(static_regular_data$mean, na.rm = TRUE),
      min(dynamic_regular_data$mean, na.rm = TRUE)
    ), na.rm = TRUE)
    
    regular_max <- max(c(
      max(static_regular_data$mean, na.rm = TRUE),
      max(dynamic_regular_data$mean, na.rm = TRUE)
    ), na.rm = TRUE)
    
    # Calculate consistent y-axis ranges for gap metrics
    gap_min <- min(c(
      min(static_gap_data$mean, na.rm = TRUE),
      min(dynamic_gap_data$mean, na.rm = TRUE)
    ), na.rm = TRUE)
    
    gap_max <- max(c(
      max(static_gap_data$mean, na.rm = TRUE),
      max(dynamic_gap_data$mean, na.rm = TRUE)
    ), na.rm = TRUE)
    
    # Create a list to track which metrics have been added to the legend
    added_to_legend <- list()
    
    # Initialize empty plot with NULL if no data
    static_regular_plot <- NULL
    dynamic_regular_plot <- NULL
    static_gap_plot <- NULL
    dynamic_gap_plot <- NULL
    
    # Define custom colors for each metric type
    perceived_colors <- c(
      "Perceived Neighbor Similarity" = "#CC3311",      # Darker red
      "Perceived Global Similarity" = "#FFAEA5",        # Lighter red
      "Perceived Similarity Gap" = "#EE7733"            # Medium red/orange
    )
    
    revealed_colors <- c(
      "Revealed Neighbor Similarity" = "#0077BB",       # Darker blue
      "Revealed Global Similarity" = "#BBDEFB",         # Lighter blue
      "Revealed Similarity Gap" = "#33BBEE"             # Medium blue
    )
    
    objective_colors <- c(
      "Objective Neighbor Similarity" = "#555555",      # Darker gray
      "Objective Global Similarity" = "#CCCCCC",        # Lighter gray
      "Objective Similarity Gap" = "#999999"            # Medium gray
    )
    
    # Combine all color definitions
    metric_colors <- c(perceived_colors, revealed_colors, objective_colors)
    
    # Define line types for each metric
    line_types <- rep("solid", length(metric_colors))
    names(line_types) <- names(metric_colors)
    
    # Make gap metrics dotted
    gap_metrics_names <- c("Perceived Similarity Gap", "Revealed Similarity Gap", "Objective Similarity Gap")
    line_types[gap_metrics_names] <- "dash"
    
    # Modify static_regular_plot to set showlegend properly
    if (nrow(static_regular_data) > 0) {
      static_regular_plot <- plotly::plot_ly() %>%
        plotly::layout(
          title = "Static Network - Regular Metrics",
          xaxis = list(title = x_axis_label),
          yaxis = list(
            title = "Mean Value",
            range = c(regular_min, regular_max)  # Consistent y-axis range
          )
        )
      
      for (m in unique(static_regular_data$metric_label)) {
        df <- static_regular_data %>% filter(metric_label == m)
        if (nrow(df) > 0) {
          # First time this metric appears, show in legend
          showInLegend <- !m %in% names(added_to_legend)
          if (showInLegend)
            added_to_legend[[m]] <- TRUE
          
          static_regular_plot <- static_regular_plot %>%
            plotly::add_trace(
              data = df,
              x = ~ x,
              y = ~ mean,
              type = 'scatter',
              mode = 'lines',
              name = m,
              legendgroup = m,
              line = list(
                width = 2,
                color = metric_colors[m]
              ),
              showlegend = showInLegend
            )
        }
      }
    }
    
    # Modify dynamic_regular_plot to set showlegend properly
    if (nrow(dynamic_regular_data) > 0) {
      dynamic_regular_plot <- plotly::plot_ly() %>%
        plotly::layout(
          title = "Dynamic Network - Regular Metrics",
          xaxis = list(title = x_axis_label),
          yaxis = list(
            title = "",  # Remove title
            showticklabels = FALSE,  # Hide tick labels
            range = c(regular_min, regular_max)  # Keep consistent range
          )
        )
      
      for (m in unique(dynamic_regular_data$metric_label)) {
        df <- dynamic_regular_data %>% filter(metric_label == m)
        if (nrow(df) > 0) {
          # Only show legend if we haven't seen this metric before
          showInLegend <- !m %in% names(added_to_legend)
          if (showInLegend)
            added_to_legend[[m]] <- TRUE
          
          dynamic_regular_plot <- dynamic_regular_plot %>%
            plotly::add_trace(
              data = df,
              x = ~ x,
              y = ~ mean,
              type = 'scatter',
              mode = 'lines',
              name = m,
              legendgroup = m,
              line = list(
                width = 2,
                color = metric_colors[m]
              ),
              showlegend = showInLegend
            )
        }
      }
    }
    
    # Modify static_gap_plot to set showlegend properly
    if (nrow(static_gap_data) > 0) {
      static_gap_plot <- plotly::plot_ly() %>%
        plotly::layout(
          title = "Static Network - Gap Metrics",
          xaxis = list(title = x_axis_label),
          yaxis = list(
            title = "Mean Value",
            range = c(gap_min, gap_max)  # Consistent y-axis range
          )
        )
      
      for (m in unique(static_gap_data$metric_label)) {
        df <- static_gap_data %>% filter(metric_label == m)
        if (nrow(df) > 0) {
          # Only show legend if we haven't seen this metric before
          showInLegend <- !m %in% names(added_to_legend)
          if (showInLegend)
            added_to_legend[[m]] <- TRUE
          
          static_gap_plot <- static_gap_plot %>%
            plotly::add_trace(
              data = df,
              x = ~ x,
              y = ~ mean,
              type = 'scatter',
              mode = 'lines',
              name = m,
              legendgroup = m,
              line = list(
                width = 2, 
                dash = "dash",
                color = metric_colors[m]
              ),
              showlegend = showInLegend
            )
        }
      }
    }
    
    # Modify dynamic_gap_plot to set showlegend properly
    if (nrow(dynamic_gap_data) > 0) {
      dynamic_gap_plot <- plotly::plot_ly() %>%
        plotly::layout(
          title = "Dynamic Network - Gap Metrics",
          xaxis = list(title = x_axis_label),
          yaxis = list(
            title = "",  # Remove title
            showticklabels = FALSE,  # Hide tick labels
            range = c(gap_min, gap_max)  # Consistent y-axis range
          )
        )
      
      for (m in unique(dynamic_gap_data$metric_label)) {
        df <- dynamic_gap_data %>% filter(metric_label == m)
        if (nrow(df) > 0) {
          # Only show legend if we haven't seen this metric before
          showInLegend <- !m %in% names(added_to_legend)
          if (showInLegend)
            added_to_legend[[m]] <- TRUE
          
          dynamic_gap_plot <- dynamic_gap_plot %>%
            plotly::add_trace(
              data = df,
              x = ~ x,
              y = ~ mean,
              type = 'scatter',
              mode = 'lines',
              name = m,
              legendgroup = m,
              line = list(
                width = 2, 
                dash = "dash",
                color = metric_colors[m]
              ),
              showlegend = showInLegend
            )
        }
      }
    }
    
    # Combine the four plots into a 2x2 grid
    return(
      plotly::subplot(
        static_regular_plot,
        dynamic_regular_plot,
        static_gap_plot,
        dynamic_gap_plot,
        nrows = 2,
        shareX = TRUE,
        shareY = TRUE,  # Share y-axis scale between plots in the same row
        margin = 0.02,  # Reduce margin between subplots
        titleX = TRUE,
        titleY = TRUE
      ) %>% plotly::layout(
        font = list(family = "Roboto"),
        height = 900,
        showlegend = TRUE,
        margin = list(t = 100, b = 100, r = 120),  # Add margins
        title = list(
          text = paste("Parameter Trends by", x_axis_label),
          y = 0.98
        ),
        legend = list(
          y = 0.98,
          x = 1.05,
          xanchor = "left",
          yanchor = "top",
          font = list(family = "Roboto", size = 12),
          borderwidth = 0,
          bgcolor = "rgba(255, 255, 255, 0.7)",
          tracegroupgap = 5
        ),
        # Add column titles as annotations
        annotations = list(
          # Column titles
          list(
            text = "Static Network",
            x = 0.15,  # Position for left column
            y = 1.05,  # Position above the plots
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, family = "Roboto", color = "black")
          ),
          list(
            text = "Dynamic Network",
            x = 0.85,  # Position for right column
            y = 1.05,  # Position above the plots
            xref = "paper",
            yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, family = "Roboto", color = "black")
          )
        )
      )
    )
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