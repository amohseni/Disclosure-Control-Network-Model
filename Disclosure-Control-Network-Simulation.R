###############################################################
# Simulation Model: Selective Disclosure & Perceptions of Polarization
###############################################################

# Load required libraries
library(igraph)
library(dplyr)
library(ggplot2)
library(tidyr)
library(visNetwork)
library(tibble)

###############################################################
# PART 1: NETWORK GENERATION AND INITIALIZATION
###############################################################

#' Generate a network based on specified model
#'
#' @param model_type Type of network model:
#' "ER" (Erdős-Rényi),
#' "WS" (Watts-Strogatz),
#' "BA" (Barabási-Albert)
#' @param N Number of nodes
#' @param p Probability of edge formation (for ER)
#' or rewiring probability (for WS)
#' @param k Initial neighbor count (for WS)
#' or edges per new node (for BA)
#' @return An igraph object representing the network
generate_network <- function(model_type, N, p = 0.1, k = 4) {
  if (model_type == "ER") {
    # Erdős-Rényi random graph
    graph <- sample_gnp(
      n = N,
      p = p,
      directed = FALSE,
      loops = FALSE
    )
  } else if (model_type == "WS") {
    # Watts-Strogatz small-world graph
    graph <- sample_smallworld(
      dim = 1,
      size = N,
      nei = k,
      p = p,
      loops = FALSE,
      multiple = FALSE
    )
  } else if (model_type == "BA") {
    # Barabási-Albert scale-free graph
    graph <- sample_pa(
      n = N,
      power = 1,
      m = k,
      directed = FALSE,
      loops = FALSE
    )
  } else {
    stop("Invalid network model type. Choose 'ER', 'WS', or 'BA'.")
  }
  
  # Ensure the graph is connected
  components <- components(graph)
  if (components$no > 1) {
    # If not connected, take the largest component and add some edges to connect others
    largest_comp <- which.max(components$csize)
    for (i in 1:N) {
      if (components$membership[i] != largest_comp) {
        # Connect to a random node in the largest component
        target <- sample(which(components$membership == largest_comp), 1)
        graph <- add_edges(graph, c(i, target))
      }
    }
  }
  
  return(graph)
}

#' Initialize agent type vectors
#'
#' @param N Number of agents
#' @param L Length of type vector
#' @param init_type "random" or "polarized"
#' @param b Bias parameter for polarized initialization
#' @return A matrix of agent type vectors
initialize_types <- function(N, L, init_type = "random", b = 0.7) {
  type_matrix <- matrix(0, nrow = N, ncol = L)
  
  if (init_type == "random") {
    # Random initialization: Each trait is 0 or 1 with equal probability
    type_matrix <- matrix(sample(c(0, 1), N * L, replace = TRUE),
                          nrow = N,
                          ncol = L)
  } else if (init_type == "polarized") {
    # Polarized initialization: Agents are assigned to two groups with opposing biases
    group1 <- sample(c(TRUE, FALSE), N, replace = TRUE)  # Randomly assign to groups
    
    # Set traits for group 1
    for (i in which(group1)) {
      type_matrix[i, ] <- rbinom(L, 1, b)  # Bias towards 1
    }
    
    # Set traits for group 2
    for (i in which(!group1)) {
      type_matrix[i, ] <- rbinom(L, 1, 1 - b)  # Bias towards 0
    }
  } else {
    stop("Invalid initialization type. Choose 'random' or 'polarized'.")
  }
  
  return(type_matrix)
}

#' Initialize belief matrices for all agents
#'
#' @param N Number of agents
#' @param L Length of type vector
#' @param type_matrix Matrix of agent type vectors
#' @return A 3D array of belief matrices (N x N x L)
initialize_beliefs <- function(N, L, type_matrix) {
  # Initialize belief matrices with ignorance prior (0.5) for all unknown traits
  belief_array <- array(0.5, dim = c(N, N, L))
  
  # The first trait is always public information
  for (i in 1:N) {
    for (j in 1:N) {
      if (i != j) {
        belief_array[i, j, 1] <- type_matrix[j, 1]
      }
    }
  }
  
  # Agents know their own type vectors
  for (i in 1:N) {
    belief_array[i, i, ] <- type_matrix[i, ]
  }
  
  return(belief_array)
}

###############################################################
# PART 2: UTILITY CALCULATIONS
###############################################################

#' Calculate the shortest path lengths between all nodes in the network
#'
#' @param graph An igraph object
#' @return A matrix of shortest path lengths
calculate_shortest_paths <- function(graph) {
  N <- vcount(graph)
  distances <- distances(graph, algorithm = "dijkstra")
  return(distances)
}

#' Compute the expected similarity between two agents from evaluator's perspective
#'
#' @param evaluator Index of the agent who is evaluating (i)
#' @param evaluated Index of the agent being evaluated (j)
#' @param beliefs 3D array of beliefs (N x N x L)
#' @param types Matrix of true type vectors (N x L)
#' @param L Length of the type vector
#' @return Expected similarity value (between 0 and 1)
compute_expected_similarity <- function(evaluator, evaluated, beliefs, types, L) {
  evaluator_type <- types[evaluator, ]
  belief_about_evaluated <- beliefs[evaluator, evaluated, ]
  
  # Expected similarity is just the average over the traits of:
  # 1 if the evaluator knows the trait, otherwise expected similarity = 1 - |type_i - belief|
  similarities <- 1 - abs(evaluator_type - belief_about_evaluated)
  return(mean(similarities))
}

#' Calculate utility for a single agent
#'
#' @param agent_id Index of the agent
#' @param distances Matrix of shortest path lengths
#' @param beliefs Belief array for all agents
#' @param types Type matrix for all agents
#' @param L Length of type vector
#' @param delta Influence decay factor
#' @return Utility value for the agent
calculate_utility <- function(agent_id,
                              distances,
                              beliefs,
                              types,
                              L,
                              delta) {
  N <- nrow(distances)
  utility <- 0
  
  for (j in 1:N) {
    if (j != agent_id) {
      # Calculate path length between agent_id and j
      k_ij <- distances[agent_id, j]
      
      # Calculate influence weight based on network distance
      w_ij <- delta^(k_ij - 1)
      
      # Compute j's evaluation of agent_id
      exp_sim <- compute_expected_similarity(j, agent_id, beliefs, types, L)
      
      # Add weighted similarity to total utility
      utility <- utility + w_ij * exp_sim
    }
  }
  
  return(utility)
}

#' Calculate utilities for all agents
#'
#' @param distances Matrix of shortest path lengths
#' @param beliefs Belief array for all agents
#' @param types Type matrix for all agents
#' @param N Number of agents
#' @param L Length of type vector
#' @param delta Influence decay factor
#' @return Vector of utility values for all agents
calculate_all_utilities <- function(distances, beliefs, types, N, L, delta) {
  utilities <- numeric(N)
  
  for (i in 1:N) {
    utilities[i] <- calculate_utility(i, distances, beliefs, types, L, delta)
  }
  
  return(utilities)
}

###############################################################
# PART 3: BEST RESPONSE FUNCTIONS
###############################################################

#' Determine the best response for a given agent
#'
#' @param agent_id Index of the agent
#' @param target_agents Indices of target agents for disclosure
#' @param distances Matrix of shortest path lengths
#' @param beliefs Belief array
#' @param types Type matrix
#' @param L Length of type vector
#' @param delta Influence decay factor
#' @return The best action for the agent
decide_best_action <- function(agent_id,
                               target_agents,
                               distances,
                               beliefs,
                               types,
                               L,
                               delta) {
  # Calculate current utility without additional revelations
  current_utility <- calculate_utility(agent_id, distances, beliefs, types, L, delta)
  
  # Skip disclosure evaluation if only one trait exists (already public)
  if (L < 2) {
    return(list(action = "reveal nothing", utility = current_utility))
  }
  
  best_action <- "reveal nothing"
  best_utility <- current_utility
  best_actions <- c(best_action)  # Track all actions tied for best
  
  # Consider revealing each trait (trait 1 is already public)
  for (l in 2:L) {
    # Skip traits already known to all target agents
    all_known <- TRUE
    for (j in target_agents) {
      if (beliefs[j, agent_id, l] != types[agent_id, l]) {
        all_known <- FALSE
        break
      }
    }
    
    if (all_known)
      next
    
    # Simulate belief update if agent reveals trait l
    new_beliefs <- beliefs
    for (j in target_agents) {
      new_beliefs[j, agent_id, l] <- types[agent_id, l]
    }
    
    # Calculate expected utility after revelation
    new_utility <- calculate_utility(agent_id, distances, new_beliefs, types, L, delta)
    
    # Update best action if this is better
    if (new_utility > best_utility) {
      best_utility <- new_utility
      best_actions <- c(paste("reveal trait", l))
    } else if (new_utility == best_utility) {
      # If tied with current best, add to list
      best_actions <- c(best_actions, paste("reveal trait", l))
    }
  }
  
  # If multiple actions tied for best, select one randomly
  if (length(best_actions) > 1) {
    best_action <- sample(best_actions, 1)
  } else {
    best_action <- best_actions[1]
  }
  
  return(list(action = best_action, utility = best_utility))
}

#' Extract trait number from action string
#'
#' @param action Action string
#' @return Trait number or NA if action is "reveal nothing"
extract_trait_number <- function(action) {
  if (action == "reveal nothing") {
    return(NA)
  } else {
    return(as.numeric(strsplit(action, "reveal trait ")[[1]][2]))
  }
}

###############################################################
# PART 4: NETWORK DYNAMICS AND REWIRING
###############################################################

#' Rewire network connections based on perceived similarity
#'
#' @param graph The network graph
#' @param agent_id Index of the agent making decisions
#' @param beliefs Belief array
#' @param types Type matrix
#' @param L Length of type vector
#' @return Updated network graph
rewire_connections <- function(graph, agent_id, beliefs, types, L) {
  # Get current neighbors
  neighbors <- neighbors(graph, agent_id)
  
  # If no neighbors, nothing to rewire
  if (length(neighbors) == 0) {
    return(graph)
  }
  
  # Calculate similarity with each neighbor
  neighbor_similarities <- sapply(neighbors, function(j) {
    compute_expected_similarity(agent_id, j, beliefs, types, L)
  })
  
  # Find worst neighbor
  worst_neighbor_idx <- which.min(neighbor_similarities)
  worst_neighbor <- neighbors[worst_neighbor_idx]
  
  # Get non-neighbors
  all_nodes <- 1:vcount(graph)
  non_neighbors <- setdiff(all_nodes, c(agent_id, neighbors))
  
  # If no non-neighbors available, return unchanged graph
  if (length(non_neighbors) == 0) {
    return(graph)
  }
  
  # Calculate similarity with each non-neighbor
  non_neighbor_similarities <- sapply(non_neighbors, function(j) {
    compute_expected_similarity(agent_id, j, beliefs, types, L)
  })
  
  # Find best non-neighbor
  best_non_neighbor_idx <- which.max(non_neighbor_similarities)
  best_non_neighbor <- non_neighbors[best_non_neighbor_idx]
  
  # Only rewire if the best non-neighbor has higher similarity than the worst neighbor
  if (non_neighbor_similarities[best_non_neighbor_idx] > neighbor_similarities[worst_neighbor_idx]) {
    # Delete edge to worst neighbor
    graph <- delete_edges(graph, E(graph)[agent_id %--% worst_neighbor])
    
    # Add edge to best non-neighbor
    graph <- add_edges(graph, c(agent_id, best_non_neighbor))
  }
  
  return(graph)
}

###############################################################
# PART 5: OUTCOME CALCULATIONS
###############################################################

#' Calculate similarity-based outcome measures
#'
#' @param graph The network graph
#' @param similarity_matrices List of matrices of similarity values
#' @param N Number of agents
#' @return List of similarity outcome measures
compute_similarity_outcomes <- function(graph, beliefs, types, N, L) {
  # Initialize matrices
  similarity_matrices <- list(
    perceived = matrix(0, N, N),
    objective = matrix(0, N, N),
    revealed = matrix(0, N, N)
  )
  
  # Populate matrices
  for (i in 1:N) {
    for (j in 1:N) {
      if (i != j) {
        ti <- types[i, ]
        tj <- types[j, ]
        bij <- beliefs[i, j, ]
        
        # Objective similarity
        similarity_matrices$objective[i, j] <- mean(ti == tj)
        
        # Revealed similarity
        match_vec <- ti == tj
        known_vec <- bij == tj
        similarity_matrices$revealed[i, j] <- mean(match_vec &
                                                     known_vec)
        
        # Perceived similarity
        sim_vec <- ifelse(bij == 0 |
                            bij == 1, as.numeric(ti == bij), 1 - abs(ti - bij))
        similarity_matrices$perceived[i, j] <- mean(sim_vec)
      }
    }
  }
  
  # Helper to compute neighbor and global averages
  compute_averages <- function(mat) {
    neighbor_avg <- mean(sapply(1:N, function(i) {
      ni <- as.integer(neighbors(graph, i))   # convert to numeric
      if (length(ni) > 0)
        mean(mat[i, ni])
      else
        NA
    }), na.rm = TRUE)
    global_avg <- mean(mat[lower.tri(mat) | upper.tri(mat)])
    list(neighbor = neighbor_avg,
         global = global_avg,
         gap = neighbor_avg - global_avg)
  }
  
  # Compute all
  perceived <- compute_averages(similarity_matrices$perceived)
  objective <- compute_averages(similarity_matrices$objective)
  revealed  <- compute_averages(similarity_matrices$revealed)
  
  return(
    list(
      perceived_neighbor_similarity = perceived$neighbor,
      perceived_all_similarity = perceived$global,
      perceived_similarity_gap = perceived$gap,
      objective_neighbor_similarity = objective$neighbor,
      objective_all_similarity = objective$global,
      objective_similarity_gap = objective$gap,
      revealed_neighbor_similarity = revealed$neighbor,
      revealed_all_similarity = revealed$global,
      revealed_similarity_gap = revealed$gap,
      similarity_matrices = similarity_matrices
    )
  )
}

#' Calculate polarization metrics
#'
#' @param perceived_similarity_matrix Matrix of similarity values
#' @return List of polarization metrics
calculate_polarization_metrics <- function(perceived_similarity_matrix) {
  # Flatten the similarity matrix (excluding self-similarities)
  flat_similarities <- perceived_similarity_matrix[lower.tri(perceived_similarity_matrix) |
                                                     upper.tri(perceived_similarity_matrix)]
  
  # Calculate mean similarity
  mean_similarity <- mean(flat_similarities)
  
  # Calculate variance of similarity beliefs
  variance_similarity <- var(flat_similarities)
  
  # Calculate skewness and kurtosis for bimodality index
  deviations <- flat_similarities - mean_similarity
  m3 <- mean(deviations^3) / (sqrt(variance_similarity)^3)  # skewness
  m4 <- mean(deviations^4) / (variance_similarity^2)  # kurtosis
  
  # Calculate bimodality index
  bimodality_index <- (m3^2 + 1) / m4
  
  return(
    list(
      variance = variance_similarity,
      bimodality_index = bimodality_index,
      skewness = m3,
      kurtosis = m4
    )
  )
}

#' Calculate network structure metrics
#'
#' @param graph The network graph
#' @return List of network metrics
calculate_network_metrics <- function(graph) {
  # Calculate clustering coefficient
  clustering_coeffs <- transitivity(graph, type = "local", isolates = "zero")
  avg_clustering <- mean(clustering_coeffs, na.rm = TRUE)
  
  # Calculate average path length
  distances <- distances(graph)
  finite_distances <- distances[is.finite(distances) &
                                  distances > 0]
  avg_path_length <- mean(finite_distances)
  
  # Calculate modularity using fast greedy community detection
  community <- cluster_fast_greedy(graph)
  modularity_value <- modularity(community)
  
  return(
    list(
      avg_clustering = avg_clustering,
      clustering_coeffs = clustering_coeffs,
      avg_path_length = avg_path_length,
      modularity = modularity_value,
      communities = community
    )
  )
}

#' Calculate disclosure metrics
#'
#' @param disclosure_history History of disclosures
#' @param N Number of agents
#' @param T Number of rounds
#' @param L Length of type vector
#' @return List of disclosure metrics
calculate_disclosure_metrics <- function(disclosure_history, N, T, L) {
  # Initialize per-agent, per-trait counts
  agent_trait_counts <- matrix(0, nrow = N, ncol = L)
  
  # Count total disclosures per agent
  agent_disclosures <- numeric(N)
  
  # Count disclosures per trait
  trait_disclosures <- numeric(L)
  
  # Process disclosure history
  for (t in seq_len(T)) {
    for (i in seq_len(N)) {
      action <- disclosure_history[[t]][i]
      if (action != "reveal nothing") {
        # Extract trait number safely
        trait <- extract_trait_number(action)
        
        # Validate trait before doing anything with it
        if (is.na(trait) || trait < 1 || trait > L) {
          message(glue::glue("Invalid trait index at round {t}, agent {i}, action = '{action}', trait = '{trait}'"))
          next  # skip this iteration
        }
        
        # If valid, update counts
        agent_disclosures[i]     <- agent_disclosures[i] + 1
        trait_disclosures[trait] <- trait_disclosures[trait] + 1
        agent_trait_counts[i, trait] <- agent_trait_counts[i, trait] + 1
      }
    }
  }
  
  # Calculate overall disclosure rate
  disclosure_rate <- sum(agent_disclosures) / (N * T)
  
  # Calculate per-trait disclosure rates
  trait_disclosure_rates <- trait_disclosures / (N * T)
  
  return(
    list(
      disclosure_rate = disclosure_rate,
      agent_disclosures = agent_disclosures,
      trait_disclosure_rates = trait_disclosure_rates,
      agent_trait_counts = agent_trait_counts
    )
  )
}

#' Calculate welfare metrics
#'
#' @param utilities Vector of utility values
#' @return List of welfare metrics
calculate_welfare <- function(utilities) {
  # Calculate total welfare
  mean_welfare <- mean(utilities)
  
  # Calculate Gini coefficient for inequality measurement
  sorted_utilities <- sort(utilities)
  n <- length(utilities)
  cumulative_utilities <- cumsum(sorted_utilities)
  
  sum_product <- sum((1:n) * sorted_utilities)
  
  # Gini coefficient formula
  coefficient_numerator <- (2 * sum_product) - (n + 1) * sum(sorted_utilities)
  coefficient_denominator <- n * sum(sorted_utilities)
  
  if (coefficient_denominator > 0) {
    gini_coefficient <- coefficient_numerator / coefficient_denominator
  } else {
    gini_coefficient <- 0
  }
  
  return(
    list(
      mean_welfare = mean_welfare,
      individual_utilities = utilities,
      gini_coefficient = gini_coefficient
    )
  )
}

###############################################################
# PART 6: MAIN SIMULATION FUNCTION
###############################################################

#' Run a single simulation round
#'
#' @param graph The network graph
#' @param beliefs Belief array
#' @param types Type matrix
#' @param params List of simulation parameters
#' @param round Current round number
#' @return List containing updated graph, beliefs, and outcomes
run_simulation_round <- function(graph,
                                 beliefs,
                                 types,
                                 params,
                                 round) {
  N <- params$N
  L <- params$L
  delta <- params$delta
  s <- params$s
  model_version <- params$model_version
  
  # Randomize agent order for this round
  agent_order <- sample(1:N)
  
  # Store actions for this round
  round_actions <- rep("reveal nothing", N)
  
  # Calculate shortest paths for utility calculations
  distances <- distances(graph)
  
  # Process each agent in the randomized order
  for (idx in 1:N) {
    i <- agent_order[idx]
    
    # For dynamic networks: rewire connections
    if (model_version == "dynamic") {
      graph <- rewire_connections(graph, i, beliefs, types, L)
      
      # Recalculate distances after rewiring
      distances <- distances(graph)
    }
    
    # Determine scale of disclosure
    # Always use params$s as number of disclosures; 
    # Global disclosure is s == N
    other_agents <- setdiff(1:params$N, i)
    if (params$s >= params$N) {
      target_agents <- other_agents
    } else {
      target_agents <- sample(other_agents, params$s)
    }
    
    # Decide best action
    action_result <- decide_best_action(i, target_agents, distances, beliefs, types, L, delta)
    best_action <- action_result$action
    round_actions[i] <- best_action
    
    # Update beliefs based on action
    if (best_action != "reveal nothing") {
      trait_to_reveal <- extract_trait_number(best_action)
      if (!is.na(trait_to_reveal)) {
        for (j in target_agents) {
          beliefs[j, i, trait_to_reveal] <- types[i, trait_to_reveal]
        }
      }
    }
  }
  
  # Calculate outcome measures for this round
  perceived_similarity_matrix <- compute_similarity_outcomes(graph, beliefs, types, N, L)$similarity_matrices$perceived
  revealed_perceived_similarity_matrix <- compute_similarity_outcomes(graph, beliefs, types, N, L)$similarity_matrices$perceived
  
  # Store results
  outcomes <- list(
    similarity = compute_similarity_outcomes(graph, beliefs, types, N, L),
    polarization = calculate_polarization_metrics(perceived_similarity_matrix),
    network = calculate_network_metrics(graph),
    utilities = calculate_all_utilities(distances, beliefs, types, N, L, delta),
    actions = round_actions
  )
  
  # Add welfare metrics
  outcomes$welfare <- calculate_welfare(outcomes$utilities)
  
  return(list(
    graph = graph,
    beliefs = beliefs,
    outcomes = outcomes,
    round = round
  ))
}

#' Run a complete simulation
#'
#' @param params List of simulation parameters
#' @return List of simulation results
run_simulation <- function(params) {
  # Extract parameters
  N <- params$N
  L <- params$L
  T <- params$T
  network_type <- params$network_type
  init_type <- params$init_type
  model_version <- params$model_version
  p <- params$p
  k <- params$k
  b <- params$b
  delta <- params$delta
  s <- params$s
  
  # Initialize network
  graph <- generate_network(network_type, N, p, k)
  
  # Initialize agent type vectors
  types <- initialize_types(N, L, init_type, b)
  
  # Initialize belief matrices
  beliefs <- initialize_beliefs(N, L, types)
  
  # Initialize storage for simulation results
  results <- list(
    params = params,
    rounds = list(),
    disclosure_history = list()
  )
  
  # Run simulation for T rounds
  for (t in 1:T) {
    # Run a single round
    round_result <- run_simulation_round(graph, beliefs, types, params, t)
    
    # Update graph and beliefs
    graph <- round_result$graph
    beliefs <- round_result$beliefs
    
    # Store results
    results$rounds[[t]] <- round_result$outcomes
    results$disclosure_history[[t]] <- round_result$outcomes$actions
  }
  
  # Calculate overall disclosure metrics
  results$disclosure_metrics <- calculate_disclosure_metrics(results$disclosure_history, N, T, L)
  
  # Store final network state
  results$final_network <- graph
  results$final_beliefs <- beliefs
  
  return(results)
}

#' Process simulation results for analysis and visualization
#'
#' @param results List of simulation results
#' @return List of processed data frames for analysis
process_simulation_results <- function(results) {
  T <- length(results$rounds)
  
  # Extract time series data
  rounds_data <- data.frame(
    round = 1:T,
    perceived_neighbor_similarity = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$perceived_neighbor_similarity),
    perceived_all_similarity = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$perceived_all_similarity),
    perceived_similarity_gap = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$perceived_similarity_gap),
    objective_neighbor_similarity = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$objective_neighbor_similarity),
    objective_all_similarity = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$objective_all_similarity),
    objective_similarity_gap = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$objective_similarity_gap),
    revealed_neighbor_similarity = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$revealed_neighbor_similarity),
    revealed_all_similarity = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$revealed_all_similarity),
    revealed_similarity_gap = sapply(1:T, function(t)
      results$rounds[[t]]$similarity$revealed_similarity_gap),
    variance = sapply(1:T, function(t)
      results$rounds[[t]]$polarization$variance),
    bimodality = sapply(1:T, function(t)
      results$rounds[[t]]$polarization$bimodality_index),
    clustering = sapply(1:T, function(t)
      results$rounds[[t]]$network$avg_clustering),
    path_length = sapply(1:T, function(t)
      results$rounds[[t]]$network$avg_path_length),
    modularity = sapply(1:T, function(t)
      results$rounds[[t]]$network$modularity),
    mean_welfare = sapply(1:T, function(t)
      results$rounds[[t]]$welfare$mean_welfare),
    gini = sapply(1:T, function(t)
      results$rounds[[t]]$welfare$gini_coefficient)
  )
  
  # Process disclosure data
  # pull N×L matrix from disclosure_metrics
  mat <- results$disclosure_metrics$agent_trait_counts
  
  # Name columns "1","2",…,"L" so pivoted trait values are numeric strings
  colnames(mat) <- seq_len(ncol(mat))
  
  # pivot to long form: one row per (agent, trait)
  agent_disclosure_df <- as.data.frame(mat, stringsAsFactors = FALSE) %>%
    tibble::rownames_to_column("agent") %>%
    tidyr::pivot_longer(cols = -agent,
                        names_to  = "trait",
                        values_to = "n_disclosed") %>%
    dplyr::mutate(agent = as.integer(agent), trait = as.integer(trait))
  
  # Process trait disclosure data
  trait_disclosure_data <- data.frame(
    trait = 1:results$params$L,
    disclosure_rate = results$disclosure_metrics$trait_disclosure_rates
  )
  
  # Save disclosure metrics
  results$params$disclosure_metrics <- results$disclosure_metrics
  
  # Round all numeric columns to *two* decimals
  rounds_data <- as.data.frame(lapply(rounds_data, function(x) {
    if (is.numeric(x))
      round(x, 2)
    else
      x
  }))
  
  # Return processed data
  return(
    list(
      time_series = rounds_data,
      agent_disclosures = agent_disclosure_df,
      trait_disclosures = trait_disclosure_data,
      params = results$params
    )
  )
}

###############################################################
# PART 7: VISUALIZATION FUNCTIONS
###############################################################

#' Create time series plots of key metrics
#'
#' @param processed_results Processed simulation results
#' @return List of ggplot objects
create_time_series_plots <- function(processed_results) {
  df <- processed_results$time_series
  
  # Similarity plot
  expanded_similarity_plot <- ggplot(df, aes(x = round)) +
    geom_line(aes(y = perceived_neighbor_similarity, color = "Perceived (Neighbor)"),
              linewidth = 1) +
    geom_line(
      aes(y = perceived_all_similarity, color = "Perceived (All)"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    geom_line(aes(y = objective_neighbor_similarity, color = "Objective (Neighbor)"),
              linewidth = 1) +
    geom_line(
      aes(y = objective_all_similarity, color = "Objective (All)"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    geom_line(aes(y = revealed_neighbor_similarity, color = "Revealed (Neighbor)"),
              linewidth = 1) +
    geom_line(
      aes(y = revealed_all_similarity, color = "Revealed (All)"),
      linewidth = 1,
      linetype = "dashed"
    ) +
    labs(
      title = "Expanded Similarity Measures Over Time",
      x = "Round",
      y = "Similarity",
      color = "Measure"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Paired")
  
  # Polarization plot
  polarization_plot <- ggplot(df, aes(x = round)) +
    geom_line(aes(y = variance, color = "Variance"), linewidth = 1) +
    geom_line(aes(y = bimodality, color = "Bimodality"), linewidth = 1) +
    labs(
      title = "Polarization Measures Over Time",
      x = "Round",
      y = "Value",
      color = "Measure"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set2")
  
  # Network metrics plot
  network_plot <- ggplot(df, aes(x = round)) +
    geom_line(aes(y = clustering, color = "Clustering"), linewidth = 1) +
    geom_line(aes(y = modularity, color = "Modularity"), linewidth = 1) +
    labs(
      title = "Network Metrics Over Time",
      x = "Round",
      y = "Value",
      color = "Measure"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set3")
  
  # Welfare plot
  welfare_plot <- ggplot(df, aes(x = round)) +
    geom_line(aes(y = mean_welfare, color = "Total Welfare"), linewidth = 1) +
    geom_line(aes(y = gini * 100, color = "Gini Coefficient"), linewidth = 1) +
    labs(
      title = "Welfare Measures Over Time",
      x = "Round",
      y = "Value",
      color = "Measure"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2") +
    scale_y_continuous(sec.axis = sec_axis( ~ . / 100, name = "Gini Coefficient"))
  
  return(
    list(
      expanded_similarity = expanded_similarity_plot,
      polarization = polarization_plot,
      network = network_plot,
      welfare = welfare_plot
    )
  )
}

#' Visualize the network with node colors based on similarity
#'
#' @param graph The network graph
#' @param perceived_similarity_matrix Matrix of similarity values
#' @return ggplot object of the network
visualize_network <- function(graph, perceived_similarity_matrix) {
  # Calculate average similarity for each node
  n <- vcount(graph)
  avg_similarities <- rowMeans(perceived_similarity_matrix, na.rm = TRUE)
  
  # Normalize for color mapping
  node_colors <- (avg_similarities - min(avg_similarities)) /
    (max(avg_similarities) - min(avg_similarities))
  
  # Get layout
  layout <- layout_with_fr(graph)
  
  # Convert to data frames for ggplot
  nodes <- data.frame(
    id = 1:n,
    x = layout[, 1],
    y = layout[, 2],
    similarity = avg_similarities
  )
  
  edges <- data.frame(from = ends(graph, E(graph))[, 1], to = ends(graph, E(graph))[, 2])
  
  edges_for_plot <- data.frame(
    x = layout[edges$from, 1],
    y = layout[edges$from, 2],
    xend = layout[edges$to, 1],
    yend = layout[edges$to, 2]
  )
  
  # Create network plot
  plot <- ggplot() +
    geom_segment(data = edges_for_plot,
                 aes(
                   x = x,
                   y = y,
                   xend = xend,
                   yend = yend
                 ),
                 alpha = 0.5) +
    geom_point(
      data = nodes,
      aes(x = x, y = y, fill = similarity),
      shape = 21,
      size = 5,
      color = "black"
    ) +
    scale_fill_gradient(low = "blue", high = "red") +
    labs(title = "Network Visualization", fill = "Avg. Similarity") +
    theme_void() +
    theme(legend.position = "right")
  
  return(plot)
}

###############################################################
# PART 8: PARAMETER SWEEP FUNCTIONS
###############################################################

#' Run multiple simulations with different parameter combinations
#'
#' @param base_params Base parameter list
#' @param param_grid List of parameter grids to sweep
#' @param num_runs Number of runs per parameter combination
#' @return Data frame of aggregated results
# Adds an optional progress callback to report completion % to the UI
<<<<<<< HEAD
run_parameter_sweep <- function(base_params, param_grid, num_runs) {
  
  # Dbug
  future::plan("sequential")
  set.seed(42)
  
  # 1. Expand parameter grid
  grid <- expand.grid(param_grid, stringsAsFactors = FALSE)
  n_grid <- nrow(grid)
  
  # 2. Replicate each grid row num_runs times and assign run IDs
  combos <- grid[rep(seq_len(n_grid), each = num_runs), , drop = FALSE]
  combos$run_id <- rep(seq_len(num_runs), times = n_grid)
=======
run_parameter_sweep <- function(base_params, param_grid, num_runs, progress_callback = NULL) {
  # 1. Expand and replicate grid of parameter combinations
  combos <- expand.grid(param_grid, stringsAsFactors = FALSE)
  combos <- combos %>%
    slice(rep(seq_len(nrow(.)), each = num_runs)) %>%
    mutate(run_id = rep(seq_len(num_runs), times = nrow(param_grid)))
>>>>>>> parent of 336fb28 (Run param sweep debug)
  
  total_iters <- nrow(combos)
  results_list <- vector("list", total_iters)
  
  # 2. Iterate with progress reporting
  for (i in seq_len(total_iters)) {
    row <- combos[i, ]
    
<<<<<<< HEAD
    # Merge base params with this combination
    # Only use the names in param_grid (e.g. N, L, T, network_type, model_version)
    params <- base_params
    for (name in names(param_grid)) {
      params[[name]] <- row[[name]]
    }    
    cat("▶ combo", i, "params:", paste(names(params), params, sep="=", collapse=", "), "\n")
    if (is.null(params$T) || params$T < 1) stop("Invalid T: must be ≥ 1")
    
    # Run simulation and process results
    sim <- tryCatch({
      run_simulation(params)
    }, error = function(e) {
      cat("Simulation failed for params:\n")
      print(params)
      cat("Error message:\n")
      print(e$message)
      stop("Aborting sweep due to simulation error.")
    })
    out  <- process_simulation_results(sim)$time_series
    if (nrow(out) == 0) stop("❌ Time series output is empty!")
    last <- out[nrow(out), , drop = FALSE]    
    print(paste("Column names in `last`:", toString(names(last))))
    
=======
    # Merge parameters
    params <- modifyList(base_params, list(
      network_type    = row$network_type,
      model_version   = row$model_version,
      disclosure_type = row$disclosure_type,
      delta           = row$delta,
      b               = row$b
    ))
    
    # Run and process
    sim <- run_simulation(params)
    out <- process_simulation_results(sim)$time_series
    last <- tail(out, 1)
>>>>>>> parent of 336fb28 (Run param sweep debug)
    
    # Collect final metrics
    results_list[[i]] <- tibble(
<<<<<<< HEAD
      network_type   = params$network_type,
      model_version  = params$model_version,
      delta          = params$delta,
      b              = params$b,
      run_id         = row$run_id,
      final_perceived_neighbor_similarity = last$perceived_neighbor_similarity[[1]],
      final_perceived_all_similarity      = last$perceived_all_similarity[[1]],
      final_perceived_similarity_gap      = last$perceived_similarity_gap[[1]],
      final_objective_neighbor_similarity = last$objective_neighbor_similarity[[1]],
      final_objective_all_similarity      = last$objective_all_similarity[[1]],
      final_objective_similarity_gap      = last$objective_similarity_gap[[1]],
      final_revealed_neighbor_similarity  = last$revealed_neighbor_similarity[[1]],
      final_revealed_all_similarity       = last$revealed_all_similarity[[1]],
      final_revealed_similarity_gap       = last$revealed_similarity_gap[[1]],
      final_variance                      = last$variance[[1]],
      final_bimodality                    = last$bimodality[[1]],
      final_clustering                    = last$clustering[[1]],
      final_modularity                    = last$modularity[[1]],
      final_mean_welfare                  = last$mean_welfare[[1]],
      final_gini                          = last$gini[[1]]
=======
      network_type  = row$network_type,
      model_version = row$model_version,
      disclosure_type = row$disclosure_type,
      delta         = row$delta,
      b             = row$b,
      run_id        = row$run_id,
      final_perceived_neighbor_similarity = last$perceived_neighbor_similarity,
      final_perceived_all_similarity      = last$perceived_all_similarity,
      final_perceived_similarity_gap      = last$perceived_similarity_gap,
      final_objective_neighbor_similarity = last$objective_neighbor_similarity,
      final_objective_all_similarity      = last$objective_all_similarity,
      final_objective_similarity_gap      = last$objective_similarity_gap,
      final_revealed_neighbor_similarity  = last$revealed_neighbor_similarity,
      final_revealed_all_similarity       = last$revealed_all_similarity,
      final_revealed_similarity_gap       = last$revealed_similarity_gap,
      final_variance    = last$variance,
      final_bimodality  = last$bimodality,
      final_clustering  = last$clustering,
      final_modularity  = last$modularity,
      final_mean_welfare= last$mean_welfare,
      final_gini        = last$gini
>>>>>>> parent of 336fb28 (Run param sweep debug)
    )
  }
  
<<<<<<< HEAD
  # Combine and return results
=======
  # 3. Bind and return
>>>>>>> parent of 336fb28 (Run param sweep debug)
  results <- dplyr::bind_rows(results_list)
  return(results)
}


#' Process sweep results into a flat data frame
#'
#' @param results List of simulation results
#' @param param_combinations Data frame of parameter combinations
#' @return Data frame with aggregated metrics
process_sweep_results <- function(results, param_combinations) {
  num_combinations <- nrow(param_combinations)
  num_runs <- length(results[[1]]$runs)
  
  # Initialize data frame for storing results
  result_rows <- list()
  
  for (i in 1:num_combinations) {
    for (run in 1:num_runs) {
      # Get final round metrics
      final_round <- results[[i]]$runs[[run]]$time_series[nrow(results[[i]]$runs[[run]]$time_series), ]
      
      # Create row with parameters and final metrics
      row <- c(
        as.list(param_combinations[i, ]),
        run = run,
        final_perceived_neighbor_similarity = final_round$perceived_neighbor_similarity,
        final_perceived_all_similarity = final_round$perceived_all_similarity,
        final_perceived_similarity_gap = final_round$perceived_similarity_gap,
        final_objective_neighbor_similarity = final_round$objective_neighbor_similarity,
        final_objective_all_similarity = final_round$objective_all_similarity,
        final_objective_similarity_gap = final_round$objective_similarity_gap,
        final_revealed_neighbor_similarity = final_round$revealed_neighbor_similarity,
        final_revealed_all_similarity = final_round$revealed_all_similarity,
        final_revealed_similarity_gap = final_round$revealed_similarity_gap,
        final_variance = final_round$variance,
        final_bimodality = final_round$bimodality,
        final_clustering = final_round$clustering,
        final_path_length = final_round$path_length,
        final_modularity = final_round$modularity,
        final_welfare = final_round$mean_welfare,
        final_gini = final_round$gini,
        # disclosure_rate = results[[i]]$runs[[run]]$params$disclosure_metrics$disclosure_rate
      )
      
      result_rows[[length(result_rows) + 1]] <- row
    }
  }
  
  # Convert list to data frame
  results_df <- do.call(rbind.data.frame, result_rows)
  
  # Round all numeric columns to *two* decimals
  results_df <- as.data.frame(lapply(results_df, function(x) {
    if (is.numeric(x))
      round(x, 2)
    else
      x
  }))
  
  return(results_df)
}

###############################################################
# PART 9: SAMPLE USAGE
###############################################################

# Example parameter set
example_params <- list(
  N = 50,
  # Number of agents
  L = 5,
  # Length of type vector
  T = 20,
  # Number of rounds
  network_type = "WS",
  # Network type: "ER", "WS", or "BA"
  p = 0.1,
  # Probability parameter for network generation
  k = 4,
  # Degree parameter for network generation
  init_type = "random",
  # Type vector initialization: "random" or "polarized"
  b = 0.7,
  # Bias parameter for polarized initialization
  delta = 0.5,
  # Influence decay factor
  s = 10,
  # Size of target subset for selective disclosure
  model_version = "static",
  # Network version: "static" or "dynamic"
)

# # Run a single simulation (uncomment to run)
# sim_results <- run_simulation(example_params)
# processed_results <- process_simulation_results(sim_results)
# plots <- create_time_series_plots(processed_results)
#
# # Example parameter sweep (uncomment to run)
# param_grid <- list(
#   disclosure_type = c("selective", "global"),
#   model_version = c("static", "dynamic")
# )
# sweep_results <- run_parameter_sweep(example_params, param_grid, num_runs = 2)
# lapply(sweep_results, function(x) if (is.numeric(x)) round(x, 2) else x)

###############################################################
# END OF CODE
###############################################################