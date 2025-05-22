###############################################################
# Global: Shared Libraries, Functions, and Constants
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

# Source simulation functions
source("simulation.R", local = TRUE)

# Helper function for handling NULL values
`%||%` <- function(x, y) if (is.null(x)) y else x

# Color palette generator
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

# Standard color scheme for consistency across plots
app_colors <- list(
  network_types = c(
    "Static Network" = "#4E79A7",  # Blue
    "Dynamic Network" = "#F28E2B"  # Orange
  ),
  panel_bg = "#EBF5FB",           # Light blue background for panel headers
  highlight = "#3498DB"           # Accent color for buttons and important elements
)

# Configure parallel processing based on environment
if (interactive()) {
  future::plan(multisession)
} else {
  future::plan(sequential)
}

# App launch options
options(shiny.launch.browser = TRUE)