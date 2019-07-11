pkgs = c("shiny", "shinydashboard", "dplyr", "reshape2", "stringr", "tibble", 
         "ggplot2", "plotly", "DT", "Metabase", "R6", "MatCorR", "shinyjs")
for(pkg in pkgs) {
    suppressPackageStartupMessages(library(pkg, character.only = T))
}

load("data/data.rda")

import::here(ShinyModule, .from = "components/modules/ShinyModule.R")
import::here(DashboardBody, .from = "components/layout/body.R")
import::here(DashboardSidebar, .from = "components/layout/sidebar.R")