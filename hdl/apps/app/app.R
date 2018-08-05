pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'Metabase',
         'ggsci', "shiny", "shinydashboard", "ggmetaplots", "heatmaply",
         "RColorBrewer", "ggtree", "treeio", "cowplot", "grid", "gridExtra",
         "knitr", "dendextend")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load("data/data.rda")
load("data/similar_matrix.rda")
source("layout/sidebar.R")
source("layout/body.R")

ui <- dashboardPage(
    header = dashboardHeader(title = "Fast Food Study"),
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    source("layout/inputs.R", local = TRUE)
    
    source("lpd/boxplot.R",   local = TRUE)
    source("lpd/hist.R",      local = TRUE)
    source("lpd/hclust.R",    local = TRUE)
    source("lpd/pca.R",       local = TRUE)
    source("lpd/clado.R",     local = TRUE)
    source("lpd/pie.R",       local = TRUE)
    source("lpd/corr_fct.R",  local = TRUE)
    source("lpd/corr_cli.R",  local = TRUE)
    
    source("glc/boxplot.R",   local = TRUE)
    source("glc/hist.R",      local = TRUE)
    source("glc/corr_fct.R",  local = TRUE)
    source("glc/corr_cli.R",  local = TRUE)
    
    source("fct/boxplot.R",   local = TRUE)
    source("fct/corr_fct.R",  local = TRUE)
    source("fct/corr_cli.R",  local = TRUE)
    
    source("cli/boxplot.R",   local = TRUE)
}

shinyApp(ui = ui, server = server)