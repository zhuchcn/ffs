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
source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
    header = dashboardHeader(
        title = tags$a(
            href="http://www.chenghaozhu.net/studies/ffs/docs/hdl.html",
            style="color:inherit;",
            "Fast Food Study"
        )
    ),
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    source("ui/inputs.R", local = TRUE)
    
    source("server/lpd/boxplot.R",   local = TRUE)
    source("server/lpd/hist.R",      local = TRUE)
    source("server/lpd/hclust.R",    local = TRUE)
    source("server/lpd/pca.R",       local = TRUE)
    source("server/lpd/clado.R",     local = TRUE)
    source("server/lpd/pie.R",       local = TRUE)
    source("server/lpd/corr_fct.R",  local = TRUE)
    source("server/lpd/corr_cli.R",  local = TRUE)
    
    source("server/glc/boxplot.R",   local = TRUE)
    source("server/glc/hist.R",      local = TRUE)
    source("server/glc/corr_fct.R",  local = TRUE)
    source("server/glc/corr_cli.R",  local = TRUE)
    
    source("server/fct/boxplot.R",   local = TRUE)
    source("server/fct/corr_fct.R",  local = TRUE)
    source("server/fct/corr_cli.R",  local = TRUE)
    
    source("server/cli/boxplot.R",   local = TRUE)
}

shinyApp(ui = ui, server = server)