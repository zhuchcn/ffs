pkgs = c('dplyr','stringr','reshape2','tibble', 'plotly', 'DT', 'phyloseq', 
         'phylox', 'Metabase', 'ggsci', "ggplot2", "ggtree", "shiny", "ape",
         "shinydashboard", "ggmetaplots", "ggrepel")
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}

load("data/data.rda")
source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
    header = dashboardHeader(title = "Fast Food Study"),
    sidebar = sidebar,
    body = body
)

server <- function(input, output) {
    source("ui/inputs.R", local = TRUE)
    
    source("server/mcb/boxplot.R", local = TRUE)
    source("server/mcb/hist.R", local = TRUE)
    source("server/mcb/corr_bga.R", local = TRUE)
    source("server/mcb/corr_bac.R", local = TRUE)
    source("server/mcb/clado.R", local = TRUE)
    source("server/mcb/pcoa.R", local = TRUE)
    source("server/mcb/richness.R", local = TRUE)
    source("server/mcb/scatter.R", local = TRUE)
    
    source("server/pcr/boxplot.R", local = TRUE)

    source("server/bga/boxplot.R", local = TRUE)
    source("server/bga/tree.R", local = TRUE)
    
    source("server/bac/boxplot.R", local = TRUE)
    
    source("server/sfa/boxplot.R", local = TRUE)
}

shinyApp(ui = ui, server = server)
