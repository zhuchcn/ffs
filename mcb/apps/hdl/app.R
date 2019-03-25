pkgs = c("shiny", "shinydashboard", "dplyr", "reshape2", "stringr", "tibble", 
         "ggplot2", "plotly", "DT", "Metabase")
for(pkg in pkgs) {
    library(pkg, character.only = T, warn.conflicts = F, quietly = T, verbose = F)
}

ui <- dashboardPage(
    
)

server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)

