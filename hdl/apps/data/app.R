pkgs = c("shiny", "shinydashboard", "dplyr", "reshape2", "stringr", "glue",
         "Metabase", "DT")
for(pkg in pkgs) {
    library(pkg, quietly = T, verbose = F, warn.conflicts = F, character.only = T)
}

load("../app/data/data.rda")
source("ui/sidebar.R")
source("ui/body.R")

ui <- dashboardPage(
   header = dashboardHeader(title = "Fast Food Study"),
   sidebar = sidebar,
   body = body,
   skin = "purple"
)

server <- function(input, output) {
    source("ui/inputs.R", local = TRUE)
    
    # lpd
    output$lpdDataTable = renderDataTable({
        t(data$data$lpd[[input$lpd_level]][[input$lpd_norm]]$conc_table)
    },extensions = c("Buttons"),
      options = list(dom = 'Bfrtip',
                     pageLength = 40,
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # glc
    output$glcDataTable = renderDataTable({
        t(data$data$glc[[input$glc_level]]$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # fct
    output$fctDataTable = renderDataTable({
        t(data$data$fct$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # cli
    output$cliDataTable = renderDataTable({
        t(data$data$cli$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    #output$glcDataTable = renderDataTbale()
}

# Run the application 
shinyApp(ui = ui, server = server)

