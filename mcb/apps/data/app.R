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
    
    # Microbiome
    output$mcbDataTable = renderDataTable({
        t(data$data$mcb[[input$mcb_norm]][[input$mcb_level]]$conc_table)
    },extensions = c("Buttons"),
      options = list(dom = 'Bfrtip',
                     pageLength = 40,
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # PICRUSt
    output$pcrDataTable = renderDataTable({
        t(data$data$pcr[[input$pcr_level]]$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # Biogenic Amines
    output$bgaDataTable = renderDataTable({
        t(data$data$bga$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # Bile Acids
    output$bacDataTable = renderDataTable({
        t(data$data$bac[[input$bac_level]]$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # Short Chain Fatty Acids
    output$sfaDataTable = renderDataTable({
        t(data$data$sfa$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    # Diet
    output$dietDataTable = renderDataTable({
        t(data$data$diet$conc_table)
    },extensions = c("Buttons"),
    options = list(dom = 'Bfrtip',
                   pageLength = 40,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    #output$glcDataTable = renderDataTbale()
}

# Run the application 
shinyApp(ui = ui, server = server)

