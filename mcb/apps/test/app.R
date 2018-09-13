library(shiny); library(shinydashboard)

ui <- dashboardPage(
    header = dashboardHeader(title = "Fast Food Study"),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
        fluidPage(
            fluidRow(
                column(width = 12,
                       box(width=NULL, height = 200))
            ),
            fluidRow(
                column(width = 12,
                       box(width=NULL))
            ),
            fluidRow(
                column(width = 12,
                       box(width=NULL))
            )
        )
    )
)


server <- function(input, output) {
   

}

shinyApp(ui = ui, server = server)

