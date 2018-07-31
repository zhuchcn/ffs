source("layout/sidebar.R")
source("layout/body.R")

ui <- dashboardPage(
    header = dashboardHeader(title = "Fast Food Study"),
    sidebar = sidebar,
    body = body
)
