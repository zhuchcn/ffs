tabGenerator = function(tabName){
    outputid = str_c(tabName, "DataTable")
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 12,
                box(width = NULL,
                    DTOutput(outputid)
                )
            )
        )
    )
}
body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        tabGenerator("mcb"),
        tabGenerator("pcr"),
        tabGenerator("bga"),
        tabGenerator("bac"),
        tabGenerator("sfa"),
        tabGenerator("diet")
    )
)