boxplotTabGen = function(tabName) {
    type = substr(tabName,0,3)
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    DTOutput(paste0(type, "_diff")), height = "90%")
            ),
            column(
                width = 6,
                box(width = NULL, 
                    plotlyOutput(paste0(type, "_boxplot")))
            )
        )
    )
}

histTabGen = function(tabName) {
    type = substr(tabName,0,3)
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    plotlyOutput(paste0(type, "_hist_pval")), height = "100%"),
                box(width = NULL,
                    plotlyOutput(paste0(type, "_hist_padj")), height = "100%")
            ),
            column(
                width = 6,
                box(width = NULL,
                    plotlyOutput(paste0(type, "_volcano")), height = "100%")
            )
        )
    )
}

corrTabGen = function(tabName) {
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    DTOutput(paste0(tabName, "_dt")), height = "100%" )
            ),
            column(
                width = 6,
                box(width = NULL, height = "10%",
                    column(
                        width = 6, 
                        uiOutput(paste0(tabName, "_Selector")))
                ),
                box(width = NULL, height = "90%",
                    plotlyOutput(paste0(tabName,"_scatter")))
            )
        )
    )
}

mcb_pca = tabItem(
    tabName = "mcb_pcoa"
)

body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        boxplotTabGen("mcb_boxplot"),
        histTabGen("mcb_hist"),
        mcb_pca,
        corrTabGen("mcb_bga"),
        corrTabGen("mcb_sfa"),
        boxplotTabGen("pcr_boxplot"),
        boxplotTabGen("bga_boxplot"),
        boxplotTabGen("sfa_boxplot")
    )
)