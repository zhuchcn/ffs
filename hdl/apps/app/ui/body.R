boxplotTabGen = function(tabName) {
    type = substr(tabName,0,3)
    tabItem(
        tabName = tabName,
        fluidRow(
            column(
                width = 6,
                box(width = NULL,
                    DTOutput(paste0(type, "_limma")), height = "90%")
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

lpd_pca = tabItem(
    tabName = "lpd_pca",
    fluidRow(
        column(
            width = 6,
            box(
                width = NULL,
                column(
                    width = 6,
                    checkboxInput("lpd.change", "Subtract baseline",
                                  value = TRUE)
                ),
                column(
                    width = 6,
                    sliderInput("lpd.cutoff", "P Value Cutoff:",
                                max = 1, min = 0, step = 0.01, value = 0.05)
                )
            ),
            box(width = NULL,
                plotlyOutput("lpd_pca"))
        ),
        column(
            width = 6,
            box(
                width = NULL,
                column(
                    width = 6,
                    selectInput("lpd.scale", "How to scale the data?",
                                choices = c("none", "log2", "z-score feature", "z-score sample", "absolute feature", "absolute sample"),
                                selected = "z-score feature")
                ),
                column(
                    width = 6,
                    selectInput("lpd.p", "Filter by:",
                                choices = c("pvalue", "padj"),
                                selected = "P.Values")
                )
            ),
            box(width = NULL,
                plotlyOutput("lpd_heatmap", height = 500))
        )
    )
)

lpd_clado = tabItem(
    tabName = "lpd_clado",
    fluidRow(
        column(
            width = 9,
            box(
                width = NULL, height = 600,
                plotOutput("lpd_clado", height = "580px")
            )
        ),
        column(
            width = 3,
            box(
                width = NULL,
                tags$div(
                    tags$style("width:80%;margin:auto"),
                    tags$h2("Cladogram"),
                    tags$br(),
                    tags$p("The cladograme is made using the Tanimoto similarity between lipid spceis molecules.")
                )
            )
        )
    )
)

lpd_pie = tabItem(
    tabName = "lpd_pie",
    fluidRow(
        column(
            width = 6,
            box(
                width = NULL,
                plotOutput("lpd_pie")
            )
        )
    )
)

lpd_clust = tabItem(
    tabName = "lpd_clust",
    fluidRow(
        box(
            width = 12,
            plotOutput("lpd_hc")
        ),
        column(
            width = 6,
            box(
                width = NULL,
                column(
                    width = 6,
                    numericInput("lpd.hc.height", "The height to cut the tree",
                                 value = 7, min = 0, max = 12, step = 0.1)
                )
            ),
            box(
                width = NULL,
                DTOutput("lpd_hc_dt")
            ),
            box(
                width = NULL,
                title = "Lipid spiecies members in the selected cluster",
                tags$pre(
                    class="shiny-text-output",
                    tableOutput("lpd_hc_members")
                )
            )
        ),
        column(
            width = 6,
            box(
                width = NULL,
                plotlyOutput("lpd_hc_scatter")
            ),
            box(
                width = NULL,
                plotlyOutput("lpd_hc_box")
            )
        )
    )
)

body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        boxplotTabGen("lpd_boxplot"),
        histTabGen("lpd_hist"),
        lpd_clust,
        lpd_pca,
        lpd_clado,
        lpd_pie,
        corrTabGen("lpd_fct"),
        corrTabGen("lpd_cli"),
        boxplotTabGen("glc_boxplot"),
        histTabGen("glc_hist"),
        corrTabGen("glc_fct"),
        corrTabGen("glc_cli"),
        boxplotTabGen("fct_boxplot"),
        corrTabGen("fct_fct"),
        corrTabGen("fct_cli"),
        boxplotTabGen("cli_boxplot")
    )
)