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

mcb_pcoa = tabItem(
    tabName = "mcb_pcoa",
    fluidRow(
        column(
            width = 6,
            box(width = NULL,
                selectInput("mcb.pcoa.coef", "Unadjusted or Adjusted P Value?",
                            choices = c("pvalue", "padj")),
                numericInput("mcb.pcoa_cutoff", "Input a cutoff",
                             min = 0, max = 1, value = 0.05, step = 0.001),
                selectInput("mcb.ord_method", "Select a Ordinate Method: ",
                            choices = phyloseq::ordinate("list")),
                uiOutput("mcb.dist_method"))
        ),
        column(
            width = 6,
            box(width = NULL,
                plotlyOutput("mcb.pcoa")
            )
        )
    )
)

mcb_clado = tabItem(
    tabName = "mcb_clado",
    fluidRow(
        column(
            width = 12,
            box(width = NULL,
                plotOutput("mcb_clado_FF"))
        )
    ),
    fluidRow(
        column(
            width = 12,
            box(width = NULL,
                plotOutput("mcb_clado_Med"))
        )
    ),
    fluidRow(
        column(
            width = 12,
            box(width = NULL,
                plotOutput("mcb_clado_mix"))
        )
    )
)

bga_tree = tabItem(
    tabName = "bga_tree",
    fluidRow(
        column(
            width = 8,
            box(width = NULL, height = 900,
                plotOutput("bga.tree"))
        )
    )
)

bga_structures = tabItem(
    tabName = "bga_structures",
    fluidRow(
        uiOutput("bga_structures")
    )
)

mcb_richness = tabItem(
    tabName = "mcb_richness",
    fluidRow(
        column(
            width = 6,
            box(width = NULL,
                DTOutput("mcb_richness_limma"))
        ),
        column(
            width = 6,
            box(width = NULL,
                plotlyOutput("mcb_richness_plot"))
        )
    )
)

mcb_scatter = tabItem(
    tabName = "mcb_scatter",
    fluidRow(
        column(
            width = 8,
            box(width = NULL,
                plotOutput("mcb_genus_scatter"))
        )
    )
)

mcb_bac_corr = tabItem(
    tabName = "mcb_bac",
    fluidRow(
        column(
            width = 6,
            box(width = NULL,
                DTOutput("mcb_bac_dt"))
        ),
        column(
            width = 6,
            box(width = NULL,
                column(
                    width = 6,
                    uiOutput("mcb_bac_level_Selector")
                ),
                column(width = 6, uiOutput("mcb_bac_Selector")  
                )),
            box(width = NULL,
                plotlyOutput("mcb_bac_scatter"))
        )
    )
)

body = dashboardBody(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
        ## Microbiome
        boxplotTabGen("mcb_boxplot"),
        histTabGen("mcb_hist"),
        mcb_richness,
        mcb_pcoa,
        mcb_clado,
        mcb_scatter,
        corrTabGen("mcb_bga"),
        mcb_bac_corr,
        corrTabGen("mcb_sfa"),
        corrTabGen("mcb_cli"),
        ## Biogenic Amines
        boxplotTabGen("pcr_boxplot"),
        boxplotTabGen("bga_boxplot"),
        bga_tree,
        bga_structures,
        corrTabGen("bga_bga"),
        corrTabGen("bga_sfa"),
        ## Bile Acids
        boxplotTabGen("bac_boxplot"),
        corrTabGen("bac_bga"),
        corrTabGen("bac_sfa"),
        ## Short Chain Fatty Acids
        boxplotTabGen("sfa_boxplot")
    )
)