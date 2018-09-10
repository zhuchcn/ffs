sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), "Micorbiome", 
            menuSubItem("Boxplot", tabName = "mcb_boxplot"),
            menuSubItem("Histogram", tabName = "mcb_hist"),
            menuSubItem("Heatmap and PCoA", tabName = "mcb_pcoa"),
            menuScubItem("Cladogram", tabName = "mcb_clado"),
            menuSubItem("vs Biogenic Amines", tabName = "mcb_bga"),
            menuSubItem("vs Short Chain Fatty Acids", tabName = "mcb_sfa")
        ),
        menuItem(
            icon = icon("caret-right"), "PICRUSt Functions",
            menuSubItem("Boxplot", tabName = "pcr_boxplot")
        ),
        menuItem(
            icon = icon("caret-right"), "Biogenic Amines",
            menuSubItem("Boxplot", tabName = "bga_boxplot")
        ),
        menuItem(
            icon = icon("caret-right"), "Short Chain Fatty Acids",
            menuSubItem("Boxplot", tabName = "sfa_boxplot")
        )
    )
)