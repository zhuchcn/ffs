sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), "Micorbiome", 
            menuSubItem("Boxplot", tabName = "mcb_boxplot"),
            menuSubItem("Histogram", tabName = "mcb_hist"),
            menuSubItem("Ordinate Plots", tabName = "mcb_pcoa"),
            menuSubItem("Cladogram", tabName = "mcb_clado"),
            menuSubItem("vs Biogenic Amines", tabName = "mcb_bga"),
            menuSubItem("vs Short Chain Fatty Acids", tabName = "mcb_sfa")
        ),
        menuItem(
            icon = icon("caret-right"), "PICRUSt Functions",
            menuSubItem("Boxplot", tabName = "pcr_boxplot")
        ),
        menuItem(
            icon = icon("caret-right"), "Biogenic Amines",
            menuSubItem("Boxplot", tabName = "bga_boxplot"),
            menuSubItem("Structure Similarity Tree", tabName = "bga_tree")
        ),
        menuItem(
            icon = icon("caret-right"), "Short Chain Fatty Acids",
            menuSubItem("Boxplot", tabName = "sfa_boxplot")
        )
    )
)