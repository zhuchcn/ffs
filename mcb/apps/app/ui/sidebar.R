sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), 
            text = "Micorbiome", 
            menuSubItem("Boxplot", tabName = "mcb_boxplot"),
            menuSubItem("Histogram", tabName = "mcb_hist"),
            menuSubItem("Richness", tabName = "mcb_richness"),
            menuSubItem("Ordinate Plots", tabName = "mcb_pcoa"),
            menuSubItem("Cladogram", tabName = "mcb_clado"),
            menuSubItem("Genus Scatter Plot", tabName = "mcb_scatter"),
            menuSubItem("vs Biogenic Amines", tabName = "mcb_bga"),
            menuSubItem("vs Bile Acids", tabName = "mcb_bac"),
            menuSubItem("vs Short Chain Fatty Acids", tabName = "mcb_sfa")
        ),
        menuItem(
            icon = icon("caret-right"), 
            text = "PICRUSt Functions",
            menuSubItem("Boxplot", tabName = "pcr_boxplot")
        ),
        menuItem(
            icon = icon("caret-right"), 
            text = "Biogenic Amines",
            menuSubItem("Boxplot", tabName = "bga_boxplot"),
            menuSubItem("Structure Similarity Tree", tabName = "bga_tree")
        ),
        menuItem(
            icon = icon("caret-right"), 
            text = "Bile Acids",
            menuSubItem("Boxplot", tabName = "bac_boxplot")
        ),
        menuItem(
            icon = icon("caret-right"), 
            text = "Short Chain Fatty Acids",
            menuSubItem("Boxplot", tabName = "sfa_boxplot")
        )
    )
)