sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(
            icon = icon("caret-right"), "Lipidome", 
            menuSubItem("Boxplot", tabName = "lpd_boxplot"),
            menuSubItem("Histograms", tabName = "lpd_hist"),
            menuSubItem("Clustering", tabName = "lpd_clust"),
            menuSubItem("Heatmap and PCA", tabName = "lpd_pca"),
            menuSubItem("Cladogram", tabName = "lpd_clado"),
            menuSubItem("Pie Chart", tabName = "lpd_pie"),
            menuSubItem("vs HDL Function", tabName = "lpd_fct"),
            menuSubItem("vs Clinical Values", tabName = "lpd_cli")
        ),
        menuItem(
            icon = icon("caret-right"), "Glycopeptides",
            menuSubItem("Boxplot", tabName = "glc_boxplot"),
            menuSubItem("Histograms", tabName = "glc_hist"),
            menuSubItem("vs HDL Function", tabName = "glc_fct"),
            menuSubItem("vs Clinical Values", tabName = "glc_cli")
        ),
        menuItem(
            icon = icon("caret-right"), "HDL Functions",
            menuSubItem("Boxplot", tabName = "fct_boxplot"),
            menuSubItem("vs HDL Function", tabName = "fct_fct"),
            menuSubItem("vs Clinical Values", tabName = "fct_cli")
        ),
        menuItem(
            icon = icon("caret-right"), "Clinical Values",
            menuSubItem("Boxplot", tabName = "cli_boxplot")
        )
    )
)