sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem("Home", icon = icon("home"), newtab = FALSE,
                 href = "http://www.chenghaozhu.net/studies/ffs/docs/hdl.html"),
        menuItem(
            icon = icon("caret-right"), "Lipidome", 
            menuSubItem("Boxplot", tabName = "lpd_boxplot"),
            menuSubItem("Histograms", tabName = "lpd_hist"),
            menuSubItem("Heatmap and PCA", tabName = "lpd_pca"),
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