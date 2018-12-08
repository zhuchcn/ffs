sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(icon = icon("caret-right"), "Lipidome", tabName = "lpd"),
        menuItem(icon = icon("caret-right"), "Glycopeptides", tabName = "glc"),
        menuItem(icon = icon("caret-right"), "HDL Functions", tabName = "fct"),
        menuItem(icon = icon("caret-right"), "Clinical Values", tabName = "cli")
    )
)