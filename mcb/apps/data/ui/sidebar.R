sidebar = dashboardSidebar(
    uiOutput("VarsInput"),
    sidebarMenu(
        id = "sidebar",
        menuItem(icon = icon("caret-right"), "Microbiome", tabName = "mcb"),
        menuItem(icon = icon("caret-right"), "PICRUSt", tabName = "pcr"),
        menuItem(icon = icon("caret-right"), "Biogenic Amines", tabName = "bga"),
        menuItem(icon = icon("caret-right"), "Bile Acides", tabName = "bac"),
        menuItem(icon = icon("caret-right"), "Short Chain Fatty Acids", tabName = "sfa"),
        menuItem(icon = icon("caret-right"), "Diet", tabName = "diet")
    )
)