source('global.R')

App = R6Class(
    "App",
    inherit = ShinyModule,
    public = list(
        # attributes
        sidebar = DashboardSidebar$new(),
        body = DashboardBody$new(),
        
        # UI
        ui = function(){
            dashboardPage(
                header = dashboardHeader(title = "Fast Food Study"),
                sidebar = self$sidebar$ui(),
                body = self$body$ui()
            )
        },
        
        # server
        server = function(input, output, session){
            sidebarInputs = self$sidebar$call()
            self$body$call(props = sidebarInputs)
            
            shinyjs::removeClass(class = "btn-default", selector = ".btn")
        }
    )
)

app = App$new()

shiny::shinyApp(app$ui(), app$server)