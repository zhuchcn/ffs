DashboardBody = R6Class(
    "DashboardBody",
    inherit = ShinyModule,
    public = list(
        # attributes
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href = "styles.css", rel = "stylesheet"),
                shinyjs::useShinyjs(),
                column(
                    width = 6,
                    box(
                        width = NULL,
                        dataTableOutput("table")
                    )
                ),
                column(
                    width = 6,
                    box(
                        width = NULL,
                        plotlyOutput("plot")
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session, props){
            output$table = renderDataTable({
                datatable(
                    MatCor(props$Y, props$X, method = "pearson")[[1]],
                    selection = list(mode = "single", selected = 1),
                    options = list(
                        order = list(4, "asc")
                    )
                ) %>%
                    formatSignif(columns = 1:4, digits = 4)
            })
            output$plot = renderPlotly({
                `x-var` = rownames(props$X)[input$table_rows_selected]
                data.frame(
                    x = as.numeric(props$X[`x-var`,]),
                    y = as.numeric(props$Y),
                    Treatment = data$fct$sample_table$Treatment,
                    Timepoint = data$fct$sample_table$Timepoint,
                    Subject = data$fct$sample_table$Subject
                ) %>%
                    ggplot(aes(Treatment = Treatment, Timepoint = Timepoint,
                               Subject = Subject)) +
                    geom_point(aes(x = x, y = y, color = Subject)) +
                    stat_smooth(
                        inherit.aes = FALSE, aes(x = x, y = y), method = "lm"
                    ) +
                    labs(x = `x-var`, y = rownames(props$Y)[1]) +
                    theme_bw()
            })
        }
    )
)