DashboardSidebar = R6Class(
    "DashboardSidebar",
    inherit = ShinyModule,
    public = list(
        # attributes
        variableChoices = list(
            "HDL Function" = "fct",
            "HDL Lipidome" = "lpd",
            "HDL Glycopeptides" = "glc",
            "Clinical Values" = "cli",
            "Microbiome" = "mcb",
            "Biogenic Amines" = "bga",
            "Bile Acids" = "bac",
            "Short Chain Fatty Acids" = "sfa"
        ),
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            dashboardSidebar(
                sidebarMenu(
                    selectInput(
                        inputId = "x",
                        label = "X-axis Variable",
                        choices = self$variableChoices,
                        selected = "mcb"
                    ),
                    selectInput(
                        inputId = "y",
                        label = "Y-axis Variable",
                        choices = self$variableChoices,
                        selected = "fct"
                    ),
                    uiOutput("params"),
                    uiOutput("y-var"),
                    actionButton(
                        inputId = "submit",
                        label = "Submit",
                        class = "btn-primary",
                        width = "87.5%"
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            states = reactiveValues(
                X = NULL,
                Y = NULL
            )
            
            output$params = renderUI({
                vars = c(input$x, input$y)
                inputs = tagList()
                if("mcb" %in% vars){
                    inputs = tagAppendChildren(
                        inputs,
                        selectInput(
                            inputId = "mcb-norm",
                            label = "Count or Proportion?",
                            choices = c("count", "proportion"),
                            selected = "proportion"
                        ),
                        selectInput(
                            inputId = "mcb-level",
                            label = "Phylogenic Level",
                            choices = names(data$mcb$count),
                            selected = "kingdom"
                        )
                    )
                }
                if("lpd" %in% vars){
                    inputs = tagAppendChildren(
                        inputs,
                        selectInput(
                            inputId = "lpd-level",
                            label = "Lipidomics level",
                            choices = names(data$lpd),
                            selected = "class"
                        )
                    )
                }
                if("glc" %in% vars){
                    inputs = tagAppendChildren(
                        inputs,
                        selectInput(
                            inputId = "glc-level",
                            label = "Peptide or Glycopeptide?",
                            choices = c("peptide", "glycans"),
                            selected = "glycans"
                        )
                    )
                }
                if("bca" %in% vars){
                    inputs = tagAppendChildren(
                        inputs,
                        selectInput(
                            inputId = "bca-level",
                            labels = c("Raw or Summarized Bile Acids?"),
                            choices = c("raw", "summarized"),
                            selected = "raw"
                        )
                    )
                }
                inputs
            })
            
            output$`y-var` = renderUI({
                y_mset = data[[input$y]]
                y_mset = switch(
                    input$y,
                    "mcb" = y_mset[[input$`mcb-norm`]][[input$`mcb-level`]],
                    "lpd" = y_mset[[input$`lpd-level`]],
                    "glc" = y_mset[[input$`glc-level`]],
                    "bca" = y_mset[[input$`bca-level`]],
                    y_mset
                )
                selectInput(
                    inputId = "y-var",
                    label = "Select the Y Variable",
                    choices = featureNames(y_mset)
                )  
            })
            
            observeEvent(input$submit, {
                x_mset = data[[input$x]]
                x_mset = switch(
                    input$x,
                    "mcb" = x_mset[[input$`mcb-norm`]][[input$`mcb-level`]],
                    "lpd" = x_mset[[input$`lpd-level`]],
                    "glc" = x_mset[[input$`glc-level`]],
                    "bca" = x_mset[[input$`bca-level`]],
                    x_mset
                )
                states$X = x_mset$conc_table
                y_mset = data[[input$y]]
                y_mset = switch(
                    input$y,
                    "mcb" = y_mset[[input$`mcb-norm`]][[input$`mcb-level`]],
                    "lpd" = y_mset[[input$`lpd-level`]],
                    "glc" = y_mset[[input$`glc-level`]],
                    "bca" = y_mset[[input$`bca-level`]],
                    y_mset
                )
                states$Y = y_mset$conc_table[input$`y-var`, , drop = FALSE]
            })
            
            return(states)
        }
    )
)