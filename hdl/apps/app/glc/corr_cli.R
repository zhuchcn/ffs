output$glc_cli_Selector = renderUI({
    selectInput("glc_cli", "Select HDL Function Variable",
                choices = featureNames(data$data$cli),
                selected = featureNames(data$data$cli)[1])
})

glc_cli_dt = reactive({
    data$corr$glc$cli[[input$glc.level]][[input$glc.method]][[input$glc_cli]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$glc_cli_dt = renderDT(
    glc_cli_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
glc_cli_selector = reactive({
    rownames(glc_cli_dt())[input$glc_cli_dt_rows_selected]
})

output$glc_cli_scatter = renderPlotly({
    df = data.frame(
        x = data$data$glc[[input$glc.level]]$conc_table[glc_cli_selector(),],
        y = data$data$cli$conc_table[input$glc_cli,],
        Treatment = data$data$cli$sample_table$Treatment,
        Timepoint = data$data$cli$sample_table$Timepoint,
        Subject = data$data$cli$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(glc_cli_selector()), 
             y = input$glc_cli)
    ggplotly(p)
})