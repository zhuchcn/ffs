output$glc_fct_Selector = renderUI({
    selectInput("glc_fct", "Select HDL Function Variable",
                choices = featureNames(data$data$fct),
                selected = featureNames(data$data$fct)[1])
})

glc_fct_dt = reactive({
    data$corr$glc$fct[[input$glc.level]][[input$glc.method]][[input$glc_fct]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$glc_fct_dt = renderDT(
    glc_fct_dt(), 
    selection = list(mode = "single", selected = 1),
    server=T
)
glc_fct_selector = reactive({
    rownames(glc_fct_dt())[input$glc_fct_dt_rows_selected]
})

output$glc_fct_scatter = renderPlotly({
    df = data.frame(
        x = data$data$glc[[input$glc.level]]$conc_table[glc_fct_selector(),],
        y = data$data$fct$conc_table[input$glc_fct,],
        Treatment = data$data$fct$sample_table$Treatment,
        Timepoint = data$data$fct$sample_table$Timepoint,
        Subject = data$data$fct$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(glc_fct_selector()), 
             y = input$glc_fct)
    ggplotly(p)
})