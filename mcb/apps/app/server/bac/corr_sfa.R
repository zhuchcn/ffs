output$bac_sfa_Selector = renderUI({
    choices = featureNames(data$data$sfa)
    selectInput("bac_sfa", "Select A Short Chain Fatty Acid",
                choices = choices, selected = choices[1])
})

bac_sfa_dt = reactive({
    data$corr$bac$sfa[[input$bac.level]][[input$bac.method]][[input$bac_sfa]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$bac_sfa_dt = renderDT(
    bac_sfa_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
bac_sfa_selector = reactive({
    rownames(bac_sfa_dt())[input$bac_sfa_dt_rows_selected]
})

output$bac_sfa_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$bac[[input$bac.level]]$conc_table[bac_sfa_selector(),]),
        y = data$data$sfa$conc_table[input$bac_sfa,],
        Treatment = data$data$sfa$sample_table$Treatment,
        Timepoint = data$data$sfa$sample_table$Timepoint,
        Subject = data$data$sfa$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = bac_sfa_selector(), 
             y = input$bac_sfa)
    ggplotly(p)
})