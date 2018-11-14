output$bga_sfa_Selector = renderUI({
    choices = featureNames(data$data$sfa)
    selectInput("bga_sfa", "Select A Short Chain Fatty Acid",
                choices = choices, selected = choices[1])
})

bga_sfa_dt = reactive({
    data$corr$bga$sfa[[input$bga.method]][[input$bga_sfa]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$bga_sfa_dt = renderDT(
    bga_sfa_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
bga_sfa_selector = reactive({
    rownames(bga_sfa_dt())[input$bga_sfa_dt_rows_selected]
})

output$bga_sfa_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$bga$conc_table[bga_sfa_selector(),]),
        y = data$data$sfa$conc_table[input$bga_sfa,],
        Treatment = data$data$bga$sample_table$Treatment,
        Timepoint = data$data$bga$sample_table$Timepoint,
        Subject = data$data$bga$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(bga_sfa_selector(), " [", input$bga.norm, "]"), 
             y = input$bga_bga)
    ggplotly(p)
})