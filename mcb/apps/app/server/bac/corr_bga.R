output$bac_bga_Selector = renderUI({
    choices = featureNames(data$data$bga)
    selectInput("bac_bga", "Select A Short Chain Fatty Acid",
                choices = choices, selected = choices[1])
})

bac_bga_dt = reactive({
    data$corr$bac$bga[[input$bac.level]][[input$bac.method]][[input$bac_bga]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$bac_bga_dt = renderDT(
    bac_bga_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
bac_bga_selector = reactive({
    rownames(bac_bga_dt())[input$bac_bga_dt_rows_selected]
})

output$bac_bga_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$bac[[input$bac.level]]$conc_table[bac_bga_selector(),]),
        y = data$data$bga$conc_table[input$bac_bga,],
        Treatment = data$data$bga$sample_table$Treatment,
        Timepoint = data$data$bga$sample_table$Timepoint,
        Subject = data$data$bga$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = bac_bga_selector(), 
             y = input$bac_bga)
    ggplotly(p)
})