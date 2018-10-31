output$bga_bga_Selector = renderUI({
    choices = featureNames(data$data$bga)
    selectInput("bga_bga", "Select A Biogenic Amine",
                choices = choices, selected = choices[1])
})

bga_bga_dt = reactive({
    data$corr$bga$bga[[input$bga.method]][[input$bga_bga]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$bga_bga_dt = renderDT(
    bga_bga_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
bga_bga_selector = reactive({
    rownames(bga_bga_dt())[input$bga_bga_dt_rows_selected]
})

output$bga_bga_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$bga$conc_table[bga_bga_selector(),]),
        y = data$data$bga$conc_table[input$bga_bga,],
        Treatment = data$data$bga$sample_table$Treatment,
        Timepoint = data$data$bga$sample_table$Timepoint,
        Subject = data$data$bga$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(bga_bga_selector(), " [", input$bga.norm, "]"), 
             y = input$bga_bga)
    ggplotly(p)
})