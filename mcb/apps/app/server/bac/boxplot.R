bac_diff = reactive({
    data$diff$bac[[input$bac.level]] %>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$bac_diff = renderDT(
    bac_diff(), 
    selection = list(mode = "single", selected = 1),
    server=T
)

bac_boxplot_selector = reactive({
    rownames(bac_diff())[input$bac_diff_rows_selected]
})

output$bac_boxplot = renderPlotly({
    p = Metabase::plot_boxplot(
        data$data$bac[[input$bac.level]], 
        feature = bac_boxplot_selector(),
        x = "Timepoint", 
        cols = "Treatment",
        line = "Subject",
        color = "Subject",
        color.pal = pal_jama()(7)
    ) +
        labs(x = "")
    ggplotly(p)
})