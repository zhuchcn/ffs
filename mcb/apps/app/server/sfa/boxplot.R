sfa_diff = data$diff$sfa %>%
    rownames_to_column("Feature") %>%
    arrange(pvalue) %>%
    sapply(function(col){
        if(!is.numeric(col)) return(col)
        round(col, digits = 3)
    }) %>%
    as.data.frame %>%
    column_to_rownames("Feature")

output$sfa_diff = renderDT(
    sfa_diff, 
    selection = list(mode = "single", selected = 1),
    server=T
)

sfa_boxplot_selector = reactive({
    rownames(sfa_diff)[input$sfa_diff_rows_selected]
})

output$sfa_boxplot = renderPlotly({
    p = Metabase::plot_boxplot(
        data$data$sfa, 
        feature = sfa_boxplot_selector(),
        x = "Timepoint", 
        cols = "Treatment",
        line = "Subject",
        color = "Subject",
        color.pal = pal_jama()(7)
    ) +
        labs(x = "")
    ggplotly(p)
})