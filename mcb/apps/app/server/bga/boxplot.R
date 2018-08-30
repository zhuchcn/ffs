bga_diff = data$diff$bga %>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")

output$bga_diff = renderDT(
    bga_diff, 
    selection = list(mode = "single", selected = 1),
    server=T
)

bga_boxplot_selector = reactive({
    rownames(bga_diff)[input$bga_diff_rows_selected]
})

output$bga_boxplot = renderPlotly({
    p = Metabase::plot_boxplot(
        data$data$bga, 
        feature = bga_boxplot_selector(),
        x = "Timepoint", 
        cols = "Treatment",
        line = "Subject",
        color = "Subject",
        color.pal = pal_jama()(7)
    ) +
        labs(x = "")
    ggplotly(p)
})