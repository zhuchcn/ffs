pcr_diff_table = reactive({
    data$diff$pcr[[input$pcr.level]]
})

pcr_diff = reactive({
    pcr_diff_table()%>%
        as.data.frame %>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
}) 

output$pcr_diff = renderDT(
    pcr_diff(), 
    selection = list(mode = "single", selected = 1),
    server=T
)

pcr_boxplot_selector = reactive({
    rownames(pcr_diff())[input$pcr_diff_rows_selected]
})

output$pcr_boxplot = renderPlotly({
    p = Metabase::plot_boxplot(
        data$data$pcr[[input$pcr.level]], 
        feature = pcr_boxplot_selector(),
        x = "Timepoint", 
        cols = "Treatment",
        line = "Subject",
        color = "Subject",
        color.pal = pal_jama()(7)
    ) +
        labs(x = "")
    ggplotly(p)
})