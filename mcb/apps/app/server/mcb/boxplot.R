mcb_diff_table = reactive({
    norm = if(input$mcb.norm == "count") "deseq2" else "limma"
    data$diff$mcb[[norm]][[input$mcb.level]]
})

mcb_diff = reactive({
    mcb_diff_table()%>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
}) 

output$mcb_diff = renderDT(
    mcb_diff(), 
    selection = list(mode = "single", selected = 1),
    server=T
)

mcb_boxplot_selector = reactive({
    rownames(mcb_diff())[input$mcb_diff_rows_selected]
})

output$mcb_boxplot = renderPlotly({
    p = Metabase::plot_boxplot(
        data$data$mcb[[input$mcb.norm]][[input$mcb.level]], 
        feature = mcb_boxplot_selector(),
        x = "Timepoint", 
        cols = "Treatment",
        line = "Subject",
        color = "Subject",
        color.pal = pal_jama()(7)
    ) +
        labs(x = "")
    ggplotly(p)
})