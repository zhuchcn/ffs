glc_limma_table = reactive({
    data$limma$glc[[input$glc.level]]
})

glc_limma = reactive({
    glc_limma_table()%>%
        rownames_to_column("Feature") %>%
        arrange(P.Value) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
}) 

output$glc_limma = renderDT(
    glc_limma(), 
    selection = list(mode = "single", selected = 1),
    server=T
)

glc_boxplot_selector = reactive({
    rownames(glc_limma())[input$glc_limma_rows_selected]
})

output$glc_boxplot = renderPlotly({
    mset = data$data$glc[[input$glc.level]]
    p = plot_boxplot(mset, 
                     x = "Timepoint", 
                     feature = glc_boxplot_selector(),
                     cols = "Treatment",
                     line = "Subject",
                     color = "Subject",
                     color.pal = pal_jama()(7)) +
        labs(x = "")
    ggplotly(p)
})