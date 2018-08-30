output$mcb_bga_Selector = renderUI({
    choices = featureNames(data$data$bga)
    selectInput("mcb_bga", "Select A Biogenic Amine",
                choices = choices, selected = choices[1])
})

mcb_bga_dt = reactive({
    data$corr$mcb$bga[[input$mcb.norm]][[input$mcb.level]][[input$mcb.method]][[input$mcb_bga]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$mcb_bga_dt = renderDT(
    mcb_bga_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
mcb_bga_selector = reactive({
    rownames(mcb_bga_dt())[input$mcb_bga_dt_rows_selected]
})

output$mcb_bga_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$mcb[[input$mcb.norm]][[input$mcb.level]]$conc_table[mcb_bga_selector(),]),
        y = data$data$bga$conc_table[input$mcb_bga,],
        Treatment = data$data$bga$sample_table$Treatment,
        Timepoint = data$data$bga$sample_table$Timepoint,
        Subject = data$data$bga$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(mcb_bga_selector(), " [", input$mcb.norm, "]"), 
             y = input$mcb_bga)
    ggplotly(p)
})