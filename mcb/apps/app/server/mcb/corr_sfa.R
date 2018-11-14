output$mcb_sfa_Selector = renderUI({
    choices = featureNames(data$data$sfa)
    selectInput("mcb_sfa", "Select A Biogenic Amine",
                choices = choices, selected = choices[1])
})

mcb_sfa_dt = reactive({
    data$corr$mcb$sfa[[input$mcb.norm]][[input$mcb.level]][[input$mcb.method]][[input$mcb_sfa]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$mcb_sfa_dt = renderDT(
    mcb_sfa_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
mcb_sfa_selector = reactive({
    rownames(mcb_sfa_dt())[input$mcb_sfa_dt_rows_selected]
})

output$mcb_sfa_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$mcb[[input$mcb.norm]][[input$mcb.level]]$conc_table[mcb_sfa_selector(),]),
        y = data$data$sfa$conc_table[input$mcb_sfa,],
        Treatment = data$data$sfa$sample_table$Treatment,
        Timepoint = data$data$sfa$sample_table$Timepoint,
        Subject = data$data$sfa$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = mcb_sfa_selector(), 
             y = input$mcb_sfa)
    ggplotly(p)
})