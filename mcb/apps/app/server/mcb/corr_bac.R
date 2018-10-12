output$mcb_bac_Selector = renderUI({
    choices = featureNames(data$data$bac)
    selectInput("mcb_bac", "Select A Biogenic Amine",
                choices = choices, selected = choices[1])
})

mcb_bac_dt = reactive({
    data$corr$mcb$bac[[input$mcb.norm]][[input$mcb.level]][[input$mcb.method]][[input$mcb_bac]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$mcb_bac_dt = renderDT(
    mcb_bac_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
mcb_bac_selector = reactive({
    rownames(mcb_bac_dt())[input$mcb_bac_dt_rows_selected]
})

output$mcb_bac_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$mcb[[input$mcb.norm]][[input$mcb.level]]$conc_table[mcb_bac_selector(),]),
        y = data$data$bac$conc_table[input$mcb_bac,],
        Treatment = data$data$bac$sample_table$Treatment,
        Timepoint = data$data$bac$sample_table$Timepoint,
        Subject = data$data$bac$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = paste0(mcb_bac_selector(), " [", input$mcb.norm, "]"), 
             y = input$mcb_bac)
    ggplotly(p)
})