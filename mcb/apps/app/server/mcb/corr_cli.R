output$mcb_cli_Selector = renderUI({
    choices = featureNames(data$data$cli)
    selectInput("mcb_cli", "Select A Biogenic Amine",
                choices = choices, selected = choices[1])
})

mcb_cli_dt = reactive({
    data$corr$mcb$cli[[input$mcb.norm]][[input$mcb.level]][[input$mcb.method]][[input$mcb_cli]] %>%
        rownames_to_column("Feature") %>%
        arrange(pval) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$mcb_cli_dt = renderDT(
    mcb_cli_dt(), 
    selection = list(mode = "single", selected = 1),
    server = T
)
mcb_cli_selector = reactive({
    rownames(mcb_cli_dt())[input$mcb_cli_dt_rows_selected]
})

output$mcb_cli_scatter = renderPlotly({
    df = data.frame(
        x = as.numeric(data$data$mcb[[input$mcb.norm]][[input$mcb.level]]$conc_table[mcb_cli_selector(),]),
        y = data$data$cli$conc_table[input$mcb_cli,],
        Treatment = data$data$cli$sample_table$Treatment,
        Timepoint = data$data$cli$sample_table$Timepoint,
        Subject = data$data$cli$sample_table$Subject
    )
    p = ggscatterplot(df, "x", "y", color = "Subject", color.pal = pal_jama()(7)) +
        labs(x = mcb_cli_selector(), 
             y = input$mcb_cli)
    ggplotly(p)
})