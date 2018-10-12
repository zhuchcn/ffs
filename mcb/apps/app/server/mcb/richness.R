estimateRichness = function(){
    ps = Metabase::as_phyloseq(data$data$mcb$count$otu)
    estimate_richness(ps)
}
mcb.richness = estimateRichness()

richnessLimma = function(){
    mset = Metabase::MultxSet(
        conc_table = Metabase::conc_table(t(mcb.richness)),
        sample_table = data$data$mcb$count$otu$sample_table
    )
    mset$sample_table$Timepoint = factor(mset$sample_table$Timepoin, 
                                        levels = c("Pre", "Post"))
    design = model.matrix(
        data = as(mset$sample_table, "data.frame"),
        ~ Treatment * Timepoint + Subject + 1
    )
    mSet_limma(mset, design, coef = 13, p.value = 13)
}

mcb.richness_limma = richnessLimma()

output$mcb_richness_limma = renderDT(
    mcb.richness_limma %>%
        rownames_to_column("feature") %>%
        sapply(function(x){
            if(is.numeric(x)) round(x, 3) else x
        }) %>%
        as.data.frame() %>%
        arrange(pvalue) %>%
        column_to_rownames("feature"),
    selection = list(mode = "single", selected = 1),
    server=T
)

plotAlphaDiversity = function(input){
    method = rownames(mcb.richness_limma)[input$mcb_richness_limma_rows_selected]
    ps = Metabase::as_phyloseq(data$data$mcb$count$otu)
    df = data.frame(
        richness = as.numeric(mcb.richness[,method]),
        Timepoint = sam_data(ps)$Timepoint,
        Treatment = sam_data(ps)$Treatment,
        Subject = sam_data(ps)$Subject
    )
    p = ggplot(df, aes(x = Timepoint, y = richness)) +
        geom_boxplot() +
        geom_point(aes(color = Subject), size = 3) +
        geom_line(aes(group = Subject, color = Subject)) +
        facet_grid(cols = vars(Treatment)) +
        scale_color_npg() +
        labs(x = "", y = "", title = method ) +
        theme_bw()
    ggplotly(p)
}

output$mcb_richness_plot = renderPlotly({plotAlphaDiversity(input)})