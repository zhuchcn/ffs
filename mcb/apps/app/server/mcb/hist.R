gghist = function(data, x){
    ggplot(data) +
        geom_histogram(aes_string(x), breaks = seq(0,1,0.025),
                       color = "white") +
        geom_vline(xintercept = 0.05, color = "red", linetype = "dashed",
                   size = 1) +
        theme_bw()
}

output$mcb_hist_pval = renderPlotly({
    p = gghist(mcb_diff_table(), "pvalue")
    ggplotly(p)
})

output$mcb_hist_padj = renderPlotly({
    p = gghist(mcb_diff_table(), "padj")
    ggplotly(p)
})

output$mcb_volcano = renderPlotly({
    p = mcb_diff_table() %>%
        rownames_to_column("Feature") %>%
        # mutate(`P < 0.05` = pvalue < 0.05) %>%
        ggplot() +
        geom_point(aes(x = logFC, y = -log(pvalue), Feature = Feature, 
                       pvalue = pvalue, padj = padj),
                   shape = 21, size=2, color = "white", fill = "gray19", alpha = 0.75) +
        geom_hline(yintercept = 2.99, color = "red", linetype = "dashed") +
        scale_color_lancet() +
        theme_bw()
    ggplotly(p)
})