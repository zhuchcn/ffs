lpdset = data$data$lpd$feature$Proportion
hc.lpd = hclust(dist(t(apply(lpdset$conc_table, 1, scale))))

output$lpd_hc = renderPlot({
    ggplot(as.dendrogram(hc.lpd, hang = 0.1)) +
        geom_hline(yintercept = input$lpd.hc.height, color = "red") +
        scale_y_continuous(limits = c(-2,12)) +
        theme(axis.line.y = element_line(),
              axis.ticks.y = element_line(),
              axis.text.y = element_text())
})

hclusters = reactive({cutree(hc.lpd, h = input$lpd.hc.height)})

lpd_hc = reactive({
    mset = data$data$lpd$feature$Proportion
    mset$feature_data$hclusters = hclusters()
    summarize_features(mset, "hclusters")
})

lpd_hc_limma = reactive({
    design = model.matrix(data = as(lpdset$sample_table, "data.frame"),
                          ~ Treatment*Timepoint + Subject + 1)
    mSet_limma(lpd_hc(), design, coef = 13, p.value = 13)
})

lpd_hc_dt = reactive({
    lpd_hc_limma() %>%
        rownames_to_column("Feature") %>%
        arrange(pvalue) %>%
        sapply(function(col){
            if(!is.numeric(col)) return(col)
            round(col, digits = 3)
        }) %>%
        as.data.frame %>%
        column_to_rownames("Feature")
})

output$lpd_hc_dt = renderDT(
    lpd_hc_dt(),
    selection = list(mode = "single", selected = 1),
    server=T
)

lpd_hc_boxplot_selector = reactive({
    rownames(lpd_hc_dt())[input$lpd_hc_dt_rows_selected]
})

output$lpd_hc_scatter = renderPlotly({
    df = lpd_hc()$conc_table %>%
        t %>% as.data.frame %>%
        mutate(
            Treatment = lpd_hc()$sample_table$Treatment,
            Timepoint = lpd_hc()$sample_table$Timepoint,
            Subject   = lpd_hc()$sample_table$Subject
        ) %>%
        melt(id.var = c("Treatment", "Timepoint", "Subject"),
             variable.name = "cluster") %>%
        dcast(cluster + Subject + Treatment ~ Timepoint) %>%
        mutate(change = Post - Pre) %>%
        group_by(cluster, Treatment) %>%
        summarize(change = mean(change)) %>%
        dcast(cluster~Treatment) %>%
        mutate(pvalue = lpd_hc_limma()$pvalue)
    p = ggplot(df) +
        geom_point(aes(x = FF, y = Med, 
                       color = pvalue <= 0.05, 
                       cluster = cluster, pvalue = pvalue),
                   size = 4, alpha = 0.5) +
        geom_point(data = df[df$cluster == lpd_hc_boxplot_selector(),],
                   aes(x = FF, y = Med, cluster = cluster),
                   color = "#3C5488FF", size = 4) +
        guides(color = guide_legend(title = "P<0.05")) +
        theme_bw() 
    ggplotly(p)
})

output$lpd_hc_box = renderPlotly({
    p = plot_boxplot(lpd_hc(), x = "Timepoint", 
                     feature = lpd_hc_boxplot_selector(),
                     cols = "Treatment", color = "Subject", line = "Subject",
                     color.pal = pal_npg()(10))
    ggplotly(p)
})

output$lpd_hc_members = renderTable({
    members = names(hclusters())[hclusters() == lpd_hc_boxplot_selector()]
    length(members) = ceiling(length(members)/4) * 4
    mat = matrix(members, ncol = 4, byrow = TRUE)
    mat
}, colnames = FALSE, na = "", hover = TRUE, striped = TRUE, width = "90%")