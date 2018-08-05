mset = data$data$lpd$class$Proportion
df = mset$conc_table %>%
    t %>% as.data.frame %>%
    # mutate(
    #     Treatment = mset$sample_table$Treatment,
    #     Timepoint = mset$sample_table$Timepoint,
    #     Subject   = mset$sample_table$Subject
    # ) %>%
    # melt(id.var = c("Treatment", "Timepoint", "Subject")) %>%
    # group_by(Treatment,Timepoint, variable) %>%
    rownames_to_column("id") %>%
    melt(id.var = "id") %>%
    group_by(variable) %>%
    summarize(mean = mean(value)) %>%
    arrange(desc(mean)) %>%
    mutate(variable = factor(variable, levels = variable))

output$lpd_pie = renderPlot({
    set.seed(25)
    ggplot(df, aes(x = 1, y = mean, fill = variable)) +
        geom_bar(stat = "identity", color = "white") +
        geom_text(data=df[1:3,], 
                  aes(x = 1+0.2, y = mean, 
                      label = paste(format(mean * 100, digits = 4) , "%")),
                  color = "white") +
        coord_polar("y") +
        #facet_grid(rows = "Treatment") +
        scale_fill_manual(values = colorRampPalette(pal_npg()(9))(9)[sample(1:9)]) +
        theme_bw() +
        theme(
            # legend
            legend.title = element_blank(),
            # plot
            panel.border = element_blank(),
            panel.grid = element_blank(),
            # scales
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()
        )
})