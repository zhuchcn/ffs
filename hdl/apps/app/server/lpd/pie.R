mset = data$data$lpd$class$Proportion
df = mset$conc_table %>%
    t %>% as.data.frame %>%
    rownames_to_column("id") %>%
    melt(id.var = "id") %>%
    group_by(variable) %>%
    summarize(mean = mean(value)) %>%
    arrange(desc(mean)) %>%
    mutate(variable = factor(variable, levels = variable),
           offset = rev(cumsum(rev(df$mean))) - df$mean / 2)

output$lpd_pie = renderPlot({
    set.seed(25)
    ggplot(df, aes(x = 1, y = mean, fill = variable)) +
        geom_bar(stat = "identity", color = "white") +
        geom_text(data=df[1:6,], 
                  aes(x = 1.25, y = offset, 
                      label = paste(format(mean * 100, digits = 2) , "%")),
                  color = "white", check_overlap = FALSE) +
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