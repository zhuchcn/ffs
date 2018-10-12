fitLimma = function(object, tx){
    mset = subset_samples(object, object$sample_table$Treatment == tx)
    design = model.matrix(
        data = as(mset$sample_table, "data.frame"),
        ~ Timepoint + Subject + 1
    )
    mSet_limma(mset, design, coef=2, p.value = 2)
}

output$mcb_genus_scatter = renderPlot({
    genus = data$data$mcb$proportion$genus
    lm_ff =  fitLimma(genus, "FF")
    lm_med = fitLimma(genus, "Med")
    df = data.frame(
        feature = rownames(lm_ff),
        fc_ff = lm_ff$logFC,
        fc_med = lm_med$logFC,
        p_ff = lm_ff$pvalue,
        p_med = lm_med$pvalue,
        p_mix = data$diff$mcb$limma$genus$pvalue
    )%>%
        mutate(
            Sig = ifelse(
                p_mix <= 0.05, "Mix Model",
                ifelse((p_ff <= 0.05 & p_med <= 0.05), "Both TX",
                       ifelse(p_ff <= 0.05, "FF",
                              ifelse(p_med <= 0.05, "Med", "none")))
            )
        ) %>%
        mutate(
            Sig = factor(Sig, levels = c("Mix Model","Both TX","Med","FF","none"))
        ) %>%
        arrange(desc(Sig))
    ggplot(df, aes(fc_ff, fc_med, feature = feature,
                   p_ff = p_ff, p_med = p_med,
                   p_mix = p_mix)) +
        geom_label_repel(
            data = df[df$Sig != "none",],
            aes(label = feature))+
        geom_point(aes(color = Sig), size=4)  +
        scale_color_manual(
            values = c(
                alpha(pal_lancet()(9)[1:4], 0.8), 
                alpha(pal_lancet()(9)[5],0.5))) +
        guides(col = guide_legend(title = "Significant\n(P<0.05) in")) +
        labs (
            x = "Fold Change(log)\nFast Food",
            y = "Fold Change(log)\nMediterranean"
        ) +
        theme_bw() +
        theme(
            # legend
            legend.title = element_text(size = 13),
            legend.text = element_text(size=12),
            legend.position = c(0.888,0.7),
            legend.background = element_rect(fill="#FFFFFF", color="#444444",
                                             size=0.5,linetype = "solid"),
            # axis
            axis.title = element_text(size=15),
            axis.title.x = element_text(vjust=-2),
            axis.title.y = element_text(vjust=2),
            axis.text = element_text(size=11, color="#000000"),
            # margin
            plot.margin = margin(t=10,b=15,l=15,r=5)
        )
})
