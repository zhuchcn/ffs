load("server/mcb/clado.rda")

output$mcb_clado_FF = renderPlot({p.FF})
output$mcb_clado_Med = renderPlot({p.Med})
output$mcb_clado_mix = renderPlot({p.mix})
