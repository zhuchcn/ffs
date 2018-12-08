output$VarsInput = renderUI({
    tab = input$sidebar
    
    if(tab == "mcb") {
        choices = names(data$data$mcb)
        tagList(
            selectInput(inputId = str_c(tab, "_norm"), 
                        label = "Microbiome Normalization:",
                        choices = choices, selected = choices[1]),
            uiOutput("mcbLevelSelector")
        )    
    }else if (tab == "pcr") {
        choices = names(data$data$pcr)
        tagList(
            selectInput(inputId = str_c(tab, "_level"), 
                        label = "PICRUSt Function Level",
                        choices = choices, selected = choices[1])
        )    
    }else if (tab == 'bac') {
        choices = names(data$data$bac)
        tagList(
            selectInput(inputId = str_c(tab, "_level"), 
                        label = "Bile Acid Raw data or Summairzed",
                        choices = choices, selected = choices[1])
        )
    }
})

output$mcbLevelSelector = renderUI({
    choices = names(data$data$mcb[[input$mcb_norm]])
    selectInput(inputId = "mcb_level", "Phynogenic Levels",
                choices = choices, selected = choices[1])
})