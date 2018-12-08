output$VarsInput = renderUI({
    tab = input$sidebar
    downloadID = str_c(tab, "Download")
    
    if(tab == "lpd") {
        choices = names(data$data$lpd)
        tagList(
            selectInput(inputId = str_c(tab, "_level"), 
                        label = "Lipidomics Level",
                        choices = choices, selected = choices[1]),
            uiOutput("lpdNormSelector")
        )    
    }else if (tab == "glc") {
        choices = names(data$data$glc)
        tagList(
            selectInput(inputId = str_c(tab, "_level"), 
                        label = "Glycomics Level",
                        choices = choices, selected = choices[1])
        )    
    }
})

output$lpdNormSelector = renderUI({
    choices = names(data$data$lpd[[input$lpd_level]])
    selectInput(inputId = "lpd_norm", "Normalization Method",
                choices = choices, selected = choices[1])
})