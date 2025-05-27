library(shiny)
library(tidyverse)

quant_to_cat <- function(x, cuts, labels = NULL, 
                         include.lowest = FALSE){
  cut(x, c(-Inf, cuts, Inf), labels = labels, include.lowest = include.lowest)
}

quant_to_cat_UI <- function(id){
  fillRow(flex = c(1, 1, 2), height = "100%",
          fillCol(width = "200px",
                  tagList(
                    textInput(NS(id, "new_name"), label = "New variable name", value = "new_var"),
                    numericInput(NS(id, "n_classes"), label = "Select the number of categories", value = 2, step = 1),
                    uiOutput(NS(id, "cuts"))
                  )),
          fillCol(width = "200px",
                  tagList(
                    checkboxInput(NS(id, "if_labels"), "Insert categories labels"),
                    uiOutput(NS(id, "labels_textIn")))
          ),
          fillCol(width = "400px",
                  div(
                    DT::dataTableOutput(NS(id, "tab_check")))
          )
  )
}

quant_to_cat_SERVER <- function(id, x){
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
    
    output$cuts <- renderUI({
      req(input$n_classes)
      tagList(
        purrr::map(1:(input$n_classes - 1),
                   ~numericInput(inputId = NS(id, paste0("class_", .x)),
                                 label = paste("Cut ", .x),
                                 value = NA)
        )
      )
    })
    
    cuts_quant_to_qual <- reactive({
      req(input$class_1)
      out <- numeric((input$n_classes - 1))
      for(k in 1:(input$n_classes - 1)){
        out[k] <- input[[paste0("class_", k)]]
      }
      out
    })
    
    output$labels_textIn <- renderUI({
      req(input$n_classes)
      if(input$if_labels){
        tagList(
          purrr::map(1:input$n_classes,
                     ~textInput(inputId = NS(id, paste0("label_", .x)),
                                label = paste("Label for category ", .x),
                                value = NA)
          )
        )
      }
    })
    
    labs_quant_to_qual <- reactive({
      req(input$label_1)
      out <- numeric(input$n_classes)
      for(k in 1:input$n_classes){
        out[k] <- input[[paste0("label_", k)]]
      }
      out
    })
    
    x_ric <- reactive({
      req(input$class_1)
      labs <- NULL
      if(length(input[[paste0("label_", input$n_classes)]])>0){
        labs <- labs_quant_to_qual()
      }
      out <- quant_to_cat(x(), cuts_quant_to_qual(), labels = labs)
      factor(out, levels = sort(levels(out)))
    })
    
    output$tab_check <-     DT::renderDT({
      req(input$class_1)
      req(input$new_name)
      data <- data.frame(
        orig_var = x(),
        b = x_ric()
      )
      names(data)[2] <- input$new_name
      DT::datatable(data, rownames = TRUE, fillContainer = TRUE, options = list(
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "700px",
        fixedColumns = FALSE)) 
    })
    
    reactive({
      out <- list(a = x_ric())
      names(out) <- input$new_name
      out
    })
    
  })}
