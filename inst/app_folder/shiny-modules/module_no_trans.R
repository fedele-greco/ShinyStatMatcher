library(shiny)
library(tidyverse)

# 2. ricodifica delle modalità di una variabile qualitativa
func_no_trans <- function(x){
    out <- x
    return(out)
}

no_trans_UI <- function(id){
  fillRow(flex = c(1, 1), height = "100%",
          fillCol(width = "400px", height = "100%",
                  div(
                    textInput(NS(id, "new_name"), "New variable name", value = "new_name"),
                    uiOutput(NS(id, "ui_rec"))
                  )
          ),
          fillCol(width = "400px", height = "100%",
                  DT::dataTableOutput(NS(id, "tab_check"), height = "40em")
          )
  )
}

no_trans_SERVER <- function(id, x){
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
    
    x_ric <- reactive({
      req(input$new_name)
      func_no_trans(x())
    })
    
    output$tab_check <- DT::renderDT({
      req(input$new_name)
      data <- data.frame(
        orig_var = x(),
        b = x_ric()
      )
      names(data)[2] <- input$new_name
      
      DT::datatable(data, rownames = FALSE, fillContainer = TRUE, options = list(
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
    
  })
}