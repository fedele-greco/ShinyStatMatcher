library(shiny)
library(shinyWidgets)
library(tidyverse)

somma_vec <- function(df, variables){
  out <- numeric(nrow(df))
    out <-  rowSums(df, na.rm = TRUE)
  out
}

somma_vec_UI <- function(id){
  fillRow(flex = c(1, 2), height = "100%",
          fillCol(width = "80%", height = "100%",
                  div(
                    textInput(NS(id, "new_name"), "New variable name", value = "new_name"),
                    uiOutput(NS(id, "ui_rec")))
          ),
          fillCol(width = "90%", height = "100%",
                  DT::dataTableOutput(NS(id, "tab_check"), height = "40em")
          )
  )
}

somma_vec_SERVER <- function(id, x){
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
  
    
    x_ric <- reactive({
      somma_vec(df = x(), variables = names(x()))
    })
    
    output$tab_check <- DT::renderDT({
      req(input$new_name)
      data <- data.frame(
        a = x_ric(),
        x()
      )

      names(data)[1] <- input$new_name
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
