library(shiny)
library(shinyWidgets)
library(tidyverse)

check_se_vec <- function(df, variables, value, type_out = "Numeric"){
  nlvls <- length(value)
  out <- numeric(nrow(df))
  for(k in 1:nlvls){
    out <- out + rowSums(df == value[k], na.rm = TRUE)
  }
  allna <- rowMeans(is.na(df))==1
  out[allna] <- NA
  out <- (out > 0) * 1
  if(type_out == "Numeric"){
    out <- out
  }
  if(type_out == "Factor"){
    out <- as.factor(out)
  }
  out
}

check_se_vec_UI <- function(id){
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

check_se_vec_SERVER <- function(id, x){
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
    
    output$ui_rec <- renderUI({
      vals <- sort(unique(unlist(map(x(), unique))))
      tagList(
        pickerInput(NS(id, "quale_val"), label = "", choices = vals, multiple = TRUE,
                    options = pkr_opt),
        prettyRadioButtons(
          inputId = NS(id, "type"),
          label = "Type of the output variable", 
          choices = c("Factor", "Numeric"),
          inline = TRUE, selected = "Factor"
        ))
    })
    
    x_ric <- reactive({
      req(input$quale_val)
      check_se_vec(df = x(), variables = names(x()), value = input$quale_val, type_out = input$type)
    })
    
    output$tab_check <- DT::renderDT({
      req(input$quale_val)
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
