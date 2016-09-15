# Insert file header for R script
# todo
#

insertHeaderAddin <- function() {
  insertHeaderGadget(getActiveDoc())
}

init_template <- function() {
	require(stringr)
	fn <- file.path(system.file('', package = 'RSExpert'), 'config/CodeTemplates.txt')
	temp <- readLines(fn, warn = F)
	idx <- grep('^---', temp)
	
	if (length(idx) == 0) return(NULL)
	
	temp_names <- str_match(temp[idx], '^---(.*)')[, 2]	  
	temp <- select_template(temp, idx, 1)
	vars <- str_match_all(temp, '\\%(.+?)\\%')
	vars <- do.call(rbind, vars)[, 2]
	list(vars=vars, template=temp) # temp: char vector of template text
}

select_template <- function(text, indices, idx) {
  i <- indices[idx] + 1
  j <- if (idx == length(indices)) length(text) else indices[idx + 1]
  text[seq(i, j)]
}

insertHeaderGadget <- function(doc) {
	require(shiny)
	require(miniUI)

	ui <- miniUI::miniPage(
    gadgetTitleBar("Insert Header", right = miniTitleBarButton("done", "Execute", primary = TRUE)),
    miniContentPanel(
  		wellPanel(
				tags$h4("Templates"),				
				uiOutput("inputGroup")
				)
    )
  )

  server <- function(input, output, session) {
    ret <- init_template()
    if (is.null(ret)) {
      warning('Please define templates.')
      stopApp()
    }
    
    nvars <- length(ret$vars)
    
  	observeEvent(input$done, {
  	  
  	  for (i in 1:nvars) {
  		  varname <- ret$vars[i]
  		  ret$template <- sub(sprintf('%%%s%%', varname), input[[varname]], ret$template)
  	  }
  	  
  	  codes <- do.call(rbind, str_match_all(ret$template, '`(.+?)`'))[, 2]
  	  codes_ret <- lapply(codes, function(x) eval(parse(text = x)))
  	  for (i in 1:length(codes)) {
  	    #print(codes[i])
  	    #print(codes_ret[[i]])
  	    ret$template <- sub(sprintf('`%s`', codes[i]), codes_ret[[i]], ret$template, fixed = TRUE)
  	  }
  	  
  	  require(rstudioapi)
  		insertText(document_position(1, 1), 
  		           text = paste(ret$template, collapse = '\n'))
  		stopApp()
  	})

  	observeEvent(input$cancel, {
		  stopApp()
  	})
		
		output$inputGroup = renderUI({
      input_list <- lapply(1:nvars, function(i) {
        inputName <- ret$vars[i]
        textInput(inputName, inputName, "")
      })
      do.call(tagList, input_list)
    })
    
    }

  #viewer <- paneViewer(300)  # paneViewer dialogViewer browserViewer
  viewer <- dialogViewer('Insert Header')
  runGadget(ui, server, viewer = viewer)

}

# Try running
# library(shiny)
# library(miniUI)
# insertHeaderAddin()


