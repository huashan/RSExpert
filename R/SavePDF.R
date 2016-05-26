c_header <- "library(showtext)
library(Cairo)
font.add(\"songti\", \"simsun.ttc\")
pdf('simone.pdf',  paper = 'a4r')
showtext.begin()
"

c_footer <- "showtext.end()
dev.off()"

savePDFAddin <- function() {
  savePDFAddinGadget(getActiveDoc())
}

savePDFAddinGadget <- function(doc) {
	require(shiny)
	require(miniUI)

  doc <- rstudioapi::getActiveDocumentContext()
  
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
	ui <- miniUI::miniPage(
    gadgetTitleBar("Export PDF", right = miniTitleBarButton("done", "Execute", primary = TRUE)),
    miniContentPanel(
  		wellPanel(
				singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
function(message) {
  alert("[R]markdown file not exists: " + message.fn);
}
);'))),

				div(style="display:inline-block", actionButton("actDoc", "Update")),
				#tags$hr(),
				tags$textarea(id="src", rows=8, cols=80, "Default value"),
				
				tags$hr(),
	      radioButtons("paper", "Paper Type",
	                     c("Portrait" = "a4",
	                       "Landscape" = "a4r"), selected = "a4", inline = TRUE)
				)
    )
  )

  server <- function(input, output, session) {
		#observe({})
		observeEvent(input$actDoc, {
			src = as.character(getCode()$selection[[1]]$range)
			updateTextInput(session, "src", value = src)
		})

  	observeEvent(input$done, {
  	  rng <- getCode()$selection[[1]]$range
  	  
  	  # insert footer
  	  rng1 <- document_range(document_position(rng$end[1] + 1, 1),
  	                         document_position(rng$end[1]+ 1, 1))
  	  rstudioapi::modifyRange(rng1, paste0('\n', c_footer, '\n'))
      # insert header
  	  rng1 <- document_range(document_position(rng$start[1], 1),
  	                        document_position(rng$start[1], 1))
      rstudioapi::modifyRange(rng1, paste0('\n', c_header))

      message('\nDone!\n')
  	})

  	observeEvent(input$cancel, {
		  stopApp()
  	})
	}

  # We'll use a pane viwer, and set the minimum height at
  # 300px to ensure we get enough screen space to display the clock.
  viewer <- paneViewer(300)  # paneViewer dialogViewer browserViewer
  runGadget(ui, server, viewer = viewer)

}

getCode <- function() {
  rstudioapi::getActiveDocumentContext()
}
  
# Try running
# library(shiny)
# library(miniUI)
# savePDFAddin()
