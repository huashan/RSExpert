slidesAddin <- function() {
  slidesAddinGadget(getActiveDoc())
}

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
slidesAddinGadget <- function(doc) {
	require(shiny)
	require(miniUI)

  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
	ui <- miniUI::miniPage(
    gadgetTitleBar("Create HTML Slides", right = miniTitleBarButton("done", "Execute", primary = TRUE)),
    miniContentPanel(
  		wellPanel(
				singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
function(message) {
  alert("[R]markdown file not exists: " + message.fn);
}
);'))),

				checkboxInput("preview", "Preview Slides"),

				tags$hr(),
				
 				textInput("fn", "Input File:", doc, placeholder = "Input [R]md file name here"),
	      stableColumnLayout(
					actionButton("actDoc", "Use Active Document"),				
					actionButton("paste", "Paste Path")
				),		
				tags$hr(),
	      radioButtons("slidetype", "Slide Type",
	                     c("HTML5" = "h5",
	                       "PDF" = "pdf"), selected = "h5", inline = TRUE),
				selectInput("h5", "HTML5 Slides framework:",
	                    c("io-2012" = "io2012",
	                      "flowtime" = "flowtime")),

	      stableColumnLayout(
					checkboxGroupInput("widgets", "Widgets:",
	                           c("MathJax" = "mathjax",
	                             "Bootstrap" = "bootstrap",
	                             "Quiz" = "quiz"), selected = c('mathjax')))		
				)
    )
  )

  server <- function(input, output, session) {
		#observe({})
		observeEvent(input$actDoc, {
			fn = getActiveDoc()
			updateTextInput(session, "fn", value = fn)
		})

  	observeEvent(input$done, {
  	  if (!file.exists(input$fn)) {
  	    session$sendCustomMessage(type = 'testmessage',
      	  message = list(fn=input$fn))
      	return(NULL)
			}
  		try(gen_slides(input), silent = F)
  		message('\nDone!\n')
  	  #cmd = list(input$fn, framework=input$slidetype, widgets=input$widgets)
  		#ss = print(cmd)
  		#rstudioapi::insertText(paste(ss, collapse='\n'))
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

getActiveDoc <- function() {
  fn = rstudioapi::getActiveDocumentContext()$path
  Encoding(fn) ='UTF-8'
  if (grepl('\\.[Rr]?md$', fn)) fn else ''
}

gen_slides <- function(input) {
  require(huashan)
	ret = NULL
	
  if (input$slidetype == "h5") {
  	try(ret <- createSlides(input$fn, framework = input$h5, mode = "selfcontained",
  			widgets = as.list(input$widgets)), silent = T)
  } else {
    try(ret <- createPDFSlides(input$fn), silent = T)
  }
 	if (input$preview & !is.null(ret)) shell.exec(ret)
}

# Try running
#library(shiny)
#library(miniUI)
#slidesAddin()


