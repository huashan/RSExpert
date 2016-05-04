# todo
# add -V variables for templates
# create more templates?

pdfAddin <- function() {
  pdfAddinGadget(getActiveDoc())
}

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
pdfAddinGadget <- function(doc) {
	require(shiny)
	require(miniUI)

	temps <- list.files('d:/Tools/pandoc/data/templates/', pattern = '\\.context$')
	
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
	ui <- miniUI::miniPage(
    gadgetTitleBar("Create PDF", right = miniTitleBarButton("done", "Execute", primary = TRUE)),
    miniContentPanel(
  		wellPanel(
				singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
function(message) {
  alert("[R]markdown file not exists: " + message.fn);
}
);'))),

				checkboxInput("preview", "Preview PDF"),

				tags$hr(),
				
 				textInput("fn", "Input File:", doc, placeholder = "Input [R]md file name here"),
				div(style="display:inline-block", actionButton("actDoc", "Use Active Document")),
				div(style="display:inline-block", actionButton("paste", "Paste Path")),
				
				tags$hr(),
				selectInput("template", "Select a template:",
	                    choices = temps,
	                    selected = "default.context"),
				
				tags$h4("Options"),
				checkboxInput("n", "Numbered", value = F),
				checkboxInput("chapter", "Chapter", value = F),
        uiOutput("moreVar")
				)
    )
  )

  server <- function(input, output, session) {
    eventReactive(input$template, {
      vars <- get_variables(input$template)
    })
    
    output$moreVar <- renderUI({
      vars <- get_variables(input$template)
      #cat(vars, '\n')
      textInput('tempVar', 'Template Vars:', value = paste('-V', vars, collapse = ' '))
    })

    observeEvent(input$paste, {
      fn <- readLines("clipboard", warn = FALSE)
      # True 文件夹，False 文件， NA 非法名称
      if (length(fn) > 0 && !is.na(file.info(fn)$isdir)) {
        fn <- gsub("\\", "/", fn, fixed = T)
        updateTextInput(session, "fn", value = fn)
      }
    })
    
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
  		try(gen_pdf(input), silent = F)
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

gen_pdf <- function(input) {
  require(huashan)
	ret = NULL
	#cat(input$template, '\n')
	pandoc_cmd <- paste(if (input$n) '-N' else '', 
	                    if (input$chapter) '--chapters' else '',
	                    collapse = ' ')
	#cat(pandoc_cmd, '\n')
  try(ret <- createPDFSlides(input$fn, pandoc_cmd, input$template), 
      silent = T)
  if (input$preview & !is.null(ret)) shell.exec(ret)
}

createPDFSlides <- function(fn, pandoc_cmd = '', template = 'default.context',
                            contex_path = 'd:\\context\\tex\\texmf-win64\\bin;d:\\context\\tex\\;d:\\pandoc\\;') {
  s_wd = getwd()
  on.exit(setwd(s_wd), add = TRUE)
  setwd(dirname(fn))
  
  fn = md2slides.contex(fn)
  do_knit = isRmdFile(fn)
  
  if (do_knit) {
    destfn = sub('\\.Rmd$', '\\.md', fn, perl = T, ignore.case = T)
    knitr::knit(fn, destfn, encoding = 'UTF-8')
    fn = destfn
  }
  
  # convert to context format
  fn_tex = sub('\\.(.+)$', '.tex', fn)
  #rmarkdown:::find_pandoc()  --chapters -N 
  args = sprintf('-S -s %s -f markdown %s -w context -o %s --data-dir=d:\\tools\\pandoc\\data --template=%s', 
                 pandoc_cmd, shQuote(fn), shQuote(fn_tex), template)
  #todo '-V showprogress -V waterink -V watertxt=NISD')
  command = paste(shQuote(rmarkdown:::pandoc()), args, collapse = ' ')
  #cat(command)
  #return(NULL)
  ret = system(command)
  if (ret != 0)
    stop("pandoc document conversion failed with error ", ret, call. = F)
  
  # delete target pdf
  fn_pdf = sub('.tex', '.pdf', fn_tex, fixed = T)
  if (file.exists(fn_pdf) & !file.remove(fn_pdf)) {
    if (tolower(readline(prompt = 'pdf file may be in use, retry? Y/N    ')) != 'y') stop('pdf file in use')
  }
  # compile pdf
  pp = Sys.getenv('path')
  Sys.setenv(path = paste0(pp, ';', contex_path))
  shell('call setuptex2.bat')
  ret = shell(paste('context', fn_tex))
  if (ret != 0)
    stop("ConTeXt compiling failed with error ", ret, call. = F)
  
  fn_pdf
}

# extract template variables from context template
get_variables <- function(fn) {
  ss <- readLines(file.path('d:/Tools/pandoc/data/templates/', fn), encoding = 'UTF-8')
  require(stringr)
  tmp <- str_match(ss, '\\$if\\((.+?)\\)\\$')[, 2]
  setdiff(unique(tmp[!is.na(tmp)]),
    c('number-sections', 'toc', 'author', 'title', 'subtitle',
      'author.name', 'author.affiliation', 'date'))
}

# Try running
# library(shiny)
# library(miniUI)
# pdfAddin()


