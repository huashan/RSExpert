buildPackageAddin <- function() {
  bvdPkgAddinGadget()
}

# We'll wrap our Shiny Gadget in an addin.
# Let's call it 'clockAddin()'.
bvdPkgAddinGadget <- function() {
	require(shiny)
	require(miniUI)

	config = loadSettings() 
	if (!is.null(config)) {
		pkg = config$pkg
		COxygenize = config$oxygen
		CMakeMarkdown = config$markdownDoc
		CIncrementBvdNum = config$incrementBvdNo
		CMultiArch = config$multiArch
		CNoHelp = config$nohelp
		CCompileRcpp = config$compileRcpp
	} else {
	  pkg = ''
		COxygenize = FALSE
		CMakeMarkdown = FALSE
		CIncrementBvdNum = TRUE
		CMultiArch = FALSE
		CNoHelp = FALSE
		CCompileRcpp = FALSE
	}
	
  # Our ui will be a simple gadget page, which
  # simply displays the time in a 'UI' output.
	ui <- miniUI::miniPage(
    gadgetTitleBar("Build Package", right = miniTitleBarButton("done", "Execute", primary = TRUE)),
    miniContentPanel(
  		wellPanel(
  		  stableColumnLayout(
				checkboxInput("oxygen", "Oxygenize", value = COxygenize),
				checkboxInput("incrementBvdNo", "Increment Build Number", value =  CIncrementBvdNum),
				checkboxInput("multiArch", "Multi Architacture", value = CMultiArch)
  		  ),
  		  stableColumnLayout(
				checkboxInput("nohelp", "No Help", value = CNoHelp),
				checkboxInput("compileRcpp", "Compile Rcpp", value = CCompileRcpp),
				checkboxInput("markdownDoc", "Generate Markdown Docuemnt", value = CMakeMarkdown)
				),
				singleton(
          tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
function(message) {
  alert("[R]markdown file not exists: " + message.fn);
}
);'))),
				
				textInput("pkg", "Package Directory:", pkg, placeholder = "Paste package path here"),
				actionButton("paste", "Paste Path")
			)
    )
  )

  server <- function(input, output, session) {
		
		observeEvent(input$paste, {
  		fn <- readLines("clipboard", warn = FALSE)
  		# True 文件夹，False 文件， NA 非法名称
  		if (length(fn) > 0 && !is.na(file.info(fn)$isdir)) {
    		fn <- gsub("\\", "/", fn, fixed = T)
				updateTextInput(session, "pkg", value = fn)
  		}
		})

  	observeEvent(input$done, {
  	  if (!dir.exists(input$pkg)) {
  	    session$sendCustomMessage(type = 'testmessage',
      	  message = list(fn=input$pkg))
      	return(NULL)
			}
			ret = FALSE
			#debug(compile_pkg)
  		try(ret <- compile_pkg(input))
  		
  		if (ret) message('\nDone!\n')
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

compile_pkg <- function(input) {
	pkg = input$pkg
	COxygenize = input$oxygen
	CMakeMarkdown = input$markdownDoc
	CIncrementBvdNum = input$incrementBvdNo
	CMultiArch = input$multiArch
	CNoHelp = input$nohelp
	CCompileRcpp = input$compileRcpp

	#opts = c("--build", "--no-test-load")
	opts = '--build'
	opts = if (CMultiArch) c(opts, '--force-multiarch') else c(opts, '--no-multiarch')
	opts = if (CNoHelp) c(opts, '--no-help') else opts

	saveSettings(input)
	
	# check if package was loaded
	pkgname = basename(pkg)
	
	if (pkgname == "RSExpert") {
		warning("Don't build RSExpert from RSExpert.")
		return(TRUE)
	}
	#if (pkgname %in% loadedNamespaces()) {
	if (any(grepl(sprintf(':%s$', pkgname), search()))) {
			try(detach(sprintf('package:%s', pkgname), character.only = T, unload = TRUE))
	}
	
	if (CCompileRcpp) Rcpp::compileAttributes(pkgdir = pkg, verbose = getOption("verbose"))

	pkgutils::install_packages(pkg, oxygenize=COxygenize, incrementBvdNum = CIncrementBvdNum,
                           opts = opts)

	return(TRUE)
	# todo
	#if (CMakeMarkdown) {
  #	fn = pkgutils::md_roclet(pkg)
  #	pkgutils::md2context(fn, cmdCxtInit = 'setuptex2.bat')
	#}
}

saveSettings <- function(input) {
	fn = file.path(system.file('', package = 'RSExpert'), 'config/bvdpkg.rds')
	dir.create(dirname(fn), recursive = TRUE, showWarnings = FALSE)
	cfg = list(
		pkg = input$pkg,
		oxygen = input$oxygen,
		markdownDoc = input$markdownDoc,
		incrementBvdNo = input$incrementBvdNo,
		multiArch = input$multiArch,
		nohelp = input$nohelp,
		compileRcpp = input$compileRcpp
	)
	saveRDS(cfg, file = fn)
}

loadSettings <- function() {
	fn = file.path(system.file('', package = 'RSExpert'), 'config/bvdpkg.rds')
	if (file.exists(fn)) readRDS(fn) else NULL
}

# Try running
#slidesAddin()


