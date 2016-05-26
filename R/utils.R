getActiveDoc <- function() {
  fn = rstudioapi::getActiveDocumentContext()$path
  Encoding(fn) ='UTF-8'
  if (grepl('\\.[Rr]?md$', fn)) fn else ''
}
