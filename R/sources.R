#' 将剪贴板中路径名转换为 linux 风格
#' 
#' 读取剪贴板内容，并判断是否文件或文件夹。
#' 用于读入用户指定的文件夹或文件名设置，并将其转化为合法的 R 文件名格式。
#' @note 在运行命令前，先粘贴文件名到剪贴板。
#' @export
#' @keywords clipboard directory
#' @family directory clipboard
pathConverterAddin <- function() {
  fn <- readLines("clipboard", warn = FALSE)
  # True 文件夹，False 文件， NA 非法名称
  if (!is.na(file.info(fn)$isdir)) {
    fn<-gsub("\\", "/", fn, fixed = T)
    writeClipboard(fn, format = 1)
  }
  rstudioapi::insertText(fn)
}
