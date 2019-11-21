globalVariables(c(".OUTPUTFOLDER_SLIDES", ".TEMPLATEFILE_SLIDES"))

.onLoad <- function(libname, pkgname) {
  options(IQSlide.markdown = c(bold = "**", italic = "*", code = "`", subscript = "_", superscript = "^"))
}
