globalVariables(c(".OUTPUTFOLDER_SLIDES", ".TEMPLATEFILE_SLIDES"))

.onLoad <- function(libname, pkgname) {

  # Check for available options (some of the might have been set in Rprofile)
  op <- options()

  # Default options for IQSlides
  op.IQSlides <- list(
    IQSlide.markdown = c(bold = "**", italic = "*", code = "`", subscript = "~", superscript = "^"),
    IQSlide.template = "Default",
    IQSlide.ratio = "16:9",
    IQSlide.outputfolder = "../Output/Slides",
    IQSlide.templatefolder = c("/IQDESKTOP/.templates/IQSlides", file.path(system.file(package = "IQSlides"), "templates"))
  )

  # Check if IQ Templates available. If yes, set as default.
  if (file.exists(file.path(op.IQSlides[["IQSlide.templatefolder"]][1], "TemplateIQ_169.pptx"))) {
    op.IQSlides[["IQSlide.ratio"]] <- "16:9"
    op.IQSlides[["IQSlide.template"]] <- "IQ"
  }

  # Set missing options to default values
  toset <- !(names(op.IQSlides) %in% names(op))
  if (any(toset)) options(op.IQSlides[toset])

  invisible()

}
