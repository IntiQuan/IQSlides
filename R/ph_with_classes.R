
#' Create bullet point list
#'
#' Allows to create a formatted bullet point list using basic elements of
#' markdown syntax.
#'
#' @param ... Each bullet point is entered as a string. The string should start with "*" and
#' can be indented by multiples of two spaces to change the item level. See example.
#'
#' @return Object of type `bullet_list`.
#'
#' @example inst/examples/bullet_list.R
#'
#' @export
bullet_list <- function(...) {


  x <- unlist(list(...))

  elements <- getOption("IQSlide.markdown")

  # x <- c(
  #   "* Bullet point with **bold** and *italic*" ,
  #   "* Another bullet point",
  #   "  * Indented by two spaces",
  #   "  * Another one",
  #   "* And back ***really* outer** level",
  #   "* Some sort of formula `E_rel_ = m\\*c^2^`"
  # )

  out <- lapply(x, function(content) {
    # Determine Level
    nws <- nchar(content) - nchar(trimws(content, "left"))
    level <- floor(nws/2)+1
    # Remove leading bullet point
    content <- sub("^\\*\\W", "", trimws(content))
    # Add trailing space
    content <- paste0(content, " ")
    # Replace reserved characters
    reserved <- c("*", "`", "_", "^")
    replacement <- c("|@asterisk|", "|@code|", "|@subscript|", "|@superscript|")
    for (r in seq_along(reserved)) content <- gsub(paste0("\\", reserved[r]), replacement[r], content, fixed = TRUE)
    for (k in seq_along(elements)) {
      operator <- elements[[k]]
      action <- names(elements)[k]
      for (i in 1:1000) {
        content <- strsplit(content, operator, fixed = TRUE)[[1]]
        if (length(content) == 1) break
        content <- paste(c(content[1], paste0("<@", action, ">"), paste(content[-1], collapse = operator)), collapse = "")
      }
    }
    for (k in seq_along(elements)) content <- gsub(paste0("<@", names(elements)[k], ">"),
                                                   paste0("\n<@", names(elements)[k], ">\n"),
                                                   content)
    content <- strsplit(content, "\n")[[1]]
    content[length(content)] <- sub("\\W$", "", content[length(content)])
    content <- sapply(content, function(mycontent) {
      for (r in seq_along(reserved)) mycontent <- gsub(replacement[r], reserved[r], mycontent, fixed = TRUE)
      return(mycontent)
    }, USE.NAMES = FALSE)
    list(level = level, content = content)
  })

  class(out) <- c("bullet_list")

  return(out)

}


ph_with.bullet_list <- function(x, value, ...) {

  mytype <- "body"
  bullet_list <- value
  elements <- getOption("IQSlide.markdown")

  # Create styles
  text_normal <- officer::fp_text(font.family = "", font.size = 0)

  # Iterate over items
  for (i in seq_along(bullet_list)) {
    # Iterate over text/action elements per item
    for (j in seq_along(bullet_list[[i]]$content)) {

      # Level and content
      level <- bullet_list[[i]]$level
      content <- bullet_list[[i]]$content

      # Initialize
      if (i == 1 & j == 1) {
        x <- officer::ph_with(x, value = "", ...)
        mystyle <- text_normal
      }
      # At each item start reset format to text_normal and start a new par with the correct level
      if (i != 1 & j == 1) {
        x <- officer::ph_add_par(x, level = level, type = mytype)
        mystyle <- text_normal
      }
      if (i != 1 | (i == 1 & j != 1)) {
        # Change style or put contents on slide
        if (content[j] %in% paste0("<@", names(elements), ">")) {
          mystyle <- switch(content[j],
                            "<@bold>" = stats::update(mystyle, bold = !mystyle$bold),
                            "<@italic>" = stats::update(mystyle, italic = !mystyle$italic),
                            "<@code>" = stats::update(mystyle, font.family = ifelse(mystyle$font.family == "", "Consolas", "")),
                            "<@subscript>" = stats::update(mystyle, vertical.align = ifelse(mystyle$vertical.align == "baseline", "subscript", "baseline")),
                            "<@superscript>" = stats::update(mystyle, vertical.align = ifelse(mystyle$vertical.align == "baseline", "superscript", "baseline")))
        } else if (content[j] != "") {
          x <- officer::ph_add_text(x, str = content[j], style = mystyle, type = mytype)
        }

      }

    }
  }

  return(x)


}
