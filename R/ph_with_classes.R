
# -------------------------------------------------------------------------#
# Bullet point lists ----
# -------------------------------------------------------------------------#

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
#' @import officer
#' @method ph_with IQ_bullet_list
IQ_bullet_list <- function(...) {


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
    reserved <- getOption("IQSlide.markdown")[c("italic", "code", "subscript", "superscript")]
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

  class(out) <- c("IQ_bullet_list")

  return(out)

}

#' @export
#' @method ph_with bullet_list
#' @rdname IQ_bullet_list
bullet_list <- function(...) {
  IQ_bullet_list(...)
}

ph_with.bullet_list <- function(x, value, location, ...) ph_with.IQ_bullet_list(x, value, location, ...)


ph_with.IQ_bullet_list <- function(x, value, location, ...) {


  # Does not seem to be needed any more because location became a required argument in officer:ph_with
  # location <- as.character(substitute(...))[2]

  bullet_list <- value

  # Default elements and fonts according to IQSlide options
  elements <- getOption("IQSlide.markdown")
  template <- getOption("IQSlide.template")
  font_normal <- switch(template, Default = "Calibri", IQ = "Open Sans", IQNew = "Open Sans")
  font_console <- switch(template, Default = "Consolas", IQ = "Consolas", IQNew = "Consolas")

  # Create styles
  text_normal <- officer::fp_text(font.family = font_normal, font.size = 0)

  # Iterate over items
  z <- list()
  zlevels <- NULL
  for (i in seq_along(bullet_list)) {
    # Iterate over text/action elements per item
    y <- list()

    # Level and content
    level <- bullet_list[[i]]$level
    content <- bullet_list[[i]]$content

    for (j in seq_along(bullet_list[[i]]$content)) {
      # Initialize
      if (i == 1 & j == 1) {
        mystyle <- text_normal
        #x <- officer::ph_with(x, value = "", ...)
      }
      # At each item start reset format to text_normal and start a new par with the correct level
      if (i != 1 & j == 1) {
        mystyle <- text_normal
      }
      #if (i != 1 | (i == 1 & j != 1)) {
        # Change style or put contents on slide
        if (content[j] %in% paste0("<@", names(elements), ">")) {
          mystyle <- switch(content[j],
                            "<@bold>" = stats::update(mystyle, bold = !mystyle$bold),
                            "<@italic>" = stats::update(mystyle, italic = !mystyle$italic),
                            "<@code>" = stats::update(mystyle, font.family = ifelse(mystyle$font.family == font_normal, font_console, font_normal)),
                            "<@subscript>" = stats::update(mystyle, vertical.align = ifelse(mystyle$vertical.align == "baseline", "subscript", "baseline")),
                            "<@superscript>" = stats::update(mystyle, vertical.align = ifelse(mystyle$vertical.align == "baseline", "superscript", "baseline")))
        } else if (content[j] != "") {
          toadd <-officer::ftext(text = content[j],
                                 prop = officer::fp_text(color = mystyle$color,
                                                         bold = mystyle$bold,
                                                         italic = mystyle$italic,
                                                         vertical.align = mystyle$vertical.align,
                                                         font.family = mystyle$font.family,
                                                         font.size = mystyle$font.size))
          y[[length(y)+1]] = toadd
        }

      #}

    }
    # combine text segments to point
    z[[length(z)+1]] <- do.call(officer::fpar, y)
    zlevels <- c(zlevels, level)
  }

  # combine points to block_list
  mylist <- do.call(officer::block_list, z)

  x <- officer::ph_with(x = x, value = mylist, location =  officer::ph_location_label(location), level_list = zlevels)
  return(x)
}



# -------------------------------------------------------------------------#
# Tables ----
# -------------------------------------------------------------------------#


#' Table output for IQRoutputPPTX
#'
#' Prepares objects which can be interpreted as tables (matrix, data.frame, flextable) for
#' output via IQRoutputPPTX.
#'
#' @param x R object (matrix, IQRtable, data.frame, flextable)
#' @param ... currently not used
#'
#' @return ggplot object (any table will be printed as image)
#'
#' @export
IQ_table <- function(x, ...) {
  UseMethod("IQ_table", x)
}

#' @export
#' @rdname IQ_table
IQ_table.IQRtable <- function(x, ...) {

  table__ <- as.data.frame(x)
  IQ_table.data.frame(table__, ...)

}

#' @export
#' @rdname IQ_table
IQ_table.matrix <- function(x, ...) {

  table__ <- as.data.frame(x)
  IQ_table.data.frame(table__, ...)

}

#' @export
#' @rdname IQ_table
IQ_table.data.frame <- function(x, ...) {

  table__ <- flextable::flextable(x)
  table__ <- flextable::autofit(table__)

  IQ_table.flextable(table__, ...)

}

#' @export
#' @rdname IQ_table
IQ_table.flextable <- function(x, ...) {

  path__ <- tempfile(fileext = ".png")
  try(flextable::save_as_image(x, path__, zoom = 2, expand = 5), silent = TRUE)
  tmax <- 10 # seconds
  tini <- tnow <- Sys.time()
  # Wait for max. 5 seconds if image file appears
  while(as.double(tnow - tini) < tmax) {
    if (file.exists(path__)) break
    Sys.sleep(.1)
    tnow <- Sys.time()
  }

  as_gg_file(path__)

}


# -------------------------------------------------------------------------#
# Plots ----
# -------------------------------------------------------------------------#

#' Image output for IQRoutputPPTX
#'
#' Prepares objects which can be interpreted as plots (image files, ggplot) for
#' output via IQRoutputPPTX.
#'
#' @param x path to image file or ggplot object
#' @param ... currently not used
#'
#' @return ggplot object (any table will be printed as image)
#' @export
IQ_image <- function(x, ...) {
  UseMethod("IQ_image", x)
}

#' @export
#' @param pages integer, the page from the pdf to be extracted
#' @rdname IQ_image
IQ_image.character <- function(x, pages = 1, ...) {
  as_gg_file(x, pages = pages[1])
}

#' @export
#' @rdname  IQ_image
IQ_image.gg <- function(x, ...) {
  x
}


