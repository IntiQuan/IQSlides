
# Converts a file to a ggplot object
as_gg_file <- function(file, pages = 1) {

  if (grepl("\\.pdf$", file)) {
    image__ <- try(magick::image_read_pdf(file, pages = pages), silent = TRUE)
  } else if (grepl("\\.svg$", file)) {
    image__ <- try(magick::image_read_svg(file), silent = TRUE)
  } else {
    image__ <- try(magick::image_read(file), silent = TRUE)
  }

  if (!inherits(image__, "try-error"))
    value <- cowplot::ggdraw() + cowplot::draw_image(image__)
  else
    value <- "<< Table could not be produced >>"

  return(value)

}
