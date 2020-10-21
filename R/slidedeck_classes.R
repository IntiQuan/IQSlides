

#' Push contents into a PPTX output
#'
#' The function pushes a plot or table with additional information like
#' title and section into an rds file within a slide folder.
#' From that folder, a slide deck can be generated using the
#' \link{IQSlidedeck}() function.
#'
#' @param ... The contents to be presented on a slide. Currently supported are one or two objects
#' of type plot or table.
#' @param section character, optional section name. Different section names will generate different
#' subfolders of the slide output folder.
#' @param title character, optional slide title.
#' @param layout character or NULL (default), currently supported are "Title and Content" (one object) and
#' "Two Content" (two objects). If `NULL`, the layout is chosen automatically according to the number of
#' arguments passed via `...`.
#' @param filename character, filename of the rds file.
#' @param outputFolder character or NULL (default). The slide folder where the rds files are saved.
#' By default, slides are saved in the folder `getOption("IQSlide.outputfolder")`.
#' @param verbose logical, if `TRUE` (default) show verbose information.
#' @md
#' @export
#' @author Daniel Kaschek, IntiQuan
#' @family Slidedeck functions
#' @example inst/examples/IQSlidedeck.R
IQRoutputPPTX <- function(...,
                          section = NULL, title = NULL, layout = NULL,
                          filename, outputFolder = NULL,
                          verbose = TRUE) {


  args__ <- list(...)
  # In no contents provided, return empty character
  if (length(args__) == 0) args__ <- list("")

  # Where the rds output goes (-> outputFolder)
  if (is.null(outputFolder)) outputFolder <- getOption("IQSlide.outputfolder") # .OUTPUTFOLDER_SLIDES


  # Basic checking of input arguments
  stopifnot(is.character(filename) & length(filename) == 1)
  stopifnot(is.null(title) | (is.character(title) & length(title) == 1))
  stopifnot(is.null(section) | (is.character(section) & length(section) == 1))
  stopifnot(is.character(outputFolder) & length(outputFolder) == 1)

  # Catch IQRtools cases
  args__ <- lapply(args__, function(args_i__) {

    if (inherits(args_i__, "IQRoutputFigure")) {
      # IQRoutputFigures are translated into list
      args_i__ <- translate_IQRoutputFigure(args_i__)
    } else if (inherits(args_i__, "IQRoutputTable")) {
      # IQRoutputTables are
      args_i__ <- translate_IQRoutputTable(args_i__)
    }

    return(args_i__)

  })


  # Auto-determine Slide Layout (-> layout__)
  if (is.null(layout)) {
    nargs__ <- max(1, length(args__))
    if (nargs__ > 2) stop("Cannot automatically determine layout for more than two inputs.",
                          "Please specify a valid layout for the number of provided inputs.")
    if (nargs__ == 2) {
      if (is.null(caption(args__[[1]])) & is.null(caption(args__[[2]])))
        layout__ <- "Two Content"
      if (!is.null(caption(args__[[1]])) & is.null(caption(args__[[2]])))
        layout__ <- "Two Content and Caption Left"
      if (is.null(caption(args__[[1]])) & !is.null(caption(args__[[2]])))
        layout__ <- "Two Content and Caption Right"
      if (!is.null(caption(args__[[1]])) & !is.null(caption(args__[[2]])))
        layout__ <- "Two Content and Caption"
    }
    if (nargs__ == 1) {
      if (is.null(caption(args__[[1]])))
        layout__ <- "Title and Content"
      if (!is.null(caption(args__[[1]])))
        layout__ <- "Title and Content and Caption"
    }

  } else {
    layout__ <- layout
  }

  # Sanitize filename (-> filename__)
  filename__ <- paste0(sub("\\.rds$", "", filename, ignore.case = TRUE), ".rds")

  # Check classes contents
  checks__ <- sapply(args__, function(myarg__) {
    length(intersect(class(myarg__), c("data.frame", "character", "numeric",
                                       "factor", "logical", "block_list", "unordered_list",
                                       "gg", "external_img", "xml_document", "flextable",
                                       "IQ_bullet_list", "IQ_table", "IQ_plot",
                                       "IQRoutputFigure", "IQRoutputTable", "list"))) > 0
  })
  if (!all(checks__)) {
    stop("The type of input argument ", paste(which(!checks__), collapse = ", "), " is not supported. Please convert first.")
  }

  # Check if the number of ... inputs fits to the (user-provided layout)
  if (grepl("Title and Content", layout__)) length__ <- 1
  if (grepl("Two Content", layout__)) length__ <- 2
  if (length(args__) > length__) {
    warning("Too many inputs for selected layout. Leftover inputs will not be used.")
  }





  # Determine length of list arguments, 0 if no list
  n_is_list__ <- sapply(args__, function(args_i__) {
    if (class(args_i__)[1] == "list") n__ <- length(args_i__) else n__ <- 0
    return(n__)
  })

  # Recycle lists to have same length
  args__ <- lapply(1:length(args__), function(i__) {
    if (n_is_list__[i__] > 0)
      args_i__ <- rep_len(args__[[i__]], max(n_is_list__))
    else
      args_i__ <- args__[[i__]]
    return(args_i__)
  })

  if (all(n_is_list__ == 0)) {
    # Call single slide output
    argslist__ <- c(args__,
                    list(section = section,
                         title = title,
                         layout = layout__,
                         filename = filename__,
                         outputFolder = outputFolder,
                         verbose = verbose))
    do.call(IQRoutputPPTX_single, argslist__)
  } else {
    # Call single slide output multiple times
    for (i__ in 1:max(n_is_list__)) {
      argslist__ <- c(
        lapply(1:length(args__), function(k__) {
          if (n_is_list__[k__] > 0)
            args_k__ <- args__[[k__]][[i__]]
          else
            args_k__ <- args__[[k__]]
          return(args_k__)
        }),
        list(section = section,
             title = paste0(title, " (", i__, ")"),
             layout = layout__,
             filename = sub("\\.rds$", paste0("_", i__, ".rds"), filename__),
             outputFolder = outputFolder,
             verbose = verbose))
      do.call(IQRoutputPPTX_single, argslist__)
    }
  }


  invisible()


}


IQRoutputPPTX_single <- function(...,
                          section = NULL, title = NULL, layout = NULL,
                          filename, outputFolder = NULL,
                          verbose = TRUE) {
  args__ <- list(...)
  layout__ <- layout
  filename__ <- filename



  # Create output object
  output__ <- c(list(
    title = title,
    layout = layout__
  ), args__)

  # Create output directory if not existent
  if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)

  # Create section folder, detect if section already exists, etc.
  if (!is.null(section)) {
    dirs__ <- list.dirs(outputFolder, full.names = FALSE)
    dirs__ <- dirs__[dirs__ != ""]
    if (length(dirs__) > 0) {
      splitdirs__ <- splitNumNames(dirs__)
      prefix__ <- splitdirs__[["prefix"]]
      dirnames__ <- splitdirs__[["name"]]
      if (!is.numeric(prefix__))
        stop('The output folder contains subfolders not matching the pattern "[[number]]_*". ',
             'Please check and rename or remove these folders before pushing slides.')
      nmax__ <- splitdirs__[["nmax"]]

    } else {
      dirnames__ <- NULL
      nmax__ <- 0
    }

    if (section %in% dirnames__) {
      sectionFolder__ <- file.path(outputFolder, names(dirnames__)[match(section, dirnames__)])
    } else {
      sectionFolder__ <- file.path(outputFolder, paste(nmax__ + 1, section, sep = "_"))
    }

  } else {
    sectionFolder__ <- outputFolder
  }
  if (!dir.exists(sectionFolder__)) dir.create(sectionFolder__, recursive = TRUE)

  # List all slide rds files in the section
  files__ <- list.files(sectionFolder__, pattern = "\\.rds$", ignore.case = TRUE)
  if (length(files__) > 0) {
    splitfiles__ <- splitNumNames(files__)
    prefix__ <- splitfiles__[["prefix"]]
    filenames__ <- splitfiles__[["name"]]
    if (!is.numeric(prefix__))
      stop('The output folder "', sectionFolder__, '" contains rds files not matching the pattern "[[number]]_*.rds". ',
           'Please check and rename or remove these files before pushing slides.')
    nmax__ <-splitfiles__[["nmax"]]
  } else {
    filenames__ <- NULL
    nmax__ <- 0
  }


  # Determine action from filename and list of filenames
  if (filename__ %in% filenames__) {
    action__ <- "updated"
    outfile__ <- file.path(sectionFolder__, names(filenames__)[match(filename__, filenames__)])
  } else {
    action__ <- "created"
    outfile__ <- file.path(sectionFolder__, paste0(nmax__ + 1, "_", filename__))
  }


  # Create slide footnote with compliance information
  footnote__ <- genComplianceFooter(filename__)
  output__[["footnote"]] <- footnote__

  # Write to file
  saveRDS(output__, file = outfile__)

  # Verbose output
  if (verbose) cat('Slide file "', filename__, '" was ', crayon::bold(action__), '.\n', sep = "")

  invisible()


}


#' Build a slide deck from a slide folder
#'
#' Reads all rds files and generates a slide deck from the input
#'
#' If non of the title-relevant arguments are supplied, a slide deck without
#' title page is produced.
#'
#' @param title character or NULL (default), title of the slide deck.
#' @param subtitle character or NULL (default), subtitle of the slide deck.
#' @param affiliation character or NULL (default), author name and company.
#' @param date character or NULL (default), date.
#' @param filename character, output file name of the slide deck. If a single file name, the output is
#' generated in rdspath. If the filename contains path information, the file will be generated in the
#' exact filename location.
#' @param section character or `NULL` (default), allows to create slides for a specific section only.
#' @param rdspath character or `NULL` (default), path to the slide files. Searches for slides
#' in `getOption("IQSlide.outputfolder")` if `NULL`.
#' @param template character or `NULL` (default), path to the template PPTX file. Uses the
#' internal template if `NULL`.
#' @details The appearance of the slides can be changed by setting the corresponding options via
#' The R `options()` interface:
#' * Select a template via `options(IQSlide.template = "TemplateName")`. Currently supported are "Default" and "IQ".
#' * Select the aspect ratio via `options(IQSlide.ratio = "Ratio")`. Currently supported are "16:9" and "4:3".
#' To change your settings permanently, please include your preferred options in your RProfile file.
#' @md
#' @export
#' @author Daniel Kaschek, IntiQuan
#' @family Slidedeck functions
#' @example inst/examples/IQSlidedeck.R
IQSlidedeck <- function(title = NULL, subtitle = NULL, affiliation = NULL, date = NULL,
                        filename = "slides.pptx", section = NULL, rdspath = NULL, template = NULL) {


  if (is.null(rdspath)) rdspath <- getOption("IQSlide.outputfolder")  #.OUTPUTFOLDER_SLIDES
  if (!is.null(section)) {
    dirs__ <- list.dirs(rdspath, full.names = FALSE, recursive = FALSE)
    sections__ <- sapply(dirs__, function(x__) paste(strsplit(x__, "_")[[1]][-1], collapse = "_"))
    if (!section %in% sections__) stop("Section not found. Please check section name.")
    rdspath <- file.path(rdspath, dirs__[match(section, sections__)])
  }
  if (is.null(template)) template <- system.file(package="IQSlides",
                                                 file.path("templates",
                                                           paste0("Template", getOption("IQSlide.template"), "_",
                                                                  sub("\\:", "", getOption("IQSlide.ratio")), ".pptx")))
  if (filename == basename(filename)) filename <- file.path(rdspath, filename) else {
    if (!dir.exists(dirname(filename))) dir.create(dirname(filename), recursive = TRUE)
  }
  filenameparts <- strsplit(filename, "/")[[1]]
  tempfilepath <- paste0(paste0(head(filenameparts,-1), collapse = "/"),"/~$",tail(filenameparts,1))
  if (file.exists(tempfilepath)) stop("Close file before running IQSlidedeck")

  rdsfiles__ <- list.files(rdspath, pattern = "^[[:digit:]]+.*\\.rds$", recursive = TRUE)
  rdsfiles__[!grepl("/", rdsfiles__)] <- paste0("./", rdsfiles__[!grepl("/", rdsfiles__)])
  rdsfiles__ <- sub("^\\.", "0_GENERAL", rdsfiles__)

  slidestructure__ <- do.call(rbind, lapply(rdsfiles__, function(x__) {
    tmp__ <- strsplit(x__, "/")[[1]]
    section__ <- splitNumNames(tmp__[1])
    rdsfile__ <- splitNumNames(tmp__[2])
    dplyr::tibble(
      Sec_Number = section__[["prefix"]],
      Section = section__[["name"]],
      Page_Number = rdsfile__[["prefix"]],
      Page = rdsfile__[["name"]],
      File = rdsfile__[["original"]],
      Fullpath = file.path(rdspath, sub("^0_GENERAL/", "", x__)))
  }))

  slidestructure__ <- slidestructure__[order(slidestructure__[["Sec_Number"]], slidestructure__[["Page_Number"]]), ]
  slidestructure__[["Contents"]] <- lapply(slidestructure__[["Fullpath"]], readRDS)
  slidestructure__[["New Section"]] <- !duplicated(slidestructure__[["Sec_Number"]]) & (slidestructure__[["Sec_Number"]] > 0)


  mymaster__ <- "Office Theme"
  baseppt__ <- officer::read_pptx(template)
  keywords__ <- c("title", "layout", "footnote")

  #layout_summary(baseppt)

  # Add title slide
  if (!is.null(title) | !is.null(subtitle) | !is.null(date) | !is.null(affiliation)) {

    if (is.null(title)) title__ <- "Overview of Results" else title__ <- title
    if (is.null(subtitle)) subtitle__ <- "" else subtitle__ <- subtitle
    if (is.null(date)) date__ <- Sys.Date() else date__ <- date
    if (is.null(affiliation)) affiliation__ <- "IntiQuan" else affiliation__ <- affiliation

    baseppt__ <- officer::add_slide(baseppt__, layout = "Title Slide", master = mymaster__)
    baseppt__ <- officer::ph_with(baseppt__, value = title__,
                                  location = officer::ph_location_type("ctrTitle"))
    baseppt__ <- officer::ph_with(baseppt__, value = paste0(subtitle__, "\n\n", affiliation__, "\n",  date__),
                                  location = officer::ph_location_type("subTitle"))
  }


  for (i__ in 1:nrow(slidestructure__)) {

    section__ <- slidestructure__[["Section"]][i__]
    contents__ <- slidestructure__[["Contents"]][i__][[1]]
    elements__ <- contents__[!names(contents__) %in% keywords__]

    # Create new section
    if (slidestructure__[["New Section"]][[i__]]) {
      baseppt__ <- officer::add_slide(baseppt__, layout = "Title Only", master = mymaster__)
      baseppt__ <- officer::ph_with(baseppt__, value = section__,
                                    location = officer::ph_location_type(type = "title"))
    }

    # Create new slide
    baseppt__ <- officer::add_slide(baseppt__, layout = contents__[["layout"]],
                                    master = mymaster__)

    # Put slide title
    if (!is.null(contents__[["title"]])) {
      baseppt__ <- officer::ph_with(baseppt__, value = contents__[["title"]],
                                    location = officer::ph_location_type(type = "title"))
    }


    # When introducing new layouts, add new section here
    # - put browser() command
    # - when in debugger mode type layout_properties(baseppt__, "$NewLayoutName", "Office Theme")
    # - only use ph_location_label!, other wise bullet_list will not work properly
    if (contents__[["layout"]] == "Title and Content") {
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[1]],
                              location = officer::ph_location_label("Content Placeholder 2"))
    } else if (contents__[["layout"]] == "Two Content") {
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[1]],
                                    location = officer::ph_location_label("Content Placeholder 2"))
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[2]],
                                    location = officer::ph_location_label("Content Placeholder 3"))
    } else if (contents__[["layout"]] == "Title and Content and Caption") {
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[1]],
                                    location = officer::ph_location_label("Content Placeholder 2"))
      baseppt__ <- officer::ph_with(baseppt__, value = attr(elements__[[1]], "caption"),
                                    location = officer::ph_location_label("Text Placeholder 8"))
    } else if (contents__[["layout"]] == "Two Content and Caption") {
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[1]],
                                    location = officer::ph_location_label("Content Placeholder 2"))
      baseppt__ <- officer::ph_with(baseppt__, value = attr(elements__[[1]], "caption"),
                                    location = officer::ph_location_label("Text Placeholder 2"))
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[2]],
                                    location = officer::ph_location_label("Content Placeholder 3"))
      baseppt__ <- officer::ph_with(baseppt__, value = attr(elements__[[2]], "caption"),
                                    location = officer::ph_location_label("Text Placeholder 3"))
    } else if (contents__[["layout"]] == "Two Content and Caption Left") {
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[1]],
                                    location = officer::ph_location_label("Content Placeholder 2"))
      baseppt__ <- officer::ph_with(baseppt__, value = attr(elements__[[1]], "caption"),
                                    location = officer::ph_location_label("Text Placeholder 2"))
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[2]],
                                    location = officer::ph_location_label("Content Placeholder 3"))
    } else if (contents__[["layout"]] == "Two Content and Caption Right") {
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[1]],
                                    location = officer::ph_location_label("Content Placeholder 2"))
      baseppt__ <- officer::ph_with(baseppt__, value = elements__[[2]],
                                    location = officer::ph_location_label("Content Placeholder 3"))
      baseppt__ <- officer::ph_with(baseppt__, value = attr(elements__[[2]], "caption"),
                                    location = officer::ph_location_label("Text Placeholder 2"))
    } else {
      warning("Ignoring unknown layout ", contents__[["layout"]], ".")
    }


    # Add footer
    footer__ <- slidestructure__[["Contents"]][[i__]][["footnote"]]
    if (is.null(footer__)) footer__ <- "Compliance Mode Disabled"

    baseppt__ <- officer::ph_with(baseppt__, value = footer__,
                                  location = officer::ph_location_type(type = "ftr"))

  }

  print(baseppt__, target = filename)
}





#' Set and retrieve caption for an object
#'
#' Get or set a caption attribute on any object. Is used by [IQRoutputPPTX] to
#' select the proper slide layout.
#'
#' @param x any R object
#' @param value character, the caption.
#' @return The object x with caption attribute
#'
#' @md
#' @export
#' @author Daniel Kaschek, IntiQuan
caption <- function(x) {
  # Check for caption in attribute
  mycaption <- attr(x, "caption")
  # If not available, check for caption in object itself
  # (ensures compatibility with IQRoutputTable and IQRoutputFigure)
  if (is.null(mycaption) & is.list(x)) {
    # Check for possible footer
    footer <- grep("footer", names(x), value = TRUE)
    if (length(footer) > 0) mycaption <- x[[footer]]
    # Check for possible caption in first list element
    x.caption <- attr(x[[1]], "caption")
    if (!is.null(x.caption)) mycaption <- paste(x.caption, mycaption, sep = "\n")
  }

  return(mycaption)

}

#' @rdname caption
#' @export
"caption<-" <- function(x, value) {
  attr(x, "caption") <- value
  return(x)
}

