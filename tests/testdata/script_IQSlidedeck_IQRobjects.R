library(IQSlides)
library(IQRtools)
library(ggplot2)
library(flextable)

# For compliance information in the footer of the slides
IQRinitCompliance("IQSlidedeck_IQRobjects.R")

mywd <- getwd()
setwd(tempdir())

# -------------------------------------------------------------------------#
# Create some output ----
# -------------------------------------------------------------------------#

plots <- lapply(1:12, function(i) {

  mydata <- rbind(
    data.frame(x = rnorm(30), y = rnorm(30), mean = "0"),
    data.frame(x = rnorm(30, mean = 1), y = rnorm(30, mean = 1), mean = "1")
  )

  IQRggplot(mydata, aes(x = x, y = y, color = mean)) + geom_point() + scale_color_IQRtools()

})

figures <- IQRoutputFigure(plots, nrow = 2, ncol = 2, opt.layout = opt.layout(legend.option = "common"),
                           footer = "Normally distributed random numbers were used.", filename = "../Output/01_test/normal_distributions.pdf")

figures_onepage <- IQRoutputFigure(plots[1:4], nrow = 2, ncol = 2, filename = "../Output/01_test/subset_normal_distributions.pdf")

table <- IQRoutputTable(mtcars[1:10,], xfooter = "See R datasets::mtcars", xtitle = "Cars data set", filename = "../Output/01_test/cars_table.txt")

figures_longlist <- IQRoutputFigure(plots,
                                    footer = "Normally distributed random numbers were used.", filename = "../Output/01_test/normal_distributions_longlist.pdf")

# -------------------------------------------------------------------------#
# Slide 1 - IQRoutputFigure ----
# -------------------------------------------------------------------------#

IQRoutputPPTX(
  info = bullet_list(
    "* A list of ggplot objects was created",
    "* Called `**IQRoutputFigure**` with arguments `nrow`, `ncol`, `footer` and `filename`",
    "* Called `IQRoutputPPTX` with bullet point list and `IQRoutputFigure` object."
  ),
  figures,
  section = "Test IQR Objects",
  title = "Plotting white noise",
  filename = "white_noise"
)

# -------------------------------------------------------------------------#
# Slide 2 - IQRoutputTable ----
# -------------------------------------------------------------------------#

IQRoutputPPTX(
  info = bullet_list(
    "* Used well known standard data set `mtcars`",
    "* Applied `**IQRoutputTable**` with arguments `xfooter` and `filename`",
    "* Called `IQRoutputPPTX` with bullet point list and `IQRoutputTable` object"
  ),
  table,
  section = "Test IQR Objects",
  title = "Well-known table",
  filename = "cars_table_manual_filename"
)



# -------------------------------------------------------------------------#
# Slide 3 - IQRoutputFigure (One Page) ----
# -------------------------------------------------------------------------#

IQRoutputPPTX(
  figures_onepage,
  section = "Test IQR Objects",
  title = "Plotting white noise",
  filename = "white_noise_onepage"
)

# -------------------------------------------------------------------------#
# Slide 4 - IQRoutputFigure (One Page with wide title) ----
# -------------------------------------------------------------------------#

IQRoutputPPTX(
  figures_onepage,
  section = "Test IQR Objects",
  title = "Plotting white noise and testing the wide title layout",
  layout = "Title and Content and Caption Wide",
  filename = "white_noise_onepage"
)

# -------------------------------------------------------------------------#
# Slide 5 - IQRoutputTable with filename and title derived ----
# -------------------------------------------------------------------------#

IQRoutputPPTX(table, section = "Test IQR Objects")

# -------------------------------------------------------------------------#
# Slide 6 - IQRoutputFigure with filename and title derived ----
# -------------------------------------------------------------------------#

IQRoutputPPTX(select_plot(figures_longlist, 1),
              select_plot(figures_longlist, 2),
              section = "Test IQR Objects")

# ------------------------------------------------------ #
# Finally: Create ----
# ------------------------------------------------------ #
# Copy-paste the filename shown in R console
# to explorer to open the file

# All slides
options(IQSlide.ratio = "16:9")
options(IQSlide.template = "Default")
IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_Default_16_9.pptx")
)


options(IQSlide.ratio = "4:3")
options(IQSlide.template = "Default")
IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_Default_4_3.pptx")
)


options(IQSlide.ratio = "16:9")
options(IQSlide.template = "IQ")
IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_IQ_16_9.pptx")
)

options(IQSlide.ratio = "4:3")
options(IQSlide.template = "IQ")
IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_IQ_4_3.pptx")
)

options(IQSlide.ratio = "16:9")
options(IQSlide.template = "IQNew")
IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_IQnew_16_9.pptx")
)

IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  titlelayout = "Title Slide white",
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_IQnew_16_9_white.pptx")
)


IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  titlelayout = "Title Slide white",
  QCed = TRUE,
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_IQnew_16_9_white_QCed.pptx")
)


# -------------------------------------------------------------------------#
# Test cleaning ----
# -------------------------------------------------------------------------#

# Remove the existing slide folder
clean_IQRoutputSection("Test Slide Deck")

# Remove non-existing slide folder
clean_IQRoutputSection("Test Slide Deck")

# Remove all folders
clean_IQRoutputSection()


setwd(mywd)

