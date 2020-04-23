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

legend <- cowplot::get_legend(plots[[1]])
plots <- lapply(plots, function(x) x + theme(legend.position = "none"))

figures <- IQRoutputFigure(plots, nrow = 2, ncol = 2, opt.layout = list(legend = legend),
                           footer = "Normally distributed random numbers were used.", filename = "../Output/01_test/normal_distributions.pdf")

figures_onepage <- IQRoutputFigure(plots[1:4], nrow = 2, ncol = 2, filename = "../Output/01_test/subset_normal_distributions.pdf")

table <- IQRoutputTable(mtcars[1:10,], xfooter = "See R datasets::mtcars", xtitle = "Cars data set", filename = "../Output/01_test/cars_table.txt")



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
  filename = "cars_table"
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
# ------------------------------------------------------ #
# Finally: Create ----
# ------------------------------------------------------ #
# Copy-paste the filename shown in R console
# to explorer to open the file

# All slides
IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects.pptx")
)


options(IQSlide.ratio = "4:3")
IQSlidedeck(
  title = "Test Slide Deck",
  subtitle = "IQR Objects",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = Sys.Date(),
  filename = file.path(mywd, "../testresults/script_IQSlidedeck_IQobjects_43.pptx")
)




setwd(mywd)
