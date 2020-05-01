library(IQSlides)
library(ggplot2)
library(IQRtools)
library(flextable)

IQRinitCompliance("script_IQSlidedeck.R")

mywd <- getwd()
setwd(tempdir())

# ------------------------------------------------------ #
# Slide 0: Simple bullet point list ----
# ------------------------------------------------------ #

IQRoutputPPTX(
  c("Cars data set from R datasets",
    "The data vie the speed of cars and distances taken to stop shown as plot"),
  section = "Plain Slides",
  title = "Just a test",
  filename = "firstTest"
)


# ------------------------------------------------------ #
# Slide 1: Create a plot and a bullet point list ----
# ------------------------------------------------------ #

p1 <- ggplot(cars, aes(x = speed, y = dist)) + geom_point()
caption(p1) <- "Source: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley."

IQRoutputPPTX(
  c("Cars data set from R datasets",
    "The data vie the speed of cars and distances taken to stop shown as plot"),
  p1,
  section = "Slides with plots",
  title = "Overview of cars distance vs time to stop",
  filename = "carsPlot"
)


# ------------------------------------------------------ #
# Slide 2: Create a table and a bullet point list ----
# ------------------------------------------------------ #

table1 <- IQ_table(cars[1:16,])

IQRoutputPPTX(
  c("Cars data set from R datasets",
    "The data vie the speed of cars and distances taken to stop"),
  table1,
  section = "Slides with tables",
  title = "Overview of cars distance vs time to stop shown as table",
  filename = "carsTable2"
)


# ------------------------------------------------------ #
# Slide 3: Create a table and a formatted bullet point list ----
# ------------------------------------------------------ #

table1 <- IQ_table(cars[1:16,])

IQRoutputPPTX(
  IQ_bullet_list(
    "* Bullet point with **bold** and *italic*" ,
    "* Another bullet point",
    "  * Indented by two spaces",
    "  * Another one",
    "* And back to ***really* outer** level",
    "* Some sort of formula `E~rel~ = m\\*c^2^`"
  ),
  table1,
  section = "Slides with tables",
  title = "Overview of cars distance vs time to stop shown as table",
  filename = "carsTable3"
)


# ------------------------------------------------------ #
# Slide 4: Create a table and a formatted bullet point list (switch sides) ----
# ------------------------------------------------------ #

table1 <- IQ_table(cars[1:16,])

IQRoutputPPTX(
  table1,
  IQ_bullet_list(
    "* Bullet point with **bold** and *italic*" ,
    "* Another bullet point",
    "  * Indented by two spaces",
    "  * Another one",
    "* And back to ***really* outer** level",
    "* Some sort of formula `E~rel~ = m\\*c^2^`"
  ),
  section = "Slides with tables",
  title = "Overview of cars distance vs time to stop shown as table",
  filename = "carsTable4"
)


# -------------------------------------------------------------------------#
# Slide 5: Plots from file ----
# -------------------------------------------------------------------------#

myplot <- ggplot2::ggplot(mapping = aes(x = 1:3, y = 1:3)) + geom_line()

pdf("plot01.pdf")
print(myplot)
dev.off()

png("plot01.png")
print(myplot)
dev.off()

IQRoutputPPTX(
  plot = IQ_image("plot01.png"),
  section = "Slides with plots",
  title = "Plot from png file",
  filename = "plot_png"
)

IQRoutputPPTX(
  plot = IQ_image("plot01.pdf", pages = 1),
  section = "Slides with plots",
  title = "Plot from pdf file",
  filename = "plot_pdf"
)

IQRoutputPPTX(
  plot = IQ_image(myplot),
  section = "Slides with plots",
  title = "Plot from ggplot object",
  filename = "plot_gg"
)


# ------------------------------------------------------ #
# Finally: Create ----
# ------------------------------------------------------ #
# Copy-paste the filename shown in R console
# to explorer to open the file

# All slides
IQSlidedeck(
  title = "My first slidedeck with IQSlides",
  subtitle = "Cars and the time they take to stop",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = "2019-11-20",
  filename = file.path(mywd, "../testresults/script_IQSlidedeck.pptx")
)


# Just section slides
IQSlidedeck(section = "Slides with plots")

setwd(mywd)

