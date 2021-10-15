\dontrun{


library(IQRtools)
library(ggplot2)
library(flextable)
setwd(tempdir())

# For compliance information in the footer of the slides
IQRinitCompliance("IQSlidedeck.R")

# ------------------------------------------------------
# Slide 1: Create a plot and a bullet point list
# ------------------------------------------------------

p1 <- IQRggplot(cars, aes(x = speed, y = dist)) + geom_point()
caption(p1) <- "Source: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley."

IQRoutputPPTX(
  c("Cars data set from R datasets",
    "The data vie the speed of cars and distances taken to stop shown as plot"),
  p1,
  section = "Slides with plots",
  title = "Overview of cars distance vs time to stop",
  filename = "carsPlot"
)


# ------------------------------------------------------
# Slide 2: Create a table and a bullet point list
# ------------------------------------------------------

IQRoutputPPTX(
  info = c("Cars data set from R datasets",
           "The data vie the speed of cars and distances taken to stop"),
  table = IQ_table(cars[1:16,]),
  section = "Slides with tables",
  title = "Overview of cars distance vs time to stop shown as table",
  filename = "carsTable"
)

# ------------------------------------------------------
# Slide 3: Create a table and a formatted bullet point list
# ------------------------------------------------------

IQRoutputPPTX(
  IQ_bullet_list(
    "* Bullet point with **bold** and *italic*" ,
    "* Another bullet point",
    "  * Indented by two spaces",
    "  * Another one",
    "* And back to ***really* outer** level",
    "* Some sort of formula `E_rel_ = m\\*c^2^`"
  ),
  IQ_table(
    cars[1:16,]
  ),
  section = "Slides with tables",
  title = "Overview of cars distance vs time to stop shown as table",
  filename = "carsTable2"
)



# ------------------------------------------------------
# Finally: Create
# ------------------------------------------------------
# Copy-paste the filename shown in R console
# to explorer to open the file

# All slides
IQSlidedeck(
  title = "My first slidedeck with IQSlides",
  subtitle = "Cars and the time they take to stop",
  affiliation = "Daniel Kaschek, IntiQuan",
  date = "2019-11-20"
)


# Just section slides
IQSlidedeck(section = "Slides with plots")

}
