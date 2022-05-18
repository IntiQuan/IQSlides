pkgname <- "IQSlides"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "IQSlides-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('IQSlides')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("IQRoutputPPTX")
### * IQRoutputPPTX

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IQRoutputPPTX
### Title: Push contents into a PPTX output
### Aliases: IQRoutputPPTX

### ** Examples

## Not run: 
##D 
##D 
##D library(IQRtools)
##D library(ggplot2)
##D library(flextable)
##D setwd(tempdir())
##D 
##D # For compliance information in the footer of the slides
##D IQRinitCompliance("IQSlidedeck.R")
##D 
##D # ------------------------------------------------------
##D # Slide 1: Create a plot and a bullet point list
##D # ------------------------------------------------------
##D 
##D p1 <- IQRggplot(cars, aes(x = speed, y = dist)) + geom_point()
##D caption(p1) <- "Source: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley."
##D 
##D IQRoutputPPTX(
##D   c("Cars data set from R datasets",
##D     "The data vie the speed of cars and distances taken to stop shown as plot"),
##D   p1,
##D   section = "Slides with plots",
##D   title = "Overview of cars distance vs time to stop",
##D   filename = "carsPlot"
##D )
##D 
##D 
##D # ------------------------------------------------------
##D # Slide 2: Create a table and a bullet point list
##D # ------------------------------------------------------
##D 
##D IQRoutputPPTX(
##D   info = c("Cars data set from R datasets",
##D            "The data vie the speed of cars and distances taken to stop"),
##D   table = IQ_table(cars[1:16,]),
##D   section = "Slides with tables",
##D   title = "Overview of cars distance vs time to stop shown as table",
##D   filename = "carsTable"
##D )
##D 
##D # ------------------------------------------------------
##D # Slide 3: Create a table and a formatted bullet point list
##D # ------------------------------------------------------
##D 
##D IQRoutputPPTX(
##D   IQ_bullet_list(
##D     "* Bullet point with **bold** and *italic*" ,
##D     "* Another bullet point",
##D     "  * Indented by two spaces",
##D     "  * Another one",
##D     "* And back to ***really* outer** level",
##D     "* Some sort of formula `E_rel_ = m\\*c^2^`"
##D   ),
##D   IQ_table(
##D     cars[1:16,]
##D   ),
##D   section = "Slides with tables",
##D   title = "Overview of cars distance vs time to stop shown as table",
##D   filename = "carsTable2"
##D )
##D 
##D 
##D 
##D # ------------------------------------------------------
##D # Finally: Create
##D # ------------------------------------------------------
##D # Copy-paste the filename shown in R console
##D # to explorer to open the file
##D 
##D # All slides
##D IQSlidedeck(
##D   title = "My first slidedeck with IQSlides",
##D   subtitle = "Cars and the time they take to stop",
##D   affiliation = "Daniel Kaschek, IntiQuan",
##D   date = "2019-11-20"
##D )
##D 
##D 
##D # Just section slides
##D IQSlidedeck(section = "Slides with plots")
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IQRoutputPPTX", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IQSlidedeck")
### * IQSlidedeck

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IQSlidedeck
### Title: Build a slide deck from a slide folder
### Aliases: IQSlidedeck

### ** Examples

## Not run: 
##D 
##D 
##D library(IQRtools)
##D library(ggplot2)
##D library(flextable)
##D setwd(tempdir())
##D 
##D # For compliance information in the footer of the slides
##D IQRinitCompliance("IQSlidedeck.R")
##D 
##D # ------------------------------------------------------
##D # Slide 1: Create a plot and a bullet point list
##D # ------------------------------------------------------
##D 
##D p1 <- IQRggplot(cars, aes(x = speed, y = dist)) + geom_point()
##D caption(p1) <- "Source: Ezekiel, M. (1930) Methods of Correlation Analysis. Wiley."
##D 
##D IQRoutputPPTX(
##D   c("Cars data set from R datasets",
##D     "The data vie the speed of cars and distances taken to stop shown as plot"),
##D   p1,
##D   section = "Slides with plots",
##D   title = "Overview of cars distance vs time to stop",
##D   filename = "carsPlot"
##D )
##D 
##D 
##D # ------------------------------------------------------
##D # Slide 2: Create a table and a bullet point list
##D # ------------------------------------------------------
##D 
##D IQRoutputPPTX(
##D   info = c("Cars data set from R datasets",
##D            "The data vie the speed of cars and distances taken to stop"),
##D   table = IQ_table(cars[1:16,]),
##D   section = "Slides with tables",
##D   title = "Overview of cars distance vs time to stop shown as table",
##D   filename = "carsTable"
##D )
##D 
##D # ------------------------------------------------------
##D # Slide 3: Create a table and a formatted bullet point list
##D # ------------------------------------------------------
##D 
##D IQRoutputPPTX(
##D   IQ_bullet_list(
##D     "* Bullet point with **bold** and *italic*" ,
##D     "* Another bullet point",
##D     "  * Indented by two spaces",
##D     "  * Another one",
##D     "* And back to ***really* outer** level",
##D     "* Some sort of formula `E_rel_ = m\\*c^2^`"
##D   ),
##D   IQ_table(
##D     cars[1:16,]
##D   ),
##D   section = "Slides with tables",
##D   title = "Overview of cars distance vs time to stop shown as table",
##D   filename = "carsTable2"
##D )
##D 
##D 
##D 
##D # ------------------------------------------------------
##D # Finally: Create
##D # ------------------------------------------------------
##D # Copy-paste the filename shown in R console
##D # to explorer to open the file
##D 
##D # All slides
##D IQSlidedeck(
##D   title = "My first slidedeck with IQSlides",
##D   subtitle = "Cars and the time they take to stop",
##D   affiliation = "Daniel Kaschek, IntiQuan",
##D   date = "2019-11-20"
##D )
##D 
##D 
##D # Just section slides
##D IQSlidedeck(section = "Slides with plots")
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IQSlidedeck", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("IQ_bullet_list")
### * IQ_bullet_list

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: IQ_bullet_list
### Title: Create bullet point list
### Aliases: IQ_bullet_list bullet_list

### ** Examples

bl <- IQ_bullet_list(
  "* Bullet point with **bold** and *italic*" ,
  "* Another bullet point",
  "  * Indented by two spaces",
  "  * Another one",
  "* And back to ***really* outer** level",
  "* Some sort of formula `E~rel~ = m\\*c^2^`"
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("IQ_bullet_list", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
