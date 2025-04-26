# IQSlides

## Installation 

The package requires `officer` and `flextable` packages from CRAN rather than an older version from MRAN. To get the packages from CRAN, please run

```
install.packages("officer", repos = "https://cloud.r-project.org")
install.packages("flextable", repos = "https://cloud.r-project.org")
install.packages("webshot", repos = "https://cloud.r-project.org")
```

IQSlides 0.4.0 and higher requires officer 0.6.8, flextable 0.9.7, webshot 0.5.5 and zip 2.3.2 or higher.
IQSlides 0.3.0 and higher requires officer 0.3.14, flextable 0.5.9, webshot 0.5.2 and zip 2.1.1 or higher.
IQSlides 0.2.2 requires officer 0.3.5, flextable 0.5.5 and webshot 0.5.1.

After installing `webshot`, do not forget to run the phantomJS installer:

```
webshot::install_phantomjs()
```

## Getting started

After installation, please have a look at the example:

```
library(IQSlides)
?IQSlidedeck
```
