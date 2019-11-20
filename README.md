# IQSlides

## Installation

The package requires `officer` and `flextable` packages from CRAN rather than an older version from MRAN. To get the packages from CRAN, please run

```
install.packages("officer", repos = "https://cloud.r-project.org")
install.packages("flextable", repos = "https://cloud.r-project.org")
install.packages("webshot", repos = "https://cloud.r-project.org")
```

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
