# To update the vignette:
knitr::knit("vignettes/dstidyverse.Rmd.orig", output = "vignettes/dstidyverse.Rmd")

# To create a script that uploads the files:
knitr::purl("vignettes/dstidyverse.Rmd.orig", output = "vignettes/dstidyverse.R")
