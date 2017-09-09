## ----include=FALSE-------------------------------------------------------
library(dplyr)
library(knitr)

## ------------------------------------------------------------------------
lstParams <- LognormalParams(10e3, 1.5)
lstParams$mu
lstParams$sigma

## ------------------------------------------------------------------------
sample_data <- rlnorm(500, lstParams$mu, lstParams$sigma)
dfL <- CalcLognormalLikelihood(sample_data, c(9e3, 11e3), c(1, 2))

## ------------------------------------------------------------------------
dfL %>% head(5) %>% knitr::kable()

## ------------------------------------------------------------------------
library(ggplot2)
plt <- ggplot(dfL, aes(Mean, CV, z = Likelihood)) + geom_contour(bins=100) # + geom_raster(aes(fill = Likelihood))
plt

