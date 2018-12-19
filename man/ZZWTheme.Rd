% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ZZWTheme.R
\name{ZZWTheme}
\alias{ZZWTheme}
\title{ZZWTheme}
\usage{
ZZWTheme(type = "common")
}
\description{
ggplot theme
}
\examples{
mydata <- data.frame(
 ymax   = c(7.5,9.5,9,13.5,4.2),
 class  = c("A","A","A","C","C")
)
test <- ggplot(mydata) +
 geom_linerange(aes(x = xstart, ymin = ymin , ymax = ymax , colour = class) , size = 1.5)
test
mytheme <- ZZWTheme()
test + mytheme
}
\author{
Zhiwei Zhou
\email{zhouzw@sioc.ac.cn}
}