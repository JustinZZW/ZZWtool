% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ZZWImpute.R
\name{ZZWImpute}
\alias{ZZWImpute}
\title{ZZWImpute}
\usage{
ZZWImpute(raw_matrix, label, k = 10, rowmax = 0.5, colmax = 0.8,
  maxp = 1500, rng.seed = 362436069)
}
\arguments{
\item{raw_matrix}{row: feature; column: sample}

\item{label}{feature id}
}
\description{
impute NA using KNN algorthem
}
\author{
Zhiwei Zhou
}
