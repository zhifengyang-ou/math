% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Bootstrap method to estimate the confidence interval of certain characteristics
by certain function of samples}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{The times of iteration}

\item{x}{The sample data}

\item{fun}{the function to calculate the property of samples}

\item{alpha}{The confidence}

\item{cx}{The size of text in histogram plot}

\item{...}{}
}
\value{
plot of a proporty of resampling data and list of confidence intervals
}
\description{
Bootstrap method to estimate the confidence interval of certain characteristics
by certain function of samples
}
\examples{
set.seed(39); sam=rnorm(25,mean=25,sd=10)
windows();myboot2(10000,x=sam,fun="mean",alpha=0.05,xlab="mean",col="Purple",cx=1.5) # mac quartz()
}
