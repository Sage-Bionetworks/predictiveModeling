\name{createENetTuneGrid}
\alias{createENetTuneGrid}
\title{createENetTuneGrid}
\usage{
  createENetTuneGrid(lambdamin = -5, lambdamax = 0,
    lambdas = NULL, alphas = NULL, alphamin = NULL,
    len = 5, verbose = TRUE)
}
\arguments{
  \item{lambdamin}{}

  \item{lambdamax}{}

  \item{lambdas}{a specific vector of lambdas to use
  instead of the range specified by the lambdamin and
  lambdamax parameters}

  \item{alphamin}{}

  \item{alphamax}{}

  \item{alphas}{a specific vector of alphas to use instead
  of the range specified by the alphamin and alphamax
  parameters}

  \item{len}{number of rows desired in the resulting tuning
  grid}

  \item{verbose}{whether to print verbose information about
  the tuning grid}
}
\value{
  the tuning grid
}
\description{
  Create a tuning grid appropriate for elastic net models
}
\author{
  Adam Margolin
}

