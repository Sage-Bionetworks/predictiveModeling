\name{trainPartition}
\alias{trainPartition}
\title{Perform one iteration of model training on a portion of the data}
\usage{
  trainPartition(testIndices, featureData, responseData,
    model, trControl = defaultTrainControl(), ...)
}
\arguments{
  \item{testIndices}{}

  \item{featureData}{}

  \item{responseData}{}

  \item{method}{either an instance of PredictiveModel or a
  string holding the name of one of the machine learning
  methods that caret supports}

  \item{trControl}{defaults to defaultTrainControl}
}
\value{
  an instance of PredictiveModelResults
}
\description{
  Perform one iteration of model training on a portion of
  the data
}
\author{
  Nicole Deflaux
}

