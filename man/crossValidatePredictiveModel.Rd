\name{crossValidatePredictiveModel}
\alias{crossValidatePredictiveModel}
\title{Perform cross validation on a preditive model, this method support both caret models and PredictiveModels}
\usage{
  crossValidatePredictiveModel(featureData, responseData,
    model, numFolds = 5, trControl = defaultTrainControl(),
    ...)
}
\arguments{
  \item{featureData}{}

  \item{responseData}{}

  \item{method}{either an instance of PredictiveModel or a
  string holding the name of one of the machine learning
  methods that caret supports}

  \item{numFolds}{defaults to 5}

  \item{trControl}{defaults to defaultTrainControl}
}
\value{
  a list of PredictiveModelPerformance one per fold
}
\description{
  Perform cross validation on a preditive model, this
  method support both caret models and PredictiveModels
}
\author{
  Adam Margolin
}
\seealso{
  defaultTrainControl
}

