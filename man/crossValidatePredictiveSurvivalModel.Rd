\name{crossValidatePredictiveSurvivalModel}
\alias{crossValidatePredictiveSurvivalModel}
\title{Perform cross validation on a preditive model, this model supports Cox model and predictiveModeling}
\usage{
  crossValidatePredictiveSurvivalModel(model, exprData,
    copyData, clinicalFeaturesData, clinicalSurvData,
    numFolds = 5, ...)
}
\arguments{
  \item{featureData}{}

  \item{responseData}{}

  \item{model}{either an instance of PredictiveModel or a
  string holding the name of one of the machine learning
  methods that caret supports}

  \item{numFolds}{defaults to 5}

  \item{trControl}{defaults to defaultTrainControl}
}
\value{
  a list of PredictiveModelPerformance one per fold
}
\description{
  Perform cross validation on a preditive model, this model
  supports Cox model and predictiveModeling
}
\author{
  Adam Margolin
}
\seealso{
  defaultTrainControl
}

