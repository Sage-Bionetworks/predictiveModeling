\name{filterPredictiveModelData}
\alias{filterPredictiveModelData}
\title{filterPredictiveModelData filters}
\usage{
  filterPredictiveModelData(featureData, responseData,
    filterResponseNas = TRUE,
    filterFeatureNasBy = "columns",
    featureVarianceThreshold = 0, corPValThresh = NULL)
}
\arguments{
  \item{featureData}{}

  \item{responseData}{}
}
\value{
  a list of the filtered feature and response data
}
\description{
  Filters feature and response data as specificied by the
  users based on rows or columns that contain NAs. features
  with low variance, or features not correlated with
  response.
}
\author{
  Adam Margolin
}

