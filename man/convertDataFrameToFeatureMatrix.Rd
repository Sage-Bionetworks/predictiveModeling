\name{convertDataFrameToFeatureMatrix}
\alias{convertDataFrameToFeatureMatrix}
\title{convertDataFrameToFeatureMatrix}
\usage{
  convertDataFrameToFeatureMatrix(dataFrame)
}
\description{
  Convert a data frame to a numeric feature matrix that can
  be used as features in a predictive model Numeric
  features are left as numeric. Character or factor
  features are converted to binary features if they have 2
  unique values or if there are more than 2 unique values
  it is expanded to a binary feature for each possible
  value.
}
\author{
  Adam Margolin
}

