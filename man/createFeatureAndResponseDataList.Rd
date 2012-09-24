\name{createFeatureAndResponseDataList}
\alias{createFeatureAndResponseDataList}
\title{createFeatureAndResponseDataList}
\usage{
  createFeatureAndResponseDataList(featureData,
    responseData)
}
\arguments{
  \item{featureData}{a matrix of feature data with samples
  on rows and features on columns}

  \item{responseData}{a matrix or AnnotatedDataFrame of
  pheno data with samples on rows and features on columns}
}
\value{
  a list containing two items: the feature data and the
  response data with intersecting sample names
}
\description{
  Prepares data for input to predictive modeling by
  matching column names of feature and response data and
  creating feature and response matrices with columns in
  the same order
}
\author{
  Adam Margolin
}

