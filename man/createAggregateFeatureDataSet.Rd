\name{createAggregateFeatureDataSet}
\alias{createAggregateFeatureDataSet}
\title{createAggregateFeatureDataSet}
\usage{
  createAggregateFeatureDataSet(dataSetList)
}
\description{
  Combines multiple datasets into a feature matrix. Takes a
  list of matrices or ExpressionSets and rbinds them using
  their intersection of column names. The key value for
  each dataset given as a argument is pre-pended to the
  name of the features of that dataset.
}
\author{
  Adam Margolin
}

