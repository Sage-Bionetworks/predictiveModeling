\name{plotPredictiveModelHeatmap}
\alias{plotPredictiveModelHeatmap}
\title{Plot a heatmap displaying predictive features selected by a regression model.}
\usage{
  plotPredictiveModelHeatmap(coefs, featureData,
    responseData, featureNames = NULL, featurePerc = NULL,
    responseLabel = "", suffix, outputFile = NULL,
    heatmapThresh = 3, responseName = "sensitivity",
    cex.factor = 1)
}
\description{
  Plots a heatmap displaying selected features, with
  regression weights displayed to the left of each feature,
  and response values displayed below the feature heatmap.
}
\author{
  Adam Margolin
}

