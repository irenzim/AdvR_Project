% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{calc_metrics_classification}
\alias{calc_metrics_classification}
\title{Calculate Metrics for Classification}
\usage{
calc_metrics_classification(labels, predicted)
}
\arguments{
\item{labels}{The true labels}

\item{predicted}{The predicted labels}
}
\value{
A list containing the calculated metrics:
\item{accuracy}{Accuracy of the classification}
\item{recall}{Recall (true positive rate)}
\item{precision}{Precision (positive predictive value)}
\item{f1}{F1-score (harmonic mean of precision and recall)}
}
\description{
This function calculates various evaluation metrics for a binary classification problem,
including accuracy, recall, precision, and F1-score.
}
