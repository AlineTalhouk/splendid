% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequential.R
\name{sequential}
\alias{sequential}
\alias{sequential_train}
\alias{sequential_pred}
\title{Sequential Algorithm}
\usage{
sequential_train(sm, data, class, boxplot = FALSE)

sequential_pred(fit, sm, data, class, boxplot = FALSE)
}
\arguments{
\item{sm}{a \code{splendid_model} object}

\item{data}{data frame with rows as samples, columns as features}

\item{class}{true/reference class vector used for supervised learning}

\item{boxplot}{if \code{TRUE}, boxplots are plotted showing the distribution of
F1-scores per class, for every algorithm.}

\item{fit}{list of fitted models from \code{sequential_train}}
}
\value{
\code{sequential_train} returns a list of fits over the top-ranked
sequence.

\code{sequential_pred} returns a list of two elements
\itemize{
\item \code{prob}: predicted sequential probabilities
\item \code{cm}: confusion matrices for each class
}
}
\description{
Sequentially train top ranked algorithms on each class ordered by class
performance and predict a given class using the sequentially trained fits.
}
\details{
\code{sequential_train} sequentially trains One-Vs-All models until all classes
have been classified. Hence for \code{n} classes, there are \code{n - 1} sequential
fits. \code{sequential_pred} predicts class membership for each One-Vs-All
sequential model. Performance is evaluated on by-class F1-scores, since these
are better for evaluation than other metrics such as accuracy, precision, and
recall.
}
\examples{
dat <- iris[, 1:4]
class <- iris$Species
sm <- splendid_model(dat, class, n = 2, algorithms = c("slda", "xgboost"))
st <- sequential_train(sm, dat, class)
sp <- sequential_pred(st, sm, dat, class)
}
\author{
Dustin Johnson, Derek Chiu
}
