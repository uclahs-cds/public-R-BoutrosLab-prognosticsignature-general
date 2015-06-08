\name{score.multifeature.signature}
\alias{score.multifeature.signature}
\title{Classify each patient in a meta-analysis object using weighted-features}
\description{Given a meta-analysis object, take a list of features and their weights, and use these to score each patient. Scores are given by median-dichotomizing each feature and up (+1) and down (-1), and then weighting those as specified by the user.  Those per-feature scores are summed for each patient, and then patients are median-dichotomized according to their net-score /bold{within each cohort}. Classifications across cohorts can then be merged for an overall analysis.}
\usage{
score.multifeature.signature(expression.data, survival.data, feature.names,
    feature.weights, feature.thresholds = NULL, score.cutoff = NULL);
}
\arguments{
  \item{expression.data}{A list where each component is an expression matrix (patients = columns, features = rows) for a different dataset}
  \item{survival.data}{A list where each component is an object of class Surv}
  \item{feature.names}{A vector containing the names of each feature to include in the model}
  \item{feature.weights}{A vector containing the weights of each feature to include in the model}
  \item{feature.thresholds}{A vector, or NULL by default, containing the thresholds for low/high expression classification, of each feature to include in the model. If NULL (default), each feature is median dichotimized.}
  \item{score.cutoff}{A cutoff score which is used to group the patients in 2, defaults to median score of signature}
}
\value{Returns a data-frame with patients as rows and columns as (group, survtime, survstat, score, max.score, dataset, patient.ID) tuples.}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)) {
	x <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	y <- score.multifeature.signature(x$all.data, x$all.survobj, "10_at", 3);
	}
}
}
\keyword{survival}
