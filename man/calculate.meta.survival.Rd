\name{calculate.meta.survival}
\alias{calculate.meta.survival}
\title{Fit a meta-analytic Cox proportional hazards model to a single feature}
\description{Takes a meta-analysis data object and fits a Cox proportional hazards model (possibly with adjustment for some specific covariates) by median-dichotomizing patients within each individual dataset.}
\usage{
calculate.meta.survival(feature.name, expression.data, survival.data, rounding = 3,
    other.data = NULL);
}
\arguments{
  \item{feature.name}{Character indicate what feature (gene/probe/etc.) should be extracted for analysis}
  \item{expression.data}{A list where each component is an expression matrix (patients = columns, genes = rows) for a different dataset}
  \item{survival.data}{A list where each component is an object of class Surv}
  \item{rounding}{How many digits after the decimal place to include}
  \item{other.data}{A list of other covariates to be passed to the Cox model (all elements in this list are used}
}
\details{TODO}
\value{Returns a vector containing the HR, p-value, n, and 95\% confidence limits of the HR (see fit.coxmodel() for details)}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)) {
	x1 <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	x2 <- calculate.meta.survival("10_at", x1$all.data, x1$all.survobj);
	x3 <- calculate.meta.survival("100_at", x1$all.data, x1$all.survobj, rounding = 5);
	}
}
}
\keyword{survival}
