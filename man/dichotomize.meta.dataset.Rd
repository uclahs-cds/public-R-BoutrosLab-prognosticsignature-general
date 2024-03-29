\name{dichotomize.meta.dataset}
\alias{dichotomize.meta.dataset}
\title{Dichotomize and unlist a meta-analysis list}
\description{Takes a meta-analysis list (and possibly extra data) and median dichotomizes based on a specific gene, then returns the unlisted data to the caller.}
\usage{
dichotomize.meta.dataset(feature.name, expression.data, survival.data, other.data = NULL);
}
\arguments{
  \item{feature.name}{Character indicate what feature (gene/probe/etc.) should be extracted for analysis}
  \item{expression.data}{A list where each component is an expression matrix (patients = columns, genes = rows) for a different dataset}
  \item{survival.data}{A list where each component is an object of class Surv}
  \item{other.data}{A list of other covariates to be unlisted in the final output (all elements in this list are used)}
}
\details{NB: other.data handling of missing components (i.e. those present in only some datasets) has not been debugged (but may work regardless).}
\value{Returns a list containing components  groups (the median dichotomization), survtime (in the units of the input data), and survstat.  Additional vectors are unlisted from other.data if that parameter is not NULL.}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)) {
	x1 <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	x2 <- dichotomize.meta.dataset("10_at", x1$all.data, x1$all.survobj);
	}
}
}
\keyword{survival}
\keyword{manip}
