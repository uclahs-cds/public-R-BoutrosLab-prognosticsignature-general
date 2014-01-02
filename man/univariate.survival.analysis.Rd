\name{univariate.survival.analysis}
\alias{univariate.survival.analysis}
\title{Calculate the prognostic capacity of all genes in a meta-analysis object}
\description{Using a meta-analysis dataset take each gene one by one and Cox model them. Extract HRs, confidence intervals, and p-values.}
\usage{
univariate.survival.analysis(object, studywise = FALSE, rounding = 3, other.data = NULL);
}
\arguments{
  \item{object}{A meta-analysis object}
  \item{studywise}{A boolean flag for processing all studies independently or as one group}
  \item{rounding}{How many decimal places of precision should be returned?}
  \item{other.data}{A list of other covariates to be unlisted in the final output (all elements in this list are used)}
}
\details{The meta-analysis object must contain (at minimum) components: all.probesets, all.data, and all.survobj}
\value{Returns a data-frame with six columns: HR, lower 95\% CI of the HR, upper 95\% CI of the HR, P (Wald test), n (samples in analysis), Q (FDR-corrected P)}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)) {
	x1 <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	x2 <- univariate.survival.analysis(x1);
	x3 <- univariate.survival.analysis(x1, rounding = 5);
	}
}
}
\keyword{survival}
\keyword{univar}
