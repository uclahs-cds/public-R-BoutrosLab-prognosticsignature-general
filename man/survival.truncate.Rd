\name{survival.truncate}
\alias{survival.truncate}
\title{Utility function for truncating a meta-analytic survival list}
\description{Takes a list of survival objects and truncates each to a specified time. Removes any events that happen after the specified time and truncates any time-values beyond that time to the threshold itself.}
\usage{
survival.truncate(all.survobj, time);
}
\arguments{
  \item{all.survobj}{A list where each component is a survobj}
  \item{time}{An integer time-threshold in the time-units of the survobj itself}
}
\details{TODO}
\value{Returns a list of survobj}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)){
	x1 <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	x2 <- survival.truncate(x1$all.survobj, 1);
	}
}
}
\keyword{survival}
\keyword{manip}
