\name{sample.metadata}
\alias{sample.metadata}
\title{Extract a dataset-wise sample from a meta-analysis list}
\description{Returns a fraction of a meta-analysis list of patient data}
\usage{
sample.metadata(data, fraction = 0.5, return.all = TRUE);
}
\arguments{
  \item{data}{A list of lists with components all.data and all.survobj (i.e. a metadata object)}
  \item{fraction}{What fraction of the data should be selected from each dataset?}
  \item{return.all}{Return both the sample and its complement (TRUE) or just the sample (FALSE)?}
}
\value{Returns a meta-analysis list}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)) {
	x1 <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	y1 <- sample.metadata(x1);
	y2 <- sample.metadata(x1, fraction = 0.2);
	}
}
}
\keyword{survival}
