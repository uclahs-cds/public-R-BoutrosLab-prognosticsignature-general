\name{flatten.expression.data}
\alias{flatten.expression.data}
\title{Utility function for collapsing a meta-analysis survival object}
\description{Takes a meta-analysis object and collapses all the expression data into a data-frame}
\usage{
flatten.expression.data(object);
}
\arguments{
  \item{object}{A meta-analysis object}
}
\details{TODO}
\value{Returns a data-frame with components dataset, survtime, and survstat}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)){
	x1 <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	x2 <- flatten.expression.data(x1);
	}
}
}
\keyword{survival}
\keyword{manip}
