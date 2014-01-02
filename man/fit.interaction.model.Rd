\name{fit.interaction.model}
\alias{fit.interaction.model}
\title{Cox model two features separately and together}
\description{Using a meta-analysis dataset take two features and Cox model them separately and together and extract HRs and p-values.}
\usage{
fit.interaction.model(feature1, feature2, expression.data, survival.data);
}
\arguments{
  \item{feature1}{String indicate what feature (gene/probe/etc.) should be extracted for analysis}
  \item{feature2}{String indicate what feature (gene/probe/etc.) should be extracted for analysis}
  \item{expression.data}{A list where each component is an expression matrix (patients = columns, features = rows) for a different dataset}
  \item{survival.data}{A list where each component is an object of class Surv}
}
\details{
The interaction model compares cases where feature1 and feature2 concord (both high or both low) to those where they do not.
That is, the model is y = x1 + x2 + (x1 == x2) and not the typical y = x1 + x2 + x1:x2
}
\value{Returns a vector of six elements containing (HR,P) pairs for feature1, feature2, and the interaction}
\author{Paul C. Boutros}
\examples{
\dontrun{
if (require(BoutrosLab.datasets.breast.cancer)) {
	x1 <- load.breast.cancer.datasets(datasets.to.load = 'Bild');
	x2 <- fit.interaction.model("10919_at", "109_at", x1$all.data, x1$all.survobj);
	}
}
}
\keyword{survival}
