\name{create.survobj}
\alias{create.survobj}
\title{Utility function for loading meta-analysis lists}
\description{Create Surv objects from an annotation-matrix with handling for different time units.}
\usage{
create.survobj(annotation);
}
\arguments{
  \item{annotation}{A patient annotation matrix (patients = rows) with (at least) columns for survtime, survstat, and survtime.unit}
}
\value{Returns an object of class Surv}
\author{Paul C. Boutros}
\examples{
\dontrun{
#FIXME
if (require(BoutrosLab.datasets.breast.cancer) && file.exists(paste(.libPaths()[1], 
    '/BoutrosLab.datasets.breast.cancer/datasets/Bild/patient_annotation.txt', sep=''))) {

	annotation <- read.table(
		paste(.libPaths()[1],
		    '/BoutrosLab.datasets.breast.cancer/datasets/Bild/patient_annotation.txt',
		     sep=''),
		header = TRUE,
		row.names = 1,
		sep = "\t"
		);

	# select the appropriate survtime and survstat variable for this dataset
	annotation$survstat      <- annotation[,'e.os'];
	annotation$survtime      <- annotation[,'t.os'];
	annotation$survtime.unit <- annotation[,'t.os.unit'];
		
	# only keep samples with survival data
	annotation <- annotation[!is.na(annotation$survstat) & !is.na(annotation$survstat),];
	
	o <- create.survobj(annotation = annotation);
	}
}
}
\keyword{survival}
