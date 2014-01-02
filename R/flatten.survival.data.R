# The BoutrosLab.prognosticsignature.general package is copyright (c) 2011 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

# aggregate all survival data into one file
flatten.survival.data <- function(object) {

	# verify that we can work with this object
	if (is.null(object$all.data)) {
		warning('Unable to access the all.data slot!');
		return(NA);
		}
	else if (is.null(object$all.survobj)) {
		warning('Unable to access the all.survobj slot!');
		return(NA);
		}

	# save the data in temporary objects to ease aggregating
	all.IDs <- vector();
	all.survtimes <- vector();
	all.survstats <- vector();
	all.datasets  <- vector();

	# loop through all datasets
	for (dataset in names(object$all.survobj)) {
		all.IDs <- c(all.IDs, colnames(object$all.data[[dataset]]));
		all.datasets  <- c(all.datasets,  rep(dataset, nrow(object$all.survobj[[dataset]])));
		all.survtimes <- c(all.survtimes, object$all.survobj[[dataset]][,1]);
		all.survstats <- c(all.survstats, object$all.survobj[[dataset]][,2]);
		}

	# give results back to the caller
	return(
		data.frame(
			ID = all.IDs,
			dataset = all.datasets,
			survtime = all.survtimes,
			survstat = all.survstats
			)
		);

	}
