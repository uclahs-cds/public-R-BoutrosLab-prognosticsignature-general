# The BoutrosLab.prognosticsignature.general package is copyright (c) 2010 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

dichotomize.meta.dataset <- function(feature.name, expression.data, survival.data, other.data = NULL) {

	# we'll return the overall groups and survival data to the caller
	groups   <- vector();
	survival <- vector();
	status   <- vector();

	# loop over all datasets
	for (i in 1:length(expression.data)) {

		# localize the current dataset
		expression.values <- expression.data[[i]];
		expression.values <- as.vector( unlist( expression.values[feature.name,] ) );
		survival.object   <- survival.data[[i]];

		# dichotomize this dataset
		dichotomized.results <- dichotomize.dataset(expression.values);

		# add the returned data to return-objects
		groups   <- c(groups,   dichotomized.results);
		survival <- c(survival, survival.object[,1]);
		status   <- c(status,   survival.object[,2]);

		}

	# create an object of data to return
	to.return <- list(
		groups = groups,
		survtime = survival,
		survstat = status
		);

	# if there is an extra data unlist it and pass it back
	if (!is.null(other.data) & class(other.data) == 'list') {

		# loop over all elements
		for (i in 1:length(other.data)) {

			# create a temporary vector to store the data
			temp <- vector();

			# loop over all datasets and concatenate the data
			for (j in 1:length(other.data[[i]])) {

				# check that the feature exists in this dataset
				expression.values <- expression.data[[j]];
				expression.values <- as.vector( unlist( expression.values[feature.name,] ) );
				if ( all( is.na(expression.values) ) ) { next; }

				# if it does exist in this dataset keep the annotation data
				temp <- c(temp, as.character(other.data[[i]][[j]]));

				}

			# ensure data is right length
			if (length(temp) != length(to.return$groups)) { next; }

			# add the data to the return object
			to.return[[3+i]] <- temp;
			names(to.return)[[3+i]] <- names(other.data)[[i]];

			}

		}

	return(to.return);

	}
