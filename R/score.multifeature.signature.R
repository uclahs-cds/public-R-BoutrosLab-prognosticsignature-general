# The BoutrosLab.prognosticsignature.general package is copyright (c) 2013 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

score.multifeature.signature <- function(expression.data, survival.data, feature.names, feature.weights, score.cutoff = NULL) {

	# verify that we got appropriate input data
	if ("list" != class(expression.data)) { warning('Invalid expression.data'); return(NA); }
	if ("list" != class(survival.data)) { warning('Invalid survival.data'); return(NA); }
	if (length(expression.data) != length(survival.data)) { warning('Invalid survival.data length'); return(NA); }
	if (length(feature.names) != length(feature.weights)) { warning('Invalid feature.weights length'); return(NA); }
	if (length(names(expression.data)) == 1) { if (is.null(names(expression.data))) { warning("Ensure lists are named"); return(NA); } }

	# create locals to store the patient information
	overall.patient.groups   <- vector();
	overall.patient.survival <- vector();
	overall.patient.status   <- vector();
	overall.patient.scores   <- vector();
	maximum.patient.scores   <- vector();
	overall.dataset.names    <- vector();
	overall.patient.IDs      <- vector();

	# loop through the datasets one by one
	for (i in 1:length(expression.data)) {

		# localize the current working data
		expression.temp <- expression.data[[i]];
		survobj         <- survival.data[[i]];

		# if none of the genes are present in the dataset give NAs
		if ( 0 == sum(feature.names %in% rownames(expression.temp)) ) {

			scores     <- rep(NA, length = nrow(survobj));
			max.scores <- rep(NA, length = nrow(survobj));

			}

		else {

			# create a vector to store scores
			scores     <- rep(0, length = nrow(survobj));
			max.scores <- rep(0, length = nrow(survobj));

			for (j in 1:length(feature.names)) {

				# localize feature name and weights
				feature.name   <- feature.names[j];
				feature.weight <- feature.weights[j];

				# skip this feature if it isn't found
				if (! feature.name %in% rownames(expression.temp)) { next; }

				# extract expression data for this dataset & feature
				values <- as.vector( expression.temp[feature.name,] );

				# calculate the maximum possible score each patient could get
				max.scores <- max.scores + as.numeric( !is.na(values) );

				# median dichotomize
				scores.temp <- values > median(as.numeric(values), na.rm = TRUE);

				# convert into score = {+1/high, -1/low} coding
				scores.temp[scores.temp == TRUE] <-  1;
				scores.temp[scores.temp != 1]    <- -1;

				# weight scores and keep a running total
				scores.temp <- scores.temp * feature.weight
				scores      <- scores + scores.temp;

				}

			}

		# save the patient scores before dichotomizing them
		overall.patient.scores <- c(overall.patient.scores, scores);
		maximum.patient.scores <- c(maximum.patient.scores, max.scores);

		# median dichotomize based on scores or split patients based on preset cutoff
		if (is.null(score.cutoff)) {
			scores <- scores > median(scores, na.rm = TRUE);
			}
		else {
			scores <- scores > score.cutoff;
			}

		# store the patient groupings somewhere for fun & games
		overall.patient.groups   <- c(overall.patient.groups, scores);
		overall.patient.survival <- c(overall.patient.survival, survobj[,1]);
		overall.patient.status   <- c(overall.patient.status, survobj[,2]);
		overall.dataset.names    <- c(overall.dataset.names, rep(names(expression.data)[i], length = nrow(survobj)));
		overall.patient.IDs      <- c(overall.patient.IDs, colnames(expression.temp));

		}

	# check that not all the data is missing (i.e. features are found in at least one dataset)
	if (0 == length(overall.patient.groups)  ) { warning('No data: no overall.patient.groups found'); return(NA); }
	if (0 == length(overall.patient.survival)) { warning('No data: no overall.patient.survival found'); return(NA); }
	if (0 == length(overall.patient.status)  ) { warning('No data: no overall.patient.status found'); return(NA); }

	# create a summary of all results
	results.summary <- data.frame(
		group       = overall.patient.groups,
		survtime    = overall.patient.survival,
		survstat    = overall.patient.status,
		scores      = overall.patient.scores,
		max.scores  = maximum.patient.scores,
		datasets    = overall.dataset.names,
		patient.IDs = overall.patient.IDs
		);

	# return all the results to the caller
	return(results.summary);

	}

