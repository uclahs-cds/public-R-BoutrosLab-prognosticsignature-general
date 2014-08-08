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

sample.metadata <- function(data, fraction = 0.5, return.all = TRUE) {

	# extract patient IDs for each dataset
	patient.sample <- list();
	data.sample1 <- list(all.probesets = data$all.probesets);
	data.sample2 <- list(all.probesets = data$all.probesets);

	# loop over all datasets
	for (dataset in names(data[['all.data']])) {

		# dataset to sample from
		x <- colnames(data[['all.data']][[dataset]]);
	
		# sample about half the names for one dataset
		x.sample <- sample(
			x = x,
			size = ceiling( length(x) * fraction ),
			replace = FALSE
			);

		# save the patient grouping
		patient.sample[[dataset]] <- x %in% x.sample;

		}
	
	# loop over all datasets
	for (dataset in names(patient.sample)) {

		# localize the patient sample for this dataset
		x <- patient.sample[[dataset]];

		# save the data
		data.sample1[['all.data']][[dataset]]    <- data[['all.data']][[dataset]][,x];
		data.sample2[['all.data']][[dataset]]    <- data[['all.data']][[dataset]][,!x];
		data.sample1[['all.survobj']][[dataset]] <- data[['all.survobj']][[dataset]][x,];
		data.sample2[['all.survobj']][[dataset]] <- data[['all.survobj']][[dataset]][!x,];

		}

	# return as much data as requested to caller
	to.return <- list(d1 = data.sample1);
	if (return.all) { to.return$d2 <- data.sample2; }
	return(to.return);

	}
