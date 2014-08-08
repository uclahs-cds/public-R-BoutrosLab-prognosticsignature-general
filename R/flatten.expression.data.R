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

flatten.expression.data <- function(object) {

	# verify that we can work with this object
	if (is.null(object$all.data)) {
		warning('Unable to access the all.data slot!');
		return(NA);
		}
		
	# initialize our return object with the first dataset
	to.return <- object$all.data[[1]];

	# if there is only one dataset, we're done!
	if (length(object$all.data) == 1) { return(to.return); }

	# otherwise loop through each dataset
	for (i in 1:length(object$all.data)) {

		# skip the first case
		if (1 == i) { next; }

		# merge the two datasets
		to.return <- merge(
			x = to.return,
			y = object$all.data[[i]],
			all.x = TRUE,
			all.y = TRUE,
			by = 0
			);

		# reset the rownames
		rownames(to.return) <- to.return$Row.names;
		to.return <- subset(to.return, select = -Row.names);

		}

	# return the merged data-frame to the caller as a matrix
	return(as.matrix(to.return));

	}
