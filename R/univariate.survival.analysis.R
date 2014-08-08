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

univariate.survival.analysis <- function(object, studywise = FALSE, rounding = 3, other.data = NULL) {
	
	# verify the input object has all required components
	if (!'all.probesets' %in% names(object)) { warning('Missing probesets'); return(0); }
	if (!'all.data' %in% names(object)) { warning('Missing expression-data'); return(0); }
	if (!'all.survobj' %in% names(object)) { warning('Missing survival data'); return(0); }

	# store the data
	cox.fits <- matrix(
		nrow = length(object$all.probesets),
		ncol = 5,
		dimnames = list(
			object$all.probesets,
			c('HR', 'HR95L', 'HR95U', 'P', 'n')
			)
		);		

	# do all studies together
	if (studywise == FALSE) {
		for (i in 1:length(object$all.probesets)) {
			cox.fits[i,] <- calculate.meta.survival(
				feature.name = object$all.probesets[i],
				expression.data = object$all.data,
				survival.data = object$all.survobj,
				rounding = rounding,
				other.data = other.data
				);
			}

		# fill in the results matrix row by row
		univariate.results <- as.data.frame(cox.fits);
		univariate.results$Q <- p.adjust(univariate.results$P, method = 'fdr');
		}

	# do studies independently
	else {
		univariate.results <- list();
		for (study in names(object[["all.data"]])) {
			expression.object <- list();
			survival.object <- list();
			expression.object[[study]] <- object$all.data[[study]];
			survival.object[[study]] <- object$all.survobj[[study]];
			for (i in 1:length(object$all.probesets)) {
				cox.fits[i,] <- calculate.meta.survival(
					feature.name = object$all.probesets[i],
					expression.data = expression.object,
					survival.data = survival.object,
					rounding = rounding,
					other.data = other.data[[study]]
					);
				}
			# fill in the results matrix row by row
			univariate.results[[study]] <- as.data.frame(cox.fits);
			univariate.results[[study]]$Q <- p.adjust(univariate.results[[study]]$P, method = 'fdr');
			}
		}
		
	# return the overall fit
	return(univariate.results);
	
	}
