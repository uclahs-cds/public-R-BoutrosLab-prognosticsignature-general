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

calculate.meta.survival <- function(feature.name, expression.data, survival.data, rounding = 3, other.data = NULL) {

	# verify that we got appropriate input data
	to.abort <- FALSE;
	if ("list" != class(expression.data)) { to.abort <- TRUE; }
	if ("list" != class(survival.data)) { to.abort <- TRUE; }
	if (length(expression.data) != length(survival.data)) { to.abort <- TRUE; }
	# TODO: SANITY CHECK other.data HERE ***************************************************************************
	
	# stop processing if we have bad data
	if (to.abort) {
		warning('Data failed sanity-checking');
		return( rep(NA,5) );
		}
	
	# dichotomize meta
	dichotomized.data <- dichotomize.meta.dataset(
		feature.name = feature.name,
		expression.data = expression.data,
		survival.data = survival.data,
		other.data = other.data
		);

	# handle all-NA values (i.e. feature not in the dataset)
	if (0 == length(dichotomized.data$groups) || 0 == length(dichotomized.data$survtime) || 0 == length(dichotomized.data$survstat) || all(is.na(dichotomized.data$groups))) {
		warning('\nFeature not in the dataset: ', feature.name);
		return( rep(NA,5) );
		}

	# fit.coxmodel
	return(
		fit.coxmodel(
			groups = dichotomized.data$groups,
			survobj = Surv(dichotomized.data$survtime, dichotomized.data$survstat),
			rounding = rounding,
			other.data = other.data
			)
		);

	}
