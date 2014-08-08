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

fit.interaction.model <- function(feature1, feature2, expression.data, survival.data) {

	groups1 <- dichotomize.meta.dataset(
		expression.data = expression.data,
		survival.data = survival.data,
		feature.name = feature1,
		other.data = NULL
		);

	groups2 <- dichotomize.meta.dataset(
		expression.data = expression.data,
		survival.data = survival.data,
		feature.name = feature2,
		other.data = NULL
		);

	# handle the all-missing case smoothly
	if (all(is.na(groups1$groups * groups2$groups)) || length(groups1$survtime) != length(groups1$groups)) {
		warning('All-missing from feature groups or improperly formatted data');
		return( rep(NA,6) );
		}

	# fit the interaction Cox model
	survobj <- Surv(groups1$survtime, groups1$survstat);
	coxmodel <- coxph(survobj ~ groups1$groups + groups2$groups + as.numeric(groups1$groups == groups2$groups));
	coxmodel <- summary(coxmodel);
	interaction.HR <- as.numeric(coxmodel$coefficients[3,2]);
	interaction.P  <- as.numeric(coxmodel$coefficients[3,5]);
	
	# fit the two univariate models
	survival1 <- calculate.meta.survival(
		feature.name = feature1,
		expression.data = expression.data,
		survival.data = survival.data
		);

	survival2 <- calculate.meta.survival(
		feature.name = feature2,
		expression.data = expression.data,
		survival.data = survival.data
		);
			
	# return the survival statistics
	return(
		c(
			survival1[c(1,4)],
			survival2[c(1,4)],
			interaction.HR,
			interaction.P
			)
		);
	
	}
