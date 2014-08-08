# The BoutrosLab.prognosticsignature.general package is copyright (c) 2012 Ontario Institute for Cancer Research (OICR)
# This package and its accompanying libraries is free software; you can redistribute it and/or modify it under the terms of the GPL
# (either version 1, or at your option, any later version) or the Artistic License 2.0.  Refer to LICENSE for the full license text.
# OICR makes no representations whatsoever as to the SOFTWARE contained herein.  It is experimental in nature and is provided WITHOUT
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OR ANY OTHER WARRANTY, EXPRESS OR IMPLIED. OICR MAKES NO REPRESENTATION
# OR WARRANTY THAT THE USE OF THIS SOFTWARE WILL NOT INFRINGE ANY PATENT OR OTHER PROPRIETARY RIGHT.
# By downloading this SOFTWARE, your Institution hereby indemnifies OICR against any loss, claim, damage or liability, of whatsoever kind or
# nature, which may arise from your Institution's respective use, handling or storage of the SOFTWARE.
# If publications result from research using this SOFTWARE, we ask that the Ontario Institute for Cancer Research be acknowledged and/or
# credit be given to OICR scientists, as scientifically appropriate.

survival.truncate <- function(all.survobj, time) {

	# loop through all datasets and truncate to n-year survival
	for (i in 1:length(all.survobj)) {

		# localize the survival object
		survobj  <- all.survobj[[i]];
		survtime <- survobj[,1];
		survstat <- survobj[,2];

		# only consider the first n years
		survstat[survtime > time] <- 0;
		survtime[survtime > time] <- time;

		# create a new survival object
		survobj <- Surv(survtime, survstat);

		# save back in the list
		all.survobj[[i]] <- survobj;

		}

	return(all.survobj);

	}
