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

create.survobj <- function(annotation) {

	# localize the survival data
	survtime <- annotation$survtime;
	survstat <- annotation$survstat;

	# check the annotation for each patient
	if (length(annotation$survtime.unit) == 0 | length(annotation$survtime) == 0) {
		warning('patient annotation missing');
		return(NA);
		}
	
	for (j in 1:nrow(annotation)) {
		if (is.na(annotation$survtime.unit[j]) | is.na(survtime[j])) { survtime[j] <- NA; }
		else if (annotation$survtime.unit[j] == "days")   { survtime[j] <- survtime[j] / 365.25; }
		else if (annotation$survtime.unit[j] == "weeks")  { survtime[j] <- survtime[j] / 52.18; }
		else if (annotation$survtime.unit[j] == "months") { survtime[j] <- survtime[j] / 12; }
		else if (annotation$survtime.unit[j] == "years")  { } # do nothing, this is the default}
		}

	# create the Surv object
	if (!is.numeric(survtime) || !is.numeric(survstat)) {
		survobj <- rep(NA, length(survtime));
		}
	else {
		survobj <- Surv(survtime, survstat);
		}

	return(survobj);

	}
