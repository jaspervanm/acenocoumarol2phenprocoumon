eligibility_data <- readRDS("cache/eligibility_data.rds")

em <- eligibility_data[ , .(
	patient_id, cc, switch_date, target_range,
	suff_inrs =  nINRs > (180 + 180) / 42,
	suff_time = (before_switch >= 140 & after_switch >= 140),
	cont_inrs = maxdiff < 56
)]
for(i in seq_len(ncol(em))) {
	set(em, which(is.na(em[ , ..i])), i, FALSE)
}
em[ , cc := factor(cc, c("case", "control"))]
em[ , target_range := factor(target_range)]

# Randomly pick one instance per person, if they have satisfy the criteria below
set.seed(123)
rand_date <- em[ suff_inrs == TRUE & suff_time == TRUE & cont_inrs == TRUE,
	.(switch_date = sample(switch_date, 1)),
	by = .(patient_id)]

em[ rand_date, sel_random := TRUE, on = .(patient_id, switch_date)]
em[ is.na(sel_random), sel_random := FALSE ]

saveRDS(em, "cache/eligibility_qoac.rds")
