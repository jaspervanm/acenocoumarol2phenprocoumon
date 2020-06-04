# Filename should be set in Makefile!
# args <- list("cache/splitted_inrs/split_1.rds")
args <- commandArgs(trailingOnly = TRUE)
inr_pat_data <- readRDS(args[[1]])
setDT(inr_pat_data$patient_data, key = "patient_id")
setDT(inr_pat_data$INR_data, key = c("patient_id", "inr_date"))

inr_data <- inr_pat_data$INR_data[ , .(inr = mean(inr)), by = .(patient_id, inr_date)]

periods <- tribble(
	~period_id,     ~from, ~to,
	"acenocoumarol", -180,   0,
	"short-term",       0, 180,
	"long-term",      180, 360
)
setDT(periods)

calc_qoac <- function(pid) {
	tr <- inr_pat_data$patient_data[patient_id == pid,
				   .(low = target_range_lower, high = target_range_upper)]
	
	act_inrs <- inr_data[patient_id == pid, .(INR_date = inr_date, INR = inr)] %>%
		.[between(INR_date, -180 - 60, 360)]
	intp_inrs <- with(act_inrs, approx(INR_date, INR, seq(-180, max(act_inrs$INR_date), by = 1))) %>%
		as.data.table()
	
	ttr <- intp_inrs[periods, .(period_id, inr_date = x.x, inr = x.y),
					 on = list(x >= from, x <= to), allow.cartesian = TRUE][
					 	, tibble(
					 		below_range = mean(inr < tr$low, na.rm = TRUE),
					 		above_range = mean(inr > tr$high, na.rm = TRUE),
					 		in_range    = 1 - below_range - above_range
					 	),
					 	by = "period_id"]
	
	act_inr_stats <- act_inrs[periods, .(period_id, INR_date = x.INR_date, INR = x.INR),
							  on = list(INR_date >= from, INR_date <= to), allow.cartesian = TRUE][
							  	, .(vgr = sqrt(mean(diff(INR)^2 / as.numeric(diff(INR_date)) )),
							  		mean_inr = mean(INR),
							  		nINR = length(INR),
							  		timespan = max(INR_date) - min(INR_date)
							  	),
							  	by = "period_id"]
	
	ttr[act_inr_stats, on = "period_id"]
}

temp_qoac <- inr_data[ , .(qoac = list(calc_qoac(patient_id))), by = .(patient_id)]

temp_qoac %>%
	unnest(qoac) %>%
	left_join(
		inr_pat_data$patient_data %>%
			select(-contains("merge")),
		by = "patient_id"
	) %>%
	saveRDS(stringr::str_replace(args[[1]], "splitted_inrs/split", "splitted_qoac/qoac") )
