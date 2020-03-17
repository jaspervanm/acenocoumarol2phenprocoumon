library(MatchIt)

patient_data <- readRDS("data/patient_data.rds") %>%
	inner_join(
		readRDS("data/all_doses.rds") %>% select(patient_id, switch_date, dos_acenocoumarol),
		by = c("patient_id", "switch_date")
	)
patient_data$dose_cat <- cut(patient_data$dos_acenocoumarol,
							 breaks = c(0, 1.999, 4, Inf),
							 labels = c("low", "medium", "high")) %>%
	relevel("medium")
setDT(patient_data, key = c("patient_id", "switch_date"))

# Filename should be set in Makefile!
# args <- list("cache/qoac-tr_2.5-3.5.rds")
args <- commandArgs(trailingOnly = TRUE)

all_qoac <- readRDS(args[[1]])
qoac <- all_qoac %>%
	semi_join(
		# to make sure we have enough follow-up
		all_qoac %>% filter(period_id == "short-term") %>% select(patient_id),
		by = "patient_id"
	) %>%
	filter(period_id == "acenocoumarol") %>%
	select(patient_id, target_range, switch_date, below_range, above_range, vgr, mean_inr) %>%
	inner_join(patient_data) %>%
	mutate(lvgr = log(vgr)) %>% # is ook gedaan in what_to_match_on.R
	filter(!is.na(lvgr), is.finite(lvgr), vgr != 0) %>%
	drop_na() %>%
	mutate(
		case = (cc == "case")
	)

subgroups <- readRDS("data/subgroups.rds") %>%
	inner_join(
		qoac %>% select(patient_id, switch_date, target_range),
		by = c("patient_id", "switch_date", "target_range")
	) %>%
	as.data.frame() %>%
	group_by(target_range, subgroup) %>%
	nest(.key = "members")
setDT(subgroups)
subgroups[ , qoac := map(members, inner_join, qoac, by = c("patient_id", "cc", "peso_id", "center", "switch_date"))]

subgroups[ , match_formula := "case ~ age + gender + VKA_exp + above_range + below_range + log(vgr) + dose_cat + ind_VTE + ind_MVR"]
subgroups[ subgroup == "valve",    match_formula := "case ~ age + gender + VKA_exp + above_range + below_range + log(vgr) + dose_cat + ind_VTE"]
subgroups[ subgroup == "low_dose", match_formula := "case ~ age + gender + VKA_exp + above_range + below_range + log(vgr) + ind_VTE + ind_MVR"]

do_match <- function(match_formula, Data) {
	Data <- as.data.frame(Data)
	output <- list()
	output$mobj <- matchit(as.formula(match_formula), data = Data,
						   method = "optimal", ratio = 2)
	output$match_data <- match.data(output$mobj) %>%
		select(patient_id, set = subclass, ps = distance)
	list(output)
}

mobjects <- subgroups[ , .(do_match(match_formula, qoac)), by = c("target_range", "subgroup")]

mobjects[ , matched_qoac := map(map(V1, "match_data"), inner_join, all_qoac, by = "patient_id")]

# Save files
pwalk(mobjects, function(...) {
	x <- list(...)
	saveRDS(x$V1$mobj, paste0("cache/matchobjects/matchobject-tr_", gsub(" ", "", x$target_range), "_", x$subgroup, ".rds"))
})

mobjects %>%
	select(subgroup, matched_qoac) %>%
	unnest(matched_qoac) %>%
	saveRDS( . , paste0("cache/matched_qoac-tr_", gsub(" ", "", unique(.$target_range)), ".rds"))
