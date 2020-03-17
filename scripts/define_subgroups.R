qoac <- readRDS("cache/qoac.rds") %>%
	filter(period_id == "acenocoumarol")

patient_data <- readRDS("data/patient_data.rds") %>%
	inner_join(distinct(qoac, patient_id, switch_date)) %>%
	filter_tr()

doses <- readRDS("data/all_doses.rds") %>%
	select(patient_id, dos_acenocoumarol)

subgroups <- bind_rows(
	patient_data %>%
		distinct(patient_id, center, peso_id, cc, target_range, switch_date) %>%
		mutate(subgroup = "all"),
	patient_data %>%
		filter(age >= 70) %>%
		distinct(patient_id, center, peso_id, cc, target_range, switch_date) %>%
		mutate(subgroup = "elderly"),
	patient_data %>%
		filter(ind_MVR) %>%
		distinct(patient_id, center, peso_id, cc, target_range, switch_date) %>%
		mutate(subgroup = "valve"),
	patient_data %>%
		semi_join(
			doses %>%
				filter(dos_acenocoumarol < 1.5),
			by = "patient_id"
		) %>%
		distinct(patient_id, center, peso_id, cc, target_range, switch_date) %>%
		mutate(subgroup = "low_dose"),
	patient_data %>%
		semi_join(
			qoac %>%
				filter(in_range < 0.6),
			by = "patient_id"
		) %>%
		distinct(patient_id, center, peso_id, cc, target_range, switch_date) %>%
		mutate(subgroup = "poor_ttr"),
	patient_data %>%
		semi_join(
			qoac %>%
				group_by(target_range_lower, target_range_upper) %>%
				filter(vgr >= quantile(vgr, 0.75)) %>%
				ungroup(),
			by = "patient_id"
		) %>%
		distinct(patient_id, center, peso_id, cc, target_range, switch_date) %>%
		mutate(subgroup = "volatile")
) %>%
	group_by(subgroup, target_range) %>%
	filter(sum(cc == "case") >= 30) %>%
	ungroup()

saveRDS(subgroups, "data/subgroups.rds")
