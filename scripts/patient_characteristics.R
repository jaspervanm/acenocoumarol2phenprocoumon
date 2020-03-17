library(tableone)

pretty_names <- c(pretty_names, # from .Rprofile
				  gender = "Male gender",
				  VKA_exp = "VKA experience",
				  low_dose = "Dose <1.5mg",
				  poor_ttr = "TTR ≤60%",
				  ind_AF = "Atrial fibrillation",
				  ind_VTE = "Venous thromboembolism",
				  ind_MVR = "Mechanical heart valve"
				  )

# Patient characteristics table
all_data <- readRDS("data/patient_data.rds") %>%
	filter_tr() %>%
	inner_join(
		# this filters eligibile patients
		readRDS("cache/qoac.rds") %>%
			filter(period_id == "acenocoumarol")
	) %>%
	full_join(
		# having dose data available is not a requirement here
		readRDS("data/all_doses.rds") %>%
			select(patient_id, switch_date, dose = dos_acenocoumarol),
		by = c("patient_id", "switch_date")
	) %>%
	full_join(readRDS("data/subgroups.rds")) %>%
	left_join(readRDS("cache/matched_qoac.rds") %>%
			  	filter(period_id == "acenocoumarol") %>%
			  	mutate(matched = TRUE)) %>%
	within({
		matched[is.na(matched)] <- FALSE
	}) %>%
	mutate(
		low_dose = dose < 1.5,
		poor_ttr = in_range <= 0.6
	) %>%
	mutate_at(vars(below_range, in_range, above_range), `*`, 100) %>%
	as.data.table(key = c("patient_id", "subgroup"))

create_characteristics_table <- function(Data, prefix = "", strata = "cc",
							 vars = c("center", "age", "gender", "VKA_exp", "dose", "low_dose", "below_range", "in_range", "above_range", "poor_ttr", "mean_inr", "vgr", "tbi", "ind_AF", "ind_VTE", "ind_MVR"),
							 nonnormal = intersect(vars, c("age", "VKA_exp", "below_range", "in_range", "above_range", "mean_inr", "vgr", "tbi", "dose")),
							 ...) {
	
	if("cc" %in% strata) {
		Data$cc <- factor(pretty_names[Data$cc], levels = pretty_names[c("case", "control")])
	}
	
	tab <- CreateTableOne(vars = vars, strata = strata, data = Data ) %>%
		print(nonnormal = nonnormal,
			  contDigits = 1, showAllLevels = TRUE,
			  quote = FALSE, noSpaces = TRUE, printToggle = FALSE,
			  cramvars = intersect("center", colnames(Data)), ...)
	
	tab <- cbind(RN = rownames(tab), as.data.table(tab))
	
	tab[ RN == "", RN := NA]
	tab[ , RN := zoo::na.locf(RN, FALSE)]
	tab <- tab[ level != FALSE & level != "Female" ]
	tab[ level == "Male", level := "" ]
	
	tab[ , var := paste(RN, level) ]	
	tab[ level == TRUE, var := RN ]
	tab[ , var := make_pretty_names(var) ]
	
	tab[grepl("center", RN)] <- map(tab[grepl("center", RN)], ~gsub(" \\(.*?\\)", "", .x)) %>% as.data.table()

	tab <- tab[ , c(ncol(tab), seq(3, ncol(tab) - 1)), with = FALSE]

	if(nchar(prefix) > 0) {
		colnames(tab)[-1] <- paste(prefix, colnames(tab)[-1], sep = "_")
	}

	colnames(tab)[1] <- ""
	tab
}
combine_tables <- function(tables, remove_tests = FALSE) {
	tables <- map(tables, function(x) {
		colnames(x)[1] <- "RN"
		if(remove_tests) {
			select(x, -contains("_test"))
		} else {
			x
		}
	})
	x <- Reduce(full_join, tables)
	colnames(x)[1] <- ""
	x
}
make_pretty_names <- function(string) {
	for(i in names(pretty_names)) {
		string <- str_replace(string, i, pretty_names[i])
	}
	Hmisc::capitalize(string)
}
save_table <- function(x, path, ...) {
	knitr::kable(x, ...) %>%
		write(path)
}

# One table with only information about cases
cases <- all_data %>%
	filter(cc == "case", subgroup == "all") %>%
	create_characteristics_table(strata = "target_range", test = FALSE)

# One table with all potential controls, to show population
controls <- all_data %>%
	filter(cc == "control", subgroup == "all") %>%
	create_characteristics_table(strata = "target_range", test = FALSE)

# A table which compares cases with matched controls
# Consider removing column of cases to save space (see above)
# But be aware: are all cases matched?
matched_controls <- all_data %>%
	filter(matched, subgroup == "all") %>%
	split( . , .$target_range ) %>%
	imap(create_characteristics_table, test = FALSE) %>%
	combine_tables(TRUE)

# Table with subgroups
subgroup_table <- all_data %>%
	filter(matched, subgroup != "all") %>%
	nest(-target_range, -subgroup) %>%
	arrange(target_range, subgroup) %>%
	nest(-target_range) %>%
	with({map2(data, target_range, function(x, target_range) {
		map2(x$data, paste(target_range, x$subgroup), create_characteristics_table, test = FALSE)
	})}) %>%
	map(combine_tables)

# Save all tables
# Prefix patchar_ to make rendering with Makefile easy
save_table(cases, "tables/patchar_cases.md")
save_table(controls, "tables/patchar_all_controls.md")
save_table(matched_controls, "tables/patchar_matched.md")
save_table(subgroup_table, "tables/patchar_subgroups.md")

# Save extra tables for easy inspection
all_data %>%
	filter(subgroup == "all") %>%
	create_characteristics_table(strata = c("cc", "target_range"), test = FALSE) %>%
	write_csv2("tables/patchar_cases_unselected_controls.csv")

all_data %>%
	filter(subgroup == "all") %>%
	split( . , .$center) %>%
	map_df(create_characteristics_table, strata = c("cc", "target_range"), test = FALSE, .id = "center") %>%
	write_csv2("tables/patchar_cases_unselected_controls_center.csv")

# Compare patient characteristics
comp_data <- all_data %>%
	filter(subgroup == "all") %>%
	select(patient_id, cc, center, target_range, age:VKA_exp, below_range:vgr, dose, low_dose, poor_ttr)

smd <- function(x, i) {
	dat <- data.table(x = x, i = i)
	
	M <- dat[ !is.na(x), .(M = mean(x)), by = "i"]
	if(nrow(M) != 2) {
		error("Not two groups!")
	} else {
		( M$M[[1]] - M$M[[2]] ) / sd(dat[!is.na(x)]$x)
	}
}

x <- comp_data %>%
	select(patient_id, cc, target_range, which(map_lgl(comp_data, is_logical))) %>%
	pivot_longer( . , seq(4, ncol( . ))) %>%
	nest(-target_range, -name) %>%
	mutate(
		RR = map_dbl(data, ~mean(.x$value[.x$cc == "case"], na.rm = TRUE) / mean(.x$value[.x$cc == "control"], na.rm = TRUE))
	)

y <- comp_data %>%
	select(patient_id, cc, target_range, which(map_lgl(comp_data, ~is.numeric(.x) & !is.factor(.x)))) %>%
	pivot_longer( . , seq(4, ncol( . ))) %>%
	nest(-target_range, -name) %>%
	mutate(
		SMD = map_dbl(data, ~smd(.x$value, .x$cc))
	)

bind_rows(
	x %>% select(-data),
	y %>% select(-data)
) %>%
	write_csv2("tables/comparison_patchar.csv")
