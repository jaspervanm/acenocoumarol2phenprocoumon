matched <- readRDS("cache/matched_dataset.rds")
setDT(matched, key = c("peso_id", "case"))

with(matched, table(case))
aggregate(age ~ case, matched, mean)
with(matched, table(case, gender)) %>% prop.table(margin = 1)
aggregate(below_range ~ case, matched, mean)
aggregate(above_range ~ case, matched, mean)
aggregate(ind_overlap ~ case, matched, mean)
