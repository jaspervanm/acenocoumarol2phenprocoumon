mfiles   <- list.files("cache/matchobjects", "matchobject-tr", full.names = TRUE)
mobjects <- map(mfiles, readRDS)
mnames   <- str_match(mfiles, "tr_(.*?)_(.*?)\\.rds")

# Check for balance of matching
library(cobalt)

bal_mat <- map(mobjects, bal.tab, m.threshold = 0.1, v.threshold = 2)

lovePlot <- function(x, y, ...) {
	love.plot(x, title = y, ...)
}

map2(bal_mat, mfiles,
	 ~paste(
	 	str_match(.y, "-tr_(.*?)\\.rds")[[2]],
	 	paste(capture.output(print(.x)), collapse = "\n"),
	 	sep = ":\n\n")
) %>%
	paste(collapse = "\n\n\n\n\n\n_________________________________\n") %>%
	write("tables/matching_performance.txt")

library(gridExtra)
map(mfiles, ~paste("Matching performance, ", str_match(.x, "-tr_(.*?)\\.rds")[[2]])) %>%
	map2(bal_mat, ., lovePlot, threshold = 0.1) %>%
	arrangeGrob(grobs = . , ncol = 2) %>%
	ggsave(paste0("plots/matching_performance.pdf"), . , width = 10, height = 10)

distances <- data.frame(
	target_range = mnames[,2],
	subgroup     = mnames[,3],
	ps_sdm       = map_dbl(bal_mat, list("Balance", "Diff.Adj", 1))
)

saveRDS(distances, "cache/balance_distance.rds")
