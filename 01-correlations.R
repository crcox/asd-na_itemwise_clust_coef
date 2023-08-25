library(dplyr)

clust <- readRDS("data/local_clust.rds")
vsoa <- readRDS("data/item_level_differences_bs_ci_bonf.rds")

d <- left_join(clust, vsoa %>% select(-word, -fct), by = "num_item_id") %>%
    rename(vsoa_asd = asd, vsoa_na = na, vsoa_diff = diff)

d %>%
    filter(vsoa_asd > 0, vsoa_asd <= 200) %>%
    summarize(
        r_global = cor(vsoa_asd, global_clust_influence),
        r_local = cor(vsoa_asd, local_clust)
    )

d %>%
    filter(vsoa_asd > 0, vsoa_asd <= 200) %>%
    summarize(
        r_global = cor(vsoa_asd, global_clust_influence),
        r_local = cor(vsoa_asd, local_clust)
    )

d %>%
    filter(na > 0, na <= 200) %>%
    summarize(r = cor(asd, diff_clust))

d %>%
    filter(na > 0, na <= 200, asd > 0, asd <= 200) %>%
    summarize(r = cor(diff, diff_clust))
