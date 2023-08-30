library(dplyr)
library(tidyr)
library(ggplot2)

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
    filter(vsoa_asd > 0, vsoa_asd <= 400) %>%
    summarize(
        r_global = cor(vsoa_asd, global_clust_influence),
        r_local = cor(vsoa_asd, local_clust)
    )

d %>%
    filter(vsoa_asd > 0, vsoa_asd <= 600) %>%
    summarize(
        r_global = cor(vsoa_asd, global_clust_influence),
        r_local = cor(vsoa_asd, local_clust)
    )

d %>%
    filter(vsoa_na > 0, vsoa_na <= 600) %>%
    summarize(
        r_global = cor(vsoa_na, global_clust_influence),
        r_local = cor(vsoa_na, local_clust)
    )

d_filt <- d %>%
    filter(
        vsoa_na > 0,
        vsoa_na <= 675,
        vsoa_asd > 0,
        vsoa_na <= 675
    ) %>%
    pivot_longer(
        cols = c(vsoa_na, vsoa_asd),
        names_to = "group",
        values_to = "vsoa"
    ) %>%
    mutate(
        group = as.factor(group)
    ) %>%
    group_by(group) %>%
    mutate(
        local_clust_z = (local_clust - mean(local_clust)) / sd(local_clust),
        vsoa_z = (vsoa - mean(vsoa)) / sd(vsoa),
    ) %>%
    ungroup()

m <- lm(vsoa_z ~ local_clust_z * group, data = d_filt)
summary(m)

d_filt_diff <- d %>%
    filter(
        vsoa_na > 0,
        vsoa_na <= 675,
        vsoa_asd > 0,
        vsoa_na <= 675
    )

m_diff <- lm(vsoa_diff ~ local_clust, data = d_filt_diff)
summary(m_diff)

m_diff_glob <- lm(vsoa_diff ~ global_clust_influence, data = d_filt_diff %>% filter(abs(global_clust_influence) < 3))
summary(m_diff_glob)


y <- d %>%
    pivot_longer(cols = c(vsoa_na, vsoa_asd), names_to = "group", values_to = "vsoa")


ggplot(y %>% filter(vsoa > 0, vsoa <= 600), aes(x = local_clust, y = vsoa, color = group)) +
    geom_point() +
    geom_smooth()

ggplot(d_filt_diff, aes(x = local_clust, y = vsoa_diff)) +
    geom_point() +
    geom_smooth()

ggplot(d_filt_diff, aes(x = global_clust_influence, y = vsoa_diff)) +
    xlim(-3, 3) +
    geom_point() +
    geom_smooth(method = "lm")

ggplot(d_filt_diff, aes(x = global_clust_influence, y = vsoa_diff)) +
    geom_point() +
    geom_smooth(method = "lm")




d %>%
    filter(vsoa_na > 0, vsoa_na <= 200, vsoa_asd > 0, vsoa_asd <= 200) %>%
    summarize(
        r_global = cor(vsoa_asd - vsoa_na, global_clust_influence),
        r_local = cor(vsoa_asd - vsoa_na, local_clust)
    )


d %>%
    filter(na > 0, na <= 600) %>%
    summarize(r = cor(asd, diff_clust))
