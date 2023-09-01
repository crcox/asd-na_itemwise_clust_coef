library(dplyr)
library(igraph)

load("data/clust_local_global.Rdata")
m <- readRDS("data/cdi-metadata.rds")

g <- igraph::upgrade_graph(readRDS("network/child_net_graph.rds"))
global_clust <- igraph::transitivity(g, type = "global")
global_clust - clust$V1
local_clust <- igraph::transitivity(g, type = "local")

d <- data.frame(
    vertix_id = seq_len(igraph::vcount(g)),
    word = names(V(g))
) %>%
    left_join(m %>% select(num_item_id, word = cue_CoxHae), by = "word") %>%
    left_join(clust %>% select(-word), by = "num_item_id") %>%
    rename(
        global_clust_sans_word = V1,
        global_clust_influence_raw = diff_clust
    ) %>%
    mutate(
        global_clust_influence_raw = -global_clust_influence_raw,
        global_clust_influence = (global_clust_sans_word - global_clust) / sd(global_clust_sans_word),
        local_clust = local_clust
    )

saveRDS(d, file = "data/local_clust.rds")



