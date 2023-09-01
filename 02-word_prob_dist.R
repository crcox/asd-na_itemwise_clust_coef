library(dplyr)
library(purrr)
library(tidyr)
library(igraph)
library(ggplot2)

m <- readRDS("data/cdi-metadata.rds")
models <- map(1:680, ~{
    x <- file.path("data", "ci_bonf", "glm", sprintf("%03d.rds", .))
    readRDS(x)
})
names(models) <- m %>%
    arrange(num_item_id) %>%
    pull(cue_CoxHae)
saveRDS(models, file = "data/ci_bonf/glm_base_models.rds")

vocab_sizes <- seq(20, 580, by = 20)
d_new <- data.frame(
    nproduced = rep(vocab_sizes, 2),
    group = gl(2, length(vocab_sizes), labels = c("ASD", "NA"))
)

pmat <- vapply(models, function(mod) {
    predict(mod, newdata = d_new, type = "response")
}, FUN.VALUE = numeric(58), USE.NAMES = TRUE)

probs <- list(
    autistic = pmat[d_new$group == "ASD",],
    nonautistic = pmat[d_new$group == "NA",]
) %>%
    map(~{.x / rowSums(.x)})

g <- upgrade_graph(readRDS("network/child_net_graph.rds"))
prob_labs <- tibble(
    colnum = seq_len(ncol(probs$autistic)),
    word = colnames(probs$autistic)
)
vid <- tibble(
    vertex_id = seq_len(vcount(g)),
    word = names(V(g))
) %>%
    left_join(select(m, num_item_id, word = cue_CoxHae, category)) %>%
    left_join(select(prob_labs, colnum, word))

probs_sorted <- map(probs, function(p, vertex_labels) {
    p[, vertex_labels]
}, vertex_labels = vid$word)
prob_labs_sorted <- tibble(
    colnum = seq_len(ncol(probs_sorted$autistic)),
    word = colnames(probs_sorted$autistic)
)
vid <- vid %>%
    left_join(select(prob_labs_sorted, colnum_sorted = colnum, word))
all.equal(vid$vertex_id, vid$colnum_sorted)

f <- function(n, i, p, g) {
    x <- numeric(ncol(p))
    ix <- sample(ncol(p), size = n, prob = p[i, ])
    x[ix] <- transitivity(
        induced_subgraph(g, sample(ncol(p), size = n, prob = p[i, ])),
        type = "local",
        isolates = "zero"
    )
    return(x)
}

tmp <- map(probs_sorted, function(p, g) {
    imap(vocab_sizes, function(n, i, p, g) {
        replicate(1000, f(n, i, p, g))
    }, p = p, g = g)
}, g = g)

saveRDS(tmp, "local_clust_by_vocabsize.rds")

q <- map_depth(tmp, .depth = 2, ~{
    rowMeans(.)
}) %>%
    map(~{
        matrix(unlist(.), 675, length(vocab_sizes))
    })

dd <- expand_grid(
    group = factor(1:2, labels = c("autistic", "non-autistic")),
    vocab_size = vocab_sizes,
    vertex_id = seq_len(nrow(vid))
) %>%
    left_join(select(vid, vertex_id, category))
dd$local_clust <- unlist(q)
ddd <- dd %>%
    group_by(group, vocab_size, category) %>%
    summarize(
        local_clust_sd = sd(local_clust, na.rm = TRUE),
        local_clust_se = sd(local_clust, na.rm = TRUE) / sqrt(n()),
        local_clust = mean(local_clust, na.rm = TRUE)
    ) %>% ungroup() %>% mutate(category = as.factor(category)) %>%
    filter(
        category %in% c("animals", "vehicles", "places", "action_words", "games_routines", "sounds")
    ) %>%
    droplevels()

ggplot(ddd, aes(x = vocab_size, y = local_clust)) +
    geom_ribbon(aes(ymin = local_clust - local_clust_se, ymax = local_clust + local_clust_se, fill = category), alpha = .3) +
    geom_line(aes(color = category)) +
    facet_wrap(~group)

ggplot(ddd, aes(x = vocab_size, y = local_clust)) +
    geom_ribbon(aes(ymin = local_clust - local_clust_se, ymax = local_clust + local_clust_se, fill = group), alpha = .3) +
    geom_line(aes(color = group)) +
    facet_wrap(~category)
