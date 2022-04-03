##  Produces data for use in visualising the rd method
rd2_1d_curves1 <- function(trait_tibble, trait_limits, bw = 0.5) {
  
  trait_tibble <- trait_tibble %>%
    rowwise() %>%
    mutate(kde = list(density(trait, bw = bw, from = trait_limits$min, to = trait_limits$max)$y)) %>%
    unnest(cols = c("kde")) %>%
    group_by(species) %>%
    mutate(x = seq(from = trait_limits$min, to = trait_limits$max, length = 512),
           kde = kde/(max(kde)))
  trait_tibble    
}

##  Also produces data for use in visualising the rd method
rd2_1d_curves2 <- function(trait_tibble, trait_limits, bw = 0.5) {
 
  trait_tibble1 <- trait_tibble %>%
    rowwise() %>%
    mutate(kde = list(density(trait, bw = bw, from = trait_limits$min, to = trait_limits$max)$y)) %>%
    unnest(cols = c("kde")) %>%
    group_by(species) %>%
    mutate(x = seq(from = trait_limits$min, to = trait_limits$max, length = 512),
           kde = kde/(max(kde))) %>%
    group_by(x) %>%
    summarise(kde = max(kde))
  trait_tibble1
}

##  Returns rd of a set of set of species
rd2_1d <- function(trait_tibble, trait_limits, bw = 0.5) {
  rd <- trait_tibble %>%
    rowwise() %>%
    mutate(kde = list(density(trait, bw = bw, from = trait_limits$min, to = trait_limits$max)$y)) %>%
    unnest(cols = c("kde")) %>%
    group_by(species) %>%
    mutate(x = seq(from = trait_limits$min, to = trait_limits$max, length = 512),
           kde = kde/(max(kde))) %>%
    group_by(x) %>%
    summarise(kde = max(kde)) %>%
    ungroup() %>%
    summarise(rd = sum(kde)) %>%
    pull(rd)
  rd
}

## Returns min and max of trait range to work with 
get_minmax_x <- function(x, bw) {
  x_min <- min(x) - bw/0.2
  x_max <- max(x) + bw/0.2 
  rr <- list(min=x_min, max=x_max)
}

## Visualise method
vis_rd_method <- function(rr1, rr2, rd) {
  p1 <- ggplot(mapping = aes(x = x, y = kde, col = species)) +
    geom_ribbon(data = rr2, aes(ymin = rep(0, length(x)),
                                ymax = kde, col = NULL), fill = "grey") +
    geom_line(data = rr2, aes(col = NULL), lwd = 1.5) +
    geom_line(data = rr1) +
    ggtitle(paste0("RD = ", signif(rd1,5))) +
    xlim(c(min_max_x$min, min_max_x$max)) +
    theme_bw() +
    theme(legend.position="none")
}
