devtools::load_all('.')
library(profvis)

p = c(NA_character_, '2181')
c = c('2181', '2243')
n = c("2243", '2155')


profvis(
  replicate(1e3, hammr:::sort_edge_pairs(x = p, c = c, n = n, allow_partial = TRUE))
)


