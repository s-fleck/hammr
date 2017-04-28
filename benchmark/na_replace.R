library(microbenchmark)

f1 <- function(x, replace){
  x[is.na(x)] <- replace
  x
}

f2 <- function(x, replace){
  dplyr::case_when(
    is.na(x) ~ replace,
    TRUE  ~ x
  )
}


tdat <- sample(c(1,2, NA_real_), 1e5, replace = TRUE)


microbenchmark(f1(tdat, 0), f2(tdat, 0), times = 1)
