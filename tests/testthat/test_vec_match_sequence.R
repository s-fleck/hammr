context('vec_match_seq')

t1 <- c(1:10, 1, 2, 1, 3, 2, 1, 0, 0, 0, 0, 1:10, 1, 2, 3, 1, 2)
s1 <- c(1:3)

t2 <- c('a', 'b', 'c', 'm', 'a', 't', 'c', 'h', 'd', 'e', 'm', 'a', 't', 'c')
s2 <- c('m', 'a', 't', 'c', 'h')


test_that("vec_match_seq: matching sequence to vector works.", {
  expect_identical(length(s1 %seq_in% t1), length(t1))
  expect_identical(length(s2 %seq_in% t2), length(t2))

  expect_identical(vec_match_seq(s1, t1), c(1L, 21L, 31L))
  expect_identical(vec_match_seq(s2, t2), 4L)
})


