context("triput")


test_that("triput works as expected", {


  triput(head(iris))

  x <- data.frame(
    numeric = c(1.1, 2.1, 3.1),
    integer = 1:3,
    text = c("quote \" quote", "foo", "bar"),
    factor = factor(c("1", "a", "b")),
    list = I(list(list(a = "foo", b = "bar"), list(a = "foo", b = "bar"), list(a = "foo", b = "bar"))),
    date = as.Date(c("2012-01-01", "2012-01-02", "2012-01-03"))
  )

  triput(x)

  x <- tibble::tribble(
    ~numeric, ~integer,           ~text, ~factor,                      ~list,                 ~date,
    1.1,        1, "quote  quote",     "1", list(a = "foo", b = "bar"), as.Date("2012-01-01"),
        2.1,        2,           "foo",     "a", list(a = "foo", b = "bar"), as.Date("2012-01-02"),
        3.1,        3,           "bar",     "b", list(a = "foo", b = "bar"), as.Date("2012-01-03"))



})
