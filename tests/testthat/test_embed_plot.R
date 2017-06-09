context("embed_plot")


test_that("embed_plot works as expected", {

  p <- ggplot2::ggplot(
    cars,
    ggplot2::aes(x = speed, y = dist)
  ) + ggplot2::geom_bar(stat = 'identity')


  expect_base64_image <- function(x){
    expect_match(x, '^data:image/png;base64.*')
  }

  expect_base64_image_tag <- function(x){
    expect_match(as.character(x), '^<img src="data:image/png;base64.*"/>$')
  }


  expect_base64_image_tag(
    embed_plot(p, width = 200, height = 200)
  )

  expect_base64_image(
    embed_plot(p, width = 200, height = 200, img = FALSE)
  )

  expect_base64_image_tag(
    embed_plot(function() plot(cars), width = 200, height = 200)
  )

  expect_base64_image(
    embed_plot(function() plot(cars), width = 200, height = 200, img = FALSE)
  )






})
