context("dsinfo")


test_that("dsinfo works as expected", {

  x <- 1L

  expect_silent(
    x <- set_dsinfo(x,
      # data-package recommended
      name = 'test_data',
      id = 't001',
      version = '0.0.1',
      reference_date = as.Date("2016-01-01"),

      # hammr-optional
      source_date = as.Date("2016-01-01"),
      source_path = "/",

      # data-package recommended
      license = 'CC',

      # data-package optional
      title = 'A test dataset',
      description = "A dataset created for testing purposes \n* test \n* data",
      homepage = "www.zombo.com",

      sources = 'self create',
      contributors = 'Foobert Bar',
      keywords = c("test", "data"),
      created = Sys.time(),


      # data package-compat
      profile = NULL,  #recommended
      image = NULL,  #optional
      blah = "blubb"
    )
  )

  expect_silent(set_dsinfo(x, reference_date = Sys.Date()))
  expect_silent(set_dsinfo(x, reference_date = lubridate::interval(Sys.Date(), Sys.Date())))
  expect_silent(set_dsinfo(x, reference_date = as_date_yq(Sys.Date())))
  expect_error(set_dsinfo(x, reference_date = 'x'))


  y <- set_dsinfo(1L, homepage = "www.zombo.com")

})



test_that("dsinfo works as expected", {
  expect_true(is_dsinfo_name('ab.1'))
  expect_true(is_dsinfo_name("ab_1"))
  expect_true(is_dsinfo_name("ab-1"))
  expect_false(is_dsinfo_name("ab(1"))
})


