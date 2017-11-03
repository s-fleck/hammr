test_that("dsinfo sources print nicely", {
  #* @testing dsi_sources
  #* @testing format_source

  x <- set_dsinfo(
    iris,
    title = "Iris",
    sources = dsi_sources(
      dsi_source("R demo data", path = "path/to/data", date = Sys.Date()),
      dsi_source("Alfred Bogus", email = c("alfred@bogus.xx", "alfred.bogus@yahoo.ru"))
    ))

  dsinfo(x)

})
