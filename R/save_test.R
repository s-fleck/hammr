
#' Save object to hard disk chache
#'
#' @param ... R objects to save to chache dir, usually /inst/chache
#' @param package the current package
#' @param subdir subdirectory of the chache dir to save data to
#'
#' @section Side effects:
#' Saves an R object to a cache dir in the current package
#'
#' @seealso \code{\link{load_cache}}.
#' @export
save_test <- function(..., pkg = '.', subdir){

  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- system.file(package = pkg$package)
  cache_dir <- file.path(pkg_dir, 'tests', 'testthat', 'test_data')

  if(!missing(subdir)){
    cache_dir <- file.path(cache_dir, subdir)
  }

  assert_that(file.exists(pkg_dir))

  if(!file.exists(cache_dir)){
    dir.create(cache_dir, recursive = TRUE)
  }

  assert_that(file.exists(cache_dir))

  to_save     <- eval(substitute(alist(...)))


  obj         <- vapply(to_save, as.character, character(1))
  save_file   <- paste0(file.path(cache_dir, obj), '.rds')

  message('Saving to ', save_file)

  for(i in seq_along(save_file)){
    saveRDS(list(...)[[i]], file = save_file[i])
  }

}


#' Load object from hard disk chache
#'
#' @param ... R objects to load from cache dir, usually /inst/chache
#' @param envir target environment
#' @param package the current package
#'
#' @section Side effects:
#' Loads an R object from a chache dir in the current package
#'
#' @seealso \code{\link{save_cache}}.
#' @export
load_test <- function(..., pkg = '.', subdir, envir = globalenv()){
  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- system.file(package = pkg$package)
  cache_dir <- file.path(pkg_dir, 'tests', 'testthat', 'test_data')

  if(!missing(subdir)){
    cache_dir <- file.path(cache_dir, subdir)
  }

  assert_that(file.exists(pkg_dir))

  to_load     <- eval(substitute(alist(...)))
  obj         <- vapply(to_load, as.character, character(1))

  path      <- paste0(file.path(cache_dir, obj), '.rds')

  readRDS(path)
}
