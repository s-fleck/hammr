#' Save object to test directory
#'
#' @param ... R objects to save to chache dir, usually /inst/chache
#' @param package the current package
#' @param subdir subdirectory of the chache dir to save data to
#'
#' @section Side effects:
#' Saves an R object to a test_data dir in the current package
#'
#' @seealso \code{\link{load_cache}}.
#' @export
save_test <- function(..., pkg = '.', subdir){

  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- base::system.file(package = pkg$package)                #base:: prevents devtools from inserting /inst/ into path
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
#' @param ... A single r object to load from /tests/testthat/test_data/(subdir)
#' @param package the current package
#'
#' @section Side effects:
#' Loads an R object from a test_data dir in the current package
#'
#' @seealso \code{\link{save_cache}}.
#' @export
read_test <- function(..., pkg = '.', subdir, envir = globalenv()){
  pkg       <- devtools::as.package(pkg)
  pkg_dir   <- base::system.file(package = pkg$package)
  cache_dir <- file.path(pkg_dir, 'tests', 'testthat', 'test_data')

  if(!missing(subdir)){
    cache_dir <- file.path(cache_dir, subdir)
  }

  assert_that(file.exists(pkg_dir))

  to_load     <- eval(substitute(alist(...)))
  obj         <- vapply(to_load, as.character, character(1))

  path <- file.path(cache_dir, obj)

  if(!grepl('.*\\.rds$', path)){
    path <- paste0(path, '.rds')
  }

  readRDS(path)
}
