#' Set and Display Info about a Data Set
#'
#' Attaches a list as the attribute `dsinfo` to `x`. `dsinfo` may contain
#' any kind of R object. The parameters section of this help file describes
#' a sensible selection of possible metadata elements, heavyly inspired by
#' https://specs.frictionlessdata.io/data-package/ with some minor
#' modifications.
#'
#' @param x any R object
#'
#' @param name A short url-usable (and preferably human-readable) name of the
#'   dataset This MUST be lower-case and contain only alphanumeric characters
#'   along with ".", "_" or "-" characters. It will function as a unique
#'   identifier and therefore SHOULD be unique in relation to any registry in
#'   which this dataset will be deposited (and preferably globally unique).
#'
#'   The name SHOULD be invariant, meaning that it SHOULD NOT change when a data
#'   dataset is updated, unless the new dataset version should be considered a
#'   distinct dataset, e.g. due to significant changes in structure or
#'   interpretation. Version distinction SHOULD be left to the version property.
#'   As a corollary, the name also SHOULD NOT include an indication of time
#'   range covered.
#' @param id A property reserved for globally unique identifiers. Examples of
#'   identifiers that are unique include UUIDs and DOIs.
#'
#'   A common usage pattern for datasets is as a packaging format within
#'   the bounds of a system or platform. In these cases, a unique identifier for
#'   a dataset is desired for common data handling workflows, such as updating
#'   an existing dataset. While at the level of the specification, global
#'   uniqueness cannot be validated, consumers using the id property MUST ensure
#'   identifiers are globally unique.
#' @param license The license(s) under which the dataset is provided.
#' @param title A string providing a title or one sentence description for this dataset
#' @param description a description of the dataset. The description MUST be
#'   markdown formatted -- this also allows for simple plain text as plain text
#'   is itself valid markdown. The first paragraph (up to the first double line
#'   break) should be usable as summary information for the dataset.
#'
#' @param homepage A URL for the home on the web that is related to this dataset.
#' @param version a version string identifying the version of the dataset. It
#'   should conform to the Semantic Versioning requirements. See http://semver.org/
#' @param sources The raw sources for this dataset. It MUST be a list of
#'   Source objects. Each Source object MAY have title, path and email
#'   properties. See [dsi_sources]
#' @param contributors The people or organizations who contributed to this
#'   dataset. It MUST be a list. Each entry is a Contributor and MUST be an
#'   object. A Contributor MUST have a name property and MAY contain path,
#'   email, role and organization properties.
#' @param keywords An character vector of keywords to assist users
#'   searching for the dataset in catalogs.
#' @param created a Datetime scalar
#' @param reference_date Reference date for the data set. May be a [base::Date],
#'   [base::POSIXt], [hammr::date_xx] or a [lubridate::period].
#' @param source_date Deprecated. Creation date of the source file(s) that the dataset
#'   `x` was created from.
#' @param source_path Deprecated. Path to the source file
#'
#' @param image Provided for compatability with the Data Package standard. An
#'   image to use for this data package. For example, when showing the package
#'   in a listing.
#'
#'   The value of the image property MUST be a string pointing to the location
#'   of the image. The string must be a url-or-path, that is a fully qualified
#'   HTTP address, or a relative POSIX path (see the url-or-path definition in
#' Data Resource for details).
#'
#' @param profile for compatability with the Data Package standard. A string
#'   identifying the profile of this descriptor as per the profiles
#'   specification. (see Data Package sepcifications)
#'
#' @param ... any number of arbitrary metadata elements that will also be
#'   attached to dsinfo.
#'
#' @return `dsinfo()` returns the `desinfo` attribute of `x` (or `NULL` if there
#'   is none).
#'
#' @param .add if `FALSE` (default), the complete `dsinfo` attribute is
#'   replaced, dropping values that are not present in the new attribute.
#'   If `TRUE`, the new values are added to the existing `dsinfo`
#'   attribute (values that exist in the original and new dsinfo are still
#'   replaced by the new values).
#'
#' @export
#'
dsinfo <- function(x){
  attr(x, 'dsinfo')
}




#' @rdname dsinfo
#'
#' @return `set_dsinfo()` returns `x` with an additional `dsinfo` attribute.
#' @export
#'
set_dsinfo <- function(
  x,

  # hammr recommended
  id = NULL,
  name = NULL,
  reference_date = NULL,
  version = NULL,

  # data-package recommended
  license = NULL,

  # hammr-optional
  source_date = NULL,
  source_path = NULL,

  # data-package optional
  title = NULL,
  description = NULL,
  homepage = NULL,
  sources = NULL,
  contributors = NULL,
  keywords = NULL,
  created = NULL,

  # data package-compat
  profile = NULL,  #recommended
  image = NULL,  #optional
  ...,
  .add = FALSE
){
  # Preconditions
    for(el in c(name, id, title, description, version)){
      assert_that(is.null(el) || rlang::is_scalar_character(el))
    }

    for(el in c(homepage, keywords, source_path, profile)){
      assert_that(is.null(el) || is.character(el))
    }

    assert_that(is.null(reference_date) || is_reference_date(reference_date) )
    assert_that(is.null(name) || is_dsinfo_name(name))
    assert_that(is.flag(.add))
    # license


  # Processing
    if (!is.null(name)) name <- tolower(name)

    if (inherits(sources, "dsinfo_source")){
      sources <- dsi_sources(sources)
    }

    assert_that(is.null(sources) || inherits(sources, "dsinfo_sources"))

    info <- c(
      list(
        # hammr recommended
        id = id,
        name = name,
        reference_date = reference_date,
        version = version,

        # data-package recommended
        license = license,

        # hammr-optional
        source_date = source_date,
        source_path = source_path,

        # data-package optional
        title = title,
        description = description,
        homepage = homepage,
        sources = sources,
        contributors = contributors,
        keywords = keywords,
        image = image,
        created = created
      ),
      list(...)
    )


  if (.add){
    old_info <- dsinfo(x)

    for (nm in names(old_info)){
      if (is.null(info[[nm]])) info[[nm]] <- old_info[[nm]]
    }
  }


  info <- info[!unlist(lapply(info, is.null))]

  class(info) <- c('dsinfo', 'list')
  attr(x, 'dsinfo') <- info

  return(x)
}



#' Title
#'
#' @param x
#' @param value
#'
#' @return
#' @export
#'
`dsinfo<-` <- function(x, value){
  attr(x, 'dsinfo') <- value
  x
}



# reference_date --------------------------------------------------------



#' @return `reference_date()` retrieves the `reference_date` field of the
#'   `dsinfo` attribute of `x` (or `NULL` if no such attribute exists)
#'
#' @rdname dsinfo
#' @export
#'
reference_date <- function(x){
  if(is.null(attr(x, 'dsinfo'))){
    return(NULL)
  } else {
    attr(x, 'dsinfo')$reference_date
  }
}



#' @param value Value to assign.
#'
#' @rdname dsinfo
#' @export
`reference_date<-` <- function(x, value){
  assert_that(is_reference_date(value))

  dsi <- hammr::dsinfo(x)

  if (inherits(dsi, "dsinfo")){
    dsi$reference_date <- value
    attr(x, "dsinfo") <- dsi
  } else {
    x <- set_dsinfo(x, reference_date = value)
  }

  x
}




#' @param y integer (year) or a `date_xx` object.
#' @param q,m integer. Quarter, month. Month and quarter are optional,
#'  and mutually exclusive (only supply one, not both). If `y` is a `date_xx`
#'  `q` and `m` must be `NULL`.
#'
#' @return `set_reference_date()` and `'reference_date<-'` can be used to
#'   directlty set the `reference_date` field of the `dsinfo` attribute of
#'   an R object.
#'
#' @rdname dsinfo
#' @export
#'
set_reference_date <- function(x, y, q = NULL, m = NULL){
  if(is_date_xx(y)){
    assert_that(is.null(q) && is.null(m))
    value <- y
  } else {
    value <- make_date_xx(y, q, m)
  }

  x <- set_dsinfo(x, reference_date = value)
}





#' @return `has_reference_date()` returns `TRUE` if `x` has a valid
#'   `reference_date`, and `FALSE` otherwise
#'
#' @rdname dsinfo
#' @export
#'
has_reference_date <- function(x){
  hammr::is_date_xx(reference_date(x))
}



#' @export
print.dsinfo <- function(x, ...){

  title_els <- c("id", "name", "reference_date", "version")

  r1 <- character()

  r1[["header"]]  <- paste_if_el(x, title_els)
  r1[['title']]   <- paste_if_el(x, "title")
  r1[['desc']]    <- paste_if_el(x, "description", prefix = '\n', suffix = '\n')
  r1[["sources"]] <- paste("sources: ", format_sources(x$sources))


  y <- x[!names(x) %in% union(title_els, c("description", "title", "sources"))] %>%
    lapply(as.character)
  r2 <- sprintf("%s: \t%s", names(y), y)

  res <- c(r1, r2)
  res <- res[res != ""]

  invisible(lapply(res, cat, '\n'))
}



paste_if_el <- function(x, els, prefix = NULL, suffix = NULL){
  sel <- grep(
    paste(els, collapse = "|"),
    names(x)
  )

  res <- x[sel] %>%
    lapply(as.character) %>%  # necessary for dates
    unlist()

  if(!is.null(res)){
    paste(prefix, res, suffix, collapse = ' - ', sep = "")
  } else {
    ""
  }
}




is_dsinfo_name <- function(x){
  isTRUE(grepl("^[A-Za-z0-9_\\.-]*$", x)) && is.scalar(x)
}




#' Create sources for dsinfo
#'
#'
#' `dsi_source()` is a constructor for the component objects that make up a
#' `dsi_sources()` object (a single source file or person).
#'
#' @param title Title of the source
#' @param path Path to the source
#' @param email Email address of the source
#' @param date Date of the source
#'
#' @return  `dsi_source()` returns a `dsinfo_source` object.
#' @export
#'
dsi_source <- function(title, path = NULL, email = NULL, date = NULL){
  res <- list(title = title, path = path, email = email, date = date)
  attr(res, "class") <- c("dsinfo_source", "list")
  res
}




#' `dsi_sources()` and `dsi_sources_list()` are constructors for objects that
#' can be used for the `source` parameter of [dsinfo()] (a list of sources)
#'
#' @param ... `dsinfo_source` objects.
#' @export
#' @return  `dsi_sources()`, `dsi_sources_list()`, and `dsi_sources_from_paths()`
#'   return a `dsinfo_sources` object, which is a list of `dsinfo_source`
#'   objects.
#' @rdname dsi_source
#'
dsi_sources <- function(...){
  dsi_sources_list(list(...))
}




#' @param sources a list of `dsinfo_source` objects.
#' @export
#'
#' @rdname dsi_source
#'
dsi_sources_list <- function(sources){
  assert_that(all(
    purrr::map_lgl(sources, function(x) inherits(x, "dsinfo_source"))
  ))
  attr(sources, "class") <- c("dsinfo_sources", "list")
  sources
}




#' `dsi_sources_from_paths()` is a helper function to automatically creates a
#' `dsi_sources` object from file system paths.
#'
#' @export
#' @param paths `character` vector of file system paths
#' @return `dsi_sources_from_paths` returns a `dsi_sources` object.
#' @rdname dsi_source
#'
#' @examples
#' x <- set_dsinfo(
#'   iris,
#'   title = "Iris",
#'   sources = dsi_sources(
#'     dsi_source("R demo data", date = Sys.Date()),
#'     dsi_source("Alfred Bogus", email = "alfred@bogus.xx")
#'   )
#' )
#'
#' dsinfo(x)
#'
dsi_sources_from_paths <- function(paths){
  dsi_sources_list(
    lapply(paths, function(x){
        dsi_source(title = basename(x), path = x, date = file.info(x)$mtime)
    })
  )
}




format_sources <- function(x, indent = "  "){
  lapply(x, format_source) %>%
    unlist() %>%
    paste0(collapse = paste0("\n", indent)) %>%
    paste0("\n", indent, .)
}



format_source <- function(x){
  title <- paste(x$title, x$date, sep = "\t")

  paths  <- purrr::map_chr(x$path, function(x.) paste("   -", x.))

  if (!isit::is_empty(x$email)) {
    emails <- paste(x$email, collapse = ", ")
  } else {
    emails <- NULL
  }




  if(!isit::is_empty(paths))  paths  <- c("  Paths: ", paths)
  if(!isit::is_empty(emails)) emails <- paste("  Contact: ", emails)

  c(title, paths, emails)
}



is_reference_date <- function(x){
  inherits(x, c('Date', 'POSIXt', 'Interval', 'date_xx'))
}
