# hammr (dev version)

* add `human_time()` for formatting seconds and difftimes
* df_drop_empty_rows gains `drop_blanks` argument for treating blank character
  cells as if they were `NA`
* added `words_to_vector()`
* added `triput` (an alternative to `dput()` for data.frames that uses 
  `tibble::tribble()` syntax)

# hammr 1.0.3

* `dsinfo()` and all `date_xx` functions moved to separate packages **dsinfo** 
  and **dint**
* documentation and cleanup for proper package release
* remove dependency on **dplyr**
* added `df_add_row()`
* added `relative_change()`
* `df_add_margin_row()` now uses `vctrs::vec_rbind()` instead of 
  `dplyr::bind_rows()`
* dropped `dplyr` from suggests and added the much lighter `vctrs` instead.



# hammr 1.0.2

* added wday2: ISO compliant wrapper for `lubridate::wday()` (i.e. Monday is day 1)
* combat updates for purrr 0.2.3 and dplyr 0.7.0
* added `add_subclass()` utility function
* added `dsi_sources_from_paths()` helper function for generating sources for
  `dsinfo()`
* added `df_rsplit_interval()` to split a `data.frame` by row at regular 
  intervals 
* added `vec_enum()` to enumerate elements of vectors
* `relative_diff()` for calculating relative difference between numbers
* `set_dsinfo()` now supports `.add` paramter 
* better `print.dsinfo()` that supports terminal colors




# hammr 1.0.1

* added dsinfo: set and display information about a data set
* added df_value_replace_if: replace a value in data frame columns that match
  a predicate
