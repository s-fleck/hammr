# hammr 1.0.2.9000

* `dsinfo()` and all `date_xx` functions moved to separate packages **dsinfo** 
  and **dint**
* documentation and cleanup for proper package release
* remove dependency on **dplyr**


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
