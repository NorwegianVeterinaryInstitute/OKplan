# Remove NOTE when running CMD check and checking dependencies
# Namespaces in Imports field not imported from:
#   'pkgname'
# All declared Imports should be used.


ignore_unused_imports <- function() {
  # Packages needed for template: "check_ok_selection.Rmd"
  OKcheck::knit_table_if_data
}
