
library("roxygen2")

roxygenise("..")

devtools::load_all("..")


devtools::test("..")

