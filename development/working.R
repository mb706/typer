
library("roxygen2")

roxygenise("..")

devtools::load_all("..")

x = function(a = .:1 | Task | 3, b = "test", c = 2:numeric) { }

compileFunction(x, "x")

devtools::test("..")

compileConditionCheck(quote(Task [[ unique ]]), quote(task))
