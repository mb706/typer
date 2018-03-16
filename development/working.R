
library("roxygen2")

roxygenise("..")

devtools::load_all("..")

selectFirst(TRUE, iris[c(1, 10), ])
selectFirst(FALSE, iris[c(1, 10), ])


x = function(a = .:1 | Task | 3, b = "test", c = 2:numeric) { }

compileFunction(x, "x")

devtools::test("..")

parseConditionCheck(quote(Task [[ unique, task > 10 ]]), "task")





quote(numeric(1)[[2]])[[2]][[1]]

str(quote("Task"(1))[[1]])

length(quote("Task"()))
is.recursive(quote(numeric))

quote(test[1,])



cat("a b
c d")


identical(quote(test[, ])[[3]], substitute())
