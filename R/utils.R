
#################################
# syntactic sugar               #
#################################

# increment by
#
# This is the C `+=` operator
`%+=%` = function(t, s) eval.parent(substitute({t = t + s}))

# decrement by
#
# This is the C `-=` operator
`%-=%` = function(t, m) eval.parent(substitute({t = t - m}))

# append
#
# X %c=% Y --> X = c(X, Y)
`%c=%` = function(t, a) eval.parent(substitute({t = c(t, a)}))

# union
#
# X %union=% Y --> X = union(X, Y)
`%union=%` = function(t, a) eval.parent(substitute({t = union(t, a)}))

# multireturn
#
# list(var1 = b, var2 = a, var3 = a + b, a) %<=% list(a = 1, b = 2, c = 3)
# -->
# var1 = 2 (b)
# var2 = 1 (a)
# var3 = 3 (a + b)
# a = 1 (a)
`%<=%` = function(a, b) {
  inexp = substitute(a)
  for (i in seq_len(length(inexp) - 1) + 1) {
    val = eval(inexp[[i]], b, enclos = parent.frame())
    assigntochr = names(inexp)[i]
    if (is.null(assigntochr) || assigntochr == "") {
      assignto = inexp[[i]]
    } else {
      assignto = asQuoted(assigntochr)
    }
    assignment = substitute({t = quote(v)}, list(t = assignto, v = val))
    eval.parent(assignment)
  }
}



