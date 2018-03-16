
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
