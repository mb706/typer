
context("type decorations")


test_that("compileFunction formals are as expected", {

  x = function(a = .:numeric(2) | Task | data.frame, b = "test", c = 2:numeric) { }

  expect_identical(formals(compileFunction(x, "x")), formals(function(a, b = "test", c = 2){}))

  expect_identical(formals(compileFunction(function(a = NULL: NULL | integer) a, "x")), formals(function(a = NULL) a))

  expect_error(compileFunction(function(a = 1 + 2:numeric) { }, "x"), "Colon used inside default value is not at outermost level")

  expect_equal(compileFunction(function(x) x, "x"), function(x) x)
  expect_equal(compileFunction(function(x, y = 1) x, "x"), function(x, y = 1) x)

})

test_that("conditions are parsed properly", {

  parsed = parseConditionCheck(quote(Task [[ x > 1, unique, named ]]), "x", "test")
  expect_equal(parsed$class, "Task")
  expect_null(parsed$values)
  expect_true(parsed$needs.unique)
  expect_false(parsed$allow.na)
  expect_equal(parsed$list.depth, 0)
  expect_null(parsed$min.listlength)
  expect_null(parsed$max.listlength)
  expect_null(parsed$min.veclength)
  expect_null(parsed$max.veclength)
  expect_length(parsed$extracons, 3)
  expect_length(parsed$extracons[[1]], 2)
  expect_length(parsed$extracons[[2]], 2)
  expect_identical(parsed$extracons[[1]][[1]], quote(isTRUE(x > 1)))
  expect_identical(parsed$extracons[[3]][[1]], quote(checkmate::testNames(names(x))))
  expect_true(is.character(parsed$extracons[[1]][[2]]))
  expect_true(is.character(parsed$extracons[[3]][[2]]))


  expect_warning(parseConditionCheck(quote(Task.na), "x", "test"), "class name Task\\.na suffixed with \\.na does not")
  expect_true(parseConditionCheck(quote(logical.na), "x", "test")$allow.na)

  parsed = parseConditionCheck(quote(value(1, 2, 3)[1][][, ][1, ]), "x", "test")
  expect_equal(parsed$class, "value")
  expect_identical(parsed$values, c(1, 2, 3))
  expect_false(parsed$needs.unique)
  expect_equal(parsed$list.depth, 4)
  expect_equal(parsed$min.listlength, c(1, 0, 0, 1))
  expect_equal(parsed$max.listlength, c(1, Inf, Inf, Inf))
  expect_null(parsed$min.veclength)
  expect_null(parsed$max.veclength)
  expect_length(parsed$extracons, 0)

  parsed = parseConditionCheck(quote(integer(3)[2, 3]), "x", "test")
  expect_equal(parsed$min.veclength, 3)
  expect_equal(parsed$max.veclength, 3)
  expect_equal(parsed$list.depth, 1)
  expect_equal(parsed$min.listlength, 2)
  expect_equal(parsed$max.listlength, 3)

  parsed = parseConditionCheck(quote(list(3, )[2, 3]), "x", "test")
  expect_equal(parsed$min.veclength, 3)
  expect_equal(parsed$max.veclength, Inf)
  expect_equal(parsed$list.depth, 1)
  expect_equal(parsed$min.listlength, 2)
  expect_equal(parsed$max.listlength, 3)

  expect_error(parseConditionCheck(quote(value(1, 2, quote)), "x", "test")  , "value list value.*must be atomic.")

  parsed = parseConditionCheck(quote(value(1, 2, 3)(1, 2)), "x", "test")
  expect_equal(parsed$class, "value")
  expect_identical(parsed$values, c(1, 2, 3))
  expect_equal(parsed$min.veclength, 1)
  expect_equal(parsed$max.veclength, 2)
  expect_null(parsed$min.listlength)
  expect_null(parsed$max.listlength)
  expect_equal(parsed$list.depth, 0)

  parsed = parseConditionCheck(quote(NULL [[ x == y ]]), "x", "test")
  expect_equal(parsed$class, "NULL")
  expect_identical(parsed$extracons[[1]][[1]], quote(isTRUE(x == y)))

  expect_error(parseConditionCheck(quote(Task(1)), "x", "test"), "can not have a specified vector length")
  expect_error(parseConditionCheck(quote(integer(1, 2, 3)), "x", "test"), "parentheses must have one or two.*arguments")
  expect_error(parseConditionCheck(quote(integer()), "x", "test"), "parentheses must have one or two.*arguments")
  expect_error(parseConditionCheck(quote(integer[1, 2, 3]), "x", "test"), "brackets must have zero, one or two.*arguments")
  expect_error(parseConditionCheck(quote(integer[x]), "x", "test"), "part x must be numeric")
  expect_error(parseConditionCheck(quote(integer[1, x]), "x", "test"), "part x must be numeric")
  expect_error(parseConditionCheck(quote(integer(x)), "x", "test"), "part x must be numeric")
  expect_error(parseConditionCheck(quote(integer(1, x)), "x", "test"), "part x must be numeric")
  expect_error(parseConditionCheck(quote(NULL(1)), "x", "test"), "list or vector of NULL is not allowed.")
  expect_error(parseConditionCheck(quote(NULL[1]), "x", "test"), "list or vector of NULL is not allowed.")

})

test_that("createCheckmateTest does its job", {

  xquote = quote(x)

  expect_identical(createCheckmateTest("NULL", xquote, FALSE, FALSE, NULL, NULL), quote(checkmate::testNull(x)))
  expect_identical(createCheckmateTest("numeric", xquote, FALSE, FALSE, NULL, NULL),
    quote(checkmate::testNumeric(x, any.missing = FALSE, unique = FALSE)))
  expect_identical(createCheckmateTest("numeric", xquote, TRUE, FALSE, NULL, NULL),
    quote(checkmate::testNumeric(x, any.missing = FALSE, unique = TRUE)))
  expect_identical(createCheckmateTest("numeric", xquote, FALSE, TRUE, NULL, NULL),
    quote(checkmate::testNumeric(x, any.missing = TRUE, unique = FALSE)))
  expect_identical(createCheckmateTest("Task", xquote, FALSE, FALSE, NULL, NULL),
    quote(checkmate::testClass(x, classes = "Task")))
  expect_identical(createCheckmateTest("Task", xquote, FALSE, TRUE, 0, Inf),
    quote(checkmate::testList(x, types = "Task", any.missing = TRUE, unique = FALSE)))

  expect_identical(createCheckmateTest("integer", quote(abc), FALSE, FALSE, NULL, NULL),
    quote(checkmate::testNumeric(abc, any.missing = FALSE, unique = FALSE) && all(abc == round(abc), na.rm = TRUE)))

  expect_equal(createCheckmateTest("value", quote(abc), TRUE, TRUE, 1, 4, c(1, 2, 10)),
    quote(checkmate::testAtomic(abc, min.len = 1, max.len = 4, any.missing = TRUE, unique = TRUE) &&
          checkmate::testSubset(abc, c(1, 2, 10))))

})

test_that("compileConditionCheck works as expected", {

  cond = compileConditionCheck(parseConditionCheck(quote(integer(2)), "test", "yooo"), "test")
  test = 1
  expect_false(eval(cond))
  test = 1:2
  expect_true(eval(cond))
  test = c(1.5, 2)
  expect_false(eval(cond))
  test = c(1, 2)
  expect_true(eval(cond))


  cond = compileConditionCheck(parseConditionCheck(quote(integer), "test", "yooo"), "test")
  test = 1
  expect_true(eval(cond))
  test = 1L
  expect_true(eval(cond))
  test = 1.2
  expect_false(eval(cond))
  test = 1.000000000001
  expect_false(eval(cond))
  test = 1:2
  expect_true(eval(cond))

  t1 = t2 = t3 = list()
  class(t1) = class(t2) = class(t3) = "Task"

  cond = compileConditionCheck(parseConditionCheck(quote(Task), "test", "yooo"), "test")
  test = t1
  expect_true(eval(cond))
  test = list(t1)
  expect_false(eval(cond))

  cond = compileConditionCheck(parseConditionCheck(quote(Task[2, 3]), "test", "yooo"), "test")
  test = t1
  expect_false(eval(cond))
  test = list(t1)
  expect_false(eval(cond))
  test = list(t1, t2)
  expect_true(eval(cond))
  cond = compileConditionCheck(parseConditionCheck(quote(Task[2, 3][[unique]]), "test", "yooo"), "test")
  test = list(t1, t2)
  expect_false(eval(cond))
  test[[2]]$x = 1
  expect_true(eval(cond))

  cond = compileConditionCheck(parseConditionCheck(quote(integer[2, 3]), "test", "yooo"), "test")
  test = 1:3
  expect_false(eval(cond))
  test = list(1, 2)
  expect_true(eval(cond))
  test = list(1:3, 3)
  expect_true(eval(cond))

  cond = compileConditionCheck(parseConditionCheck(quote(integer(2)[2, 3]), "test", "yooo"), "test")
  test = 1:3
  expect_false(eval(cond))
  test = list(1, 2)
  expect_false(eval(cond))
  test = list(1:3, 3)
  expect_false(eval(cond))
  test = list(1:2, 3:4)
  expect_true(eval(cond))
  test = list(1:2, c(1, NA))
  expect_false(eval(cond))
  cond = compileConditionCheck(parseConditionCheck(quote(integer.na(2)[2, 3]), "test", "yooo"), "test")
  expect_true(eval(cond))

  cond = compileConditionCheck(parseConditionCheck(quote(numeric.na(1)[2, 3][4]), "test", "yooo"), "test")
  test = list(list(1, 2, 3), list(1, 2), list(1, 2, 3), list(2, 2))
  expect_true(eval(cond))
  test = list(list(1, 2, 3), list(1, 2), list(1, 2, 3), list(NA, 2))
  expect_true(eval(cond))
  test = list(list(1, 2, 3), list(1, 2), list(1, 2, 3), list(NA, NA))
  expect_true(eval(cond))
  test = list(list(1, 2, 3), list(1, 2), list(1, 2, 3))
  expect_false(eval(cond))
  test = list(list(1, 2, 3), list(1, 2), list(1, 2, 3, 4), list(NA, NA))
  expect_false(eval(cond))
  test = list(list(1, 2, 3.4), list(1, 2), list(1, 2, 3), list(2, 2))
  expect_true(eval(cond))


  cond = compileConditionCheck(parseConditionCheck(quote(numeric.na(1)[2, 3][4] [[ unique ]]), "test", "yooo"), "test")
  test = list(list(1, 2, 3.5), list(1, 2), list(1, 2, 3), list(2, 2))
  expect_true(eval(cond))
  test = list(list(1, 2, 3), list(1, 2), list(1, 2, 3), list(2, 2))
  expect_false(eval(cond))
  test = list(list(1, 2, 3.5), list(1, 2), list(1, 2, 3), list(NA, 2))
  expect_true(eval(cond))
  test = list(list(1, 2, 3), list(1, 2), list(1, 2, 3), list(NA, 2))
  expect_false(eval(cond))


  cond = compileConditionCheck(parseConditionCheck(quote(value(1, 2, NA)(2, 3)[4] [[ unique ]]), "test", "yooo"), "test")
  test = list(c(1, 2), c(1, 1), c(2, 2), c(2, 1))
  expect_true(eval(cond))
  test = list(c(1, 2), c(1, 1), c(NA, 2), c(2, 1))
  expect_true(eval(cond))

  test = list(c(1, 2), c(1, 1), c(2, 1), c(2, 1))
  expect_false(eval(cond))

  test = list(c(1, 2), c(1, 1), c(2, 2), c(2, 3))
  expect_false(eval(cond))

  cond = compileConditionCheck(parseConditionCheck(quote(value(1, 2)(2, 3)[4] [[ unique ]]), "test", "yooo"), "test")
  test = list(c(1, 2), c(1, 1), c(NA, 2), c(2, 1))
  expect_false(eval(cond))

  cond = compileConditionCheck(parseConditionCheck(quote(NULL), "test", "yooo"), "test")
  expect_identical(cond, quote(checkmate::testNull(test)))


})

test_that("compiled functions behave as expected", {

  f = function(a = .:data.frame [[nrow(a) == 2]] | NULL, b = 10:integer(1) | numeric(2) [[named]] | character(3)) { b }

  fc = compileFunction(f, "f")

  expect_equal(fc(NULL), 10)

  expect_equal(fc(iris[1:2, ]), 10)

  expect_error(fc(iris), "'a' must be of format 'data.frame'.*nrow\\(a\\) == 2.*be of format 'NULL'")

  expect_error(fc(NULL, 10.5), "'b' must be of format 'integer\\(1\\)'.*must be of format.*numeric.*must be named.*character\\(3\\)")

  expect_equal(fc(NULL, letters[(1:3)*5]), c("e", "j", "o"))

  expect_equal(fc(NULL, c(a = 1, b = 2)), c(a = 1, b = 2))
  expect_equal(fc(NULL, c(a = 1, a = 2)), c(a = 1, a = 2))

  f = function(a = .:data.frame [[nrow(a) == 2]] | NULL,
               b = 10:integer(1) | numeric(2) [[uniquely.named, sum(b) < 3]] | character(3)) { b }

  fc = compileFunction(f, "f")

  expect_error(fc(NULL, c(a = 1, b = 2)), "and the expression.*sum\\(b\\)")

  expect_error(fc(NULL, c(a = 1, a = 1)), "and the expression.*sum\\(b\\)")

  expect_equal(fc(NULL, c(a = 1, b = -1)), c(a = 1, b = -1))

})

test_that("inclusion and exclusion of functions works", {

  (function() {
    a = 1
    a1 = function() {}
    ab = function() {}
    ac = function() {}
    abc = function() {}
    bc = function() {}
    b = function() {}
    expect_setequal(getAffectedFunctions(env = environment()), c("a1", "ab", "ac", "abc", "bc", "b"))
    expect_setequal(getAffectedFunctions(env = environment(), include = "a"), c("a1", "ab", "ac", "abc"))
    expect_setequal(getAffectedFunctions(env = environment(), exclude = "a"), c("bc", "b"))
    expect_setequal(getAffectedFunctions(env = environment(), exclude = "z"), c("a1", "ab", "ac", "abc", "bc", "b"))
    expect_setequal(getAffectedFunctions(env = environment(), include = "a", exclude = "c", include = "b"),
      c("a1", "ab", "abc", "bc", "b"))
    expect_error(getAffectedFunctions(env = environment(), badpar = 10), "only accepts")
  })()

})

test_that("functions are compiled", {

  (function() {
    a = function(x = .: NULL | data.frame) x
    b = function(x = 10: numeric | character [[unique]]) x
    compileTypes()
    expect_null(a(NULL))
    expect_equal(a(iris), iris)
    expect_error(a(10), "data\\.frame")
    expect_equal(b(), 10)
    expect_equal(b(c("a", "b")), c("a", "b"))
    expect_error(b(c("a", "a")), "unique")
  })()

})
