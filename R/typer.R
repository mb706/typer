



#' Convert Argument Annotations to Argument Checks
#'
#' A function annotation for a parameter has the form
#' ```
#'   parname = value : type | type | type
#' ```
#' where `type` can be a class name or a vector of possible (atomic) values
#' indicated as `value(...)`. For special class names
#' `logical`, `character`, `numeric`, `list` the [`mode`]
#' is checked instead. The special class name `integer` checks
#' for integer values (that may still be given as `numeric`, i.e.
#' floating point, value). Suffixing any of these special class names with `.na`
#' allows missing values (`NA`s): `logical.na`, `character.na` etc.
#'
#' The special class name `NULL` allows `NULL` values.
#'
#' Non-list mode class names can be suffixed with `(n)`,
#' where `n` is an integer indicating the required vector length.
#' Further possibilities are `(n, m)` to indicate length between
#' `n` and `m`. Leaving out `n` or `m` (`(, m)` or `(n, )`) only
#' sets a minimum or maximum length.
#'
#' The `(n)` suffix also works for `value(...)`: `value(1, 2)(1)`
#' specifies that a value can be `1` or `2`. Note that `value(1, 2)`
#' also allows `c(1, 2)` and `c(1, 1, 2, 1)`.
#'
#' Class names or value lists can be suffixed with `[]` to
#' indicate "list of", or `[n]` to indicate "length `n` list of".
#' Similarly to non list length indicators, a minimum and maximum
#' can also be given (`[n, m]`), any of which can be omitted.
#'
#' Conditions for each type can be added in `[[` `]]` and separated
#' by `,`. Special conditions are `named`, `unique`, `uniquely.named`,
#' which apply to lists (and, in case of nested lists, only apply to
#' the outermost list);
#' other conditions should be expressions that are evaluated in the
#' context of the given parameters as well as the function's environment
#' (and should probably involve the value at hand.
#'
#' A special value of the `value` part is `.`(dot), which indicates that no
#' default value is given for the function: `param = . : numeric`.
#'
#' @param env The environment to parse. Defaults to [parent.frame()].
#' @param ... `include` or `exclude`, given as `character(1)` [grep()] patterns.
#'   These are applied in the order given, so it is possible to
#'   first include certain functions, then exclude a subset of these,
#'   and then include a subset of these excluded functions etc.\cr
#'   This is usually not necessary to give, since `compileTypes` will
#'   automatically ignore functions that do not have decorators, but helps
#'   in cases where parameters have default values containing `:` or `|`
#'   characters. This does *not* switch of type checks for the function, and
#'   in fact may produce errors if a function containing type decorators is
#'   not converted.
#' @return `NULL`
#' @examples
#' ```
#' # this function takes a parameter that is either a vector of two integer
#' # numbers (either datatype integer or numeric), a vector of three numbers,
#' # or a named list of data.frame that all have more than two rows.
#' fun = function(a = .:integer(2) | numeric(3) |
#'     data.frame[] [[named, all(sapply(a, nrow) > 2)]]) {
#'   NULL
#' }
#'
#' compileTypes()
#' ```
#' @export
compileTypes = function(env = parent.frame(), ...) {
  affected = getAffectedFunctions(env, ...)
  for (fname in affected) {
    fun = get(fname, env)
    fun = compileFunction(fun, fname)
    assign(fname, fun, env)
  }
  invisible(NULL)
}


# Use the `include` and `exclude` parameters of `compileTypes` to get
# the functions that should be affected.
# @param env [environment] the environment to search
# @param ... 'include' or 'exclude', character values.
# @return [character] names of functions inside `env` to affect
getAffectedFunctions = function(env, ...) {
  rules = list(...)
  allnames = Filter(function(n) is.function(get(n, env, inherits = FALSE)),
    ls(envir = env, all.names = TRUE))

  included = rep(!length(rules) || names(rules)[1] == "exclude", length(allnames))

  for (idx in seq_along(rules)) {
    switch(names(rules)[idx],
      include = {
        included = included | grepl(rules[[idx]], allnames)
      },
      exclude = {
        included = included & !grepl(rules[[idx]], allnames)
      },
      stopf("Invalid parameter %s. `...` only accepts 'include' and 'exclude'.",
        names(rules)[idx]))
  }
  allnames[included]
}

# check whether `symbol` occurs in `expr`
# @param expr [language] the expression to search
# @param symbol [language] the symbol to search for
# @return [logical(1)] whether `symbol` occurs in `expr`
hassymbol = function(expr, symbol) {
  if (is.recursive(expr)) {
    any(vlapply(expr, hassymbol, symbol = symbol))
  } else {
    identical(expr, symbol)
  }
}

# Give a short character representation of `expr`
# @param expr [language] the expression to deparse
# @return [character(1)] representation of `expr` which cuts off after about 60
#   characters.
shortdeparse = function(expr) {
  dep = deparse(expr)
  if (length(dep) > 1) {
    paste0(deparse(expr)[1], "...")
  } else {
    dep
  }
}

# Give a complete character representation of `expr`
# @param expr [language] the expression to deparse
# @return [character(1)] representation of `expr` that contains the whole
#   expression. This should still only be used for error output, and should
#   not be parsed again.
alldeparse = function(expr) {
  collapse(deparse(expr), sep = "\n")
}

# Remove the type decorators of a function and add the type checks in the body
# @param fun [function] the function to change
# @param name [character(1)] name of the function to use in error messages
# @return [function] the function with removed type decorators and added type
#   checks.
compileFunction = function(fun, name) {

  insert = quote({})
  originsert = insert
  formals.in = formals(fun)

  pipesep = quote(`|`)
  colonsep = quote(`:`)
  dotsymbol = quote(.)


  # iterate through the arguments
  for (idx in seq_along(formals.in)) {
    if (identical(formals.in[[idx]], substitute())) {
      # this is necessary, since `substitute()` otherwise gives
      # "argument missing" errors.
      next
    }
    curformal = formals.in[[idx]]
    conditions = NULL
    founddots = FALSE
    errprefix = sprintf("Function %s decorator
  %s
is malformed:", name, shortdeparse(formals.in[[idx]]))
    varname = names(formals(fun))[idx]

    while (is.recursive(curformal) && identical(curformal[[1]], pipesep)) {
      # split conditions separated by `|`
      conditions = c(list(curformal[[3]]), conditions)
      curformal = curformal[[2]]
    }
    if (is.recursive(curformal) && identical(curformal[[1]], colonsep)) {
      # split the `value : condition` part at the colon
      founddots = TRUE
      conditions = c(list(curformal[[3]]), conditions)
      curformal = curformal[[2]]
    }
    if (identical(curformal, dotsymbol)) {
      founddots = TRUE
    }
    if (!founddots && hassymbol(curformal, colonsep)) {
      # it may be possible that a `:` is contained in the expression, but not
      # at the top level of the expression, e.g.
      #   1 + 2 : numeric
      # which internally is represented as
      #   1 + (2 : numeric)
      # We could reorder the AST in some complicated way, but instead we just
      # ask the user to set parentheses:
      #  (1 + 2) : numeric
      stopf("%s
 Colon used inside default value is not at outermost level.
Possibly use parentheses.",
        errprefix)
    }
    if (identical(curformal, dotsymbol)) {
      # this is apparently necessary because
      # x = substitute()
      # y = x
      # does not work.
      formals(fun)[[idx]] = substitute()
    } else {
      formals(fun)[[idx]] = curformal
    }
    if (!length(conditions)) {
      # no type decorators
      next
    }

    # compile the single conditions that are separated by `|`
    single.conds = lapply(conditions, singleConditionCheck,
      varname = varname, errprefix = errprefix)

    # the conditions are chained together using `||`
    full.cond = Reduce(function(a, b) substitute(a || b, list(a = a, b = b)),
      extractSubList(single.conds, "fullexp"))

    # the description of the condition, should it fail, that will be printed in
    # the error message:
    full.string = sprintf("Argument '%s' must %s", varname,
      collapse(extractSubList(single.conds, "fullstring"),
        sep = sprintf("\n  or\n    '%s' must ", varname)))

    # the whole check: `if (not condition) { error(description) }`
    full.check = substitute(if (!a) stop(b),
      list(a = full.cond, b = full.string))

    insert[[length(insert) + 1]] = full.check
  }
  if (!identical(insert, originsert)) {
    # add the checks to the beginning of the function
    body(fun) = substitute({a ; b}, list(a = insert, b = body(fun)))
  }
  fun
}

# Create the Check for a Single Condition
# @param condition [language] the type decorator, i.e. part behind the `:` ans
#   separated by `|`
# @param varname [character(1)] name of the variable under consideration
# @param errprefix [character(1)] What to print before error messages, should
#   type decorator be malformed.
# @return [list(language, character(1))] list of the check to perform, and an
#   error message to print if the check fails.
singleConditionCheck = function(condition, varname, errprefix) {
  parsed = parseConditionCheck(condition, varname, errprefix)
  compiled = compileConditionCheck(parsed, varname)
  fullexp = Reduce(function(a, b) substitute(a && b, list(a = a, b = b)),
    extractSubList(parsed$extracons, 1), compiled)
  fullstring = c(sprintf("be of format '%s'", parsed$formatstring),
    extractSubList(parsed$extracons, 2))
  fullstring = collapse(fullstring, sep = " and ")
  list(fullexp = fullexp, fullstring = fullstring)
}

# Parse single condition into intermediate representation
# @param condition [language] the type decorator, i.e. part behind the `:` ans
#   separated by `|`
# @param varname [character(1)] name of the variable under consideration
# @param errprefix [character(1)] What to print before error messages, should
#   type decorator be malformed.
# @return [list] list with intermediate facts about the condition, to be used
#   by `compileConditionCheck()`.
parseConditionCheck = function(condition, varname, errprefix) {
  fullcondition = condition
  varquote = asQuoted(varname)

  abort = function(msg) {  # print message and abort
    stopf("%s\nCondition %s %s",
      errprefix, shortdeparse(fullcondition), msg)
  }
  abortf = function(...) {  # abort + sprintf
    abort(sprintf(...))
  }
  abortIfNotNum = function(x) {
    if (!is.numeric(x) || is.na(x)) {
      abortf("part %s must be numeric.", shortdeparse(x))
    }
  }

  needs.unique = FALSE
  list.depth = 0
  min.listlength = NULL
  max.listlength = NULL

  min.veclength = NULL
  max.veclength = NULL

  extracons = list()
  values = NULL
  allow.na = FALSE

  # first get the `[[ ... ]]` part
  # Except the `unique` part (which is saved to `needs.unique`), all of them
  # are of the form
  #   list([condition to theck], [string to inform about the condition])
  # and will be chained using `&&` by `singleConditionCheck`.
  if (is.recursive(condition) && identical(condition[[1]], quote(`[[`))) {
    # extra conditions
    extracons = lapply(seq(3, length(condition)), function(idx) {
      curcon = condition[[idx]]
      if (identical(curcon, quote(named))) {
        list(substitute(checkmate::testNames(names(x)),
          list(x = varquote)),
          sprintf("'%s' must be named", varname))
      } else if (identical(curcon, quote(uniquely.named))) {
        list(substitute(checkmate::testNames(names(x), type = "unique"),
          list(x = varquote)),
          sprintf("'%s' must have unique names", varname))
      } else if (identical(curcon, quote(unique))) {
        needs.unique <<- TRUE
        list(TRUE, sprintf("'%s' must be unique", varname))
      } else {
        list(substitute(isTRUE(x), list(x = curcon)),
          sprintf("the expression\n      %s\n    must evaluate to TRUE",
            alldeparse(curcon)))
      }
    })
    extracons = filterNull(extracons)
    condition = condition[[2]]
  }

  # Now that the `[[ ]]` is removed, we save a string that informs the user
  # about the format of the variable. This could e.g. be 'numeric(2)'.
  formatstring = alldeparse(condition)

  # Handle the `[ ]` part of the condition. Nested lists are possible, so
  # we loop until no more `[]` are present. The information about the list
  # nesting depth and list lengths is saved in `list.depth`, `min.listlength`
  # and `max.listlength`.
  while (is.recursive(condition) && identical(condition[[1]], quote(`[`))) {
    if (length(condition) > 4) {
      abort("brackets must have zero, one or two (possibly empty) arguments.")
    }
    if (length(condition) > 2 && !identical(condition[[3]], substitute())) {
      minl = condition[[3]]
      abortIfNotNum(minl)
    } else {
      minl = 0
    }
    if (length(condition) > 3 && !identical(condition[[4]], substitute())) {
      maxl = condition[[4]]
      abortIfNotNum(maxl)
    } else {
      maxl = Inf
    }
    if (length(condition) == 3 && !identical(condition[[3]], substitute())) {
      maxl = minl
    }
    list.depth %+=% 1
    min.listlength %c=% minl
    max.listlength %c=% maxl
    condition = condition[[2]]
  }

  # Handle the `()` part, e.g. `integer(2)` or (the second paranetheses in)
  # `value(1, 2, 3)(, 2)`.
  if (is.recursive(condition) && !identical(condition[[1]], quote(value))) {
    if (!(is.symbol(condition[[1]]) || is.null(condition[[1]])) &&
        (!is.recursive(condition[[1]]) ||
         !identical(condition[[1]][[1]], quote(value)))) {
      abort("base must be a class, type name, or 'value'.")
    }
    classname = as.character(condition[[1]])
    if (length(condition) > 3 || length(condition) <= 1) {
      abort("parentheses must have one or two (possibly empty) arguments.")
    }
    if (!identical(condition[[2]], substitute())) {
      min.veclength = condition[[2]]
      abortIfNotNum(min.veclength)
    } else {
      min.veclength = 0
    }
    if (length(condition) == 2) {
      max.veclength = min.veclength
    } else {
      if (!identical(condition[[3]], substitute())) {
        max.veclength = condition[[3]]
        abortIfNotNum(max.veclength)
      } else {
        max.veclength = Inf
      }
    }
    condition = condition[[1]]
  }

  # Extract the possible values from `value(1, 2, 3)`.
  if (is.recursive(condition) && identical(condition[[1]], quote(value))) {
    values = as.list(condition)
    values[[1]] = NULL
    values = unlist(values, FALSE)
    if (!is.atomic(values)) {
      abortf("value list %s must be atomic.", shortdeparse(condition))
    }

    condition = condition[[1]]
  }

  # Get the class / mode name
  if (is.null(condition)) {
    # The class name `NULL` must be handled separately, because `quote(NULL)`
    # does not produce a symbol "NULL".
    condition = "NULL"
  } else {
    if (!is.symbol(condition)) {
      abort("base must be a class, type name, or 'value'.")
    }
    condition = as.character(condition)
  }

  # Some modes allow `NA`s if the name is suffixed with `.na`.
  if (grepl("\\.na$", condition)) {
    precon = substr(condition, 1, nchar(condition) - 3)
    if (precon %in% c("logical", "integer", "numeric", "character", "list")) {
      allow.na = TRUE
      condition = precon
    } else {
      warningf(
          "Note that class name %s suffixed with .na does not enable na values.",
          condition)
    }
  }

  # Only the special mode classes can have vector lengths; Other classes should
  # have list lengths (if at all).
  if (condition %nin%
      c("logical", "integer", "numeric", "character", "list", "value", "NULL") &&
    (!is.null(min.veclength) || !is.null(max.veclength))) {
    abortf("base type %s can not have a specified vector length", condition)
  }
  if (condition == "NULL" &&
      (list.depth != 0 || !is.null(min.veclength) || !is.null(max.veclength))) {
    abort("list or vector of NULL is not allowed.")
  }

  list(class = condition,
    values = values,
    needs.unique = needs.unique,
    allow.na = allow.na,
    list.depth = list.depth,
    min.listlength = rev(min.listlength),
    max.listlength = rev(max.listlength),
    min.veclength = min.veclength,
    max.veclength = max.veclength,
    formatstring = formatstring,
    extracons = extracons)
}

# Use the intermediate representation created by `parseConditionCheck` to create
# language expressions that check for the conditions.
# @param parsed [list] the output of `parseConditionCheck`
# @param varname [character(1)] name of the variable being handled
# @return [language] an expression that checks that the condition is fulfilled.
compileConditionCheck = function(parsed, varname) {

  minlen = parsed$min.veclength
  maxlen = parsed$max.veclength

  # For classes that are not special modes / atomics, the innermost list length
  # is treated as vector length.
  if (parsed$class %nin% c("logical", "integer", "numeric", "character", "list",
    "value", "NULL")) {
    checkmate::assertNull(parsed$min.veclength)
    checkmate::assertNull(parsed$max.veclength)
    if (parsed$list.depth > 0) {
      parsed$list.depth %-=% 1

      minlen = parsed$min.listlength[1]
      parsed$min.listlength = parsed$min.listlength[-1]

      maxlen = parsed$max.listlength[1]
      parsed$max.listlength = parsed$max.listlength[-1]
    }
  }

  varquote = asQuoted(varname)

  testquote = NULL

  # loop up the list.depth. Each level is checked in the form
  #   checkmate::testList(varname, <requirements>) &&
  #     all(sapply(varname, function(varname) { ... }))
  # where the ... nest down to check the next list level. The innermost
  # list level is checked according to the actual type and vector length
  # given (e.g. `numeric(3)`).
  repeat {
    # The innermost level is created first and then wrapped by the outer
    # (list)checks.
    newquote = createCheckmateTest(parsed$class, varquote,
      needs.unique = parsed$list.depth == 0 && parsed$needs.unique,
      allow.na = parsed$allow.na,
      minlen = minlen, maxlen = maxlen,
      values = parsed$values)
    if (is.null(testquote)) {
      testquote = newquote
    } else {
      apfun = substitute(function(x) y, list(y = testquote))
      names(apfun[[2]]) = varname
      testquote = substitute(a && all(sapply(v, b)), list(
          a = newquote, b = apfun, v = varquote))
    }
    if (parsed$list.depth == 0) {
      break
    }

    # to recurse up, treat any class as `list` now, and
    # pop the next level of list length into the vector length
    # variables.
    parsed$list.depth %-=% 1

    minlen = parsed$min.listlength[1]
    parsed$min.listlength = parsed$min.listlength[-1]

    maxlen = parsed$max.listlength[1]
    parsed$max.listlength = parsed$max.listlength[-1]

    parsed$allow.na = FALSE
    parsed$class = "list"
  }
  testquote
}

# Create a `checkmate::xxx` call according to given specifications.
# @param class [character(1)] the class / mode name to check
# @param varquote [symbol] the symbol representing the variable to check
# @param needs.unique [logical(1)] whether to check uniqueness
# @param allow.na [logical(1)] whether to allow missing values
# @param minlen [numeric(1) | NULL] minimum length to allow
# @param maxlen [numeric(1) | NULL] maximum length to allow
# @return [language] an expression calling `checkmate::testXXX` and otherwise
#   checking conformance of `varquote` to the requirements.
createCheckmateTest = function(class, varquote, needs.unique, allow.na,
  minlen, maxlen, values) {
  addtype = FALSE
  otherclass = FALSE
  cfun = switch(class,
    logical = quote(checkmate::testLogical()),
    integer = quote(checkmate::testNumeric()),
    numeric = quote(checkmate::testNumeric()),
    character = quote(checkmate::testCharacter()),
    list = quote(checkmate::testList()),
    value = {allow.na = TRUE ; quote(checkmate::testAtomic())},
    NULL = quote(checkmate::testNull()),
    if (!is.null(minlen) || !is.null(maxlen)) {
      addtype = TRUE
      quote(checkmate::testList())
    } else {
      otherclass = TRUE
      quote(checkmate::testClass())
    })

  cfun[[2]] = varquote
  if (class == "NULL" || otherclass) {
    checkmate::assertFALSE(needs.unique)
    checkmate::assertFALSE(allow.na)
    checkmate::assertNull(minlen)
    checkmate::assertNull(maxlen)
    if (otherclass) {
      cfun[["classes"]] = class
    }
    return(cfun)
  }
  if (addtype) {
    cfun[["types"]] = class
  }
  if (!is.null(minlen) && minlen != 0) {
    cfun[["min.len"]] = minlen
  }
  if (!is.null(maxlen) && maxlen != Inf) {
    cfun[["max.len"]] = maxlen
  }
  cfun[["any.missing"]] = allow.na
  cfun[["unique"]] = needs.unique
  if (class == "integer") {
    cfun = substitute(x && all(y == round(y), na.rm = TRUE),
      list(x = cfun, y = varquote))
  }
  if (class == "value") {
    cfun = substitute(x && checkmate::testSubset(y, z),
      list(x = cfun, y = varquote, z = values))
  }
  cfun
}

