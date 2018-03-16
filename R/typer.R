


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
#' A special `value` is `.`(dot), which indicates that no default
#' value is given for the function.
#'
#' Non-list mode class names can be suffixed with `(n)`,
#' where `n` is an integer indicating the required vector length.
#' Further possibilities are `(n, m)` to indicate length between
#' `n` and `m`. Leaving out `n` or `m` (`(, m)` or `(n, )`) only
#' sets a minimum or maximum length.
#'
#' The `(n)` suffix also works for `value(...)`: `value(1, 2)(1)`
#' specifies that a value can be `1` or `2`. Note that `value(1, 2)`
#' also allows `c(1, 2)` and `c(1, 1)`.
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
#'
#' @param env The environment to parse. Defaults to [parent.frame()].
#' @param ... `include` or `exclude`, given as [grep()] patterns.
#'   These are applied in the order given, so it is possible to
#'   first include certain functions, then exclude a subset of these,
#'   and then include a subset of these excluded functions etc.
#' @examples
#' ```
#' # this function takes a parameter that is either a vector of two integer
#' # numbers (either datatype integer or numeric), a vector of three numbers,
#' # or a named list of data.frame that all have more than two rows.
#' fun = function(a = .:integer(2) | numeric(3) |
#'     data.frame[] [[named, all(sapply(a, nrow) > 2)]]) {
#'   NULL
#' }
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

hassymbol = function(expr, symbol) {
  if (is.recursive(expr)) {
    any(vlapply(expr, hassymbol, symbol = symbol))
  } else {
    identical(expr, symbol)
  }
}

shortdeparse = function(expr) {
  dep = deparse(expr)
  if (length(dep) > 1) {
    paste0(deparse(expr)[1], "...")
  } else {
    dep
  }
}

alldeparse = function(expr) {
  collapse(deparse(expr), sep = "\n")
}


compileFunction = function(fun, name) {

  insert = quote({})
  originsert = insert
  formals.in = formals(fun)

  pipesep = quote(`|`)
  colonsep = quote(`:`)
  dotsymbol = quote(.)


  for (idx in seq_along(formals.in)) {
    if (identical(formals.in[[idx]], substitute())) {
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
      conditions = c(list(curformal[[3]]), conditions)
      curformal = curformal[[2]]
    }
    if (is.recursive(curformal) && identical(curformal[[1]], colonsep)) {
      founddots = TRUE
      conditions = c(list(curformal[[3]]), conditions)
      curformal = curformal[[2]]
    }
    if (identical(curformal, dotsymbol)) {
      founddots = TRUE
    }
    if (!founddots && hassymbol(curformal, colonsep)) {
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
      next
    }
    single.conds = lapply(conditions, singleConditionCheck,
      varname = varname, errprefix = errprefix)
    full.cond = Reduce(function(a, b) substitute(a || b, list(a = a, b = b)),
      extractSubList(single.conds, "fullexp"))
    full.string = sprintf("Argument '%s' must %s", varname,
      collapse(extractSubList(single.conds, "fullstring"),
        sep = sprintf("\n  or\n    '%s' must ", varname)))
    full.check = substitute(if (!a) stop(b),
      list(a = full.cond, b = full.string))
    insert[[length(insert) + 1]] = full.check
  }
  if (!identical(insert, originsert)) {
    body(fun) = substitute({a ; b}, list(a = insert, b = body(fun)))
  }
  fun
}

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


# errprefix [character(1)]: what to write in error messages for malformed
#   conditions
parseConditionCheck = function(condition, varname, errprefix) {
  fullcondition = condition
  varquote = asQuoted(varname)

  abort = function(msg) {
    stopf("%s\nCondition %s %s",
      errprefix, shortdeparse(fullcondition), msg)
  }
  abortf = function(...) {
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
  formatstring = alldeparse(condition)
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
  if (is.recursive(condition) && identical(condition[[1]], quote(value))) {
    values = as.list(condition)
    values[[1]] = NULL
    values = unlist(values, FALSE)
    if (!is.atomic(values)) {
      abortf("value list %s must be atomic.", shortdeparse(condition))
    }

    condition = condition[[1]]
  }
  if (is.null(condition)) {
    condition = "NULL"
  } else {
    if (!is.symbol(condition)) {
      abort("base must be a class, type name, or 'value'.")
    }
    condition = as.character(condition)
  }
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

compileConditionCheck = function(parsed, varname) {

  minlen = parsed$min.veclength
  maxlen = parsed$max.veclength

  if (parsed$class %nin% c("logical", "integer", "numeric", "character", "list",
    "value", "NULL")) {
    # treat listlength as veclength for non-atomics
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

  repeat {
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

