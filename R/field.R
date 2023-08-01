PORT_FIELDS <- c(
    "Package", "Version", "Library", "Function", "FuncPtr", "Enum", "Struct",
    "Union", "File"
)

#' Query and update `dynport` fields
#'
#' @param port A `dynport` object created using [port].
#'
#' @param fields A character vector of valid field names in input `dynport`
#'        object.
#'
#' @param ... Pairs of field names and values. See Details.
#'
#' @details
#' `port_fields()` lists the fields in the object.
#'
#' `port_has()` checks if one or multiple fields are present in the object.
#'
#' `port_get()` returns the current values of input fields in the object. If
#' multiple fields are given, a named list is returned. Note that `port_get()`
#' errors if non-existing fields are queried.
#'
#' `port_set()` updates one or multiple field values in the object. It accepts
#' two forms of arguments:
#'
#' - Two arguments with the first element being the name of a single field, and
#'   the second being the field value. Use `NULL` value to remove the field.
#' - Multiple named arguments with names being names of field to set, and values
#'   being the new values. Use `NULL` value to remove the field.
#'
#' For `port_set()`, you can use `NULL` to remove values of input fields. For
#' example, the following two lines of code both remove the value of the
#' `Version` field.
#' ```r
#' port_set(p, "Version", NULL)
#' port_set(p, Version = NULL)
#' ```
#' Note that, following fields are always required for a `dynport` object and
#' will be kept as `NULL` instead of removal: `Package`, `Version`, `Library`,
#' `Function`, `FuncPtr`, `Enum`, `Struct` and `Union`. Other fields are treated
#' as user customize fields and will be removed when setting to `NULL`.`
#'
#' @return
#' `port_fields()` returns a character vector.
#'
#' `port_has()` returns a logical vector.
#'
#' `port_get()` returns a character vector or a data.frame, depending on the
#' field value types.
#'
#' `port_set()` returns a new `dynport` object, invisibly.
#'
#' @rdname port_fields
#' @export
port_fields <- function(port) {
    cmap(port, .subset2, "key", use.names = FALSE)
}

#' @rdname port_fields
#' @export
port_has <- function(port, fields) {
    fields %in% cmap(port, .subset2, "key")
}

#' @rdname port_fields
#' @export
port_get <- function(port, fields) {
    if (!inherits(port, "dynport")) {
        stop("Argument 'port' should be a dynport object.")
    }

    if (!is.character(fields)) {
        stop("Argument 'fields' should be a character vector.")
    }

    if (anyNA(fields)) {
        stop("Argument 'fields' should not contain any missing values.")
    }

    if (!all(has <- port_has(port, fields))) {
        stop(spaste(
            "Argument 'fields' should be existing field names including [%s],",
            "but invalid ones found: [%s].", .vcoll = ", ",
            .v = list(sQuote(port_fields(port)), sQuote(fields[!has]))
        ))
    }
    out <- lapply(port[match(fields, port_fields(port), 0)], `[[`, "value")
    # directly return data if only one field requested
    if (length(fields) == 1L) out <- out[[1L]]
    out
}

#' @rdname port_fields
#' @export
port_set <- function(port, ...) {
    args <- list(...)

    if (is.null(names(args)) && length(args) == 2L) {
        keys <- args[[1L]]
        # make sure it is a list
        vals <- args[2]
    } else if (!is.null(names(args)) && all(names(args) != "")) {
        keys <- names(args)
        vals <- args
    } else {
        stop(paste(
            "Input should be two unnamed arguments,",
            "indicating field name and value or",
            "all named arguments giving the value for each field."
        ))
    }

    # check if any NULL
    isnull <- lmap(vals, is.null)
    if (any(isnull)) {
        # keep the required fields
        keep <- keys[isnull & keys %in% PORT_FIELDS]
        port[keep] <- port_create_fields(keep, vector("list", length(keep)), FALSE)

        # remove others
        port[setdiff(keys[isnull], keep)] <- NULL

        keys <- keys[!isnull]
        vals <- vals[!isnull]
    }

    if (length(keys)) {
        port[keys] <- port_create_fields(keys, vals, check = TRUE)
    }

    invisible(port)
}

port_create_fields <- function(keys, values, check = TRUE) {
    res <- mapply(port_create_field, key = keys, value = values, check = check, SIMPLIFY = FALSE)
    names(res) <- keys
    res
}

port_create_field <- function(key, value, check = TRUE) {
    fld <- structure(list(key = key, value = value), class = "dynportfield")
    if (check && key %in% PORT_FIELDS) {
        fld <- port_check_field(fld)
    }
    fld
}

#' @export
print.dynportfield <- function(x, n = 5L, ...) {
    if (x$key %in% c("Package", "Version", "Library")) {
        print_dynportfield_meta(x)
    } else if (x$key %in% c("Function", "FuncPtr", "Enum", "Struct", "Union")) {
        print_dynportfield_data(x, n)
    } else {
        print_dynportfield_meta(x)
    }

    invisible(x)
}

print_dynportfield_meta <- function(x, ...) {
    if (is.null(x$value)) {
        cat(x$key, ": <Unknown>\n", sep = "")
    } else {
        cat(format_dynportfield_meta(x), "\n")
    }
}

print_dynportfield_data <- function(x, n = 5L, ...) {
    if (is.null(x$value)) {
        cat(x$key, ": <None>\n", sep = "")
    } else {
        tmp <- x
        tmp$value <- x$value[seq_len(min(nrow(x$value), n)), ]
        cat(format_dynportfield_data(tmp), "\n")

        if ((left <- nrow(x$value) - n) > 0L) {
            cat("    ... [# Truncated with ", left, " more item", ngettext(left, "", "s"), "]\n", sep = "")
        }
    }
}

format.dynportfield <- function(x, raw = FALSE, ...) {
    if (x$key %in% c("Package", "Version", "Library")) {
        format_dynportfield_meta(x, raw)
    } else if (x$key %in% c("Function", "FuncPtr", "Enum", "Struct", "Union")) {
        format_dynportfield_data(x, raw)
    } else {
        format_dynportfield_meta(x, raw)
    }
}

format_dynportfield_meta <- function(x, raw = FALSE...) {
    if (is.null(x$value)) {
        ""
    } else if (raw) {
        x$value
    } else {
        format_field(x$key, x$value)
    }
}

format_dynportfield_data <- function(x, raw = FALSE...) {
    value <- switch(x$key,
        Function = format_sig_func(x$value),
        FuncPtr  = format_sig_func(x$value),
        Struct   = format_sig_struct(x$value),
        Union    = format_sig_union(x$value),
        Enum     = format_sig_enum(x$value)
    )
    if (raw) return(value)
    if (is_string(value) && value == "") value <- NULL
    format_field(x$key, value, empty = TRUE)
}

format_field <- function(key, value, empty = FALSE, show_key = TRUE, sep = "\n") {
    key <- if (show_key) paste0(key, if (length(value) > 2L) "/" else ":") else ""
    if (!length(value)) {
        if (empty) paste(key, "") else ""
    } else if (length(value) == 1L) {
        if (is.null(nms <- names(value))) {
            paste(key, value)
        } else {
            paste0(key, nms, ":\n", paste0("    ", value, collapse = sep))
        }
    } else {
        if (is.null(nms <- names(value))) {
            paste0(key, "\n", paste0("    ", value, collapse = sep))
        } else {
            paste0(key, nms, ":\n",
                cmap(value, function(fld) paste0("    ", fld, collapse = sep)),
                collapse = "\n"
            )
        }
    }
}
