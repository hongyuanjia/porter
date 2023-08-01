port_check_field <- function(x, ...) {
    utils::getFromNamespace(
        paste0("port_check_field_", tolower(x$key)),
        ns = asNamespace("porter")
    )(x, ...)
}

port_check_field_package <- function(x, ...) {
    if (!is_string(x$value)) stop("'Package' field should be a single string.")

    if (!grepl("^[a-zA-Z0-9\\.]*$", x$value)) {
        stop("'Package' field should only contain ASCII letters, numbers and dots.")
    }

    if (nchar(x$value) < 2L) {
        stop("'Package' field should be at least two character long.")
    }

    if (!grepl("^[a-zA-z]", x$value)) {
        stop("'Package' field should start with a letter.")
    }

    if (grepl("\\.$", x$value)) {
        stop("'Package' field should not end with a dot.")
    }

    x
}

port_check_field_version <- function(x, ...) {
    if (is.numeric(x$value)) x$value <- as.character(x$value)

    if (!is_string(x$value)) stop("'Version' field should be a single string.")

    re <- "^[0-9]+[-\\.][0-9]+([-\\.][0-9]+)*$"
    if (!grepl(re, x$value)) {
        stop(paste(
            "'Version' field should be a sequence of at least two non-negative",
            "integers separated by a single dot '.' or dash '-'."
        ))
    }

    x
}

port_check_field_library <- function(x, ...) {
    if (!is.character(x$value) || anyNA(x$value)) {
        stop("'Library' field should be a character vector without any missing values.")
    }
    x
}

port_check_field_df <- function(x, key, cols) {
    if (!is.data.frame(x$value)) {
        stop(sprintf("'%s' field should be a data.frame.", key))
    }

    if (is.null(names(cols))) {
        types <- NULL
    } else {
        types <- cols
        cols <- names(cols)
    }

    if (!all(cols %in% colnames(x$value))) {
        stop(spaste(
            "'%s' field should contain at least the following columns: [%s].",
            .v = list(key, sQuote(cols)), .vcoll = ", "
        ))
    }

    if (!is.null(types)) {
        mapply(function(name, type) {
            if (type == "") return()
            if (type == "chr" && (!is.character(x$value[[name]]) || anyNA(x$value[[name]]))) {
                stop(spaste(
                    "'%s' column in '%s' field should be a character column",
                    "without any missing values.",
                    .v = list(name, key)
                ))
            # NOTE:it is possible there are missing values for 'size' and 'align'
            } else if (type == "int" && !is.integer(x$value[[name]])) {
                stop(spaste(
                    "'%s' column in '%s' field should be an integer column.",
                    .v = list(name, key)
                ))
            } else if (type == "lgl" && (!is.logical(x$value[[name]]) || anyNA(x$value[[name]]))) {
                stop(spaste(
                    "'%s' column in '%s' field should be a logical column",
                    "without any missing values.",
                    .v = list(name, key)
                ))
            } else if (type == "type" &&
                (!is.list(x$value[[name]]) || any(!imap(x$value[[name]], inherits, "dynporttype")))
            ) {
                stop(spaste(
                    "'%s' column in '%s' field should be a list column of 'dynporttype'.",
                    .v = list(name, key)
                ))
            } else if (type == "types" &&
                any(
                    !imap(x$value[[name]], is.null) &
                    !imap(x$value[[name]], function(df) {
                        is.data.frame(df) &&
                        identical(names(df), "type") &&
                        all(imap(df$type, inherits, "dynporttype"))
                    })
                )
            ) {
                stop(spaste(
                    "'%s' column in '%s' field should be a list of data.frames with",
                    "one column ['type' (list of 'dynporttype')].",
                    .v = list(name, key)
                ))
            } else if (type == "name_type" &&
                any(
                    !imap(x$value[[name]], is.null) &
                    !imap(x$value[[name]], function(df) {
                        is.data.frame(df) &&
                        identical(names(df), c("name", "type")) &&
                        is.character(df$name) && !anyNA(df$name) &&
                        all(imap(df$type, inherits, "dynporttype"))
                    })
                )
            ) {
                stop(spaste(
                    "'%s' column in '%s' field should be a list of data.frames with",
                    "two columns ['name' (character), 'type' (list of 'dynporttype')].",
                    .v = list(name, key)
                ))
            } else if (type == "name_init" &&
                any(
                    !imap(x$value[[name]], is.null) &
                    !imap(x$value[[name]], function(df) {
                        is.data.frame(df) &&
                        identical(names(df), c("name", "init")) &&
                        is.character(df$name) && !anyNA(df$name) &&
                        is.integer(df$init) && !anyNA(df$init)
                    })
                )
            ) {
                stop(spaste(
                    "'%s' column in '%s' field should be a data.frame with",
                    "two columns ['name' (character), 'init' (integer)].",
                    .v = list(name, key)
                ))
            }
        }, name = cols, type = types)
    }

    x$value <- x$value[, cols]
    x
}

port_check_field_function <- function(x, ...) {
    port_check_field_df(x, "Function",
        c("name" = "chr", "returns" = "type", "arguments" = "name_type", "ellipsis" = "lgl")
    )
}
port_check_field_funcptr <- function(x, ...) {
    port_check_field_df(x, "FuncPtr",
        c("name" = "chr", "returns" = "type", "arguments" = "types")
    )
}
port_check_field_struct <- function(x, ...) {
    port_check_field_df(x, "Struct",
        c("name" = "chr", "members" = "name_type", "size" = "int", "align" = "int")
    )
}
port_check_field_union <- function(x, ...) {
    port_check_field_df(x, "Union",
        c("name" = "chr", "members" = "name_type", "size" = "int", "align" = "int")
    )
}
port_check_field_enum <- function(x, ...) {
    port_check_field_df(x, "Enum",
        c("name" = "chr", "values" = "name_init", "size" = "int", "align" = "int")
    )
}
