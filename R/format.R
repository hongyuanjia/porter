#' Print summary of an `dynport` object
#'
#' @param x A `dynport`.
#' @param n The number of items to print
#'
#' @return `x` invisibly.
#'
#' @export
print.dynport <- function(x, n = 5L, ...) {
    stopifnot(is_count(n))

    if (is.null(x$name)) {
        cat("Package: <Unknown>\n")
    } else {
        cat(format_field("Package", x$name), "\n")
    }

    if (is.null(x$version)) {
        cat("Version: <Unknown>\n")
    } else {
        cat(format_field("Version", x$version), "\n")
    }

    print_field <- function(field, name) {
        if (is.null(x[[name]])) {
            cat(name, ": <None>\n")
        } else {
            val <- x[[name]][seq_len(min(nrow(x[[name]]), n)), ]
            str <- switch(name,
                func    = format_sig_func(val),
                funcptr = format_sig_func(val),
                struct  = format_sig_struct(val),
                union   = format_sig_union(val),
                enum    = format_sig_enum(val)
            )
            cat(format_field(field, str, TRUE), "\n")

            if ((left <- nrow(x[[name]]) - n) > 0L) {
                cat("    ... [# Truncated with ", left, " more item", ngettext(left, "", "s"), "]\n", sep = "")
            }
        }
    }

    print_field("Function", "func")
    print_field("FuncPtr", "funcptr")
    print_field("Struct", "struct")
    print_field("Union", "union")
    print_field("Enum", "enum")

    invisible(x)
}

format_field <- function(name, field, empty = FALSE, sep = "\n") {
    if (!length(field)) {
        if (empty) paste0(name, ": ") else ""
    } else if (length(field) == 1L) {
        if (is.null(nms <- names(field))) {
            paste0(name, ": ", field)
        } else {
            paste0(name, "/", nms, ":\n", paste0("    ", field, collapse = sep))
        }
    } else {
        if (is.null(nms <- names(field))) {
            paste0(name, ":\n", paste0("    ", field, collapse = sep))
        } else {
            paste0(name, "/", nms, ":\n",
                cmap(field, function(fld) paste0("    ", fld, collapse = sep)),
                collapse = "\n"
            )
        }
    }
}


format_sig_func <- function(func) {
    if (is.null(func)) return("")

    sprintf("%s(%s)%s%s;",
        # function name
        func$name,

        # function arguments in dyncall format
        cmap(func$arguments, function(args) {
            if (!length(args$type)) return("")
            spaste(cmap(args$type, dyncall_sig, FALSE), .fcoll = "")
        }),

        # function returns in dyncall format
        cmap(func$returns, dyncall_sig, TRUE),

        # function argument names
        cmap(func$arguments, function(args) {
            if (!length(args$name)) return("")
            paste0(" ", spaste(unlist(args$name), .rcoll = ""))
        })
    )
}

format_sig_struct <- function(struct) {
    format_sig_struct_union(struct)
}

format_sig_union <- function(union) {
    format_sig_struct_union(union)
}

format_sig_struct_union <- function(struct) {
    if (is.null(struct)) return("")

    sprintf("%s{%s}%s;",
        # struct name
        struct$name,

        # struct member types in dyncall format
        cmap(struct$members, function(mem) {
            if (!length(mem$type)) return("")
            spaste(cmap(mem$type, dyncall_sig, FALSE), .fcoll = "")
        }),

        # struct member names
        cmap(struct$members, function(mem) {
            if (!length(mem$name)) return("")
            spaste(mem$name, .fcoll = " ")
        })
    )
}

format_sig_enum <- function(enum) {
    if (is.null(enum)) return("")

    vals <- enum$values
    names(vals) <- enum$name
    lapply(vals, function(val) {
        sprintf("%s=%s", val$name, as.integer(val$init))
    })
}
