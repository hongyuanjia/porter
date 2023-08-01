BASETYPE_MAP <- c(
    "B" = "_Bool",
    "c" = "char",
    "C" = "unsigned char",
    "s" = "short int",
    "S" = "short unsigned int",
    "i" = "int",
    "I" = "unsigned int",
    "j" = "long int",
    "J" = "long unsigned int",
    "l" = "long long int",
    "L" = "long long unsigned int",
    "f" = "float",
    "d" = "double",
    "p" = "pointer",
    "Z" = "string",
    "v" = "void"
)

dyncall_sig <- function(type, empty = FALSE) {
    len <- length(type$kind)

    # no return
    if (len == 0L) return(if (empty) "v" else NA_character_) # void

    # If ends up with base type, then directly convert
    if (type$kind[len] == "base") {
        # the simplest case
        if (len == 1L) {
            bt <- names(BASETYPE_MAP)[match(type$type, BASETYPE_MAP)]
            if (is.na(bt)) {
                stop(spaste("Internal error: unsupported base type found ('%s').", .v = type$type))
            }
            return(bt)
        }

        # pointer of a base type
        if (len == 2L) {
            bt <- names(BASETYPE_MAP)[match(type$type[[len]], BASETYPE_MAP)]
            if (is.na(bt)) {
                stop(spaste("Internal error: unknown base type found ('%s').", .v = type$type[[len]]))
            }

            # *void
            if (type$type[[1L]] == "*") {
                if (bt == "v") return("p") else return(paste0("*", bt))
            # const int, etc.
            } else if (type$type[[1L]] == "const") {
                return(bt)
            } else if (type$type[[1L]] == "[]") {
                return("p")
            }
        }

        tp <- sort(type$type)

        if (len == 3L) {
            # const char *
            if (identical(tp, c("*", "char", "const"))) {
                return("Z") # string
            } else if (tp[[1L]] == "*") {
                return("p") # pointer
            }
        }

        # const char **, return as pointer
        if (len == 4L && tp[[1L]] == "*") return("p") # pointer
    }

    if (!(type$kind[len] %in% c("enum", "struct", "union", "function"))) {
        stop(spaste(
            "Internal error: unsupported signature found ('%s'['%s'])",
            .v = list(type$type, type$kind), .vcoll = " "
        ))
    }

    # NOTE: only support function pointer
    if (type$kind[len] == "function") {
        if (any(!type$type[-len] %in% c("*", "[]", "const"))) {
            stop(spaste(
                "Internal error: found 'function'",
                "with an invalid qualifier ('%s'['%s']).",
                .v = list(type$type[-len], type$kind[-len]), .vcoll = " "
            ))
        }
        # directly return pointer type
        return("p")
    }

    # handle enums/structs/unions
    if (len == 1L) {
        type$type
    } else if (len == 2L) {
        if (type$type[[1L]] == "const") {
            type$type[[len]]
        } else if (type$type[[1L]] %in% c("*", "[]")) {
            paste0("*", type$type[[len]])
        } else {
            stop(spaste(
                "Internal error: found enum/struct/union",
                "with an invalid qualifier ('%s'['%s']).",
                .v = list(type$type[-len], type$kind[-len]), .vcoll = " "
            ))
        }
    } else if (len == 3L) {
        if (identical(sort(type$type[-len]), c("*", "const"))) {
            paste0("*", type$type[len])
        } else if (identical(type$type[-len], c("*", "*"))) {
            "p"
        } else {
            stop(spaste(
                "Internal error: found enum/struct/union",
                "with an invalid qualifier ('%s'['%s']).",
                .v = list(type$type[-len], type$kind[-len]), .vcoll = " "
            ))
        }
    } else {
        stop(spaste(
            "Internal error: failed to detect signature ('%s')",
            .v = list(type$type, type$kind), .vcoll = " "
        ))
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

format_sig_funcptr <- format_sig_func

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
        sprintf("%s=%s", val$name, val$init)
    })
}
