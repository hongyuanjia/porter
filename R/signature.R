BASETYPE_MAP <- c(
    "B" = "_Bool",
    "B" = "bool",
    "c" = "char",
    "c" = "signed char",
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

valid_dynport_name <- function(name) {
    !is.na(name) & nzchar(name) & name == make.names(name)
}

format_type_chain <- function(type) {
    paste(sprintf("%s:%s", type$kind, type$type), collapse = " -> ")
}

signature_type_references <- function(sig) {
    # Typed pointers may refer to opaque C tags that are not valid R object
    # names, such as *_SDL_Joystick or *__sFILE. These are safe as pointer-only
    # references, so only validate by-value references in the remaining text.
    sig <- gsub("\\*<[^>]+>", "", sig, perl = TRUE)
    refs <- regmatches(sig, gregexpr("<[^>]+>", sig, perl = TRUE))[[1L]]
    if (!length(refs) || identical(refs, character(0))) return(character())
    unique(sub("^<(.+)>$", "\\1", refs))
}

signature_references_valid_names <- function(sig) {
    refs <- signature_type_references(sig)
    !length(refs) || all(valid_dynport_name(refs))
}

drop_array_types <- function(type) {
    keep <- type$kind != "array"
    type$type <- type$type[keep]
    type$kind <- type$kind[keep]
    type
}

array_suffix <- function(type) {
    paste0(type$type[type$kind == "array"], collapse = "")
}

dyncall_sig <- function(type, empty = FALSE, array = FALSE) {
    len <- length(type$kind)

    # no return
    if (len == 0L) return(if (empty) "v" else NA_character_) # void

    if (any(type$kind == "array")) {
        if (!array || any(type$kind == "pointer")) return("p")
        return(paste0(dyncall_sig(drop_array_types(type), empty, FALSE), array_suffix(type)))
    }

    pointer_count <- sum(type$kind == "pointer")

    # If ends up with base type, then directly convert
    if (type$kind[len] == "base") {
        bt <- names(BASETYPE_MAP)[match(type$type[[len]], BASETYPE_MAP)]
        if (is.na(bt)) {
            stop(spaste("Internal error: unsupported base type found ('%s').", .v = type$type[[len]]))
        }

        # the simplest case
        if (len == 1L) return(bt)

        if (pointer_count >= 2L) return("p")

        # pointer of a base type
        if (pointer_count == 1L) {
            # *void
            if (bt == "v") return("p")
            # const char *
            if (bt == "c" && "const" %in% type$type) return("Z")
            if (len == 2L && type$type[[1L]] == "*") return(paste0("*", bt))
            return("p")
        }

        # const int, etc.
        if (all(type$type[-len] == "const")) return(bt)
    }

    if (!(type$kind[len] %in% c("enum", "struct", "union", "function"))) {
        stop(spaste(
            "Internal error: unsupported signature found ('%s'['%s'])",
            .v = list(type$type, type$kind), .vcoll = " "
        ))
    }

    # NOTE: only support function pointer
    if (type$kind[len] == "function") {
        if (any(!type$kind[-len] %in% c("pointer", "array", "cvqualified"))) {
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
    target <- type$type[[len]]
    target_name <- sub("^<(.*)>$", "\\1", target)
    target_valid <- valid_dynport_name(target_name)
    if (pointer_count >= 2L) return("p")
    if (pointer_count == 1L) {
        if (!nzchar(target_name)) return("p")
        if (all(type$kind[-len] %in% c("pointer", "cvqualified"))) return(paste0("*", target))
    }
    if (!target_valid) {
        stop(spaste(
            "Internal error: found enum/struct/union with an invalid type name ('%s').",
            .v = target
        ))
    }

    if (len == 1L) {
        target
    } else if (len == 2L) {
        if (type$type[[1L]] == "const") {
            target
        } else if (type$type[[1L]] == "*" || type$kind[[1L]] == "array") {
            paste0("*", target)
        } else {
            stop(spaste(
                "Internal error: found enum/struct/union",
                "with an invalid qualifier ('%s'['%s']).",
                .v = list(type$type[-len], type$kind[-len]), .vcoll = " "
            ))
        }
    } else if (len == 3L) {
        if (identical(sort(type$type[-len]), c("*", "const"))) {
            paste0("*", target)
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
            .v = format_type_chain(type)
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

format_sig_constant <- function(constant) {
    if (is.null(constant)) return("")
    sprintf("%s=%s", constant$name, constant$value)
}

format_sig_struct <- function(struct) {
    format_sig_struct_union(struct, "struct")
}

format_sig_union <- function(union) {
    format_sig_struct_union(union, "union")
}

format_sig_struct_union <- function(struct, kind = c("struct", "union")) {
    if (is.null(struct)) return("")
    kind <- match.arg(kind)

    cmap(seq_len(nrow(struct)), function(i) {
        mem <- struct$members[[i]]
        sig <- if (!length(mem$type)) {
            ""
        } else {
            spaste(cmap(mem$type, dyncall_sig, FALSE, array = TRUE), .fcoll = "")
        }

        fields <- format_member_names(mem)
        layout <- infer_layout_directives(kind, struct$size[[i]], struct$align[[i]], mem)
        if (length(layout)) fields <- c(fields, layout)

        sprintf("%s{%s}%s;", struct$name[[i]], sig, spaste(fields, .fcoll = " "))
    })
}

format_member_names <- function(mem) {
    if (!length(mem$name)) return("")
    nms <- mem$name
    if ("bits" %in% names(mem)) {
        has_bits <- !is.na(mem$bits)
        nms[has_bits] <- paste0(nms[has_bits], ":", mem$bits[has_bits])
    }
    nms
}

SIG_SIZE_ALIGN <- list(
    B = c(size = 1L, align = 1L),
    c = c(size = 1L, align = 1L),
    C = c(size = 1L, align = 1L),
    s = c(size = 2L, align = 2L),
    S = c(size = 2L, align = 2L),
    i = c(size = 4L, align = 4L),
    I = c(size = 4L, align = 4L),
    j = c(size = .Machine$sizeof.long, align = .Machine$sizeof.long),
    J = c(size = .Machine$sizeof.long, align = .Machine$sizeof.long),
    l = c(size = 8L, align = 8L),
    L = c(size = 8L, align = 8L),
    f = c(size = 4L, align = 4L),
    d = c(size = 8L, align = 8L),
    p = c(size = .Machine$sizeof.pointer, align = .Machine$sizeof.pointer),
    Z = c(size = .Machine$sizeof.pointer, align = .Machine$sizeof.pointer)
)

signature_type_info <- function(sig) {
    if (!is_string(sig) || sig == "") return(NULL)

    dims <- regmatches(sig, gregexpr("\\[[0-9]+\\]", sig, perl = TRUE))[[1L]]
    array_len <- 1L
    if (length(dims)) {
        len <- as.integer(gsub("[^0-9]", "", dims))
        array_len <- prod(len)
        sig <- sub("(\\[[0-9]+\\])+$", "", sig, perl = TRUE)
    }

    if (startsWith(sig, "*")) sig <- "p"
    if (startsWith(sig, "<")) return(NULL)

    info <- SIG_SIZE_ALIGN[[sig]]
    if (is.null(info)) return(NULL)
    c(size = as.integer(info[["size"]] * array_len), align = as.integer(info[["align"]]))
}

member_has_unsupported_signature <- function(mem) {
    if (!length(mem$type)) return(FALSE)
    any(vapply(mem$type, function(type) {
        sig <- tryCatch(dyncall_sig(type, FALSE, array = TRUE), error = function(e) NA_character_)
        is.na(sig) || !signature_references_valid_names(sig)
    }, logical(1)))
}

align_offset <- function(offset, alignment) {
    if (is.na(alignment) || alignment <= 0L) return(offset)
    as.integer(as.integer((offset + alignment - 1L) / alignment) * alignment)
}

is_power_of_two <- function(x) {
    is_integerish(x) && length(x) == 1L && !is.na(x) && x > 0L &&
        bitwAnd(as.integer(x), as.integer(x) - 1L) == 0L
}

infer_layout <- function(kind, size, align, mem) {
    unknown <- list(status = "unknown", directives = character())
    if (!length(mem$type) || !"offset" %in% names(mem) || anyNA(mem$offset)) return(unknown)
    if ("bits" %in% names(mem) && any(!is.na(mem$bits))) return(unknown)
    if (is.na(size) || is.na(align) || !is_power_of_two(align)) return(unknown)

    sigs <- vapply(mem$type, function(type) {
        tryCatch(dyncall_sig(type, FALSE, array = TRUE), error = function(e) NA_character_)
    }, character(1))
    if (anyNA(sigs)) return(list(status = "unsupported", directives = character()))
    infos <- lapply(sigs, signature_type_info)
    if (any(vapply(infos, is.null, logical(1)))) return(unknown)
    sizes <- vapply(infos, function(x) x[["size"]], integer(1))
    aligns <- vapply(infos, function(x) x[["align"]], integer(1))

    packs <- c(NA_integer_, 1L, 2L, 4L, 8L, 16L, 32L, 64L)
    for (pack in packs) {
        member_aligns <- if (is.na(pack)) aligns else pmin(aligns, pack)
        max_align <- max(member_aligns, 1L)
        align_directive <- if (align > max_align) align else NA_integer_
        final_align <- if (is.na(align_directive)) max_align else align_directive

        if (kind == "union") {
            offsets <- rep.int(0L, length(sizes))
            calc_size <- align_offset(max(sizes, 0L), final_align)
        } else {
            offset <- 0L
            offsets <- integer(length(sizes))
            for (i in seq_along(sizes)) {
                offset <- align_offset(offset, member_aligns[[i]])
                offsets[[i]] <- offset
                offset <- offset + sizes[[i]]
            }
            calc_size <- align_offset(offset, final_align)
        }

        if (identical(as.integer(offsets), as.integer(mem$offset)) && calc_size == size && final_align == align) {
            out <- character()
            if (!is.na(pack)) out <- c(out, if (pack == 1L) "@packed" else sprintf("@pack(%d)", pack))
            if (!is.na(align_directive)) out <- c(out, sprintf("@align(%d)", align_directive))
            return(list(status = "supported", directives = out))
        }
    }

    list(status = "unsupported", directives = character())
}

infer_layout_directives <- function(kind, size, align, mem) {
    infer_layout(kind, size, align, mem)$directives
}

is_unsupported_layout <- function(kind, size, align, mem) {
    identical(infer_layout(kind, size, align, mem)$status, "unsupported")
}

format_sig_enum <- function(enum) {
    if (is.null(enum)) return("")

    vals <- enum$values
    names(vals) <- enum$name
    lapply(vals, function(val) {
        sprintf("%s=%s", val$name, val$init)
    })
}
