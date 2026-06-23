if (getRversion() < "3.2.0") {
    trimws <- function(x) {
        sub("[ \t\r\n]+$", sub("[ \t\r\n]+$", x, perl = TRUE), perl = TRUE)
    }

    lengths <- function(x, use.names = TRUE) {
        sapply(x, length, USE.NAMES = use.names)
    }
}

if (getRversion() < "3.3.0") {
    startsWith <- function(x, prefix) {
        substr(x, 1L, nchar(prefix)) == prefix
    }
}

is_integerish <- function(x) {
    all(!is.na(x)) && (is.integer(x) || (is.double(x) && all(x == trunc(x))))
}

is_scalar  <- function(x) length(x) == 1L

is_count   <- function(x) is_scalar(x) && is_integerish(x) && x > 0L

is_number  <- function(x) is_scalar(x) && !is.na(x) && is.numeric(x)

is_string  <- function(x) is_scalar(x) && is.character(x) && !is.na(x)

is_named   <- function(x) !is.null(names(x)) && all(!is.na(names(x))) && all(names(x) != "")

is_flag    <- function(x) is_scalar(x) && is.logical(x) && !is.na(x)

is_windows <- function() .Platform$OS.type == "windows"

is_linux   <- function() Sys.info()["sysname"] == "Linux"

is_macos   <- function() Sys.info()["sysname"] == "Darwin"

lmap <- function(x, f, ..., use.names = TRUE) {
    vapply(x, f, logical(1), ..., USE.NAMES = use.names)
}

cmap <- function(x, f, ..., use.names = TRUE) {
    vapply(x, f, character(1), ..., USE.NAMES = use.names)
}

imap <- function(x, f, ..., use.names = TRUE) {
    vapply(x, f, integer(1), ..., USE.NAMES = use.names)
}

read_utf8 <- function(con) {
    old <- options(encoding = "native.enc")
    on.exit(options(old), add = TRUE)
    readLines(con, encoding = "UTF-8", warn = FALSE)
}

spaste <- function(..., .v = NULL, .fsep = " ", .fcoll = " ", .vcoll = NULL, .rcoll = NULL) {
    # create format specs
    fmt <- do.call(paste, c(list(...), sep = .fsep, collapse = .fcoll))

    # if no value is specified, directly return the format
    if (is.null(.v)) return(fmt)

    # convert input to list if not
    if (!is.list(.v)) .v <- list(.v)

    # collapse each value
    if (!is.null(.vcoll)) .v <- lapply(.v, paste0, collapse = .vcoll)

    # do sprintf
    res <- do.call(sprintf, c(fmt = fmt, .v))

    # collapse each result
    if (!is.null(.rcoll) && length(res) > 1L) res <- paste0(res, collapse = .rcoll)

    res
}

reg_match <- function(x, pattern, n = NULL, flat = TRUE) {
    stopifnot(is_string(pattern))
    stopifnot(is.null(n) || (is_integerish(n) && all(n > 0)))
    stopifnot(is_flag(flat))

    x <- as.character(x)

    match <- gregexpr(pattern, x, perl = TRUE)

    # number of groups
    ngrp <- length(attr(match[[1L]], "capture.names"))

    # stop if invalid capture group is specified
    if (!is.null(n) && any(invld_n <- ngrp < n)) {
        stop(spaste(
            if (ngrp) {
                "There are only %s capure groups in 'pattern'."
            } else {
                "There are %s capure group in 'pattern'."
            },
            "Invalid 'n' found: [%s].",
            .v = list(ngrp, n[invld_n]), .vcoll = ", "
        ))
    }

    # whether nothing is matched
    nomatch <- vapply(match, function(m) length(m) == 1L && m == -1L, logical(1L))

    # init results
    res <- replicate(length(x), list(), simplify = FALSE)

    if (any(nomatch)) {
        res[nomatch] <- list(NULL)
    }

    if (any(!nomatch)) {
        res[!nomatch] <- mapply(
            text = x[!nomatch], match = match[!nomatch], SIMPLIFY = FALSE,
            function(text, match) {
                whole <- substring(
                    text, match, match + attr(match, "match.length") - 1L
                )

                # directly return the whole match if n is NULL or no capture
                # groups specified
                if (is.null(n) || is.null(attr(match, "capture.start"))) return(whole)

                gstart <- attr(match, "capture.start")
                glength <- attr(match, "capture.length")
                gend <- gstart + glength - 1L

                groups <- substring(text, gstart, gend)
                dim(groups) <- dim(gstart)
                res <- cbind(whole, groups)
                colnames(res) <- c(".match", attr(match, "capture.name"))
                res
            }
        )
    }

    # only return whole match and specified groups
    if (!is.null(n)) res <- lapply(res, function(mat) mat[, c(1L, n + 1L), drop = FALSE])

    # only flat when every input has at least a match
    if (flat && !any(nomatch)) {
        # if all are a single row matrix, change to a character vector
        if (ngrp && all(vapply(res, nrow, integer(1)) == 1L)) {
            res <- lapply(res, function(x) {
                chr <- as.character(x)
                names(chr) <- colnames(x)
                chr
            })
        }

        # if input is a string, directly returns the results
        if (length(x) == 1L) res <- res[[1L]]
    }

    return(res)
}

assert_version <- function(ver, all_vers = ver) {
    if (!length(ver)) stop("Empty input of version specification found.")
    if (length(ver) > 1L) stop("Multiple inputs of version specification found.")

    if (is_string(ver)) {
        if (ver == "latest") {
            # 'all_vers' have been sorted already
            ver <- all_vers[[1L]]
        } else {
            ver <- trimws(ver)
            # remove [vV] prefix
            if (startsWith(ver, "v") || startsWith(ver, "V")) {
                ver <- substr(ver, 2L, nchar(ver))
            }

            if (is.na(m <- match(trimws(ver), all_vers))) {
                stop(spaste(
                    "Invalid version specification found: '%s'.",
                    "Should be one of the following: [%s].",
                    .v = list(ver, spaste("'%s'", .v = all_vers, .rcoll = ", "))
                ))
            }

            ver <- all_vers[m]
        }
    } else {
        numver <- try(numeric_version(ver), silent = TRUE)
        # cannot use 'match' nor '%in%' for numeric versions, but '==' works
        if (inherits(numver, "try-error") ||
            !any(m <- numeric_version(numver) == numeric_version(all_vers)))
        {
            stop(spaste(
                "Invalid version specification found: '%s'.",
                "Should be one of the following: ['%s'].",
                .v = list(ver, spaste("%s", .v = all_vers, .rcoll = ", "))
            ))
        }

        ver <- all_vers[m]
    }

    ver
}

get_json_elem <- function(rawjson, ...) {
    stopifnot(is_string(rawjson))

    nms <- c(...)
    stopifnot(length(nms) > 0L)

    res <- lapply(
        sprintf('"%s": (?<value>(?:"[^"]+")|(?:\\d+))', nms),
        function(reg) {
            m <- reg_match(rawjson, reg, n = 1L)[, "value"]
            if (all(!grepl("\"", m, fixed = TRUE))) {
                as.double(m)
            } else {
                gsub("\"", "", m, fixed = TRUE)
            }
        }
    )

    if (length(res) == 1L) return(res[[1L]])

    len <- lengths(res, FALSE)
    if (length(llen <- len[has_len <- len > 0L]) && length(unique(llen)) > 1L) {
        warning(spaste(
            "The following elements from JSON do not have the same length: [%s]",
            spaste("length(%s)=%s", .v = list(nms[has_len], llen), .rcoll = ", ")
        ))
    }

    names(res) <- nms
    res
}

system3 <- function(command, ..., .capture = FALSE) {
    if (.capture) {
        system2(command, args = c(...), stdout = TRUE, stderr = TRUE)
    } else {
        system2(command, args = c(...))
    }
}

exec_ext <- function(x) if (is_windows()) paste0(x, ".exe") else x

file_ext <- function(x) {
    pos <- regexpr("[.]([[:alnum:]]+|tar[.](gz|bz2|xz))$", x)
    if (pos > -1L) substring(x, pos + 1L) else ""
}

transpose <- function(lst, flatten = TRUE) {
    if (length(unique.default(lengths(lst, FALSE))) != 1L) {
        stop("'lst' must contain lists with the same length.")
    }

    first <- .subset2(lst, 1L)
    nms <- names(first)

    res <- lapply(nms, function(name) {
        elem <- .subset2(first, name)
        if (!flatten || (!length(elem) || is.list(elem))) {
            lapply(lst, .subset2, name)
        } else {
            vapply(lst, .subset2, elem, name)
        }
    })
    names(res) <- nms
    res
}

as_df <- function(x, ...) {
    stopifnot(is.list(x))
    if (!length(x)) {
        structure(list(), names = character(), row.names = integer(),
            ..., class = "data.frame"
        )
    } else {
        # Using NULL for the value resets the row names to seq_len(nrow(x)),
        # regarded as 'automatic'.
        structure(x, names = names(x), row.names = c(NA_integer_, -length(x[[1L]])),
            ..., class = "data.frame"
        )
    }
}

df <- function(...) as_df(list(...))

subset_list <- function(x, subset, select, drop = TRUE, ...) {
    if (length(unique.default(lengths(x, FALSE))) != 1L) {
        stop("Every element in input list must have the same length.")
    }

    if (!missing(subset)) {
        r <- eval(substitute(subset), x, parent.frame())
        if (!is.logical(r)) stop("'subset' must be logical.")
        x <- lapply(x, .subset, r & !is.na(r))
    }

    if (!missing(select)) {
        nl <- as.list(seq_along(x))
        names(nl) <- names(x)
        x <- .subset(x, eval(substitute(select), nl, parent.frame()))
    }

    if (drop && length(x) == 1L) x <- x[[1L]]

    x
}

is_parent_dir <- function(dir, path) substr(path, 1, nchar(dir)) == dir
