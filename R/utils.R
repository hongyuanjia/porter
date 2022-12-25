if (getRversion() < "3.2.0") {
    trimws <- function(x) {
        sub("[ \t\r\n]+$", sub("[ \t\r\n]+$", x, perl = TRUE), perl = TRUE)
    }

    lengths <- function(x) {
        sapply(x, length, USE.NAMES = FALSE)
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

    len <- lengths(res)
    if (length(llen <- len[has_len <- len > 0L]) && length(unique(llen)) > 1L) {
        warning(spaste(
            "The following elements from JSON do not have the same length: [%s]",
            spaste("length(%s)=%s", .v = list(nms[has_len], llen), .rcoll = ", ")
        ))
    }

    names(res) <- nms
    res
}

get_bin_paths <- function(path = getOption("porter.castxml.path")) {
    sys <- if (is_windows()) {
        Sys.getenv("APPDATA")
    } else if (is_linux()) {
        "~/.local/share"
    } else if (is_macos()) {
        "~/Library/Application Support"
    } else {
        stop("Current platform is unsupported.")
    }

    if (!is.null(path) && is.na(path)) path <- NULL

    paths <- c(path, sys)
    path.expand(paths[paths != ""])
}

exec_ext <- function(x) if (is_windows()) paste0(x, ".exe") else x

download_file <- function(url, destfile, desc = NULL) {
    # init return state
    ret <- NULL

    # skip download if possible
    skip <- FALSE
    if (.global$cache$exists(url)) {
        dl <- .global$cache$get(url)
        if (skip <- (file.exists(dl$path) && tools::md5sum(dl$path) == dl$md5)) {
            ret <- 0L
            if (normalizePath(dl$path) != normalizePath(destfile)) {
                file.copy(dl$path, destfile)
            }
        }
    }

    if (is.null(ret)) {
        ret <- try(utils::download.file(url, destfile, mode = "wb"), silent = TRUE)
    }

    if (inherits(ret, "try-error")) {
        stop(spaste(
            "Failed to download %s.\n%s",
            .v = list(
                if (is.null(desc)) {
                    sprintf("'%s'", basename(url))
                } else {
                    stopifnot(is_string(desc))
                    desc
                },
                conditionMessage(attr(ret, "condition"))
            )
        ))
    } else if (ret != 0L) {
        stop(spaste(
            "Failed to download %s from URL '%s'",
            .v = list(
                if (is.null(desc)) {
                    sprintf("'%s'", basename(url))
                } else {
                    stopifnot(is_string(desc))
                    desc
                },
                url
            )
        ))
    }

    # cache the downloaded file
    if (!skip) {
        val <- list(path = destfile, md5 = unname(tools::md5sum(destfile)))
        .global$cache$set(url, val)
    }

    ret
}

dir_copy <- function(from, to) {
    stopifnot(is_string(from), is_string(to))

    if (!dir.exists(to)) dir.create(to, recursive = TRUE)

    files <- list.files(from, full.names = TRUE)
    status <- file.copy(files, to, recursive = TRUE)
    success <- all(status)

    if (!success) {
        failed <- files[!status]
        attr(success, "failed") <- normalizePath(failed, mustWork = FALSE)
    }

    success
}

# support tar.gz
file_ext <- function(x) {
    pos <- regexpr("[.]([[:alnum:]]+|tar[.](gz|bz2|xz))$", x)
    if (pos > -1L) substring(x, pos + 1L) else ""
}

read_url <- function(url) {
    if (!inherits(url, "url")) {
        key <- url
    } else {
        # a hack to extract the URL from a connection
        key <- reg_match(
            capture.output(print(url))[2L],
            '\"(?<key>[^"]+)\"$', 1L
        )["key"]
    }
    # use previous results if possible
    if (.global$cache$exists(key)) return(.global$cache$get(key))

    res <- read_utf8(url)

    # store current query results
    .global$cache$set(key, res)

    res
}

from_json <- function(json, verbose = FALSE) {
    stopifnot(is.character(json))
    stopifnot(is_flag(verbose))

    if (!length(json)) return(NULL)

    if (length(json) > 1L) json <- paste0(json, collapse = "\n")

    chars <- strsplit(json, "", fixed = TRUE)[[1L]]
    len <- length(chars)

    # init index
    i <- 1L

    # for error printing
    call <- sys.call()
    lnum <- 1L
    lnum_i <- 1L
    new_line <- FALSE

    # current character
    char <- .subset2(chars, i)
    # indicate whether current element is valid
    valid <- TRUE

    error <- function(desc = NULL, offset = 10L, depth = 1L) {
        ctx <- sub("parse_", "", deparse(sys.call(-depth)[[1L]]))

        offset <- min(lnum_i, offset)
        line <- substring(json, i - offset + 1L, i)
        arrow <- "(right here) ------^"
        pad <- nchar(arrow) - nchar(line)
        if (pad > 0) {
            line <- paste0(strrep(" ", pad), line)
        } else {
            arrow <- paste0(strrep(" ", -pad), arrow)
        }
        if (nchar(arrow) < 30) {
            line <- paste0(strrep(" ", 30 - nchar(arrow)), line)
            arrow <- paste0(strrep(" ", 30 - nchar(arrow)), arrow)
        }

        msg <- spaste(
            "Failed to parse JSON %s. %sPossible malformed position at line %i:\n",
            "%s\n",
            "%s",
            .v = list(
                ctx,
                if (is.null(desc)) sprintf("Incomplete %s data. ", ctx) else paste0(desc, " "),
                lnum,
                line,
                arrow
            )
        )
        stop(simpleError(msg, call))
    }

    # check if current element is out of bound
    check <- function() {
        if (i > len) valid <<- FALSE
        valid
    }

    # get the character at current position
    read_char <- function() if (check()) char <<- .subset2(chars, i) else NULL
    # read_char <- function() if (check()) char <<- .subset2(chars, i) else NULL

    # move position
    forward <- function(step = 1L) {
        i <<- i + step
        lnum_i <<- lnum_i + 1L
    }
    backward <- function(step = 1L) {
        i <<- i - step
        lnum_i <<- lnum_i - 1L
    }

    # move position, check out-of-bound and get the character after specified
    # step
    # if i is out-of-bound and strict is TRUE, directly return NULL
    pop <- function(step = 1L) {
        forward(step)
        if (!check()) error(depth = 2L)
        read_char()
    }

    peek <- function(step = 1L) {
        forward(step)
        if (!check()) error(depth = 2L)
        char <- read_char()
        backward(step)
        read_char()
        char
    }

    # skip white spaces
    skip_whitespace <- function() {
        while (char == " " || char == "\t" || char == "\r" || (new_line <<- char == "\n")) {
            if (new_line) {
                lnum <<- lnum + 1L
                lnum_i <<- 0L
                new_line <<- FALSE
            }
            forward(1L)
            if (!check()) error(depth = 2L)
            read_char()
        }
    }

    parse_value <- function() {
        if (!check()) error()

        skip_whitespace()

        if (!check()) error()

        if (char == "{") {
            parse_object()
        } else if (char == "[") {
            parse_array()
        } else if (char == '"') {
            parse_string()
        } else if (char %in% c("-", CHAR09)) {
            parse_number()
        } else if (char == "t") {
            parse_true()
        } else if (char == "f") {
            parse_false()
        } else if (char == "n") {
            parse_null()
        } else {
            error("Unexpected data found.")
        }
    }

    parse_object <- function() {
        if (char != "{") error("No object openning tag ('{').")

        obj <- vector("list")

        pop(1L)
        skip_whitespace()

        # empty list
        if (char == "}") {
            # only pop if this is not the last character
            if (i != len) pop(1L)
            return(list())
        }

        while (TRUE) {
            skip_whitespace()

            # empty object
            if (char == "}") break

            # key
            key <- parse_string()

            if (char != ":") {
                error("Object key and value must be separated by a colon (':').")
            }

            pop(1L)
            skip_whitespace()

            # value
            val <- parse_value()

            obj[key] <- list(val)

            skip_whitespace()

            if (char == "}") {
                # only pop if this is not the last character
                if (i != len) pop(1L)
                break
            }

            if (char != ",") error("No object closing tag ('}').")
            pop(1L)
        }

        obj
    }

    parse_string <- function() {
        # current character should be a double quote
        if (char != '"') error('No double-quote for string (\'"\').')

        # move to the next character
        pop(1L)

        # get the current position
        str_start <- i

        while (TRUE) {
            # move until see '\' or '"'
            while (char != "\\" && char != '"') pop(1L)

            # if see '\', skip it
            # otherwise, it is the other '"' and parsing stops
            if (char == "\\") pop(2L) else break
        }

        # get the end position
        str_end <- i - 1L

        # move to the next character
        pop(1L)

        # in case there are "\r\n" in the string
        eval(parse(text = substring(json, str_start - 1L, str_end + 1L)))
    }

    CHAR19 <- c("1", "2", "3", "4", "5", "6", "7", "8", "9")
    CHAR09 <- c("0", CHAR19)
    parse_number <- function() {
        num_start <- i
        decimal <- FALSE

        if (char == "-") pop(1L)

        if (!char %in% CHAR09) {
            error("Invalid number specs found.")
        }

        # check if invalid number spec in '012'
        if (char == "0" && !is.null(char_next <- peek(1L)) && char_next %in% CHAR19) {
            error("Invalid number specs found. JSON did not allow numbers in format like '012'.")
        }

        while (char %in% CHAR09) pop(1L)

        if (char == ".") {
            decimal <- TRUE
            pop(1L)
            while (char %in% CHAR09) pop(1L)
        }

        if (char == "e" || char == "E") {
            pop(1L)
            if (char == "-" || char == "+") pop(1L)
            while (char %in% CHAR09) pop(1L)
        }

        num_end <- i - 1L
        num <- substring(json, num_start, num_end)
        if (decimal) as.double(num) else as.integer(num)
    }

    parse_true <- function() {
        if (paste0(char, peek(1L), peek(2L), peek(3L), collapse = "") != "true") {
            error()
        }
        pop(4L)
        TRUE
    }

    parse_false <- function() {
        if (paste0(char, peek(1L), peek(2L), peek(3L), peek(4L), collapse = "") != "false") {
            error()
        }
        pop(5L)
        FALSE
    }

    parse_null <- function() {
        if (paste0(char, peek(1L), peek(2L), peek(3L), collapse = "") != "null") {
            error()
        }
        pop(4L)
        NULL
    }

    parse_array <- function() {
        if (char != "[") error("No array openning tag ('[').")
        arr <- list()
        flat <- TRUE

        pop(1L)
        skip_whitespace()

        # empty array
        if (char == "]") {
            # only pop if this is not the last character
            if (i != len) pop(1L)
            return(list())
        }

        while (TRUE) {
            val <- parse_value()
            arr[[length(arr) + 1L]] <- val
            if (is.list(val) || length(val) > 1L || is.null(val)) {
                flat <- FALSE
            }

            skip_whitespace()
            if (char == "]") {
                # only pop if this is not the last character
                if (i != len) pop(1L)
                break
            }
            if (char != ",") error("No array closing tag ('[').")
            pop(1L)
        }

        if (flat) arr <- unlist(arr)

        arr
    }

    parse_value()
}

transpose <- function(lst) {
    if (length(unique(lengths(lst))) != 1L) {
        stop("'lst' must contain lists with the same length.")
    }

    first <- .subset2(lst, 1L)
    nms <- names(first)

    res <- lapply(nms, function(name) {
        if (is.null(elem <- .subset2(first, name)) || is.list(elem)) {
            lapply(lst, .subset2, name)
        } else {
            vapply(lst, .subset2, elem, name)
        }
    })
    names(res) <- nms
    res
}
