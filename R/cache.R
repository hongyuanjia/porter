# a simple in-memory cache implemented using lists
# inspired by the {cachem} package
#' @include utils.R
cache_new <- function(n = getOption("porter.cache_num", 5L), age = "1 hour", persist = TRUE) {
    stopifnot(is_flag(persist) || is_string(persist))

    # store the cache as a temporary file
    if (is_flag(persist)) {
        fcache <- if (persist) normalizePath(file.path(tempdir(), "porter-cache"), mustWork = FALSE)
    } else {
        fcache <- normalizePath(persist, mustWork = FALSE)
    }

    if (!is_count(n)) {
        stop(sprintf("'n' should be a positive integer. Invalid input found '%s'.", deparse(n)))
    }

    if (is.null(age)) {
        age <- Inf
    } else if (is_number(age)) {
        if (age <= 0) {
            stop(spaste(
                "When 'age' is a number, it should be positive.",
                "Invalid input found: '%s'", .v = age
            ))
        }
    } else if (is_string(age)) {
        re <- "^\\s*(?<num>[0-9.]+)\\s*(?<unit>sec|min|hour|day)[s]?\\s*"
        age_spec <- reg_match(age, re, n = 1:2)[[1L]][1L, ]
        num <- as.numeric(age_spec["num"])
        unit <- age_spec["unit"]

        if (is.na(num) || is.na(unit)) {
            stop(spaste(
                "When 'age' is a string, it should be in format 'X (sec|min|hour|day)s.",
                "Invalid input found: '%s'", .v = age
            ))
        }

        age <- as.difftime(num, units = paste0(unit, "s"))
        units(age) <- "secs"
        age <- as.double(age)
    } else {
        stop(spaste(
            "'age' should be 'NULL', a positive number or a string.",
            "Invalid input found: '%s'", .v = deparse(age)
        ))
    }

    # directly read stored cache
    if (!is.null(fcache) && file.exists(fcache)) {
        cache <- readRDS(fcache)
    } else {
        cache <- vector("list", n)
    }

    if (!is.null(fcache)) {
        reg.finalizer(environment(), function(e) e$save(fcache), onexit = TRUE)
    }

    save <- local({
        has_ref_obj <- FALSE
        function(file) {
            saveRDS(cache, file, refhook = function(x) { has_ref_obj <<- TRUE; NULL })
            if (has_ref_obj) {
                warning(spaste(
                    "Reference objects found in the cache.",
                    "The retored object may not work as expected."
                ))
            }
        }
    })

    curtime <- function() as.double(Sys.time())

    is_key <- function(key) is_string(key) && nchar(trimws(key))

    avail <- function() {
        # if empty, use the first
        if (is.null(names(cache))) return(seq_along(cache))
        ind <- which(is.na(names(cache)) | names(cache) == "")
        if (length(ind)) ind else NULL
    }

    slot <- function() {
        ind <- avail()
        if (is.null(ind)) ind else ind[[1L]]
    }

    prune <- function() {
        ind <- c()

        if (is.null(names(cache))) return(FALSE)

        for (i in seq_along(cache)) {
            if (!is.null(cache[[i]]) && as.numeric(Sys.time()) - cache[[i]]$time > age) {
                ind <- c(ind, i)
            }
        }

        if (res <- length(ind) > 0L) {
            cache[ind] <<- list(NULL)
            names(cache)[ind] <<- NA_character_
        }

        res
    }

    get <- function(key) {
        stopifnot(is_key(key))

        # remove items that exceed the specified age
        prune()

        # empty cache
        if (is.null(names(cache))) return()

        ind <- match(key, names(cache))
        if (is.na(ind)) NULL else cache[[ind]]$value
    }

    set <- function(key, value) {
        stopifnot(is_key(key))

        # remove items that exceed the specified age
        prune()

        # if already exists, replace
        if (!is.na(ind <- match(key, names(cache)))) {
            cache[[ind]] <<- list(value = value, time = curtime())
        } else {
            # get available index
            ind <- slot()

            # check if cache is fully occupies
            if (is.null(ind)) {
                # remove the first item and append the new one
                cache <<- c(cache[-1L], list(value = value, time = curtime()))
                names(cache)[n] <<- key
            } else {
                # insert at 'ind'
                cache[[ind]] <<- list(value = value, time = curtime())
                names(cache)[ind] <<- key
            }
        }

        # save the cache
        if (!is.null(fcache)) save(fcache)
        invisible(TRUE)
    }

    keys <- function() {
        if (is.null(names(cache))) return(NULL)
        res <- names(cache)[!is.na(names(cache))]
        if (length(res)) res else NULL
    }

    exists <- function(key) {
        stopifnot(is_key(key))

        # remove items that exceed the specified age
        prune()

        key %in% names(cache)
    }

    remove <- function(key) {
        stopifnot(is_key(key))

        # remove items that exceed the specified age
        prune()

        ind <- match(key, names(cache))
        if (res <- !is.na(ind)) {
            cache[ind] <<- list(NULL)
            names(cache)[ind] <<- NA_character_
        }
        res
    }

    reset <- function() cache <<- vector("list", n)

    destroy <- function() {
        reset()
        if (is.null(fcache)) return(TRUE)

        res <- unlink(fcache)
        fcache <<- NULL
        res == 0L
    }

    size <- function() {
        # remove items that exceed the specified age
        prune()
        length(setdiff(seq_along(cache), avail()))
    }

    structure(
        list(
            get = get,
            set = set,
            exists = exists,
            keys = keys,
            remove = remove,
            reset = reset,
            prune = prune,
            size = size,
            avail = avail,
            destroy = destroy
        ),
        class = "porter_cache"
    )
}
