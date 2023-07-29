check_package_name <- function(x) {
    if (!is_string(x)) stop("Package name should be a single string.")

    if (!grepl("^[a-zA-Z0-9\\.]*$", x)) {
        stop("Package name should only contain ASCII letters, numbers and dots.")
    }

    if (nchar(x) < 2L) {
        stop("Package name should be at least two character long.")
    }

    if (!grepl("^[a-zA-z]", x)) {
        stop("Package name should start with a letter.")
    }

    if (grepl("\\.$", x)) {
        stop("Package name should not end with a dot.")
    }

    x
}

check_package_version <- function(x) {
    if (is.numeric(x)) x <- as.character(x)

    if (!is_string(x)) stop("Package version should be a single string.")

    re <- "^[0-9]+[-\\.][0-9]+([-\\.][0-9]+)*$"
    if (!grepl(re, x)) {
        stop(paste(
            "Package version should be a sequence of at least two non-negative",
            "integers separated by a single dot '.' or dash '-'."
        ))
    }

    x
}
