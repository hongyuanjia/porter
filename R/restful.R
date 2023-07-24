# GitHub Docs: The latest release is the most recent non-prerelease, non-draft
# release.
# Ref: https://docs.github.com/en/rest/releases/releases?apiVersion=2022-11-28#get-the-latest-release
github_release_latest <- function(repo) {
    stopifnot(is_string(repo))
    res <- github_api(sprintf("repos/%s/releases/latest", repo))
    extract_release_data(res)
}

github_release <- function(repo, tag) {
    stopifnot(is_string(repo))
    stopifnot(is_string(tag))
    res <- github_api(sprintf("repos/%s/releases/tags/%s", repo, tag))
    extract_release_data(res)
}

# GitHub Docs: Information about published releases are available to everyone.
# Only users with push access will receive listings for draft releases.
# Ref: https://docs.github.com/en/rest/releases/releases?apiVersion=2022-11-28#list-releases
github_releases <- function(repo, pattern = "v?[0-9.]+", limit = NULL) {
    stopifnot(is_string(repo))
    stopifnot(is_string(pattern))
    stopifnot(is.null(limit) || is_count(limit))

    # do pagination
    # Ref: https://github.com/yihui/xfun/blob/main/R/github.R
    i <- 1L
    per_page <- if (!is.null(limit)) min(100L, limit) else 100L
    rel <- list()
    repeat {
        res <- github_api(sprintf("repos/%s/releases", repo), per_page = per_page, page = i)
        if (!length(res)) break
        rel <- c(rel, res)
        if (!is.null(limit) && length(rel) >= limit) break
        if (length(res) < per_page) break  # not enough items for the next page
        i <- i + 1L
    }

    if (!length(res)) return(list())
    list2DF(transpose(lapply(rel, extract_release_data)))
}

extract_release_data <- function(release) {
    out <- list()
    out <- release[c("name", "tag_name", "draft", "prerelease", "html_url", "tarball_url", "zipball_url")]

    if (!length(release$assets)) {
        out$assets <- list()
    } else {
        assets <- transpose(release$assets)[c(
            "name", "content_type", "size", "created_at", "updated_at",
            "browser_download_url"
        )]

        out$assets <- as_df(assets)
    }

    out
}

github_token <- function(token = NULL) {
    stopifnot(is.null(token) || is_string(token))

    if (!is.null(token)) {
        token <- trimws(token)
    } else {
        # environment variables to lookup
        token <- Sys.getenv(c("GITHUB_PAT", "GITHUB_TOKEN", "GH_TOKEN"))
        token <- token[token != ""]

        # use the first one found
        token <- if (length(token)) token[1L] else NULL
    }

    if (!nchar(token)) return(NULL)

    if (!grepl("^gh[pousr]_[A-Za-z0-9_]{36,251}$", token) &&
        !grepl("[[:xdigit:]]{40}", token)) {
        return(NULL)
    }

    token
}

# ref: https://github.com/yihui/xfun/blob/main/R/github.R
github_api <- function(endpoint, ..., token = NULL, headers = NULL) {
    token <- github_token(token)

    error <- TRUE
    on.exit(add = TRUE, if (error && is.null(token))
        message(spaste(
            "There is no GitHub personal token found.",
            "You may need to set one of the environment variables to bypass",
            "the access rate: [%s]",
            .v = spaste("'%s'", .v = c("GITHUB_PAT", "GITHUB_TOKEN", "GH_TOKEN"), .rcoll = ", ")
        ))
    )

    res <- rest_api("https://api.github.com", endpoint, ..., token = token, headers = headers)
    error <- FALSE
    res
}

kitware_api <- function(type, ..., headers = NULL) {
    rest_api("https://data.kitware.com/api/v1", type, ..., headers = headers)
}

rest_api <- function(api, endpoint, ..., token = NULL, headers = NULL) {
    stopifnot(is_string(api))
    stopifnot(is_string(endpoint))
    stopifnot(is.null(headers) || (is.character(headers) && is_named(headers)))

    # use https
    if (getRversion() < "3.2.0") utils::setInternet2()

    url <- paste0(file.path(api, endpoint), query_params(...))

    if (getRversion() < "3.6.0") {
        warning(spaste(
            "The 'url()' function from current R (v%s) does not support the 'headers' argument.",
            "All headers and access tokens will not be used.",
            "Consider to upgrade R to v3.6.0 and above."
        ))
        args <- list(description = url)
    } else {
        args <- list(
            description = url,
            headers = c(
                headers,
                if (!is.null(token <- github_token(token))) {
                    c("Authorization" = token)
                }
            )
        )
    }

    con <- do.call(base::url, args)
    on.exit(close(con), add = TRUE)

    from_json(read_url(con))
}

query_params <- function(...) {
    params <- list(...)
    if (!length(params)) return(NULL)

    if (is.null(names(params)) || "" %in% names(params)) {
        stop("All parameters should be named.")
    }

    if (!all(is_scalar <- lengths(params) == 1L)) {
        stop(spaste(
            "Every parameter should have a length of 1.",
            "But the following element(s) are not: [%s]",
            .v = which(!is_scalar), .vcoll = ", "
        ))
    }

    # always order parameters by name and memorize parameters to reduce API
    # rate
    params <- unlist(params[order(names(params))], use.names = TRUE)
    params <- spaste("%s=%s", .v = list(names(params), params), .rcoll = "&")

    paste0("?", params)
}
