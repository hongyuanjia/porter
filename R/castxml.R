download_zlib_dll <- function(dir) {
    stopifnot(is_string(dir))

    url <- if (is_windows())
        "https://github.com/rust-lang/rust/files/6550164/zlib1__.zip"

    if (is.null(url)) return()

    zlib_zip <- file.path(tempdir(), "zlib.zip")
    download_file(url, zlib_zip)
    unzip(zlib_zip, exdir = dir)
}

#' @export
locate_castxml <- function(patch = FALSE) {
    dir <- normalizePath(file.path(get_bin_paths(), "castxml", "bin"), mustWork = FALSE)

    if (!dir.exists(dir)) return(NULL)

    if (!file.exists(castxml <- file.path(dir, exec_ext("castxml")))) {
        message(spaste(
            "CastXML installation found at '%s'.",
            "But failed to locate CastXML executable.",
            "You may try to reinstall CastXML using 'install_castxml()'."
        ))
    }

    res <- system2(castxml, "--version", stdout = TRUE, stderr = TRUE)

    if (!is.null(status <- attr(res, "status")) && status != 0L) {
        message(spaste(
            "CastXML installation found at '%s'.",
            "But failed to detect CastXML version.",
        ))

        if (status == 309L) {
            if (!patch) {
                message(spaste(
                    "Command 'castxml --version' returns an error code of '309'.",
                    "This may be caused by a missing dependency of 'zlib' shared library",
                    "in the same directory where CastXML is. Details can be found at",
                    "'https://github.com/CastXML/CastXMLSuperbuild/issues/50'.",
                    "You may try to set 'patch' to 'TRUE' to attempt to fix this",
                    "or install another version using 'install_castxml()'."
                ))

                names(dir) <- NA_character_
                return(dir)
            }

            download_zlib_dll(dir)
        }

        # rerun
        res <- system2(castxml, "--version", stdout = TRUE, stderr = TRUE)

        if (!is.null(status <- attr(res, "status")) && status != 0L) {
            message(spaste("Still failed to detect CastXML version."))

            if (status == 309L) {
                message(spaste(
                    "Command 'castxml --version' still returns an error code of '309'.",
                    "This indicates that it needs other dependencies other than 'zlib'.",
                    "You may try to install another version using 'install_castxml()'."
                ))
            }

            message("You may try to install another version using 'install_castxml()'.")
            names(dir) <- NA_character_
            return(dir)
        }
    }

    ver <- reg_match(res[[1L]], "\\d[.]\\d[.]\\d")[[1L]]

    names(dir) <- ver
    dir
}

#' @export
install_castxml <- function(version = "latest", force = FALSE) {
    # just to check if version is a string or a single number
    version <- assert_version(version)

    # check if 'version' is a file path
    if (local_file <- grepl("[.](zip|tar[.]gz)$", version) && file.exists(version)) {
        release <- normalizePath(version)
    } else {
        release <- download_castxml(version, dir = tempdir())

        # print the latest version
        if (version == "latest") {
            message(sprintf("The latest CastXML version is 'v%s'.", attr(release, "version")))
        }

        # get the actual version
        version <- attr(release, "version")

        # check if the version has already been installed
        if (!is.null(castxml <- locate_castxml(version)) && !force) {
            message(spaste(
                "CastXML v%s has already been installed at '%s'.",
                "To reinstall, please set 'force' to 'TRUE'.",
                .v = list(version, dirname(castxml))
            ))
            return(FALSE)
        }
    }

    switch(
        file_ext(release),
        # in case short paths are used
        zip = normalizePath(utils::unzip(release, exdir = tempdir()), "/"),
        tar.gz = utils::untar(release, exdir = tempdir())
    )

    if (!dir.exists(reldir <- file.path(tempdir(), "castxml"))) {
        stop(spaste(
            if (local_file) {
                "Failed to install CastXML"
            } else {
                sprintf("Failed to install CastXML v%s", version)
            },
            "as the expected directory of unzipped release files did not exist: '%s'",
            .v = normalizePath(reldir, mustWork = FALSE)
        ))
    }

    if (!file.exists(castxml <- file.path(reldir, "bin", exec_ext("castxml")))) {
        stop(spaste(
            if (local_file) {
                "Failed to install CastXML"
            } else {
                sprintf("Failed to install CastXML v%s", version)
            },
            "as the expected executable under the unzipped directory",
            "did not exist: '%s'", .v = castxml
        ))
    }

    # fix zlib dependency issue for v0.4.8
    if (local_file) {
        v <- system2(castxml, "--version", stdout = TRUE, stderr = TRUE)
        if (!is.null(attr(v, "status")) && attr(v, "status") == 309L) {
            message(spaste(
                "Command 'castxml --version' returns an error code of '309'.",
                "This may be caused by a missing dependency of 'zlib' shared library",
                "in the same directory where CastXML is. Details can be found at",
                "'https://github.com/CastXML/CastXMLSuperbuild/issues/50'.",
                if (is_windows()) {
                    "Will try to download the 'zlib1__.dll' file."
                } else {
                    "CastXML may fail to run on current platform."
                }
            ))

            download_zlib_dll(file.path(reldir, "bin"))
        }
    } else if (numeric_version(version) == "0.4.8" && is_windows()) {
        message(spaste(
            "CastXML v0.4.8 needs an extra dependency of 'zlib' shared library",
            "named 'zlib1__.dll'. There is a known issue. Details can be found at",
            "'https://github.com/CastXML/CastXMLSuperbuild/issues/50'.",
            "Will try to download it."
        ))

        download_zlib_dll(file.path(reldir, "bin"))
    }

    # TODO: detect if version mismatches

    # copy files
    dest <- normalizePath(file.path(get_bin_paths(), "castxml"), mustWork = FALSE)
    status <- dir_copy(reldir, dest)
    if (!status) {
        stop(spaste(
            if (local_file) {
                "Failed to install CastXML"
            } else {
                sprintf("Failed to install CastXML v%s", version)
            },
            "to '%s' as failed to copy the following files/directories: [%s]",
            .v = list(
                dest,
                spaste("'%s'", attr(status, "failed"), .rcoll = ", ")
            )
        ))
    }

    message(sprintf("CastXML has been successfully installed to '%s'", dest))

    status
}

#' @export
download_castxml <- function(version = "latest", dir = tempdir()) {
    res <- list_castxml_rel_files(version)
    if (!length(res$`_id`)) {
        stop(sprintf(
            "Failed to find the download link for CastXML v%s.",
            attr(res, "version")
        ))
    }

    if (length(res$`_id`) > 1L) {
        if (!interactive()) {
            ind <- utils::menu(
                choices = res$name,
                title = spaste(
                    "There are multiple CastXML release files for v%s.",
                    "Please choose one to download:",
                    .v = attr(res, "version")
                )
            )
            res <- lapply(res, .subset2, ind)
        } else {
            stop(spaste(
                "There are multiple CastXML release files for v%s.",
                "Failed to determine which one to download: [%s].",
                "Please rerun in an interactive R session and choose which one to download",
                .v = list(
                    attr(res, "version"),
                    spaste("'%s'", .v = res$name, .rcoll = ", ")
                )
            ))
        }
    }

    url <- sprintf("https://data.kitware.com/api/v1/item/%s/download#/%s", res$`_id`, res$name)

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    dest <- normalizePath(file.path(dir, res$name), mustWork = FALSE)
    download_file(url, dest)

    attr(dest, "version") <- attr(res, "version")
    dest
}

list_castxml_rel_files <- function(version = "latest", all = FALSE) {
    allvers <- get_castxml_vers()
    version <- assert_version(version, names(allvers))
    # get the corresponding folder id
    id <- allvers[names(allvers) == version]

    # get all items in that folder
    res <- kitware_api(type = "item", folderId = unname(id), sort = "lowerName", sortdir = -1)
    res <- get_json_elem(res, "_id", "name", "size")
    attr(res, "version") <- version

    if (all) return(res)

    # support arm since v0.4.8
    if (numeric_version(version) < "0.4.8") {
        if (is_windows()) {
            ind <- grep("castxml-windows", res$name)
        } else if (is_linux()) {
            ind <- grep("castxml-macosx", res$name)
        } else if (is_macos()) {
            ind <- grep("castxml-linux", res$name)
        } else {
            stop("Current platform is unsupported.")
        }
    } else {
        if (is_windows()) {
            ind <- grep("^castxml-windows", res$name)

            # check if there is a specific Windows 11 version
            if (length(ind) > 1L && any(has_win11 <- grepl("-win11", res$name[ind], fixed = TRUE))) {
                # check if current machine is running Windows 11
                if (startsWith(Sys.info()[["release"]], "11")) {
                    ind <- ind[has_win11]
                } else {
                    ind <- ind[!has_win11]
                }
            }
        } else if (is_linux()) {
            ind <- grep("^castxml-linux", res$name)
            if (length(ind) > 1L && any(has_aarch64 <- grepl("-aarch64", res$name[ind], fixed = TRUE))) {
                if (Sys.info()["machine"] == "aarch64") {
                    ind <- ind[has_aarch64]
                } else {
                    ind <- ind[!has_aarch64]
                }
            }
        } else if (is_macos()) {
            ind <- grep("^castxml-macos", res$name)
            if (length(ind) > 1L && any(has_arm <- grepl("-arm", res$name[ind], fixed = TRUE))) {
                if (Sys.info()["machine"] == "arm64") {
                    ind <- ind[has_arm]
                } else {
                    ind <- ind[!has_arm]
                }
            }
        } else {
            stop("Current platform is unsupported.")
        }
    }

    res <- lapply(res, .subset, i = ind)
    attr(res, "version") <- version
    res
}

# Ref: https://github.com/ScoopInstaller/Main/blob/master/bucket/castxml.json
get_castxml_vers <- function() {
    res <- kitware_api(
        type = "folder",
        parentType = "folder",
        parentId = "57b5de948d777f10f2696370",
        sort = "lowerName",
        sortdir = -1
    )

    # folder names as versions
    res <- get_json_elem(res, "_id", "name")

    # just in case
    if (length(unique(lengths(res))) != 1L) {
        stop("Internal error when fetching CastXML releases.")
    }

    # exclude nightly
    is_normver <- startsWith(res$name, "v")
    ids <- res$`_id`[is_normver]
    vers <- substr(res$name[is_normver], 2L, nchar(res$name))

    names(ids) <- vers
    ids
}

kitware_api <- function(type, ...) {
    stopifnot(is_string(type))

    baseurl <- "https://data.kitware.com/api/v1"
    url <- file.path(baseurl, type)

    params <- list(...)
    if (length(params)) {
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
        url <- sprintf("%s?%s", url, params)
    }

    # use previous results
    if (.global$cache$exists(url)) return(.global$cache$get(url))

    res <- read_utf8(url)

    # store current query results
    .global$cache$set(url, res)

    res
}
