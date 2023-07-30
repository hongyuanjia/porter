#' Manage CastXML installation
#'
#' @description
#' `download_castxml()` and `install_castxml()` download and install CastXML via the
#' [CastXML Superbuild](https://github.com/CastXML/CastXMLSuperbuild) project,
#' respectively. `locate_castxml()` finds the path of CastXML executable.
#'
#' @details
#' CastXML is a C-family abstract syntax tree XML output tool. It is maintained
#' by Kitware in support of ITK, the Insight Segmentation and Registration
#' Toolkit.
#'
#' `install_castxml()` downloads the pre-built binaries of CastXML together with
#' the LLVM/Clang dependencies via the
#' [CastXML Superbuild](https://github.com/CastXML/CastXMLSuperbuild) project.
#'
#' The location of the installed CastXML is OS-specific:
#'
#' - For Windows: `Sys.getenv("APPDATA")`
#' - For macOS: `~/Library/Application Support`
#' - For Linux: `~/.local/share`
#'
#' @note
#' Instead of using `install_castxml()`, you may directly use your package
#' manager to install CastXML, as it may already provide a `castxml` package.
#'
#' - On Windows, you can use [Scoop](https://scoop.sh/) to install CastXML via:
#'   `scoop install main/castxml"
#' - On macOS, you can use [Homebrew](https://brew.sh/) to install CastXML via:
#'   `brew install castxml"
#' - On Linux, you can check if your system package manager provides CastXML or
#'   not via [Repology](https://repology.org/project/castxml/versions).
#'
#' @param version A valid CastXML version. If `"latest"`, which is the default,
#'        the latest version is downloaded.
#'
#' @param force If `TRUE`, reinstall even there is already a CastXML
#'        installation in the same folder. Default: `FALSE`.
#'
#' @param dir A directory to save the downloaded CastXML pre-buit binaries. It
#'        will be created if not exists. Default: `tempdir()`.
#'
#' @param path A directory where the CastXML executable exists. Default is use
#'        the option `porter.castxml`. If empty, it will use the first value of:
#'
#' - Option `porter.castxml`
#' - Value of `dirname(Sys.which("castxml"))`
#' - Default installation directory for `install_castxml()`
#'     * For Windows: `Sys.getenv("APPDATA")/castxml`
#'     * For macOS: `~/Library/Application Support/castxml`
#'     * For Linux: `~/.local/share/castxml`
#'
#' @param patch Only works for CastXML v0.4.8 on Windows. The superbuild of that
#'        version has a missing dependency of `zlib` library on Windows. If
#'        `TRUE`, the `zlib` library will be downloaded into the CastXML
#'        installation directory. Default: `FALSE`.
#'
#' @return
#' - `download_castxml()` returns the full path of the downloaded CastXML
#'   binaries, with an attribute `version` being the actual downloaded version
#'   of CastXML.
#'
#' - `install_castxml()` returns either `TRUE` or `FALSE`.
#'
#' - `locate_castxml()` returns a single named character vector with the name
#'   being the CastXML version and value being the full path of CastXML
#'   executable, or `NULL` if no CastXML is found.
#'
#' @examples
#' \donotrun{
#' download_castxml("0.5.0", tempdir())
#'
#' install_castxml("latest")
#'
#' locate_castxml()
#' }
#' @rdname castxml
#' @export
install_castxml <- function(version = "latest", force = FALSE) {
    # just to check if version is a string or a single number
    version <- assert_version(version)
    stopifnot(is_flag(force))

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
        if (!is.null(castxml <- locate_castxml()) && !force) {
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
        v <- system3(castxml, "--version", .capture = TRUE)
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

    invisible(status)
}

#' @rdname castxml
#' @export
download_castxml <- function(version = "latest", dir = tempdir()) {
    stopifnot(is_string(dir))
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

#' @rdname castxml
#' @export
locate_castxml <- function(path = getOption("porter.castxml"), patch = FALSE) {
    if (!is.null(path)) {
        if (!is_string(path)) stop("Argument 'path' should be a single string.")
        if (!dir.exists(path)) {
            stop(spaste("Argument 'path' did not exists: '%s'.", path))
        }

        if (!file.exists(exe <- file.path(path, exec_ext("castxml")))) {
            message(spaste(
                "Input CastXML installation is '%s'.",
                "But failed to locate CastXML executable in it.",
                "You may try to reinstall CastXML using 'install_castxml()'.",
                .v = path
            ))
            return(NULL)
        }
    } else {
        # try 'porter.castxml' option
        if (!is.null(p <- getOption("porter.castxml"))) {
            if (dir.exists(p)) path <- p
        }

        # try system path
        if (is.null(path) && (p <- Sys.which("castxml")) != "") {
            path <- dirname(p)
        }

        # try local path
        if (is.null(path) && dir.exists(p <- file.path(get_bin_paths(), "castxml", "bin"))) {
            path <- p
        }

        # all failed
        if (is.null(path)) {
            message(paste(
                "Failed to locate CastXML installation.",
                "You may try to install CastXML using 'install_castxml()'."
            ))
            return(NULL)
        }

        if (!file.exists(exe <- file.path(path, exec_ext("castxml")))) {
            message(spaste(
                "CastXML installation found at '%s'.",
                "But failed to locate CastXML executable.",
                "You may try to reinstall CastXML using 'install_castxml()'.",
                .v = path
            ))
            return(NULL)
        }
    }

    res <- system3(exe, "--version", .capture = TRUE)
    if (!is.null(status <- attr(res, "status")) && status != 0L) {
        message(spaste(
            "CastXML installation found at '%s'.",
            "But failed to detect CastXML version.",
            .v = path
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

                names(path) <- NA_character_
                return(path)
            }

            download_zlib_dll(dir)

            # rerun
            res <- system3(exe, "--version", .capture = TRUE)

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

    }

    ver <- reg_match(res[[1L]], "\\d[.]\\d[.]\\d")

    res <- normalizePath(exe)
    names(res) <- ver
    res
}

download_zlib_dll <- function(dir) {
    stopifnot(is_string(dir))

    url <- if (is_windows())
        "https://github.com/rust-lang/rust/files/6550164/zlib1__.zip"

    if (is.null(url)) return()

    zlib_zip <- file.path(tempdir(), "zlib.zip")
    download_file(url, zlib_zip)
    unzip(zlib_zip, exdir = dir)
}

list_castxml_rel_files <- function(version = "latest", all = FALSE) {
    allvers <- get_castxml_vers()
    version <- assert_version(version, names(allvers))
    # get the corresponding folder id
    id <- allvers[names(allvers) == version]

    # get all items in that folder
    res <- kitware_api(type = "item", folderId = unname(id), sort = "lowerName", sortdir = -1)
    res <- lapply(res, .subset, c("_id", "name", "size"))

    # just in case
    if (length(unique(lengths(res))) != 1L) {
        stop("Internal error when fetching CastXML release files.")
    }

    res <- transpose(res)

    if (all) return(as_df(res, version = version))

    attr(res, "version") <- version
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
    res <- lapply(res, .subset, c("_id", "name"))

    # just in case
    if (length(unique(lengths(res))) != 1L) {
        stop("Internal error when fetching CastXML releases.")
    }

    ids <- vapply(res, .subset2, character(1L), "_id")
    nms <- vapply(res, .subset2, character(1L), "name")

    # exclude nightly
    is_normver <- startsWith(nms, "v")
    ids <- ids[is_normver]
    vers <- substr(nms[is_normver], 2L, nchar(nms[is_normver]))

    names(ids) <- vers
    ids
}
