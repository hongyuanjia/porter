#' Locate a CastXML executable
#'
#' @description
#' `locate_castxml()` finds a CastXML executable installed by the user. porter
#' does not download or install CastXML; install it with your system package
#' manager before calling [port()].
#'
#' @details
#' When `path` is supplied, only that executable or directory is checked. With
#' the default `NULL` path, the search order is:
#'
#' - `Sys.which("castxml")`.
#' - Common package-manager locations that may not be on `PATH`, including
#'   Homebrew, Linuxbrew, Scoop, Chocolatey, and conda prefixes.
#'
#' Package-manager installations are usually found by `Sys.which("castxml")`
#' when their shims or binary directories are on `PATH`.
#'
#' @param path Optional path to either the CastXML executable or a directory
#'   containing it. Defaults to `getOption("porter.castxml")`.
#'
#' @return A single named character vector. The value is the normalized path to
#'   the CastXML executable, and the name is the detected version. Returns `NULL`
#'   if no usable CastXML executable is found.
#'
#' @examplesIf !is.null(porter::locate_castxml())
#' locate_castxml()
#' @export
locate_castxml <- function(path = getOption("porter.castxml")) {
    candidates <- if (is.null(path) || length(path) == 0L || is.na(path[[1L]])) {
        c(unname(Sys.which("castxml")), castxml_known_paths())
    } else {
        normalize_castxml_candidate(path)
    }
    candidates <- unique(candidates)
    candidates <- candidates[nzchar(candidates) & !is.na(candidates)]

    for (candidate in candidates) {
        exe <- normalize_castxml_candidate(candidate)
        if (!nzchar(exe) || !file.exists(exe)) next

        version <- castxml_version(exe)
        if (!is.na(version)) {
            exe <- normalizePath(exe, mustWork = TRUE)
            names(exe) <- version
            return(exe)
        }
    }

    message(paste(
        "Failed to locate a usable CastXML executable.",
        "Install CastXML with your system package manager or set",
        "options(porter.castxml = '/path/to/castxml')."
    ))
    NULL
}

normalize_castxml_candidate <- function(path) {
    if (is.null(path) || length(path) == 0L || is.na(path[[1L]])) return(character())
    if (!is_string(path)) stop("Argument 'path' should be a single string.")

    path <- path.expand(path)
    if (dir.exists(path)) file.path(path, exec_ext("castxml")) else path
}

castxml_version <- function(exe) {
    res <- try(system3(exe, "--version", .capture = TRUE), silent = TRUE)
    if (inherits(res, "try-error")) return(NA_character_)
    if (!is.null(status <- attr(res, "status")) && status != 0L) return(NA_character_)
    if (!length(res)) return(NA_character_)

    version <- reg_match(res[[1L]], "\\d[.]\\d[.]\\d")
    if (!length(version) || is.na(version[[1L]])) NA_character_ else version[[1L]]
}

castxml_known_paths <- function() {
    path_from <- function(base, ...) {
        if (!nzchar(base)) return(character())
        file.path(base, ...)
    }

    candidates <- if (is_macos()) {
        c(
            "/opt/homebrew/bin/castxml",
            "/usr/local/bin/castxml",
            "/opt/local/bin/castxml",
            path_from(Sys.getenv("CONDA_PREFIX", ""), "bin", "castxml")
        )
    } else if (is_linux()) {
        c(
            "/usr/bin/castxml",
            "/usr/local/bin/castxml",
            "/snap/bin/castxml",
            "~/.linuxbrew/bin/castxml",
            "/home/linuxbrew/.linuxbrew/bin/castxml",
            path_from(Sys.getenv("CONDA_PREFIX", ""), "bin", "castxml")
        )
    } else if (is_windows()) {
        c(
            path_from(Sys.getenv("SCOOP", ""), "shims", "castxml.exe"),
            path_from(Sys.getenv("USERPROFILE", ""), "scoop", "shims", "castxml.exe"),
            "C:/ProgramData/scoop/shims/castxml.exe",
            "C:/ProgramData/chocolatey/bin/castxml.exe",
            path_from(Sys.getenv("ProgramData", ""), "scoop", "shims", "castxml.exe"),
            path_from(Sys.getenv("ProgramData", ""), "chocolatey", "bin", "castxml.exe"),
            path_from(Sys.getenv("CONDA_PREFIX", ""), "Library", "bin", "castxml.exe"),
            path_from(Sys.getenv("CONDA_PREFIX", ""), "Scripts", "castxml.exe")
        )
    } else {
        character()
    }

    path.expand(candidates[nzchar(candidates) & !is.na(candidates)])
}
