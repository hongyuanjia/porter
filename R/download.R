download_src_sdl2 <- function(version = "latest", dir = tempdir()) {
    version <- assert_version(version)

    repo <- "libsdl-org/SDL"
    if (version == "latest") {
        rel <- github_release_latest(repo)
    } else {
        rel <- github_release(repo, tag = paste0("release-", version))
    }

    item <- rel$assets[rel$assets == sprintf("SDL2-%s.zip", rel$name), ]
    item <- as.list(item)

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    dest <- normalizePath(file.path(tempdir(), item$name), mustWork = FALSE)
    download_file(item$browser_download_url, dest)

    attr(dest, "version") <- rel$name
    dest
}
