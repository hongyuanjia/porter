get_src_sdl2 <- function(version = "2.28.0", force = FALSE) {
    d <- file.path(tempdir(), paste0("SDL2-", version))
    if (!force && dir.exists(d)) return(d)

    zipfile <- download_src_sdl2("2.28.0")
    unzip(zipfile, exdir = tempdir())
    file.path(tempdir(), paste0("SDL2-", version))
}
