test_that("port_write() works", {
    skip_on_cran()

    header_sdl <- file.path(get_src_sdl2(), "include", "SDL.h")
    expect_s3_class(p <- port(header_sdl), "dynport")
    f <- tempfile("porter", fileext = ".dynport")
    expect_true(file.exists(port_write(p, f)))
    expect_true(file.exists(f))

    readLines
    unlink(f)
})
