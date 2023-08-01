test_that("port_fields() works", {
    skip_on_cran()

    header_sdl <- file.path(get_src_sdl2(), "include", "SDL.h")
    expect_s3_class(p <- port(header_sdl), "dynport")

    expect_equal(port_fields(p), PORT_FIELDS)
})

test_that("port_get() works", {
    skip_on_cran()

    header_sdl <- file.path(get_src_sdl2(), "include", "SDL.h")
    expect_s3_class(p <- port(header_sdl), "dynport")

    expect_equal(port_get(p, "Package"), NULL)
    expect_equal(port_get(p, "Version"), NULL)
    expect_equal(port_get(p, "Function"), p$Function$value)
    expect_equal(port_get(p, "FuncPtr"),  p$FuncPtr$value)
    expect_equal(port_get(p, "Struct"),   p$Struct$value)
    expect_equal(port_get(p, "Union"),    p$Union$value)
    expect_equal(port_get(p, "Enum"),     p$Enum$value)
})

test_that("port_set() works", {
    skip_on_cran()

    header_sdl <- file.path(get_src_sdl2(), "include", "SDL.h")
    expect_s3_class(p <- port(header_sdl), "dynport")

    expect_error(port_set(p, "Package", ".SDL2"))
    expect_s3_class(p <- port_set(p, "Package", "SDL2"), "dynport")
    expect_equal(port_get(p, "Package"), "SDL2")
    expect_equal(port_get(p, c("Package", "Version")), list(Package = "SDL2", Version = NULL))
    expect_s3_class(p <- port_set(p, Package = NULL), "dynport")
    expect_equal(port_get(p, "Package"), NULL)

    expect_error(port_set(p, "Version", "a"))
    expect_s3_class({ p <- port_set(p, "Version", "1.0"); p }, "dynport")
    expect_equal(port_get(p, "Version"), "1.0")

    expect_error(port_set(p, "Library", NA))
    expect_s3_class({ p <- port_set(p, "Library", c("sdl.so", "sdl.dll")); p }, "dynport")
    expect_equal(port_get(p, "Library"), c("sdl.so", "sdl.dll"))

    expect_error(port_set(p, "Function", "a"))
    expect_s3_class(p <- port_set(p, "Function", p$func$value), "dynport")
    expect_equal(port_get(p, "Function"), p$func$value)

    expect_error(port_set(p, "FuncPtr", "a"))
    expect_s3_class(p <- port_set(p, "FuncPtr", p$funcptr$value), "dynport")
    expect_equal(port_get(p, "FuncPtr"), p$funcptr$value)

    expect_error(port_set(p, "Enum", "a"))
    expect_s3_class(p <- port_set(p, "Enum", p$enum$value), "dynport")
    expect_equal(port_get(p, "Enum"), p$enum$value)

    expect_error(port_set(p, "Struct", "a"))
    expect_s3_class(p <- port_set(p, "Struct", p$struct$value), "dynport")
    expect_equal(port_get(p, "Struct"), p$struct$value)

    expect_error(port_set(p, "Union", "a"))
    expect_s3_class(p <- port_set(p, "Union", p$union$value), "dynport")
    expect_equal(port_get(p, "Union"), p$union$value)

    expect_s3_class(p <- port_set(p, "Encoding", "UTF-8"), "dynport")
    expect_true(port_has(p, "Encoding"))
    expect_equal(port_get(p, "Encoding"), "UTF-8")
    expect_s3_class(p <- port_set(p, "Encoding", NULL), "dynport")
    expect_error(port_get(p, "Encoding"))
})
