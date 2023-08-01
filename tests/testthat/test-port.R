test_that("port() works", {
    header <- tempfile("header", fileext = ".h")
    writeLines("", header)

    # can stop if CastXML is not found
    expect_error(port(header, castxml = NULL), "castxml")
    # can stop if input header file not found
    expect_error(port(""), "header")
    # can stop if cflags contains missing values
    expect_error(port(header, cflags = NA), "cflags")
    # can stop if limit is neither a flag nor a string
    expect_error(port(header, limit = NA), "limit")
    # can stop if keep is not flag
    expect_error(port(header, keep = ""), "keep")
    # can stop if CastXML errors
    expect_error(port(testthat::test_path("../../DESCRIPTION")), "input")
    # can warn if C++ header
    expect_warning(port({ h <- tempfile(fileext = ".hpp"); writeLines("", h); h }), "C\\+\\+")

    expect_s3_class(p <- port(header), "dynport")
    expect_named(p, unname(PORT_FIELDS))
    expect_s3_class(p$package, "dynportfield")
    expect_s3_class(p$version, "dynportfield")
    expect_s3_class(p$library, "dynportfield")
    expect_s3_class(p$func,    "dynportfield")
    expect_s3_class(p$funcptr, "dynportfield")
    expect_s3_class(p$enum,    "dynportfield")
    expect_s3_class(p$struct,  "dynportfield")
    expect_s3_class(p$union,   "dynportfield")
    expect_s3_class(p$file,    "dynportfield")
    expect_null(p$package$value)
    expect_null(p$version$value)
    expect_null(p$library$value)
    expect_null(p$func$value)
    expect_null(p$funcptr$value)
    expect_null(p$enum$value)
    expect_null(p$struct$value)
    expect_null(p$union$value)
    expect_null(p$file$value)

    unlink(header)

    skip_on_cran()

    header_sdl <- file.path(get_src_sdl2(), "include", "SDL.h")
    expect_s3_class(p <- port(header_sdl), "dynport")
    expect_named(p, unname(PORT_FIELDS))
    expect_s3_class(p$package, "dynportfield")
    expect_s3_class(p$version, "dynportfield")
    expect_s3_class(p$library, "dynportfield")
    expect_s3_class(p$func,    "dynportfield")
    expect_s3_class(p$funcptr, "dynportfield")
    expect_s3_class(p$enum,    "dynportfield")
    expect_s3_class(p$struct,  "dynportfield")
    expect_s3_class(p$union,   "dynportfield")
    expect_s3_class(p$file,    "dynportfield")
    expect_null(p$package$value)
    expect_null(p$version$value)
    expect_null(p$library$value)
    expect_s3_class(p$func$value,     "data.frame")
    expect_s3_class(p$funcptr$value,  "data.frame")
    expect_s3_class(p$enum$value,     "data.frame")
    expect_s3_class(p$struct$value,   "data.frame")
    expect_s3_class(p$union$value,    "data.frame")
    expect_type(p$file$value, "character")

    expect_named(p$func$value,    c("name", "returns", "arguments", "ellipsis"))
    expect_named(p$funcptr$value, c("name", "returns", "arguments"))
    expect_named(p$enum$value,    c("name", "values", "size", "align"))
    expect_named(p$struct$value,  c("name", "members", "size", "align"))
    expect_named(p$union$value,   c("name", "members", "size", "align"))

    expect_equal(nrow(p$func$value),    855L)
    expect_equal(nrow(p$funcptr$value), 27L)
    expect_equal(nrow(p$enum$value),    55L)
    expect_equal(nrow(p$struct$value),  92L)
    expect_equal(nrow(p$union$value),   5L)
    expect_equal(length(p$file$value),  46L)
})
