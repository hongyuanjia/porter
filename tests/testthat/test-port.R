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
    expect_named(p, c("func", "funcptr", "enum", "struct", "union", "file"))
    expect_null(p$func)
    expect_null(p$funcptr)
    expect_null(p$enum)
    expect_null(p$struct)
    expect_null(p$union)
    expect_null(p$file)

    unlink(header)

    skip_on_cran()

    header_sdl <- file.path(get_src_sdl2(), "include", "SDL.h")
    expect_s3_class(p <- port(header_sdl), "dynport")
    expect_named(p, c("func", "funcptr", "enum", "struct", "union", "file"))
    expect_s3_class(p$func,     "data.frame")
    expect_s3_class(p$funcptr,  "data.frame")
    expect_s3_class(p$enum,     "data.frame")
    expect_s3_class(p$struct,   "data.frame")
    expect_s3_class(p$union,    "data.frame")
    expect_type(p$file, "character")

    expect_named(p$func,    c("name", "returns", "arguments", "ellipsis"))
    expect_named(p$funcptr, c("name", "returns", "arguments"))
    expect_named(p$enum,    c("name", "values", "size", "align"))
    expect_named(p$struct,  c("name", "members", "size", "align"))
    expect_named(p$union,   c("name", "members", "size", "align"))

    expect_equal(nrow(p$func),    855L)
    expect_equal(nrow(p$funcptr), 27L)
    expect_equal(nrow(p$enum),    55L)
    expect_equal(nrow(p$struct),  92L)
    expect_equal(nrow(p$union),   5L)
    expect_equal(length(p$file),  46L)
})
