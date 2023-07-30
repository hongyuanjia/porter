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
})
