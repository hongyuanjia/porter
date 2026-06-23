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
    bad_header <- tempfile(fileext = ".h")
    writeLines("not valid C input @", bad_header)
    expect_error(port(bad_header), "CastXML")
    # can warn if C++ header
    expect_warning(port({ h <- tempfile(fileext = ".hpp"); writeLines("", h); h }), "C\\+\\+")

    skip_if_no_castxml()

    expect_s3_class(p <- port(header), "dynport")
    expect_named(p, unname(PORT_FIELDS))
    expect_s3_class(p$Package,  "dynportfield")
    expect_s3_class(p$Version,  "dynportfield")
    expect_s3_class(p$Library,  "dynportfield")
    expect_s3_class(p$Function, "dynportfield")
    expect_s3_class(p$Variadic, "dynportfield")
    expect_s3_class(p$FuncPtr,  "dynportfield")
    expect_s3_class(p$Enum,     "dynportfield")
    expect_s3_class(p$Struct,   "dynportfield")
    expect_s3_class(p$Union,    "dynportfield")
    expect_s3_class(p$File,     "dynportfield")
    expect_null(p$Package$value)
    expect_null(p$Version$value)
    expect_null(p$Library$value)
    expect_null(p$Func$value)
    expect_null(p$Variadic$value)
    expect_null(p$FuncPtr$value)
    expect_null(p$Enum$value)
    expect_null(p$Struct$value)
    expect_null(p$Union$value)
    expect_null(p$File$value)

    unlink(header)

    header_sample <- local_porter_header()
    expect_s3_class(p <- port(header_sample), "dynport")
    expect_named(p, unname(PORT_FIELDS))
    expect_s3_class(p$Package,  "dynportfield")
    expect_s3_class(p$Version,  "dynportfield")
    expect_s3_class(p$Library,  "dynportfield")
    expect_s3_class(p$Function, "dynportfield")
    expect_s3_class(p$Variadic, "dynportfield")
    expect_s3_class(p$FuncPtr,  "dynportfield")
    expect_s3_class(p$Enum,     "dynportfield")
    expect_s3_class(p$Struct,   "dynportfield")
    expect_s3_class(p$Union,    "dynportfield")
    expect_s3_class(p$File,     "dynportfield")
    expect_null(p$Package$value)
    expect_null(p$Version$value)
    expect_null(p$Library$value)
    expect_s3_class(p$Function$value, "data.frame")
    if (!is.null(p$Variadic$value)) expect_s3_class(p$Variadic$value, "data.frame")
    expect_s3_class(p$FuncPtr$value,  "data.frame")
    expect_s3_class(p$Enum$value,     "data.frame")
    expect_s3_class(p$Struct$value,   "data.frame")
    expect_s3_class(p$Union$value,    "data.frame")
    expect_type(p$File$value, "character")

    expect_named(p$Function$value, c("name", "returns", "arguments", "ellipsis"))
    if (!is.null(p$Variadic$value)) expect_named(p$Variadic$value, c("name", "returns", "arguments"))
    expect_named(p$FuncPtr$value,  c("name", "returns", "arguments"))
    expect_named(p$Enum$value,     c("name", "values", "size", "align"))
    expect_named(p$Struct$value,   c("name", "members", "size", "align"))
    expect_named(p$Union$value,    c("name", "members", "size", "align"))

    functions <- c(
        p$Function$value$name,
        if (is.null(p$Variadic$value)) character() else p$Variadic$value$name
    )
    expect_true(all(c("porter_add", "porter_set_callback", "porter_printf") %in% functions))
    expect_gt(nrow(p$FuncPtr$value), 0L)
    expect_equal(p$Variadic$value$name, "porter_printf")
    expect_true("PorterBool" %in% p$Enum$value$name)
    expect_true("PorterPoint" %in% p$Struct$value$name)
    expect_gt(nrow(p$Union$value), 0L)
    expect_gt(length(p$File$value), 0L)
})
