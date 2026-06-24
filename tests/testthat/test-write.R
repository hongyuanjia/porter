test_that("port_write() works", {
    skip_if_no_castxml()

    header_sample <- local_porter_header()
    expect_s3_class(p <- port(header_sample), "dynport")
    f <- tempfile("porter", fileext = ".dynport")
    expect_true(file.exists(port_write(p, f)))
    expect_true(file.exists(f))

    readLines
    unlink(f)
})

test_that("port_write() writes a single enum as Enum/name", {
    skip_if_no_castxml()

    header <- tempfile(fileext = ".h")
    writeLines("enum OnlyOne { ONLY_A = 1, ONLY_B = 2 };", header)

    p <- port(header, limit = TRUE)
    p <- port_set(p, Package = "Test", Version = "1.0", Library = "Test")

    f <- tempfile("porter", fileext = ".dynport")
    suppressWarnings(port_write(p, f))
    txt <- readLines(f, warn = FALSE)

    expect_true(any(grepl("^Enum/OnlyOne:", txt)))
    expect_false(any(grepl("^Enum:", txt)))
    expect_true(any(grepl("^    ONLY_A=1$", txt)))
    expect_true(any(grepl("^    ONLY_B=2$", txt)))
    expect_false(any(grepl("c\\(", txt)))
})
