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
