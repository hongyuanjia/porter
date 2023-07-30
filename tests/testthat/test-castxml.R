test_that("get_castxml_vers() works", {
    skip_on_cran()

    expect_type(vers <- get_castxml_vers(), "character")

    expect_true(all(grepl("\\d[.]\\d[.]\\d", names(vers))))
})

test_that("list_castxml_rel_files() works", {
    skip_on_cran()

    expect_type(res <- list_castxml_rel_files(), "list")
    expect_named(res, c("_id", "name", "size"))
    expect_type(res$size, "integer")
    expect_false(is.null(attr(res, "version")))

    expect_s3_class(res <- list_castxml_rel_files("0.4.8", TRUE), "data.frame")
    expect_equal(attr(res, "version"), "0.4.8")
    expect_equal(nrow(res), 6L)
})

test_that("Can download CastXML releases", {
    skip_on_cran()

    expect_type(dl <- download_castxml(), "character")
    expect_true(file.exists(dl))
    expect_false(is.null(attr(dl, "version")))
})

test_that("Can install CastXML releases", {
    skip_on_cran()

    expect_true(install_castxml(force = TRUE))
    expect_type(locate_castxml(), "character")
    expect_named(locate_castxml())
    expect_false(install_castxml())
})
