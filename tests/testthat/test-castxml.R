test_that("Can get all CastXML versions", {
    skip_on_cran()

    expect_type(vers <- get_castxml_vers(), "character")

    expect_true(all(grepl("\\d[.]\\d[.]\\d", names(vers))))
})

test_that("Can get all CastXML versions", {
    skip_on_cran()

    expect_type(res <- list_castxml_rel_files(), "list")

    expect_named(res, c("_id", "name", "size"))

    expect_type(res$size, "double")

    expect_false(is.null(attr(res, "version")))
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
