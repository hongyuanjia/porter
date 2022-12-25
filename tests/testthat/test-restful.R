test_that("github_release_latest() works", {
    skip_on_cran()
    rel <- github_release_latest("libsdl-org/SDL")

    expect_named(rel,
        c("name", "tag_name", "draft", "prerelease", "html_url", "tarball_url",
            "zipball_url", "assets")
    )

    expect_type(rel, "list")
    expect_type(rel$draft, "logical")
    expect_type(rel$prerelease, "logical")
    expect_s3_class(rel$assets, "data.frame")
    expect_named(rel$assets,
        c("name", "content_type", "size", "created_at", "updated_at",
            "browser_download_url")
    )
})

test_that("github_release() works", {
    skip_on_cran()
    rel <- github_release("libsdl-org/SDL", "release-2.26.1")

    expect_named(rel,
        c("name", "tag_name", "draft", "prerelease", "html_url", "tarball_url",
            "zipball_url", "assets")
    )

    expect_type(rel, "list")
    expect_type(rel$draft, "logical")
    expect_type(rel$prerelease, "logical")
    expect_s3_class(rel$assets, "data.frame")
    expect_named(rel$assets,
        c("name", "content_type", "size", "created_at", "updated_at",
            "browser_download_url")
    )
})

test_that("github_releases() works", {
    skip_on_cran()
    rel <- github_releases("libsdl-org/SDL")

    expect_s3_class(rel, "data.frame")
    expect_named(rel,
        c("name", "tag_name", "draft", "prerelease", "html_url", "tarball_url",
            "zipball_url", "assets")
    )

    expect_type(rel$draft, "logical")
    expect_type(rel$prerelease, "logical")
    expect_type(rel$assets, "list")

    expect_s3_class(rel$assets[[1]], "data.frame")
    expect_named(rel$assets[[1]],
        c("name", "content_type", "size", "created_at", "updated_at",
            "browser_download_url")
    )
})

test_that("Query of RESTful API works", {
    skip_on_cran()
    rel <- github_releases("libsdl-org/SDL")

    expect_s3_class(rel, "data.frame")
    expect_named(rel,
        c("name", "tag_name", "draft", "prerelease", "html_url", "tarball_url",
            "zipball_url", "assets")
    )

    expect_type(rel$draft, "logical")
    expect_type(rel$prerelease, "logical")
    expect_type(rel$assets, "list")

    expect_s3_class(rel$assets[[1]], "data.frame")
    expect_named(rel$assets[[1]],
        c("name", "content_type", "size", "created_at", "updated_at",
            "browser_download_url")
    )
})
