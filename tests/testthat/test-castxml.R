test_that("locate_castxml() validates path inputs", {
    expect_error(locate_castxml(1), "path")
    expect_null(suppressMessages(locate_castxml(tempfile())))
})

test_that("locate_castxml() finds an installed CastXML", {
    skip_if_no_castxml()

    castxml <- suppressMessages(locate_castxml())
    expect_type(castxml, "character")
    expect_length(castxml, 1L)
    expect_named(castxml)
    expect_true(file.exists(castxml))

    expect_identical(
        suppressMessages(locate_castxml(dirname(castxml))),
        castxml
    )
    expect_identical(
        suppressMessages(locate_castxml(unname(castxml))),
        castxml
    )
})

test_that("known CastXML paths include common package managers", {
    paths <- castxml_known_paths()
    expect_type(paths, "character")

    if (is_macos()) {
        expect_true(any(paths == "/opt/homebrew/bin/castxml"))
        expect_true(any(paths == "/usr/local/bin/castxml"))
    } else if (is_linux()) {
        expect_true(any(paths == "/home/linuxbrew/.linuxbrew/bin/castxml"))
        expect_true(any(paths == path.expand("~/.linuxbrew/bin/castxml")))
    } else if (is_windows()) {
        expect_true(any(grepl("scoop", paths, fixed = TRUE)))
        expect_true(any(grepl("chocolatey", paths, fixed = TRUE)))
    }
})
