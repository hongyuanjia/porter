test_that("rdyncall-compatible ABI details are preserved", {
    skip_if(Sys.which("castxml") == "", "CastXML is not available")

    header <- tempfile(fileext = ".h")
    writeLines(c(
        "#define MACRO_FN(x) ((x) + 1)",
        "int fixed(int x);",
        "int vf(const char *fmt, ...);",
        "typedef int (*callback_t)(int x, ...);",
        "struct Plain { int a[3]; unsigned b:5; unsigned :0; unsigned c:7; };",
        "struct __attribute__((packed, aligned(8))) PackedAligned { char c; double d; };"
    ), header)

    p <- port(header, limit = TRUE)

    expect_equal(p$Function$value$name, "fixed")
    expect_false(any(p$Function$value$ellipsis))
    expect_equal(p$Variadic$value$name, "vf")

    report <- port_report(p)
    expect_true(any(report$kind == "unsupported_macro" & report$name == "MACRO_FN"))
    expect_true(any(report$kind == "unsupported_variadic_funcptr" & report$name == "callback_t"))

    file <- tempfile(fileext = ".dynport")
    suppressWarnings(port_write(p, file))
    txt <- paste(readLines(file), collapse = "\n")

    expect_match(txt, "Function: fixed\\(i\\)i x;", fixed = FALSE)
    expect_match(txt, "Variadic: vf\\(Z\\)i fmt;", fixed = FALSE)
    expect_match(txt, "Plain\\{i\\[3\\]III\\}a b:5 :0 c:7;", fixed = FALSE)
    expect_match(txt, "PackedAligned\\{cd\\}c d @packed @align\\(8\\);", fixed = FALSE)
})

test_that("macro diagnostics use CastXML preprocessor output", {
    skip_if(Sys.which("castxml") == "", "CastXML is not available")

    root <- tempfile("porter-macros-")
    outside <- tempfile("porter-macros-outside-")
    dir.create(root)
    dir.create(outside)

    included <- file.path(root, "included.h")
    outside_header <- file.path(outside, "outside.h")
    header <- file.path(root, "main.h")

    writeLines("#define INCLUDED_FN(x) ((x) + 10)", included)
    writeLines("#define OUTSIDE_FN(x) ((x) + 20)", outside_header)
    writeLines(c(
        "#include \"included.h\"",
        sprintf("#include \"%s\"", outside_header),
        "#define ACTIVE_FN(x) ((x) + 1)",
        "#if 0",
        "#define DISABLED_FN(x) ((x) + 2)",
        "#endif",
        "#define REMOVED_FN(x) ((x) + 3)",
        "#undef REMOVED_FN",
        "#define REDEF_FN(x) ((x) + 4)",
        "#undef REDEF_FN",
        "#define REDEF_FN(x) ((x) + 5)",
        "int fixed(int x);"
    ), header)

    p <- port(header, limit = root)
    report <- port_report(p, "unsupported_macro")
    expect_true(all(c("ACTIVE_FN", "INCLUDED_FN", "REDEF_FN") %in% report$name))
    expect_false(any(c("DISABLED_FN", "REMOVED_FN", "OUTSIDE_FN") %in% report$name))
    expect_equal(report$file[report$name == "INCLUDED_FN"], normalizePath(included))
    expect_equal(report$line[report$name == "INCLUDED_FN"], 1L)
    expect_equal(report$line[report$name == "REDEF_FN"], 11L)

    p_all <- port(header, limit = FALSE)
    expect_true("OUTSIDE_FN" %in% port_report(p_all, "unsupported_macro")$name)

    p_pattern <- port(header, limit = "^ACTIVE_FN$")
    expect_equal(port_report(p_pattern, "unsupported_macro")$name, "ACTIVE_FN")
})

test_that("macro diagnostics report CastXML preprocessor failures", {
    header <- tempfile(fileext = ".h")
    writeLines("int fixed(void);", header)

    report <- scan_castxml_function_like_macros(
        header, cflags = NULL, castxml = file.path(tempdir(), "missing-castxml"),
        dirs = dirname(header), pattern = NULL
    )

    expect_equal(report$kind, c("macro_scan_failed", "macro_scan_failed"))
    expect_equal(report$name, c("-dD", "-dM"))
    expect_true(all(is.na(report$line)))
})
