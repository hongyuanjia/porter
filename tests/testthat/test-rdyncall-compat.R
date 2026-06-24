test_that("rdyncall-compatible ABI details are preserved", {
    skip_if_no_castxml()

    header <- tempfile(fileext = ".h")
    writeLines(c(
        "#define MACRO_FN(x) ((x) + 1)",
        "int fixed(int x);",
        "int vf(const char *fmt, ...);",
        "typedef int (*callback_t)(int x, ...);",
        "long double unsupported_long_double(void);",
        "struct _Hidden { int x; };",
        "enum Boolish { TRUE = 1, PORTER_OK = 2 };",
        "enum TooWide { TOO_LOW = -2147483648, TOO_WIDE_OK = 1 };",
        "struct Plain { int a[3]; unsigned b:5; unsigned :0; unsigned c:7; };",
        "struct __attribute__((packed, aligned(8))) PackedAligned { char c; double d; };",
        "struct HasAnon { int tag; union { int i; double d; }; };"
    ), header)

    p <- port(header, limit = TRUE)

    expect_equal(p$Function$value$name, "fixed")
    expect_false(any(p$Function$value$ellipsis))
    expect_equal(p$Variadic$value$name, "vf")
    expect_false("unsupported_long_double" %in% p$Function$value$name)
    expect_false("_Hidden" %in% p$Struct$value$name)
    expect_false("HasAnon" %in% p$Struct$value$name)
    expect_equal(p$Enum$value$values[[which(p$Enum$value$name == "Boolish")]]$name, "PORTER_OK")

    report <- port_report(p)
    expect_true(any(report$kind == "unsupported_macro" & report$name == "MACRO_FN"))
    expect_true(any(report$kind == "unsupported_variadic_funcptr" & report$name == "callback_t"))
    expect_true(any(report$kind == "unsupported_signature" & report$name == "unsupported_long_double"))
    expect_true(any(report$kind == "unsupported_signature" & report$name == "HasAnon"))
    expect_true(any(report$kind == "unsupported_export_name" & report$name == "_Hidden"))
    expect_true(any(report$kind == "unsupported_export_name" & report$name == "TRUE"))
    expect_true(any(report$kind == "unsupported_enum_value" & report$name == "TOO_LOW"))

    file <- tempfile(fileext = ".dynport")
    suppressWarnings(port_write(p, file))
    txt <- paste(readLines(file), collapse = "\n")

    expect_match(txt, "Function: fixed\\(i\\)i x;", fixed = FALSE)
    expect_match(txt, "Variadic: vf\\(Z\\)i fmt;", fixed = FALSE)
    expect_match(txt, "Plain\\{i\\[3\\]III\\}a b:5 :0 c:7;", fixed = FALSE)
    expect_match(txt, "PackedAligned\\{cd\\}c d @packed @align\\(8\\);", fixed = FALSE)

    validation <- port_validate_symbols(p, c("fixed", "other_symbol"))
    expect_equal(validation$name, c("fixed", "vf"))
    expect_equal(validation$field, c("Function", "Variadic"))
    expect_equal(validation$status, c("available", "missing"))
    expect_error(port_validate_symbols(p, NA_character_), "symbols")
    expect_error(port_validate_symbols(p, "fixed", lib = "c"), "reserved")
})

test_that("macro diagnostics use CastXML preprocessor output", {
    skip_if_no_castxml()

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

test_that("function type typedef pointers are written as pointers", {
    skip_if_no_castxml()

    header <- tempfile(fileext = ".h")
    writeLines(c(
        "typedef int bare_fn_t(int x);",
        "struct Holder { bare_fn_t *cb; };",
        "int fixed(int x);"
    ), header)

    p <- port(header, limit = TRUE)

    expect_equal(port_get(p, "Function")$name, "fixed")
    expect_false("bare_fn_t" %in% port_get(p, "FuncPtr")$name)
    expect_equal(port_get(p, "Struct")$name, "Holder")

    p <- port_set(p, Package = "T", Version = "1.0", Library = "T")
    file <- tempfile(fileext = ".dynport")
    suppressWarnings(port_write(p, file))
    txt <- paste(readLines(file), collapse = "\n")

    expect_match(txt, "Holder\\{p\\}cb;", fixed = FALSE)
})

test_that("CastXML bool and signed char fundamental types are supported", {
    skip_if_no_castxml()

    header <- tempfile(fileext = ".h")
    writeLines(c(
        "#include <stdbool.h>",
        "typedef signed char Sint8;",
        "bool sdl_bool(void);",
        "bool sdl_sint8(Sint8 value, Sint8 *out);"
    ), header)

    p <- port(header, limit = TRUE)
    functions <- port_get(p, "Function")

    expect_true(all(c("sdl_bool", "sdl_sint8") %in% functions$name))
    expect_false(any(port_report(p, "unsupported_signature")$name %in% functions$name))

    p <- port_set(p, Package = "T", Version = "1.0", Library = "T")
    file <- tempfile(fileext = ".dynport")
    suppressWarnings(port_write(p, file))
    txt <- paste(readLines(file), collapse = "\n")

    expect_match(txt, "sdl_bool\\(\\)B;", fixed = FALSE)
    expect_match(txt, "sdl_sint8\\(c\\*c\\)B value out;", fixed = FALSE)
})

test_that("R headers can be converted despite anonymous CastXML members", {
    skip_if_no_castxml()

    inc <- file.path(R.home(), "include")
    headers <- file.path(inc, c("R.h", "Rinternals.h"))
    skip_if_not(all(file.exists(headers)), "R headers are not available")

    header <- tempfile(fileext = ".h")
    writeLines(c("#include <R.h>", "#include <Rinternals.h>"), header)
    on.exit(unlink(header), add = TRUE)

    p <- suppressWarnings(port(header, limit = inc, cflags = paste0("-I", inc)))

    expect_true("Function" %in% port_fields(p))
    expect_true("Variadic" %in% port_fields(p))
    expect_true("R_IsNA" %in% port_get(p, "Function")$name)
    expect_true("Rprintf" %in% port_get(p, "Variadic")$name)
    expect_true("SEXPREC" %in% port_get(p, "Struct")$name)
    expect_true(any(port_report(p)$kind == "unsupported_macro"))

    p <- port_set(
        p,
        Package = "R",
        Version = as.character(getRversion()),
        Library = "R"
    )
    file <- tempfile(fileext = ".dynport")
    suppressWarnings(port_write(p, file))
    expect_true(file.exists(file))

    text <- readLines(file, warn = FALSE)
    expect_false(any(grepl("R_allocLD", text, fixed = TRUE)))
    expect_false(any(grepl("_DllInfo", text, fixed = TRUE)))
    expect_false(any(grepl("FALSE=", text, fixed = TRUE)))
    expect_false(any(grepl("TRUE=", text, fixed = TRUE)))

    report <- port_report(p)
    expect_true(any(report$kind == "unsupported_export_name"))
})
