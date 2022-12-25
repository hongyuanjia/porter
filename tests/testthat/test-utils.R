test_that("Assertion works", {
    expect_true(is_integerish(-1))
    expect_false(is_integerish(-1.1))

    expect_true(is_count(1))
    expect_false(is_count(1.1))

    expect_true(is_number(1))
    expect_false(is_number("a"))

    expect_true(is_flag(TRUE))
    expect_false(is_flag("a"))
})

test_that("spaste() works", {
    expect_equal(spaste("a", "b"), "a b")
    expect_equal(spaste("a", "b", .fsep = ","), "a,b")
    expect_equal(spaste("a", c("b", "c"), .fsep = ",", .fcoll = "|"), "a,b|a,c")
    expect_equal(spaste("a%s", "b", .v = "c"), "ac b")
    expect_equal(spaste("a%s", "b", .v = list("c")), "ac b")
    expect_equal(spaste("a%s", "b", .v = c(1:3), .vcoll = ","), "a1,2,3 b")
    expect_equal(spaste("a%s", "b", .v = c(1:3), .rcoll = ","), "a1 b,a2 b,a3 b")
})

test_that("reg_match() works", {
    cache <- cache_new()

    expect_equal(
        reg_match(c("a b", "a1b"), "[a-z]"),
        list(c("a", "b"), c("a", "b"))
    )

    expect_error(reg_match(c("a b"), "[a-z]", 1:2))

    expect_equal(
        reg_match(c("a b", "a1b"), "([a-z]).([a-z])", 1, flat = FALSE),
        list(
            matrix(c("a b", "a"), nrow = 1, dimnames = list(NULL, c(".match", ""))),
            matrix(c("a1b", "a"), nrow = 1, dimnames = list(NULL, c(".match", "")))
        )
    )

    expect_equal(
        reg_match(c("a b", "a1b"), "([a-z]).([a-z])", 1, flat = TRUE),
        list(c(".match" = "a b", "a"), c(".match" = "a1b", "a"))
    )
})

test_that("from_json() works", {
    expect_equal(from_json("{}"), list())
    expect_equal(from_json('{"a":1}'), list(a = 1L))
    expect_equal(from_json('{"a":{\n"b": 2 }}'), list(a = list(b = 2L)))
    expect_equal(from_json('{"a": [1, 2, 3]}'), list(a = 1:3))
    expect_equal(from_json('{"a": {"b": [1, 2, 3]}}'), list(a = list(b = 1:3)))
})
