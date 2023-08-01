test_that("Cache works", {
    cache <- cache_new(age = "0.01 sec", persist = FALSE)

    expect_true(cache$set("a", 1L))
    Sys.sleep(0.1)
    expect_null(cache$get("a"))
    expect_false(cache$exists("a"))

    cache <- cache_new(persist = FALSE)

    expect_null(cache$keys())
    expect_null(cache$get("a"))
    expect_false(cache$exists("a"))
    expect_null(cache$keys())
    expect_false(cache$remove("a"))
    expect_false(cache$prune())
    expect_equal(cache$size(), 0L)
    expect_equal(cache$avail(), 1:10)

    expect_true(cache$set("a", 1L))
    expect_equal(cache$get("a"), 1L)
    expect_true(cache$exists("a"))
    expect_equal(cache$keys(), "a")
    expect_equal(cache$size(), 1L)
    expect_equal(cache$avail(), 2:10)

    expect_true(cache$remove("a"))
    expect_null(cache$keys())
    expect_null(cache$get("a"))
    expect_false(cache$exists("a"))
    expect_equal(cache$size(), 0L)
    expect_equal(cache$avail(), 1:10)
})
