#' Write `dynport` file
#'
#' @param port A `dynport` object
#'
#' @param file A single string or a connection object.
#'
#' @param reorder Either `TRUE` or `FALSE`. If `TRUE`, required `dynport` fields
#'        are always written before all other customized fields. Default:
#'        `TRUE`.
#' @return
#' A single string of the file path, invisibly.
#' @export
port_write <- function(port, file, reorder = TRUE) {
    if (!inherits(port, "dynport")) {
        stop("Argument 'port' should be a 'dynport' object.")
    }

    if (!is_flag(reorder)) {
        stop("Argument 'reorder' should be either TRUE or FALSE.")
    }

    if (anyNA(m <- match(PORT_FIELDS, port_fields(port)))) {
        stop(spaste("Corrupted 'dynport' object found.",
            "Following", ngettext(sum(is.na(m)), "field is", "fields are"),
            "required but", ngettext(sum(is.na(m)), "is", "are"), "missing: [%s]",
            .v = PORT_FIELDS[is.na(m)], .vcoll = ", "
        ))
    }

    fmt <- lapply(port[names(port) != "File"], format.dynportfield, raw = TRUE)
    out <- mapply(format_field,
        key = names(fmt[names(fmt) != "Enum"]),
        value = fmt[names(fmt) != "Enum"],
        MoreArgs = list(show_key = FALSE),
        SIMPLIFY = TRUE, USE.NAMES = TRUE
    )

    # treat enums differently
    if (length(fmt$Enum) == 1L) {
        out_enum <- list(Enum = format_field("Enum", fmt$Enum, show_key = FALSE))
    } else {
        out_enum <- mapply(
            format_field, value = fmt$Enum,
            MoreArgs = list(key = "Enum", show_key = FALSE),
            SIMPLIFY = FALSE, USE.NAMES = TRUE
        )
        names(out_enum) <- paste("Enum", names(out_enum), sep = "/")
    }

    if (reorder) {
        ord <- match(PORT_FIELDS[PORT_FIELDS != "File"], names(out), 0L)
        # required fields
        req <- out[ord]
        # make sure enums are always after function pointers
        ind <- which(names(req) == "FuncPtr")
        req <- c(req[seq_len(ind)], out_enum, req[-seq_len(ind)])

        # customized fields
        oth <- out[!names(out) %in% c(PORT_FIELDS, names(out_enum))]
        out <- c(req, oth)
    }

    mat <- matrix(out, nrow = 1L, dimnames = list(NULL, names(out)))
    write.dcf(mat, file, keep.white = names(out))

    invisible(normalizePath(file, "/"))
}
