skip_if_no_castxml <- function() {
    skip_if(is.null(suppressMessages(locate_castxml())), "CastXML is not available")
}

local_porter_header <- function() {
    header <- tempfile(fileext = ".h")
    writeLines(c(
        "typedef enum PorterBool { PORTER_FALSE = 0, PORTER_TRUE = 1 } PorterBool;",
        "typedef struct PorterPoint { int x; double y; } PorterPoint;",
        "typedef union PorterValue { int i; double d; } PorterValue;",
        "typedef int (*PorterCallback)(int code, void *data);",
        "int porter_add(int a, int b);",
        "void porter_set_callback(PorterCallback cb);",
        "int porter_printf(const char *fmt, ...);"
    ), header)
    header
}
