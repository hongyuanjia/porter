# porter (development version)

* Improve rdyncall-compatible signature output for CastXML `bool`, `signed char`,
  opaque pointers, and const-qualified pointer chains (#16).

* Write single-enum dynport output as `Enum/<name>:` fields so generated files
  are accepted by rdyncall (#18).

* Preserve object-like macro constants as `Constant` dynport fields and limit
  `FuncPtr` output to real exported global function pointer variables (#20).

# porter 0.1.0

* Initial CRAN submission.

* Generate rdyncall-compatible dynport files from CastXML output, including
  functions, variadic functions, function pointer typedefs, enums, structs, and
  unions.

* Preserve ABI-relevant aggregate details where they can be represented in
  rdyncall dynport syntax, including fixed arrays, bitfields, packed/aligned
  layout annotations, and opaque fallbacks with diagnostics.

* Report unsupported function-like macros from CastXML preprocessor output
  instead of generating callable dynport entries.

* Locate user-installed CastXML executables without downloading or installing
  external binaries.
