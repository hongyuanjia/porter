
<!-- README.md is generated from README.Rmd. Please edit that file -->

# porter

<!-- badges: start -->
<!-- badges: end -->

The goal of {porter} is to generate port files for
[rdyncall](https://github.com/hongyuanjia/rdyncall) to support Foreign
Function Interface (FFI) for C Libraries in R. It uses
[CastXML](https://github.com/CastXML/CastXML), a C-family abstract
syntax tree XML output tool to parse C header files.

## Installation

You can install the development version of porter like so:

``` r
remotes::install_github("hongyuanjia/port")
```

## Example

### Manage CastXML installation

You can use `install_castxml()` to install CastXML. It downloads the
pre-built binaries of CastXML together with the LLVM/Clang dependencies
via the [CastXML
Superbuild](https://github.com/CastXML/CastXMLSuperbuild) project.

`locate_castxml()` will return the current CastXML in use:

``` r
install_castxml("latest")

locate_castxml()
#>                                         0.5.0
#> "C:\\Users\\hongy\\scoop\\shims\\castxml.exe"
```

Instead of using `install_castxml()`, you may directly use your package
manager to install CastXML, as it may already provide a `castxml`
package.

- On Windows, you can use [Scoop](https://scoop.sh/) to install CastXML
  via: \`scoop install main/castxml”
- On macOS, you can use [Homebrew](https://brew.sh/) to install CastXML
  via: \`brew install castxml”
- On Linux, you can check if your system package manager provides
  CastXML or not via
  [Repology](https://repology.org/project/castxml/versions).

### Generate port files for C libraries

Take the [SDL2](https://www.libsdl.org/) library as an example. After
downloading the SDL2
[source](https://github.com/libsdl-org/SDL/releases), run `port()` with
the path of SDL2 header file (here `dir` is directory of SDL2 source):

``` r
version <- "2.28.0"
dir <- file.path(tempdir(), paste0("SDL2-", version))
if (!dir.exists(dir)) {
  zipfile <- porter:::download_src_sdl2(version)
  unzip(zipfile, exdir = tempdir())
}
```

``` r
p <- port(file.path(dir, "include", "SDL.h"))

p <- port_set(p,
  Package = "SDL2",
  Version = "2.28.0",
  Library = c("SDL", "SDL-2.28", "SDL-2.28.so.0")
)

p
#> Package: SDL2 
#> Version: 2.28.0 
#> Library/
#>     SDL
#>     SDL-2.28
#>     SDL-2.28.so.0 
#> Function/
#>     SDL_GetPlatform()Z;
#>     SDL_malloc(L)p size;
#>     SDL_calloc(LL)p nmemb size;
#>     SDL_realloc(pL)p mem size;
#>     SDL_free(p)v mem; 
#>     ... [# Truncated with 850 more items]
#> FuncPtr/
#>     SDL_malloc_func(L)p;
#>     SDL_calloc_func(LL)p;
#>     SDL_realloc_func(pL)p;
#>     SDL_free_func(p)v;
#>     SDL_main_func(ip)i; 
#>     ... [# Truncated with 22 more items]
#> Enum/SDL_bool:
#>     SDL_FALSE=0
#>     SDL_TRUE=1
#> Enum/SDL_DUMMY_ENUM:
#>     DUMMY_ENUM_VALUE=0
#> Enum/SDL_AssertState:
#>     SDL_ASSERTION_RETRY=0
#>     SDL_ASSERTION_BREAK=1
#>     SDL_ASSERTION_ABORT=2
#>     SDL_ASSERTION_IGNORE=3
#>     SDL_ASSERTION_ALWAYS_IGNORE=4
#> Enum/SDL_errorcode:
#>     SDL_ENOMEM=0
#>     SDL_EFREAD=1
#>     SDL_EFWRITE=2
#>     SDL_EFSEEK=3
#>     SDL_UNSUPPORTED=4
#>     SDL_LASTERROR=5
#> Enum/SDL_ThreadPriority:
#>     SDL_THREAD_PRIORITY_LOW=0
#>     SDL_THREAD_PRIORITY_NORMAL=1
#>     SDL_THREAD_PRIORITY_HIGH=2
#>     SDL_THREAD_PRIORITY_TIME_CRITICAL=3 
#>     ... [# Truncated with 50 more items]
#> Struct/
#>     _SDL_iconv_t{};
#>     SDL_AssertData{iIZZiZ*<SDL_AssertData>}always_ignore trigger_count condition filename linenum function next;
#>     SDL_atomic_t{i}value;
#>     SDL_mutex{};
#>     SDL_semaphore{}; 
#>     ... [# Truncated with 87 more items]
#> Union/
#>     SDL_Event{I<SDL_CommonEvent><SDL_DisplayEvent><SDL_WindowEvent><SDL_KeyboardEvent><SDL_TextEditingEvent><SDL_TextEditingExtEvent><SDL_TextInputEvent><SDL_MouseMotionEvent><SDL_MouseButtonEvent><SDL_MouseWheelEvent><SDL_JoyAxisEvent><SDL_JoyBallEvent><SDL_JoyHatEvent><SDL_JoyButtonEvent><SDL_JoyDeviceEvent><SDL_JoyBatteryEvent><SDL_ControllerAxisEvent><SDL_ControllerButtonEvent><SDL_ControllerDeviceEvent><SDL_ControllerTouchpadEvent><SDL_ControllerSensorEvent><SDL_AudioDeviceEvent><SDL_SensorEvent><SDL_QuitEvent><SDL_UserEvent><SDL_SysWMEvent><SDL_TouchFingerEvent><SDL_MultiGestureEvent><SDL_DollarGestureEvent><SDL_DropEvent>p}type common display window key edit editExt text motion button wheel jaxis jball jhat jbutton jdevice jbattery caxis cbutton cdevice ctouchpad csensor adevice sensor quit user syswm tfinger mgesture dgesture drop padding;
#>     SDL_HapticEffect{S<SDL_HapticConstant><SDL_HapticPeriodic><SDL_HapticCondition><SDL_HapticRamp><SDL_HapticLeftRight><SDL_HapticCustom>}type constant periodic condition ramp leftright custom;
#>     SDL_WindowShapeParams{C<SDL_Color>}binarizationCutoff colorKey;
#>     hidden{<windowsio><mem><unknown>}windowsio mem unknown;
#>     value{ii<hat>}button axis hat;
```

Use `port_write()` to save the port file.

``` r
port_write(p, file.path(tempdir(), "SDL2.dynport"))
```

## Author

Hongyuan Jia

## License

The project is released under the terms of MIT License.

Copyright © 2023 Hongyuan Jia
