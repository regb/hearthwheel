load("@rules_scala//scala:scala.bzl", "scala_binary", "scala_test")
load("@sgl//bazel:cross.bzl", "scala_library")
load("@sgl//bazel:scala_opts.bzl", "SGL_SCALACOPTS")
load("@sgl//bazel/sgl:assets.bzl", "sgl_audio_assets", "sgl_drawable_assets", "sgl_texture_atlas")
load("@sgl//bazel/sgl:defs.bzl", "sgl_core_library", "sgl_desktop_lwjgl_app", "sgl_desktop_native_app", "sgl_html5_opengl_app")

# The rules of the game, which know nothing of SGL.
scala_library(
    name = "model",
    srcs = glob(["model/*.scala"]),
)

scala_test(
    name = "model-test",
    size = "medium",
    srcs = ["test/ModelSuite.scala"],
    scalacopts = SGL_SCALACOPTS,
    deps = [
        ":model",
    ],
)

# Pictures and sounds are made by tools/Art.java and tools/audio.py.
sgl_drawable_assets(
    name = "sprites",
    srcs = glob(["assets/sprites/*.png"]),
    source_density = "mdpi",
    strip_prefix = "assets/sprites",
    target_densities = ["mdpi"],
)

sgl_texture_atlas(
    name = "sprite_atlas",
    assets = [":sprites"],
    extrusion = 2,
    filter = "linear",
    max_pages = 4,
    max_texture_size = 2048,
)

sgl_audio_assets(
    name = "audio",
    srcs = glob(["assets/audio/*.wav"]),
    strip_prefix = "assets/audio",
)

sgl_core_library(
    name = "core",
    srcs = glob(["view/*.scala"]),
    assets = [
        ":audio",
        ":sprite_atlas",
    ],
    package = "hearthwheel.core",
    deps = [
        ":model",
        "@sgl//engines:sgl-gameobject",
    ],
)

sgl_desktop_lwjgl_app(
    name = "desktop-lwjgl",
    core_abstract_class = "hearthwheel.core.AbstractApp",
    data = [
        ":audio",
        ":sprite_atlas",
    ],
    extra_mixins = ["hearthwheel.core.GeneratedAssetsProvider"],
    file_save = "hearthwheel.save",
    frame_height = 720,
    frame_width = 1280,
    main_class = "Main",
    package = "hearthwheel",
    resizable = True,
    window_title = "Hearthwheel",
    deps = [":core"],
)

sgl_html5_opengl_app(
    name = "html5",
    assets = [
        ":audio",
        ":sprite_atlas",
    ],
    assets_strip_prefix = "assets",
    core_abstract_class = "hearthwheel.core.AbstractApp",
    export_name = "Hearthwheel",
    extra_mixins = [
        "hearthwheel.core.GeneratedAssetsProvider",
        "sgl.html5.LocalStorageSaveComponent",
    ],
    fullscreen = True,
    main_class = "Main",
    package = "hearthwheel",
    page_title = "Hearthwheel",
    static_folder = "static",
    deps = [":core"],
)

sgl_desktop_native_app(
    name = "desktop-native",
    assets = [
        ":audio",
        ":sprite_atlas",
    ],
    assets_strip_prefix = "assets",
    backend_app = "NativeOpenGLApp",
    core_abstract_class = "hearthwheel.core.AbstractApp",
    extra_mixins = ["hearthwheel.core.GeneratedAssetsProvider"],
    file_save = "hearthwheel.save",
    frame_height = 720,
    frame_width = 1280,
    linkopts = [
        "-lSDL2",
        "-lSDL2_image",
        "-lGLESv2",
    ],
    main_class = "Main",
    package = "hearthwheel",
    use_screen2d = False,
    deps = [":core"],
)

_HEADLESS_DEPS = [
    ":core",
    ":model",
    "@sgl//backends/desktop-jvm-shared",
    "@sgl//backends/headless-opengl:sgl-headless-opengl",
    "@sgl//backends/lwjgl-shared:sgl-lwjgl-shared",
    "@sgl//core:sgl-core",
    "@sgl//engines:sgl-gameobject",
]

# Draws scenes of the game to files, to look at.
scala_binary(
    name = "capture",
    srcs = [
        "test/Capture.scala",
        "test/Headless.scala",
    ],
    data = [":sprite_atlas"],
    main_class = "hearthwheel.Capture",
    scalacopts = SGL_SCALACOPTS,
    target_compatible_with = ["@platforms//os:linux"],
    deps = _HEADLESS_DEPS,
)

scala_test(
    name = "play-test",
    size = "large",
    srcs = [
        "test/Headless.scala",
        "test/PlaySuite.scala",
    ],
    env = {
        "DISPLAY": "",
        "EGL_PLATFORM": "surfaceless",
        "LIBGL_ALWAYS_SOFTWARE": "1",
        "WAYLAND_DISPLAY": "",
    },
    scalacopts = SGL_SCALACOPTS,
    target_compatible_with = ["@platforms//os:linux"],
    deps = _HEADLESS_DEPS,
)

# The seasons for the banner of the README: see tools/Banner.java.
scala_binary(
    name = "banner",
    srcs = [
        "test/Banner.scala",
        "test/Headless.scala",
    ],
    data = [":sprite_atlas"],
    main_class = "hearthwheel.Banner",
    scalacopts = SGL_SCALACOPTS,
    target_compatible_with = ["@platforms//os:linux"],
    deps = _HEADLESS_DEPS,
)

# How much the engine and the game can take: see README.md.
scala_binary(
    name = "stress",
    srcs = [
        "test/Headless.scala",
        "test/Stress.scala",
    ],
    data = [":sprite_atlas"],
    main_class = "hearthwheel.Stress",
    scalacopts = SGL_SCALACOPTS,
    target_compatible_with = ["@platforms//os:linux"],
    deps = _HEADLESS_DEPS + ["@maven//:org_lwjgl_lwjgl_opengl"],
)

# How valleys go for players of different skill.
scala_binary(
    name = "balance",
    srcs = ["test/Balance.scala"],
    main_class = "hearthwheel.Balance",
    scalacopts = SGL_SCALACOPTS,
    deps = [
        ":core",
        ":model",
        "@sgl//core:sgl-core",
        "@sgl//engines:sgl-gameobject",
    ],
)

# What it takes to hold each wave.
scala_binary(
    name = "siege",
    srcs = ["test/Siege.scala"],
    main_class = "hearthwheel.Siege",
    scalacopts = SGL_SCALACOPTS,
    deps = [":model"],
)
