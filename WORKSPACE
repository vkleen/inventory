workspace(name = "inventory")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-8940004c95b802151b53d3bbefbebb601fd7b4df",
    urls = ["https://github.com/tweag/rules_haskell/archive/8940004c95b802151b53d3bbefbebb601fd7b4df.tar.gz"],
    sha256 = "b7427136ef99fab22ff406967680dd1d7db035002db259f10a27ad2d3e8689c3",
    patches = [ "//:bazel/rules_haskell_core_exceptions.patch" ],
    patch_args = [ "-p1" ],
)

http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-c40b35f73e5ab1c0096d95abf63027a3b8054061",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/c40b35f73e5ab1c0096d95abf63027a3b8054061.tar.gz"],
    sha256 = "47fffc870a25d82deedb887c32481a43a12f56b51e5002773046f81fbe3ea9df",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")
rules_nixpkgs_dependencies()

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")
rules_haskell_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_git_repository",
    "nixpkgs_local_repository",
    "nixpkgs_cc_configure",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
  name = "nixpkgs",
  nix_file_deps = [
    "//:flake.nix",
    "//:flake.lock"
  ],
  nix_file = "//:pkgs.nix"
)

nixpkgs_cc_configure( repository = "@nixpkgs" )
nixpkgs_python_configure( repository = "@nixpkgs" )

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("//mgmt:deps.bzl", "stackage_deps")
load("//nih:deps.bzl", "vendored_deps", "vendored_packages")

_extra_packages = [
  d for deps in vendored_deps.values() for d in deps
]

_vendored_packages = vendored_packages()

stack_snapshot(
    name = "stackage",
    extra_deps = {},
    packages = [d for d in stackage_deps if d not in _vendored_packages.keys()] + _extra_packages,
    vendored_packages = _vendored_packages,

    snapshot = "nightly-2021-07-30",
    stack_snapshot_json = "//:stackage_snapshot.json",
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")
haskell_register_ghc_nixpkgs(
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "9.0.1",
    attribute_path = "haskell.packages.ghc901.ghc"
)
