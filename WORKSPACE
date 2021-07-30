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
    "nixpkgs_package",
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

http_archive(
    name = "csv",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "csv",
    version = packages["csv"].version,
    srcs = glob(["**"]),
    deps = packages["csv"].deps,
    visibility = ["//visibility:public"],
)
    """,
    patch_args = ["-p1"],
    patches = ["//:bazel/csv.patch"],
    strip_prefix = "csv-0.1.2",
    sha256 = "8cf43442325faa1368f9b55ad952beccf677d9980cdffa3d70a7f204a23ae600",
    urls = ["https://hackage.haskell.org/package/csv-0.1.2/csv-0.1.2.tar.gz"]
)

nixpkgs_package(
    name = "hledger-lib",
    repositories = { "nixpkgs": "@nixpkgs" },
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "hledger-lib",
    version = packages["hledger-lib"].version,
    srcs = glob(["**"]),
    deps = packages["hledger-lib"].deps,
    visibility = ["//visibility:public"],
)
    """,
    nix_file_content = """
with import <nixpkgs> {};
symlinkJoin { name = "hledger-src"; paths = [ "${sources.hledger-src}/hledger-lib" ]; }
    """,
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
stack_snapshot(
    name = "stackage",
    extra_deps = {},
    packages = [
        "aeson",
        "aeson-pretty",
        "ansi-terminal",
        "array",
        "base",
        "base-compat-batteries",
        "blaze-markup",
        "bytestring",
        "Cabal",
        "call-stack",
        "cassava",
        "cassava-megaparsec",
        "cmdargs",
        "containers",
        "cryptonite",
        "data-default",
        "Decimal",
        "directory",
        "extra",
        "file-embed",
        "filepath",
        "Glob",
        "hashtables",
        "megaparsec",
        "memory",
        "mtl",
        "old-time",
        "parsec",
        "parser-combinators",
        "pretty-simple",
        "regex-tdfa",
        "repline",
        "safe",
        "tabular",
        "tasty",
        "tasty-hunit",
        "template-haskell",
        "text",
        "time",
        "timeit",
        "transformers",
        "uglymemo",
        "unordered-containers",
        "utf8-string",
        "validation",
    ],
    snapshot = "nightly-2021-07-30",
    stack_snapshot_json = "//:stackage_snapshot.json",

    vendored_packages = {
        "csv": "@csv//:csv",
        "hledger-lib": "@hledger-lib//:hledger-lib",
    },
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")
haskell_register_ghc_nixpkgs(
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "9.0.1",
    attribute_path = "haskell.packages.ghc901.ghc"
)
