load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")

vendored_deps = {
  "csv": [
    "filepath",
    "parsec",
  ],
  "hledger-lib": [
    "aeson",
    "aeson-pretty",
    "ansi-terminal",
    "array",
    "base-compat-batteries",
    "blaze-markup",
    "call-stack",
    "cassava",
    "cassava-megaparsec",
    "cmdargs",
    "Decimal",
    "directory",
    "extra",
    "file-embed",
    "Glob",
    "hashtables",
    "megaparsec",
    "mtl",
    "old-time",
    "parser-combinators",
    "pretty-simple",
    "regex-tdfa",
    "safe",
    "tabular",
    "tasty",
    "tasty-hunit",
    "template-haskell",
    "time",
    "timeit",
    "transformers",
    "uglymemo",
    "unordered-containers",
    "utf8-string",
  ],
  "generic-data": [
    "ap-normalize",
    "base-orphans",
    "cabal-doctest",
    "contravariant",
    "ghc-boot-th",
    "show-combinators",
  ],
  "ixset-typed": [
    "base",
    "containers",
    "deepseq",
    "safecopy",
    "syb",
    "template-haskell",
  ],
}

def vendored_packages():
  """Load vendored repositories"""
  maybe(
    http_archive,
    "csv",
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
    urls = ["https://hackage.haskell.org/package/csv-0.1.2/csv-0.1.2.tar.gz"],
  )

  maybe(
    http_archive,
    "generic-data",
    build_file_content = """
load("@rules_haskell//haskell:cabal.bzl", "haskell_cabal_library")
load("@stackage//:packages.bzl", "packages")
haskell_cabal_library(
    name = "generic-data",
    version = packages["generic-data"].version,
    srcs = glob(["**"]),
    deps = packages["generic-data"].deps,
    visibility = ["//visibility:public"],
)
    """,
    strip_prefix = "generic-data-c69f38d472a0f7d03662984bb142e912b3109f58",
    sha256 = "3894013221d91712c89aca88d92756c8a93f65119949b743f5c2b67263ddc422",
    urls = ["https://github.com/Lysxia/generic-data/archive/c69f38d472a0f7d03662984bb142e912b3109f58.zip"],
  )

  maybe(
    nixpkgs_package,
    "hledger-lib",
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

  return {
      "csv": "@csv//:csv",
      "hledger-lib": "@hledger-lib//:hledger-lib",
      "generic-data": "@generic-data//:generic-data",
  }
