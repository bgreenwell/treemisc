# Setup for tinytest framework
if (requireNamespace("tinytest", quietly = TRUE)) {
  home <- length(unclass(packageVersion("fastshap"))[[1L]]) == 4
  tinytest::test_package("tinytest", at_home = home)
}
