# install_dependencies.R
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

if (!requireNamespace("ggparliament", quietly = TRUE)) {
  devtools::install_github("robwhickman/ggparliament")
}
