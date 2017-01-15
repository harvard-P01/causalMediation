##----- Generate package data from simulations

files <- list.files("inst/data/", pattern = ".csv")
files <- files[1:(length(files) - 1)]

for (f in files) {
  assign(tools::file_path_sans_ext(f), read.csv(file.path("inst", "data", f)))
  save(list = tools::file_path_sans_ext(f), file = file.path("data", paste0(tools::file_path_sans_ext(f), ".rda")))
}
