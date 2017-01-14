##----- Generate data examples

files <- list.files("inst/data/", pattern = ".csv")
files <- files[1:(length(files) - 1)]

for (f in files) {
  save(read.csv(file.path("inst", "data", f)),
       file = file.path("data", paste0(tools::file_path_sans_ext(f), ".rda")))
}
