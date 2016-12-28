##----- Generate data examples

df <- read.csv("inst/data/Mbin_int_data_10000.txt")
save(df, file = "data/Mbin_int_data_10000.RData")

files <- list.files("inst/data/", pattern = ".txt")

for (f in files) {
  df <- read.csv(paste0("inst/data/", f))
  save(df, file = paste0("data/", tools::file_path_sans_ext(f), ".RData"))
}
