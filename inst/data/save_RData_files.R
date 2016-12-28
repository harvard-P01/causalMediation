##----- Generate data examples

# files <- list.files("inst/data/", pattern = ".txt")

# "Mbin_int_data_10000.txt
Mbin_int_data_10000 <- read.csv("inst/data/Mbin_int_data_10000.txt")
save(Mbin_int_data_10000, file = "data/Mbin_int_data_10000.rda")

# "Mbin_int_data.txt"
Mbin_int_data <- read.csv("inst/data/Mbin_int_data.txt")
save(Mbin_int_data, file = "data/Mbin_int_data.rda")

# "Mbin_noint_data_10000.txt"
Mbin_noint_data_10000 <- read.csv("inst/data/Mbin_noint_data_10000.txt")
save(Mbin_noint_data_10000, file = "data/Mbin_noint_data_10000.rda")

# "Mbin_noint_data.txt"
Mbin_noint_data <- read.csv("inst/data/Mbin_noint_data.txt")
save(Mbin_noint_data, file = "data/Mbin_noint_data.rda")

# "Mcont_int_data_10000.txt"
Mcont_int_data_10000 <- read.csv("inst/data/Mcont_int_data_10000.txt")
save(Mcont_int_data_10000, file = "data/Mcont_int_data_10000.rda")

# "Mcont_int_data.txt"
Mcont_int_data <- read.csv("inst/data/Mcont_int_data.txt")
save(Mcont_int_data, file = "data/Mcont_int_data.rda")

# "Mcont_noint_data_10000.txt"
Mcont_noint_data_10000 <- read.csv("inst/data/Mcont_noint_data_10000.txt")
save(Mcont_noint_data_10000, file = "data/Mcont_noint_data_10000.rda")

# "Mcont_noint_data.txt"
Mcont_noint_data <- read.csv("inst/data/Mcont_noint_data.txt")
save(Mcont_noint_data, file = "data/Mcont_noint_data.rda")
