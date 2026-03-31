forecasts_dir <- "forecasts"

model_dirs <- list.dirs(forecasts_dir, full.names = TRUE, recursive = FALSE)


for (model_path in model_dirs) {
  
  model_name <- basename(model_path)
  
  file1 <- list.files(model_path, pattern = "1\\.csv$", full.names = TRUE)
  file2 <- list.files(model_path, pattern = "2\\.csv$", full.names = TRUE)
  
  
  # Leitura
  first_sample  <- read.csv(file1, header = FALSE, sep = ";")
  second_sample <- read.csv(file2, header = FALSE, sep = ";")
  
  # Empilha
  full_sample <- rbind(first_sample, second_sample)
  
  # Salva
  file_name <- paste0(model_name, "_forecast.csv")
  output_path <- file.path(model_path,file_name)
  write.table(
    full_sample,
    file      = output_path,
    sep       = ";",
    row.names = FALSE,
    col.names = FALSE
  )
  
  cat("  Salvo em:", output_path, "\n")
}


