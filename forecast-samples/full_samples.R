forecasts_dir <- "forecasts-samples"
model_dirs <- list.dirs(forecasts_dir, full.names = TRUE, recursive = FALSE)

for (model_path in model_dirs) {
  
  model_name <- basename(model_path)
  
  # Localiza os arquivos bicar-cpi1 e bicar-cpi2 em cada pasta
  file1 <- list.files(model_path, pattern = "1\\.rda$", full.names = TRUE)
  file2 <- list.files(model_path, pattern = "2\\.rda$", full.names = TRUE)
  
  # 1. Carrega o primeiro e "pega" os dados
  name1 <- load(file1)      # Carrega e descobre que o nome do objeto é 'bcpi'
  data1 <- get(name1)       # Pega a tabela 'bcpi' e guarda em data1
  
  # 2. Carrega o segundo e "pega" os dados
  name2 <- load(file2)      # Carrega o bicar-cpi2
  data2 <- get(name2)       # Pega a tabela e guarda em data2
  
  # 3. Empilha os dados (Amostra 1 em cima da Amostra 2)
  forecast <- rbind(data1, data2)
  
  # 4. Define o nome final: ex: "ar_forecast.rda"
  file_name <- paste0(model_name, ".rda")
  output_path <- file.path("forecasts", file_name)
  
  # Salva o novo objeto unido
  save(forecast, file = output_path)
  
  # Limpeza opcional: remove os nomes carregados para evitar confusão no próximo loop
  rm(list = c(name1, name2))
  
  cat("  Modelo", model_name, "processado e salvo em:", output_path, "\n")
}

