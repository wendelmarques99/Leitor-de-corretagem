library(pdftools)
library(tidyverse)
library(reshape2)
library(tesseract)
library(progress)

# Arquivos --------------------------------------------------------------------
files <- ""

# Padrao regex CPF, CNPJ-----------------------------------------------------
padrao_cpf_cnpj <- "\\d{3}.\\d{3}.\\d{3}-\\d{2}|\\d{3}.\\d{3}.\\d{3}-\\d{2}|\\d{2}.\\d{3}.\\d{3}/0001-\\d{2}|\\d{3}.\\d{3} .\\d{3}-\\d{2}"

# Funcao ------------------------------------------------------------------
read_function <- function(x){
  
    # Content -----------------------------------------------------------------
    
    # progress bar
    pb$tick()$print()
    
    pdf_content <- pdftools::pdf_text(x)
    
    df_total <- pdf_content %>% 
      strsplit("\n") 
              
    by_page <- function(y){
        
      df_default <- y %>%   
        
       as_tibble %>%
        
      `colnames<-`("Sentenca")
    
      # CPF ---------------------------------------------------------------------
      cpf <- df_default %>% 
        filter(str_detect(Sentenca, padrao_cpf_cnpj)) %>%
        pluck(1) %>% 
        str_extract(., "[:alpha:].*") %>% 
        as_tibble() %>%
        `colnames<-`("Sentenca") %>%
        filter(!str_detect(Sentenca, "C.N.P.J.:")) %>% 
        pluck(1) %>% 
        str_extract(., padrao_cpf_cnpj) %>% 
        `[[`(1)
      
      # Name --------------------------------------------------------------------
      name <- df_default %>% 
        filter(str_detect(Sentenca, padrao_cpf_cnpj)) %>%
        pluck(1) %>% 
        str_squish() %>% 
        str_extract(., " .*[:alpha:]") %>% 
        as_tibble() %>%
        `colnames<-`("Sentenca") %>% 
        pluck(1) %>% 
        `[[`(2) %>% 
        str_squish()
      
      # stock Account ----------------------------------------------------------
      stock_Account <- df_default %>% 
        filter(str_detect(Sentenca, "000\\d{6}-\\d{1}")) %>% 
        pluck(1) %>% 
        str_extract(., "000\\d{6}-\\d{1}") %>% 
        `[[`(1)
      
      # Data pregao
      date <- df_default %>%
        filter(str_detect(Sentenca, "Data pregão:")) %>%
        pluck(1) %>%
        str_extract(., "\\d{2}/\\d{2}/\\d{4}") %>% 
        `[[`(1)
      
    
      # Padrao regex para operacoes ---------------------------------------------
      
      padrao_temp <- paste0("(?<=", "VIS|OPC|OPV|TER", " ).+?(?= [0-9]+ [0-9]+,)")
       
    
      # string padrao operacoes -------------------------------------------------
      
      string_operacao <- df_default %>%
        filter(str_detect(Sentenca, "1-Bovespa")) %>% 
        `[[`(1) %>% 
        str_squish() %>% 
        str_remove_all("\\.")
      
      ativo <- string_operacao %>% 
        str_extract(padrao_temp) %>% 
        str_squish() %>% 
        word(1)
       
      operacoes <- string_operacao %>% 
        str_remove(padrao_temp) %>% 
        as_tibble() %>% 
        pluck(1) %>% 
        str_squish() %>% 
        gsub(., pattern = "\\.", replacement =  "") %>% 
        gsub(., pattern = "\\,", replacement =  "\\.") %>% 
        reshape2::colsplit(., " ", names = c("Praca", "C/V", "Mercado","Quantidade", "Preco", "Valor operado", "D/C"))
      
     # Liquido (A+B) -----------------------------------------------------------
      liquido_a_b <- df_default %>% 
        filter(str_detect(Sentenca, "Liquido \\(")) %>% 
        pluck(1) %>% 
        str_extract(., "[0-9]+[.,]+[0-9]+.*|-[0-9]+[.,]+[0-9]+.*") %>% 
        str_replace("D|C", replacement = "") %>% 
        str_squish() %>% 
        gsub(.,pattern = "\\.", replacement =  "") %>%
        gsub(., pattern = "\\,", replacement =  "\\.") %>% 
        as.numeric() %>% 
      abs()
      
      # Corretagem ---------------------------------------------
      corretagem <- df_default %>% 
        filter(str_detect(Sentenca, "Valor das Operações")) %>% 
        pluck(1) %>% 
        str_squish() %>% 
        str_extract(., "(-|)[0-9]+[.,]+[0-9]+.*") %>%
        str_extract("Corretagem.*") %>% 
        str_replace(., "Corretagem", "") %>% 
        str_replace(., "C|D", "") %>% 
        str_squish() %>%
        gsub(.,pattern = "\\.", replacement =  "") %>%
        gsub(., pattern = "\\,", replacement =  "\\.") %>%
        as.numeric()
      
      # Valor liquido das operacoes ---------------------------------------------
      valor_liquido_operacoes <- df_default %>% 
        filter(str_detect(Sentenca, "Valor Líquido das Operações\\(")) %>% 
        pluck(1) %>% 
        str_squish() %>% 
        str_extract(., "Valor Líquido das Operações\\(.*") %>% 
        str_extract(., "[0-9]+[.,]+[0-9]+.*|-[0-9]+[.,]+[0-9]+.*") %>% 
        str_replace("D|C", replacement = "") %>% 
        str_squish() %>% 
        gsub(.,pattern = "\\.", replacement =  "") %>%
        gsub(., pattern = "\\,", replacement =  "\\.") %>% 
        as.numeric() %>% 
        abs()
    
      custos_extras <- abs(valor_liquido_operacoes - liquido_a_b) - corretagem
      
      df_output <- operacoes %>% 
        mutate(Nome = name, CPF = cpf,
               conta_bolsa = stock_Account, 
               data = date, 
               corretagem = corretagem, 
               valor_liquido_operacoes = valor_liquido_operacoes,
               custos_extras = custos_extras,
               ativos = ativo) %>% 
        select(-c(Praca, `D/C`))
      
      
      return(df_output)
      
    
    }
    

# Retorna df com todas pags -----------------------------------------------

    return(df_total %>% 
             map_dfr(.,by_page))
    
    
}

# Rodando todos os pdfs ---------------------------------------------------
pb <- progress_estimated(length(files))

BD_PDFS <- files %>% 
  map_dfr(., read_function) 

# Tirar f do final e tirar duplicadas -------------------------------------

exportar_bd <- BD_PDFS %>% 
  mutate(data = lubridate::dmy(data)) %>% 
  mutate(ativos = str_replace(BD_PDFS$ativos, pattern = "F$", replacement = "")) %>% 
  distinct()

# Parte final: Exportar em algum formato. Seja csv, xlsx... ---------------


