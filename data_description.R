setwd("~/reuma")
library(readxl)
library(dplyr)
library(nortest)
library(tibble)
library(plyr)
library(rstatix)
library(writexl)

############## Funciones ##############
calc_singrupo<-function(x,list) {
  # Dataframe with result
  empty_df <- data.frame(
    column = character(),
    total = numeric(),
    stat = character(),
    medida = numeric(),
    sd = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (i in 2:ncol(x)){ # Quantitative data
    if(i==2 | i==3){
      column_quali<-paste0(names(x[i]),'*')
      total_quali<-sum(!is.na(x[[i]]))
      stat_quali<-'Frec. Absolute/Relative'
      medida_quali<-table(x[[i]])
      sd_quali<-(prop.table(table(x[[i]])))*100
      
      res <- data.frame(
        column=column_quali,
        total=total_quali,
        stat=stat_quali,
        freqAbs=medida_quali,
        freqRel=sd_quali,
        stringsAsFactors = TRUE
      )
      empty_df<-rbind.fill(empty_df,res)
      
      
    } else {
      # print('Quantitative')
      if(names(x[i]) %in% list) { # Filter list (in this case, markers with less than 4 values. Non parametric statistic)
        stat<-"Mediana"
        medida<-round(median(x[[i]],na.rm = T),2)
      } else {
        if (lillie.test(x[[i]])$p.value<0.05) { # detect normal distribution
          stat<-"Mediana"
          medida<-round(median(x[[i]],na.rm = T),2)
        } else {
          stat<-"Media"
          medida<-round(mean(x[[i]],na.rm = T),2)
        }
      }
      sd<-round(sd(x[[i]],na.rm = T),2)
      column<-names(x[i])
      total<-sum(!is.na(x[[i]]))
      
      res2 <- data.frame(
        column,
        total,
        stat,
        medida,
        sd,
        stringsAsFactors = FALSE
      )
      empty_df<-rbind.fill(empty_df,res2)
      
    }
  }
  return(empty_df)
}

### By groups in this case micro
calculate_stat <- function(x) {
  n_valid <- sum(!is.na(x))
  
  if (n_valid <= 4) {
    return(c(total=n_valid,stat = "Mediana",medida=round(median(x, na.rm = TRUE), 2), sd = round(sd(x, na.rm = TRUE), 2)))
  } else {
    test_result <- lillie.test(x)
    
    if (test_result$p.value < 0.05) {
      return(c(total=n_valid,stat = "Mediana",medida= round(median(x, na.rm = TRUE), 2), sd = round(sd(x, na.rm = TRUE), 2)))
    } else {
      return(c(total=n_valid,stat = "Media",medida= round(mean(x, na.rm = TRUE), 2), sd = round(sd(x, na.rm = TRUE), 2)))
    }
  }
}

calculate_factor <- function(p) {
  freq<-table(p)
  absFreq<-as.integer(freq)
  relFreq<-round(100*prop.table(freq),2)
  return(list(c(AbsFreq=absFreq,RelFreq=relFreq)))
}

filtrar_columnas_menos4 <- function(df, threshold = 5) {
  filtered_df <- df[, sapply(df, function(col) sum(!is.na(col)) < threshold)]
  return(filtered_df)
}

filtrar_columnas_mas4 <- function(df, threshold = 5) {
  filtered_df <- df[, sapply(df, function(col) sum(!is.na(col)) > threshold)]
  return(filtered_df)
}

# Cargamos los grupos
grupos <- read_excel("metadata.xlsx", range = "A1:G61")
grupos <- grupos %>% select('cod','micro','sex','age','uric_acid',"leuco")

grupos$cod<-substr(grupos$cod,7,8) %>% gsub("^0","",.) %>% paste0('m',.) # Unify codes

# Cargamos las tablas
df <- read.csv("data.csv", sep=";")
df<-df %>% select(-micro)

############## Descriptivo sin agrupar ##############

combinacion<-full_join(grupos,df)

counts<-apply(combinacion, 2, function(x) sum(!is.na(x))<=4)
df<-names(combinacion[,counts])

calculo_final<-calc_singrupo(combinacion,df)
calculo_final$freqRel.Freq<-round(calculo_final$freqRel.Freq,2)

write.table(calculo_final,"descriptive.csv",sep=";",na="",row.names = F)

############## Descriptivo agrupado ##############
# Agrupar por la columna categórica y calcular las estadísticas redondeadas
combinacion$sex<-as.factor(combinacion$sex)
summary_stats <- combinacion %>%
  group_by(micro) %>%
  reframe(across(where(is.numeric), ~calculate_stat(.x), .names = "{.col}"))

summary_stats_factor <- combinacion %>%
  group_by(micro) %>%
  reframe(across(where(is.factor), ~calculate_factor(.x), .names = "{.col}")) %>%
  unnest(cols = everything())

summary_stats<- select(summary_stats,-micro)
summary_stats_final<-cbind(summary_stats_factor,summary_stats)

nombre_columnas<-rep(c("total","stat","medida","sd"),3)
nombre_columnas_factor<-rep(c("freqAbs0","freqAbs1","freqRel0","freqRel1"),3)

summary_stats_final$nombre_columnas<-nombre_columnas
summary_stats_final$nombre_columnas_factor<-nombre_columnas_factor

summary_stats2 <- summary_stats_final %>% select(micro, nombre_columnas_factor, sexo, nombre_columnas, everything()) %>% unnest(cols = everything()) %>% as.data.frame()

write.table(summary_stats2,"descriptiveByGroupMicro.csv",sep=";",na="",row.names = F)
