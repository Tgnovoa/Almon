#almond para consumo cerveza
# rm(list = ls())
### cargar paqueterias que se usaran ----
# si alguna de las librerias no se carga (porque no esta instalada), picar el boton de Packages (normalmente en el lado inferior derecho de RStudio), picar el boton de Install y buscar el nombre del paquete que no se cargo, una vez instalado voler a cargar la paqueteria completa
library(tidyverse)
library(broom)
library(magrittr)
library(stringr)
library(lubridate)
library(data.table)
library(car)

### IMPORTANTE : Posicionarse en la carpeta donde se encuentra este script antes de correrlo dado que ahi se encuentran las bases 
# Riesgos\ABI Almon\Consumo Cerveza
setwd("C:/Users/gnovoa/Desktop/Santiago/Riesgos/Proyectos/Consumo Cerveza")

## lectura bases ----
# para cambiar de pais solo repetir los pasos que aparecen abajo, checar que la base del pais que se busca analizar se encuentre en la carpeta en que se esta trabajando
# hay 3 ejemplos de nombres de paises abajo, sin embargo, algunos otros son : arg, aus, can, chi, col, ecu, ger, ind, per, saf, tzn
# para saber que bases se encuentran en la carpeta usar dir() en la consola
archivos_carpeta <- dir()
archivos_carpeta <- archivos_carpeta[str_detect(archivos_carpeta, pattern = ".csv")]
archivos_carpeta   # los csv con la terminacion db son los datos poblacionales, los csv con la terminacion beer son los datos de consumo de cerveza

us <- read_csv("us_db.csv", col_types = list(dateid01 = col_date(format = "%d/%m/%Y")))
us <- us[,-ncol(us)]
us %>% glimpse()
us_beer <- read_csv("us_beer.csv", col_types = list(dateid01 = col_date(format = "%Y-%m-%d")), na = c("NA", "#N/A", "#NA"))
us_beer %>% glimpse()

arg <- read_csv("arg_db.csv", col_types = list(dateid01 = col_date()))
arg <- arg[,-ncol(arg)]
arg %>% glimpse()
arg_beer <- read_csv("arg_beer.csv", col_types = list(dateid01 = col_date(format = "%Y-%m-%d")), na = c("NA", "#N/A", "#NA"))
arg_beer %>% glimpse()

br <- read_csv("br_db.csv")
br %>% glimpse()
br <- br[,-ncol(br)]
br_beer <- read_csv("br_beer.csv", na = c("NA", "#N/A", "#NA"))

ch <- read_csv("chi_db.csv", col_types = list(dateid01 = col_date()))
ch <- ch[,-ncol(ch)]
ch %>% glimpse()
ch_beer <- read_csv("chi_beer.csv", col_types = list(dateid01 = col_date(format = "%Y-%m-%d")), na = c("NA", "#N/A", "#NA"))
ch_beer %>% glimpse()

ecu <- read_csv("ecu_db.csv", col_types = list(dateid01 = col_date()))
ecu <- ecu[,-ncol(ecu)]
ecu %>% glimpse()
ecu_beer <- read_csv("ecu_beer.csv", col_types = list(dateid01 = col_date(format = "%Y-%m-%d")), na = c("NA", "#N/A", "#NA"))
ecu_beer %>% glimpse()

mx <- read_csv("mx_db.csv")
mx %>% glimpse()
mx <- mx[,-ncol(mx)]
mx_beer <- read_csv("mx_beer.csv", na = c("NA", "#N/A", "#NA"))

## funciones ----
#correr el codigo de las funciones una vez para que estas se creen en la sesion

crear_Zit <- function(grado_pol = grado_pol, restriccion = restriccion, normalizar = normalizar, len_coh = len_coh, len_df = len_df, df_X = df_alm_aux[,-1], years_df = years_df){
  j_Zit <- matrix(data = NA_real_, nrow = len_coh, ncol = grado_pol + 1)
  j_Zit[,1] <- 1
  if(normalizar){
    j_Zit[,2] <- if(len_coh%%2 == 1){
      (-floor(len_coh/2)):(floor(len_coh/2))
    }else{
      (-(floor(len_coh/2)-1)):(floor(len_coh/2))
    } 
  }else{
    j_Zit[,2] <- 1:len_coh
  }
  if(grado_pol>1){
    for(i in 2:grado_pol){
      j_Zit[,i+1] <- j_Zit[,2]^i
    } 
  }
  aux_Zit <- matrix(data = NA_real_, nrow = len_df, ncol = grado_pol + 2)
  aux_Zit[,1] <- years_df
  aux_Zit[,-1] <- as.matrix(df_X) %*% as.matrix(j_Zit)
  aux_Zit <- as.data.frame(aux_Zit)
  Zit_names <- paste0("Z_", 0:grado_pol)
  names(aux_Zit) <- c("year", Zit_names)
  # aux_Zit %>% as.data.table()
  if(!restriccion == ""){
    if(restriccion == "both"){
      i_star <- (j_Zit[len_coh,grado_pol +1] -j_Zit[1,grado_pol+1])/(j_Zit[1,grado_pol] -j_Zit[len_coh,grado_pol])
      Zit <- matrix(data = NA_real_, nrow = len_df, ncol = grado_pol + 2 - 2)
      Zit[,1] <- years_df
      Zit[,2] <- aux_Zit[,grado_pol+2] + i_star*aux_Zit[,grado_pol + 1] - i_star*j_Zit[len_coh, grado_pol]*aux_Zit[,grado_pol] - j_Zit[len_coh, grado_pol+1] * aux_Zit[,grado_pol] 
      Zit <- Zit %>% as.data.frame()
      Zit_names <- paste0("Zrr", 0)
      names(Zit) <- c("year", Zit_names)
    }else{
      Zit <- matrix(data = NA_real_, nrow = len_df, ncol = grado_pol + 2 - 1)
      Zit[,1] <- years_df
      if(grado_pol==2){
        if(restriccion == "left"){
          Zit[,2] <- aux_Zit[,grado_pol+1] - j_Zit[1,grado_pol]*aux_Zit[,grado_pol] 
          Zit[,3] <- aux_Zit[,grado_pol+2] - j_Zit[1,grado_pol+1]*aux_Zit[,grado_pol]
        }else{
          Zit[,2] <- aux_Zit[,grado_pol+1] - j_Zit[len_coh,grado_pol]*aux_Zit[,grado_pol] 
          Zit[,3] <- aux_Zit[,grado_pol+2] - j_Zit[len_coh,grado_pol+1]*aux_Zit[,grado_pol]
        }
        Zit_names <- paste0("Zr_", 1:grado_pol)
        Zit <- Zit %>% as.data.frame()
        names(Zit) <- c("years", Zit_names)
      }else{
        if(restriccion == "left"){
          Zit[,2] <- aux_Zit[,grado_pol+2] - j_Zit[1,grado_pol+1]*aux_Zit[,grado_pol+1]
        }else{
          Zit[,2] <- aux_Zit[,grado_pol+2] - j_Zit[len_coh, grado_pol+1]*aux_Zit[,grado_pol+1]
        }
        Zit_names <- paste0("Zl_",0)
        Zit <- Zit %>% as.data.frame()
        names(Zit) <- c("years", Zit_names)
      }
    }
  }else{
    Zit <- aux_Zit
  }
  ret_Zit <- list(Zit = I(Zit), j_Zit = I(j_Zit), aux_Zit = I(aux_Zit))
}
alfa_a_betas <- function(alfa_almond = lm_almond$coefficients %>% as.matrix(), ind_betas = j_Zit, len_coh=len_coh, restriccion = restriccion, grado_pol = grado_pol){
  if(restriccion == ""){
    as.matrix(ind_betas) %*% as.matrix(alfa_almond[-1]) 
  }else{
    if(restriccion == "both"){
      i_star <- (ind_betas[len_coh,grado_pol+1]-ind_betas[1,grado_pol+1])/(ind_betas[1,grado_pol]-ind_betas[len_coh,grado_pol])
      a_2 <- alfa_almond[-1]
      a_1 <- a_2*i_star
      a_0 <- - a_1 * ind_betas[1,grado_pol] - a_2 * ind_betas[1, grado_pol+1]
      alfa <- c(a_0,a_1,a_2) %>% as.matrix()
    }else{
      if(grado_pol == 2){
        a_2 <- alfa_almond[grado_pol + 1]
        a_1 <- alfa_almond[grado_pol]
        if(restriccion == "left"){
          a_0 <- - a_1 * ind_betas[1, grado_pol] - a_2 * ind_betas[1, grado_pol + 1]
        }else{
          a_0 <- - a_1 * ind_betas[len_coh, grado_pol] - a_2 * ind_betas[len_coh, grado_pol + 1]
        }
        alfa <- c(a_0, a_1, a_2) %>% as.matrix()
      }else{
        a_1 <- alfa_almond[grado_pol+1]
        if(restriccion == "left"){
          a_0 <- - a_1 * ind_betas[1, grado_pol]
        }else{
          a_0 <- - a_1 * ind_betas[len_coh, grado_pol]
        }
        alfa <- c(a_0, a_1) %>% as.matrix()
      }
    }
    as.matrix(ind_betas) %*% alfa
  }
}
almond <- function(df_alm = us, cohortes = c(21:30,31,34,37,41,46,51,61,71), train_year = c(), train_pos = "last", transformacion = "log", grado_pol = 2, normalizar = T, y = us_beer$us_beer_mhl, ar1 = F, restriccion = "right"){
  ## checar parametros ----
  grado_pol <- max(grado_pol, 1, na.rm = T)
  if(grado_pol > 2){
    message("restricciones solo programadas para polinomios de grado menor a 3, no se usaran restricciones")
    restriccion <- ""
  }
  aux_y <- y
  if(!train_pos%in%c("first", "last")){
    message("train_pos solo puede tener las sig. opciones: \n first \n last")
    train_pos <- "first"
  }
  if(!restriccion%in%c("","left", "right", "both")){
    message("restriccion solo puede tener las sig. opciones: \n left \n right \n both \n \n No se usaran restricciones")
    restriccion <- ""
  }else{
    if(restriccion =="both" & grado_pol <2){
      message("Solo pueden haber 2 restricciones para polinomios de grado mayor a 1. \n No se usaran restricciones.")
      restriccion <- ""
    }
  }
  len_coh <- length(cohortes)
  len_df <- nrow(df_alm)
  years_df <- year(df_alm$dateid01)
  date_aux <- !is.na(y)
  if(train_pos == "first"){
    train_year <- min(train_year, 2015)
    date_aux <- (!is.na(aux_y) & (years_df <= train_year))
  }else{
    train_year <- max(train_year, 1962)
    date_aux <- (!is.na(aux_y) & (years_df >= train_year))
  }
  ## crear cohortes ----
  aux_names <- paste0("c_", cohortes, "-", lead(cohortes,1, default = "+"))
  mat_aux <- matrix(data = NA_real_, ncol = len_coh+1, nrow = len_df)
  mat_aux[,1] <- years_df
  coh_i_aux <- rep(0,len_coh)
  clave_p <- str_split(names(df_alm)[2],pattern = "_")[[1]][2]
  aux_names_df <- df_alm %>% names %>% gsub(pattern = paste0("t_", clave_p, "_"), replacement = "")
  j <- ncol(df_alm)
  for(i in len_coh:1){
    #for y rowSums    
    coh_i_aux[i] <- which(str_detect(string = cohortes[i], pattern = aux_names_df))
    mat_aux[,i+1] <- rowSums(df_alm[,coh_i_aux[i]:(j)])
    j <- coh_i_aux[i]-1
  }
  df_alm_aux <- mat_aux %>% as.data.frame()
  names(df_alm_aux) <- c("year", aux_names)
  # df_alm_aux %>% as.data.table()
  ## transformar datos ----
  if(transformacion == "log" | transformacion == "ln"){
    df_alm_aux <- data.frame(year = df_alm_aux$year,log(df_alm_aux[,-1]))
    names(df_alm_aux) <- c("year", aux_names %>% paste0("_LN"))
    aux_y <- log(aux_y)
  }
  # df_alm_aux %>% as.data.table()
  ## crear Zit (variables para almond) ----
  Zit_var <- crear_Zit(grado_pol = grado_pol, restriccion = restriccion, normalizar = normalizar, len_coh = len_coh, len_df = len_df, df_X = df_alm_aux[,-1], years_df = years_df)
  aux_Zit <- Zit_var$Zit
  # aux_Zit %>% as.data.table()
  ## hacer regresion ----
  aux_Zit$y <- aux_y
  f_almond <- paste0("y ~ ", paste(names(aux_Zit)[str_detect(names(aux_Zit), pattern = "Z")], collapse = " + ")) %>% as.formula()
  lm_almond <- lm(formula = f_almond, data = aux_Zit, subset = date_aux)
  ## crear lista de cosas que se regresaran ----
  alfa_almond <- lm_almond$coefficients %>% as.matrix()
  b_almond <- alfa_a_betas(alfa_almond = alfa_almond, ind_betas = Zit_var$j_Zit, len_coh = len_coh, restriccion = restriccion, grado_pol = grado_pol)
  rownames(b_almond) <- paste0("Beta_", cohortes, "-", lead(cohortes,1, default = "+"))
  aug_lm <- lm_almond %>% augment
  pred_almond <- predict(object = lm_almond, newdata = aux_Zit[!date_aux,])
  aux_Zit$pred[as.integer(names(pred_almond))] <- pred_almond
  aux_Zit$pred[date_aux] <- aug_lm$.fitted
  if(transformacion == "log" | transformacion == "ln"){
    aux_Zit$mhl <- exp(aux_Zit$y)
    aux_Zit$pred_mhl <- exp(aux_Zit$pred)
  }
  ret_almond <- list(X = I(df_alm_aux), Z = I(Zit_var),
                     Y = I(aux_Zit),
                     lm_alm = I(lm_almond), 
                     t_lm = I(lm_almond %>% tidy()), aug_lm = I(aug_lm), r_alm = I(lm_almond %>% glance()),
                     a_almond = I(alfa_almond), b_almond = I(b_almond))
  ## incluir factor autoregresivo ----
  if(ar1){
    durbin_watson <- durbinWatsonTest(lm_almond)
    print(durbin_watson)
    resid_almon <- data.frame(r = aug_lm$.resid, r_1 = lag(aug_lm$.resid , n = 1))
    res_lm <- lm(formula = as.formula("r ~ r_1-1"), data = resid_almon)
    res_ar1 <- tidy(res_lm)[["p.value"]] <= 0.1
    if(durbin_watson$p < 0.1 | res_ar1){
      aux_Zit$y_1 <- lag(aux_y, n = 1)
      f_almond_ar1 <- paste0("y ~ ", paste(names(aux_Zit)[str_detect(names(aux_Zit), pattern = "Z")], collapse = " + ")) %>%
        paste0(" + y_1") %>% 
        as.formula()
      lm_almond_ar1 <- lm(formula = f_almond_ar1, data = aux_Zit, subset = date_aux)
      aug_lm_ar1 <- lm_almond_ar1 %>% augment
      if(is.na(aux_Zit$y_1[which(date_aux)[1]])){
        date_aux[which(date_aux)[1]] <- F
      }
      aux_Zit$pred_ar1 <- rep(x = NA_real_, len_df)
      aux_Zit$pred_ar1[date_aux] <- aug_lm_ar1$.fitted
      ar_index <- which(date_aux)[length(which(date_aux))] + 1
      for(i in ar_index:len_df){
        aux_Zit$pred_ar1[i] <-  predict(object = lm_almond_ar1, newdata = aux_Zit[i,])
        if(i < len_df){
          aux_Zit$y_1[i+1] <- aux_Zit$pred_ar1[i] 
        }
      }
      alfa_almond_ar1 <- lm_almond_ar1$coefficients[-length(lm_almond_ar1$coefficients)] %>% as.matrix()
      b_almond_ar1 <- alfa_a_betas(alfa_almond = alfa_almond_ar1, ind_betas = Zit_var$j_Zit, len_coh = len_coh, restriccion = restriccion, grado_pol = grado_pol)
      rownames(b_almond_ar1) <- paste0("Beta_", cohortes, "-", lead(cohortes,1, default = "+"))
    }else{
      message("Las pruebas estadisticas (DW) no sugieren agregar un factor autorregresivo. \n")
      ar1 <- F
    }
    
    ret_almond <- list(X = I(df_alm_aux), Z = I(Zit_var), 
                       Y = I(aux_Zit),
                       lm_alm = I(lm_almond), 
                       t_lm = I(lm_almond %>% tidy()), aug_lm = I(aug_lm), r_alm = I(lm_almond %>% glance()),
                       a_almond = I(alfa_almond), b_almond = I(b_almond),
                       lm_alm_ar1 = I(lm_almond_ar1), 
                       t_lm_ar1 = I(lm_almond_ar1 %>% tidy()), aug_lm_ar1 = I(aug_lm_ar1), r_alm_ar1 = I(lm_almond_ar1 %>% glance()),
                       a_almond_ar1 = I(alfa_almond_ar1), b_almond_ar1 = I(b_almond_ar1))
  }
  
  cat("Cohortes : ","\n", paste(aux_names, "\n"),"\n", 
      "Regresion con datos de " ,  years_df[which(date_aux)[1]]," a "  ,  years_df[which(date_aux)[length(which(date_aux))]], "\n" ,
      "Transformacion: ", transformacion , "\n",
      "Grado polinomio: ", grado_pol, "\n",
      "Normalizar indices almond: ", normalizar, "\n" ,
      "Factor ar1: ", ar1, "\n",
      "Restricciones: ", restriccion)
  ret_almond
}
replicar_almond <- function(Z = almond_reg$Y, len_coh = length(cohortes), zrr0 = zrr0_ej, normalizar = normalizar){
  zrr0_db <- c(zrr0_ej, Z[,c(2:4)]) %>% as.data.frame()
  names(zrr0_db) <- c("y", "z0", "z1", "z2")
  zrr0_lm <- lm(formula = "y~. -1", data = zrr0_db)
  zrr0_lm %>% tidy()
  # grado_pol <- sum(str_detect(names(Z), pattern = "Z_"))-1
  # j_Zit <- matrix(data = NA_real_, nrow = len_coh, ncol = grado_pol + 1)
  # j_Zit[,1] <- 1
  # if(normalizar){
  #   j_Zit[,2] <- if(len_coh%%2 == 1){
  #     (-floor(len_coh/2)):(floor(len_coh/2))
  #   }else{
  #     (-(floor(len_coh/2)-1)):(floor(len_coh/2))
  #   } 
  # }else{
  #   j_Zit[,2] <- 1:len_coh
  # }
  # if(grado_pol>1){
  #   for(i in 2:grado_pol){
  #     j_Zit[,i+1] <- j_Zit[,2]^i
  #   } 
  # }
}

## llenar parametros ----
#cambiar los parametros para que la regresion se corra en el escenario correcto, no olvidar cambiar y <- por el pais que se busca analizar (es decir, cambiar tanto antes de $ como despues del mismo)
ls()
archivos_carpeta
df_alm <- br
y  <- br_beer$brl_beer_mhl     # ejemplo de cambio a us:  y <- us_beer$us_beer_mhl  (si se quiere saber como se llama la columna que se esta buscando, picar tab despues del simbolo $ una vez)
cohortes = c(18:30,31,34,37,41,46,51,61,71)
train_year = c(2010)
train_pos = "first"
date_aux <- if(train_pos == "last"){
  (!is.na(y) & (year(df_alm$dateid01) >= max(train_year,1962)))
}else{
  (!is.na(y) & (year(df_alm$dateid01) <= min(train_year,2015)))
} 
transformacion = "ln"
grado_pol = 2
normalizar = T
ar1 = T
restriccion = "both"
                   
                   

almond_reg <- almond(df_alm = df_alm, cohortes = cohortes, train_year = train_year, train_pos = train_pos,  transformacion = transformacion, grado_pol = grado_pol, normalizar = normalizar, y = y, ar1 = ar1, restriccion = restriccion)

almond_reg$t_lm
almond_reg$t_lm_ar1

almond_reg$b_almond %>% plot()
almond_reg$b_almond

almond_reg$b_almond_ar1 %>% plot()
almond_reg$b_almond_ar1
#



#graficas ----
pairs(x = as.matrix(almond_reg$Z$aux_Zit))
almond_reg
almond_reg$r_alm
almond_reg$r_alm_ar1
#residuales
hist_alm <- almond_reg$aug_lm %>%
  ggplot(aes(x = .resid)) + 
  geom_histogram(bins = 15, alpha = 0.7) + 
  xlim(c( -max(abs(almond_reg$aug_lm$.resid))-.01, max(abs(almond_reg$aug_lm$.resid))+.01)) + geom_vline(xintercept = mean(almond_reg$aug_lm$.resid), col = "red", size = 1.5, alpha = 0.5) +
  theme_bw()+ coord_flip() + xlab("residuales")
res_alm <- almond_reg$aug_lm %>%
  ggplot(aes(y = .resid, x = 1:nrow(almond_reg$aug_lm))) + 
  geom_hline(yintercept = 0, col = "midnightblue", lty= 2, size = 0.8, alpha = 1) + geom_hline(yintercept = mean(almond_reg$aug_lm$.resid), col = "red", size = 0.8, alpha = 0.5) +
  geom_point() + geom_line() + 
  xlab("index") + ylab("") + ylim(c( -max(abs(almond_reg$aug_lm$.resid))-.01, max(abs(almond_reg$aug_lm$.resid))+.01)) +
  theme_bw()
gridExtra::grid.arrange(hist_alm, res_alm, ncol = 2)

#proyecciones
# almond_reg$Y %>%
#   tidyr::gather(key = pred_real, value = valor, c(y,pred)) %>% 
#   ggplot(aes(x = year, y = exp(valor), colour = pred_real)) + 
#   theme_bw() +
#   geom_point(colour = "white") + 
#   geom_point(data = almond_reg$Y %>% 
#                dplyr::filter(!is.na(y)) %>%
#                tidyr::gather(key = pred_real, value = valor, c(y,pred)),alpha = 0.8, size = 1.5) + 
#   geom_line(data = almond_reg$Y %>% 
#               dplyr::filter(!is.na(y))%>%
#               tidyr::gather(key = pred_real, value = valor, c(y,pred)),alpha = 0.5, size = 0.9) + 
#   geom_point(data = almond_reg$Y %>% 
#                dplyr::filter(is.na(y))%>%
#                tidyr::gather(key = pred_real, value = valor, c(y,pred)),alpha = 0.8, size = 0.9, col = "skyblue") 
# #con ar1
# almond_reg$Y %>%
#   tidyr::gather(key = pred_real, value = valor, c(y,pred)) %>% 
#   ggplot(aes(x = year, y = exp(valor), colour = pred_real)) + 
#   theme_bw() +
#   geom_point(colour = "white") + 
#   geom_point(data = almond_reg$Y %>% 
#                dplyr::filter(!is.na(y))%>%
#                tidyr::gather(key = pred_real, value = valor, c(y,pred_ar1, pred)),alpha = 0.6, size = 1.5) + 
#   geom_line(data = almond_reg$Y %>% 
#               dplyr::filter(!is.na(y))%>%
#               tidyr::gather(key = pred_real, value = valor, c(y,pred, pred_ar1)),alpha = 0.4, size = 0.9) + 
#   geom_point(data = almond_reg$Y %>% 
#                dplyr::filter(is.na(y))%>%
#                tidyr::gather(key = pred_real, value = valor, c(y,pred)),alpha = 0.7, size = 1.5, col = "coral") +
#   geom_point(data = almond_reg$Y %>% 
#                dplyr::filter(is.na(y))%>%
#                tidyr::gather(key = pred_real, value = valor, c(y,pred_ar1)),alpha = 0.7, size = 1.5, col = "palegreen3")+ 
#   coord_fixed(ratio = 0.25)

compara_plot <- data.frame(years = almond_reg$Y$year,y = exp(almond_reg$Y$y),pred = exp(almond_reg$Y$pred),pred_ar1 = exp(almond_reg$Y$pred_ar1),sai = br_beer$brl_beer_mhlf)
compara_plot %>%
  tidyr::gather(key = modelo, value = pred, -years) %>%
  ggplot(aes(x = years, y = pred, colour = modelo)) +
  geom_point(alpha = 0.8, size = 2.5) + geom_line(alpha = 0.3, size = 1.5) + 
  theme_bw() + ylab("valores") #+ coord_fixed(ratio = 0.35)

#reverse ing ----
zrr0_ej <- read_csv("zrr0_us.csv", col_names = F)
zrr0_ej <- read_csv("zrr0_br.csv", col_names = F)
almond_reg$Y %>% replicar_almond(zrr0 = zrr0_ej,len_coh = length(cohortes) ,  normalizar = normalizar)


