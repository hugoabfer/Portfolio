# LIBRERÍAS Y EXTRACCIÓN DE DATOS

setwd("C:/Users/hugo/OneDrive - Universidade de Santiago de Compostela/TFG")
WD <- getwd()
WD 
#install.packages("readxl")
library("readxl")
#install.packages("zoo")
library("zoo")
#install.packages("xts")
library("xts")
#install.packages("rmgarch")
library("rmgarch")
#install.packages("openxlsx")
library("openxlsx")
#install.packages("moments")
library("moments")
#install.packages("tseries")
library("tseries")

# LEER DATOS

datos_raw.df = read_excel("Rend.xlsx")
datos_raw.df = as.data.frame(datos_raw.df)
datos_raw.z = zoo(datos_raw.df[,-1], as.Date(datos_raw.df[,1], format="%Y-%m-%d"))
datos_raw.z = window(datos_raw.z, start = as.Date("2017-09-19"))
erix=datos_raw.z[, "R_ERIX", drop=FALSE]
itf=datos_raw.z[, "R_ITF", drop=FALSE]
wh=datos_raw.z[, "R_WH", drop=FALSE]
uno = itf-itf+1

## GRÁFICOS###

par(mfrow = c(2, 2))

plot(itf, main = "R_ERIX", col = "blue")
plot(erix, main = "R_ITF", col = "blue")
plot(wh, main = "R_WH", col = "blue")

par(mfrow = c(1, 1))

# 2. ESPECIFICACIÓN DE LOS MODELOS UNIVARIANTES 
spec_ITF <- ugarchspec(
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
  distribution.model = "std"
)

spec_ERIX <- ugarchspec(
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
  distribution.model = "std"
)

spec_WH <- ugarchspec(
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,2)), 
  distribution.model = "std"
)


# MODELOS BIVARIANTES DCC
# std, norm
# mvt, mvnorm
# ITF-ERIX
############ MODELO 1 #####################3
y.ret=merge(itf,erix)

uspec_mod1 <- multispec(c(spec_ITF, spec_ERIX))

dcc1.garch11.spec = dccspec(uspec = uspec_mod1,
                    dccOrder = c(1,1),
                    distribution = "mvt")


cl = makePSOCKcluster(10)
dcc1.fit = dccfit(dcc1.garch11.spec, data = y.ret, cluster = cl)
stopCluster(cl)
dcc1.fit
DCC1rho12 = rcor(dcc1.fit)[1,2,]*uno
#---------------------------

Hdcc1=dcc1.fit@mfit$H #esto es un array
h_dcc1_11=rcov(dcc1.fit)[1,1,]*uno
h_dcc1_22=rcov(dcc1.fit)[2,2,]*uno

# RESIDUOS ESTANDARIZADOS
stdr_dcc1=dcc1.fit@mfit$stdresid #esta es una matriz
stdr1_dcc1=dcc1.fit@mfit$stdresid[,1]*uno
stdr2_dcc1=dcc1.fit@mfit$stdresid[,2]*uno

# EFECTIVIDAD DE LA COBERTURA
beta_dcc1_12 = rcov(dcc1.fit)[1,2,] /rcov(dcc1.fit)[2,2,]*uno
head(beta_dcc1_12)
rcbeta_dcc1_12 = y.ret[,1] - beta_dcc1_12 * y.ret[,2]
ec_dcc1_12 = ( var(y.ret[,1]) - var(rcbeta_dcc1_12) ) / var(y.ret[,1])
ec_dcc1_12 


################### MODELO 2 ###################

y.ret_2 = merge(itf, wh)

uspec_mod2  <- multispec(c(spec_ITF, spec_WH))
dcc2.garch11.spec_2 = dccspec(uspec = uspec_mod2,
                              dccOrder = c(1,1),
                              distribution = "mvt")

cl = makePSOCKcluster(10)
dcc2.fit = dccfit(dcc2.garch11.spec_2, data = y.ret_2, cluster = cl)
stopCluster(cl)
dcc2.fit
DCC2rho12 = rcor(dcc2.fit)[1,2,]*uno

Hdcc2=dcc2.fit@mfit$H #esto es un array
h_dcc2_11=rcov(dcc2.fit)[1,1,]*uno
h_dcc2_22=rcov(dcc2.fit)[2,2,]*uno

# RESIDUOS ESTANDARIZADOS
stdr_dcc2=dcc2.fit@mfit$stdresid #esta es una matriz
stdr1_dcc2=dcc2.fit@mfit$stdresid[,1]*uno
stdr2_dcc2=dcc2.fit@mfit$stdresid[,2]*uno

# EFECTIVIDAD DE LA COBERTURA
beta_dcc2_12 = rcov(dcc2.fit)[1,2,] /rcov(dcc2.fit)[2,2,]*uno
head(beta_dcc2_12)
rcbeta_dcc2_12 = y.ret_2[,1] - beta_dcc2_12 * y.ret_2[,2]
ec_dcc2_12 = ( var(y.ret_2[,1]) - var(rcbeta_dcc2_12) ) / var(y.ret_2[,1])
ec_dcc2_12 


# PONDERACIONES, RATIOS Y EFECTIVIDAD
calcular_metricas_32 <- function(fit, returns_data) {
  # Extraer Varianzas y Covarianzas
  h11 <- rcov(fit)[1,1,] # Var Activo 1 (ITF)
  h22 <- rcov(fit)[2,2,] # Var Activo 2 (ERIX/WH)
  h12 <- rcov(fit)[1,2,] # Covarianza
  
  # 1. Ponderación Óptima (Minimal Variance Portfolio)
  w_star <- (h22 - h12) / (h11 - 2*h12 + h22)
  #w_star <- pmin(pmax(w_star, 0), 1) # Restricción sin posiciones cortas
  
  # 2. Ratio de Cobertura Óptimo (Hedge Ratio)
  beta_star <- h12 / h22
  
  # 3. Efectividad de la Cobertura (HE)
  # Rentabilidad de la cartera con cobertura: R_itf - beta * R_index
  ret_hedged <- returns_data[,1] - beta_star * returns_data[,2]
  he <- (var(returns_data[,1]) - var(ret_hedged)) / var(returns_data[,1])
  
  return(list(w = w_star, beta = beta_star, he = he))
}

# Ejecutar para ambos modelos
met_m1 <- calcular_metricas_32(dcc1.fit, y.ret)
met_m2 <- calcular_metricas_32(dcc2.fit, y.ret_2)

### ESTADÍSTICOS
# Función para sacar el resumen
resumen_stats <- function(v) {
  c(Media = mean(v), Max = max(v), Min = min(v), Desv = sd(v))
}

# Ver resultados en consola
print("--- ESTADÍSTICOS MODELO 1 (ITF/ERIX) ---")
rbind(Peso_ITF = resumen_stats(met_m1$w), 
      Hedge_Ratio = resumen_stats(met_m1$beta))

print("--- ESTADÍSTICOS MODELO 2 (ITF/WH) ---")
rbind(Peso_ITF = resumen_stats(met_m2$w),
      Hedhe_Ratio_2 = resumen_stats(met_m2$beta))

print("--- EFECTIVIDAD COBERTURA (HE) ---")
cat("Modelo 1:", met_m1$he, "\nModelo 2:", met_m2$he)


# GRÁFICO PONDERACIONES ÓPTIMAS (w*) 

# 1. Calculamos
w_m1_raw <- (rcov(dcc1.fit)[2,2,] - rcov(dcc1.fit)[1,2,]) / (rcov(dcc1.fit)[1,1,] - 2*rcov(dcc1.fit)[1,2,] + rcov(dcc1.fit)[2,2,])
w_m1 <- pmin(pmax(w_m1_raw, 0), 1) * uno 

w_m2_raw <- (rcov(dcc2.fit)[2,2,] - rcov(dcc2.fit)[1,2,]) / (rcov(dcc2.fit)[1,1,] - 2*rcov(dcc2.fit)[1,2,] + rcov(dcc2.fit)[2,2,])
w_m2 <- pmin(pmax(w_m2_raw, 0), 1) * uno

# 2. Convertimos a zoo 
w_zoo1 <- as.zoo(w_m1)
w_zoo2 <- as.zoo(w_m2)
index(w_zoo1) <- as.Date(index(w_zoo1))
index(w_zoo2) <- as.Date(index(w_zoo2))

# 3. Ahora el recorte de fechas funcionará perfectamente
fecha_inicio <- as.Date("2017-09-19")
fecha_fin <- as.Date("2025-11-03")
w_zoo1 <- window(w_zoo1, start = fecha_inicio, end = fecha_fin)
w_zoo2 <- window(w_zoo2, start = fecha_inicio, end = fecha_fin)
w_ambas <- merge(w_zoo1, w_zoo2)

# 4. 
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))

plot(w_zoo1, main = "Peso Óptimo ITF (ITF / ERIX)", 
     col = "blue", ylab = "", xlab = "", lwd = 1.5, bty = "l", xaxs = "i", ylim = c(0,1))
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted") 

plot(w_zoo2, main = "Peso Óptimo ITF (ITF / WH)", 
     col = "red", ylab = "", xlab = "", lwd = 1.5, bty = "l", xaxs = "i", ylim = c(0,1))
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

plot(w_ambas, screens = 1, main = "Comparativa de Pesos", 
     col = c("blue", "red"), ylab = "", xlab = "", lwd = 1.5, bty = "l", xaxs = "i", ylim = c(0,1))
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

plot.new() 
legend("center", legend = c("ITF / ERIX", "ITF / WH"), 
       col = c("blue", "blue"), lty = 1, lwd = 3, cex = 1.3, bty = "n") 

par(mfrow = c(1, 1))

#
# GRÁFICO 3 RATIOS DE COBERTURA (Beta*) 

# 1. Calculamos las Betas
beta_m1 <- (rcov(dcc1.fit)[1,2,] / rcov(dcc1.fit)[2,2,]) * uno
beta_m2 <- (rcov(dcc2.fit)[1,2,] / rcov(dcc2.fit)[2,2,]) * uno

# 2.
beta_zoo1 <- as.zoo(beta_m1)
beta_zoo2 <- as.zoo(beta_m2)
index(beta_zoo1) <- as.Date(index(beta_zoo1))
index(beta_zoo2) <- as.Date(index(beta_zoo2))

# 3. Recortamos a fechas
beta_zoo1 <- window(beta_zoo1, start = fecha_inicio, end = fecha_fin)
beta_zoo2 <- window(beta_zoo2, start = fecha_inicio, end = fecha_fin)
beta_ambas <- merge(beta_zoo1, beta_zoo2)

# 4. 
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))

plot(beta_zoo1, main = "Ratio de Cobertura Beta* (ITF / ERIX)", 
     col = "blue", ylab = "", xlab = "", lwd = 1.5, bty = "l", xaxs = "i")
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted") 

plot(beta_zoo2, main = "Ratio de Cobertura Beta* (ITF / WH)", 
     col = "red", ylab = "", xlab = "", lwd = 1.5, bty = "l", xaxs = "i")
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

plot(beta_ambas, screens = 1, main = "Comparativa de Betas", 
     col = c("blue", "red"), ylab = "", xlab = "", lwd = 1.5, bty = "l", xaxs = "i")
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

plot.new() 
legend("center", legend = c("ITF / ERIX", "ITF / WH"), 
       col = c("blue", "red"), lty = 1, lwd = 3, cex = 1.3, bty = "n") 

par(mfrow = c(1, 1))


# GRÁFICOS DE CORRELACIÓN CONDICIONAL (FORMATO 2x2 CON LÍMITES DE FECHAS)

# 1. Definimos los límites exactos del gráfico (Formato AÑO-MES-DÍA)
limites_fecha <- as.Date(c("2017-09-19", "2025-11-03"))

# Convertimos a "zoo" 
cor_zoo1 <- as.zoo(DCC1rho12)
cor_zoo2 <- as.zoo(DCC2rho12)

# Juntamos las dos en una sola tabla
cor_ambas <- merge(cor_zoo1, cor_zoo2)

# 2. Configuramos la ventana 2x2 (2 filas, 2 columnas)
par(mfrow = c(2, 2), mar = c(3, 3, 3, 1))

# --- Cuadrante 1 (Arriba Izda): Modelo 1 ---
# Añadimos xlim = limites_fecha
plot(cor_zoo1, main = "ITF&ERIX", 
     col = "blue", ylab = "", xlab = "", lwd = 1.5, bty = "l",
     xlim = limites_fecha)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted") 

# --- Cuadrante 2 (Arriba Dcha): Modelo 2 ---
plot(cor_zoo2, main = "ITF&WH", 
     col = "red", ylab = "", xlab = "", lwd = 1.5, bty = "l",
     xlim = limites_fecha)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# --- Cuadrante 3 (Abajo Izda): Combinado ---
plot(cor_ambas, screens = 1, main = "Comparativa", 
     col = c("blue", "red"), ylab = "", xlab = "", lwd = 1.5, bty = "l",
     xlim = limites_fecha)
grid(nx = NA, ny = NULL, col = "lightgray", lty = "dotted")

# --- Cuadrante 4 (Abajo Dcha): Leyenda 
plot.new() 
legend("center", legend = c("ITF&ERIX", "ITF&WH"), 
       col = c("blue", "red"), lty = 1, lwd = 3, 
       cex = 1.3, bty = "n") 

# Restauramos la ventana a 1x1 para futuros gráficos
par(mfrow = c(1, 1))

########### TABLA DE RESIDUOS, DATOS ##################
# 2. Creamos la función para que no tengas que escribir el test 4 veces
diagnostico_residuos <- function(residuos_serie) {
  # Nos aseguramos de que R lo trate como una serie de números limpios
  res <- as.numeric(residuos_serie) 
  
  cat("Asimetría (Skewness): ", skewness(res), "\n")
  cat("Curtosis (Kurtosis): ", kurtosis(res), "\n")
  print(jarque.bera.test(res))
  
  cat("Ljung-Box Q(10): \n")
  print(Box.test(res, lag = 10, type = "Ljung-Box"))
  
  cat("Ljung-Box Q2(10): \n")
  print(Box.test(res^2, lag = 10, type = "Ljung-Box"))
  cat("---------------------------------------------------\n")
}

# 3. Aplicamos los tests a las variables 
print("--- DIAGNÓSTICO ITF (MOD 1) ---")
diagnostico_residuos(stdr1_dcc1)

print("--- DIAGNÓSTICO ERIX (MOD 1) ---")
diagnostico_residuos(stdr2_dcc1)

print("--- DIAGNÓSTICO ITF (MOD 2) ---")
diagnostico_residuos(stdr1_dcc2)

print("--- DIAGNÓSTICO WH (MOD 2) ---")
diagnostico_residuos(stdr2_dcc2)

# GRÁFICOS DE AUTOCORRELACIÓN (ACF) DE LOS RESIDUOS

# 1. ACF de los Residuos Estandarizados (autocorrelación lineal)
par(mfrow = c(2, 2)) # Ventana de 2 filas y 2 columnas

acf(as.numeric(stdr1_dcc1), ylim = c(-0.05,0.05), xlim = c(1, 10), main = "ACF Residuos: ITF (ITF&ERIX)", ylab = "Autocorrelación")
acf(as.numeric(stdr2_dcc1), ylim = c(-0.05,0.05), xlim = c(1, 10), main = "ACF Residuos: ERIX (ITF&ERIX)", ylab = "Autocorrelación")
acf(as.numeric(stdr1_dcc2), ylim = c(-0.05,0.05), xlim = c(1, 10), main = "ACF Residuos: ITF (ITF&WH)", ylab = "Autocorrelación")
acf(as.numeric(stdr2_dcc2), ylim = c(-0.05,0.05), xlim = c(1, 10), main = "ACF Residuos: WH (ITF&WH)", ylab = "Autocorrelación")

par(mfrow = c(1, 1)) # Restaurar la vista normal


# 2. ACF de los Residuos al Cuadrado (Comprueba volatilidad / Efecto ARCH residual)
par(mfrow = c(2, 2))

acf(as.numeric(stdr1_dcc1)^2, ylim = c(-0.05,0.05), xlim = c(1, 10), main = "ACF Residuos al Cuadrado: ITF (ITF&ERIX)", ylab = "Autocorrelación")
acf(as.numeric(stdr2_dcc1)^2, ylim = c(-0.05,0.05), xlim = c(1, 10),  main = "ACF Residuos al Cuadrado: ERIX (ITF&ERIX)", ylab = "Autocorrelación")
acf(as.numeric(stdr1_dcc2)^2, ylim = c(-0.05,0.05), xlim = c(1, 10), main = "ACF Residuos al Cuadrado: ITF (ITF&WH)", ylab = "Autocorrelación")
acf(as.numeric(stdr2_dcc2)^2, ylim = c(-0.05,0.05), xlim = c(1, 10), main = "ACF Residuos al Cuadrado: WH (ITF&WH)", ylab = "Autocorrelación")

par(mfrow = c(1, 1)) 


#########################
# 1. Recuperamos  especificación combinada exacta del Modelo 1
uspec_combinado_1 <- multispec(c(spec_ITF, spec_ERIX))
dcc1.spec <- dccspec(uspec = uspec_combinado_1, 
                     dccOrder = c(1,1), 
                     distribution = "mvt")

# 2. ESTIMACIÓN ROLLING 
# - forecast.length = 500 : Dejamos los últimos 500 días (aprox 2 años) para la prueba real
# - refit.every = 22 : Recalculamos los parámetros matemáticos cada mes de cotización
# - refit.window = "moving" : Usamos ventana móvil (tiramos los datos viejos)
dcc1.roll <- dccroll(dcc1.spec, data = y.ret, n.ahead = 1, 
                     forecast.length = 500, refit.every = 22, 
                     refit.window = "moving")

# Ver el resumen de la prueba
show(dcc1.roll)


# EXTRACCIÓN Y GRÁFICO DE LA CORRELACIÓN DINÁMICA

# Extraemos la correlación predicha por el modelo móvil
roll_cor_12 = rcor(dcc1.roll)[1,2,]

# Convertimos a serie temporal (xts) para que reconozca bien las fechas
roll_cor_xts <- as.xts(roll_cor_12)

# GRÁFICO
plot(roll_cor_xts, 
     main = "Evolución Dinámica de la Correlación (ITF - ERIX)", 
     ylab = "Correlación Condicional Predicha", 
     col = "darkblue", 
     lwd = 2)
