# 1. LIBRERÍAS Y PREPARACIÓN DE DATOS

library(readxl)
library(xts)
library(rmgarch)
# 
# Leer datos
datos_raw <-
# "C:/Users/hugo/OneDrive - Universidade de Santiago de Compostela/TFG/garch.xlsx"

# Extraer fechas y crear objeto xts ordenado y sin NAs
fechas <- as.Date(datos_raw$...1)
dat ´ os_xts <- xts(datos_raw[, c("R_ITF", "R_ERIX", "R_WH")], order.by = fechas)
datos_xts <- na.omit(datos_xts)
fechas_plot <- index(datos_xts) # Guardamos las fechas limpias para los gráficos

# 2. ESPECIFICACIÓN DE LOS MODELOS UNIVARIANTES (Dejamos que R estime)
spec_ITF <- ugarchspec(
  mean.model = list(armaOrder = c(1,0), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
  distribution.model = "norm"
)

spec_ERIX <- ugarchspec(
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
  distribution.model = "norm"
)

spec_WH <- ugarchspec(
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
  variance.model = list(model = "sGARCH", garchOrder = c(1,2)), 
  distribution.model = "norm"
)
# Solver y test (ACP de los residuos, etc.)
fit_test <- ugarchfit(spec = spec_WH, data = datos_xts$R_WH, solver = "hybrid")

print(fit_test)


# 3. MODELO BIVARIANTE 1: FINTECH vs EUROPA (ITF & ERIX)
mspec_EU <- multispec(list(spec_ITF, spec_ERIX))
dcc.spec_EU <- dccspec(uspec = mspec_EU, dccOrder = c(1, 1), distribution = "mvnorm")
dcc.fit_EU <- dccfit(dcc.spec_EU, data = datos_xts[, c("R_ITF", "R_ERIX")])
# Extraemos covarianzas de este modelo (ITF es 1, ERIX es 2)
cov_EU <- rcov(dcc.fit_EU)
h11_EU <- cov_EU[1,1,] # Varianza Fintech en el modelo europeo
h22_EU <- cov_EU[2,2,] # Varianza Europa
h12_EU <- cov_EU[1,2,] # Covarianza Fintech-Europa

# Calculamos Kroner & Ng
beta_europa <- h12_EU / h22_EU
w_ITF_EU <- (h22_EU - h12_EU) / (h11_EU - 2*h12_EU + h22_EU)
w_ITF_EU_restric <- pmin(pmax(w_ITF_EU, 0), 1)


# 4. MODELO BIVARIANTE 2: FINTECH vs USA (ITF & WH)
mspec_USA <- multispec(list(spec_ITF, spec_WH))
dcc.spec_USA <- dccspec(uspec = mspec_USA, dccOrder = c(1, 1), distribution = "mvnorm")
dcc.fit_USA <- dccfit(dcc.spec_USA, data = datos_xts[, c("R_ITF", "R_WH")])

# Extraemos covarianzas de este modelo (ITF es 1, WH es 2)
cov_USA <- rcov(dcc.fit_USA)
h11_USA <- cov_USA[1,1,] # Varianza Fintech en el modelo americano
h22_USA <- cov_USA[2,2,] # Varianza USA
h12_USA <- cov_USA[1,2,] # Covarianza Fintech-USA

# Calculamos Kroner & Ng
beta_usa <- h12_USA / h22_USA
w_ITF_USA <- (h22_USA - h12_USA) / (h11_USA - 2*h12_USA + h22_USA)
w_ITF_USA_restric <- pmin(pmax(w_ITF_USA, 0), 1)


# 5. MOSTRAR RESULTADOS 
cat("\n\n======================================================\n")
cat("RESULTADOS MODELO 1: FINTECH vs EUROPA (Para la Tabla)\n")
cat("======================================================\n")
print(dcc.fit_EU)

cat("\n\n======================================================\n")
cat("RESULTADOS MODELO 2: FINTECH vs USA (Para la Tabla)\n")
cat("======================================================\n")
print(dcc.fit_USA)


# ==============================================================================
# 6. GRÁFICOS COMPARATIVOS CON FECHAS
# ==============================================================================
try(dev.off(), silent=TRUE) 
par(mfrow=c(2,1)) 

# Gráfico 1: Betas (Cobertura)
plot(fechas_plot, beta_europa, type="l", col="blue", lwd=1.5,
     main="Ratio de Cobertura Óptimo (Beta): Europa vs USA",
     ylab="Beta", xlab="Años", 
     ylim=range(c(beta_europa, beta_usa, na.rm=TRUE))) 
lines(fechas_plot, beta_usa, col="red", lwd=1.5, lty=1)
legend("topright", legend=c("Cobertura con Europa", "Cobertura con USA"), 
       col=c("blue", "red"), lty=1, bty="n", cex=0.8)

# Gráfico 2: Pesos en Fintech
plot(fechas_plot, w_ITF_EU_restric, type="l", col="darkblue", lwd=1.5,
     main="Peso Óptimo en Fintech (w_t): Cartera EU vs Cartera USA",
     ylab="Peso en Fintech", xlab="Años", ylim=c(0,1)) 
lines(fechas_plot, w_ITF_USA_restric, col="darkred", lwd=1.5, lty=2)
legend("bottomright", legend=c("Cartera ITF + Europa", "Cartera ITF + USA"), 
       col=c("darkblue", "darkred"), lty=c(1,2), bty="n", cex=0.8)