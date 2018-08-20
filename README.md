# Myst_lab1_JavierGomez
# - Laboratorio 1
#Remover todos los objetos del "enviroment"
rm(list = ls())
# los 0s aceptados antes de expresas una cifra en notación científica
options("scipen"=100, "digits"=4)
# cargar librerias a utilizar
suppressMessages(library(plotly)) #graficas interactivas
suppressMessages(library(Quandl)) #descargar precios
suppressMessages(library(PortfolioAnalytics))#teoria moderna de portafolios
suppressMessages(library(ROI))#optimizacion para portafolio
suppressMessages(library(knitr))#opciones de documentacion + codigo
suppressMessages(library(kableExtra))#tablas en html
options(knitr.table.format = "html")
# Cargar el token de QUANDL
Quandl.api_key("dN9QssXxzTxndaqKUQ_i")

# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Funcion para descargar N cantidad de activos desde QUANDL
  # -- Dependencias: QUANDL
  # -- Columns : columnas a incluir : character : c("date", "adj_close", ... )
  # -- Tickers : Tickers o claves de pizarra de los activos : character : "TSLA"
  # -- Fecha_In : Fecha Inicial : character : "2017-01-02"
  # -- Fecha_Fn : Fecha Final : character : "2017-08-02"
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable(code = "WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}
