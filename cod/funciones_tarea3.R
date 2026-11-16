
# Recibe: un data frame, ventana y k.alerta.
# Devuelve: fecha, n.tx, media, desv y alerta.

alerta.movil <- function(df, ventana, k.alerta) {
  
  # Contar las transacciones por fecha
  diario <- aggregate(id_tx ~ fecha, data = df, FUN = length)
  
  names(diario) <- c("fecha", "n.tx")
  
  diario$fecha <- as.Date(diario$fecha)
  diario <- diario[order(diario$fecha), ]
  
  # Crear las columnas de resultados
  diario$media <- NA_real_
  diario$desv <- NA_real_
  diario$alerta <- FALSE
  
  # Evaluar cada día
  for (t in seq_len(nrow(diario))) {
    
    if (t > ventana) {
      
      anteriores <- diario$n.tx[(t - ventana):(t - 1)]
      
      diario$media[t] <- mean(anteriores)
      diario$desv[t] <- sd(anteriores)
      
      diario$alerta[t] <- diario$n.tx[t] >
        diario$media[t] + k.alerta * diario$desv[t]
      
    }
  }
  
  return(diario)
}


# Recibe: un vector de montos.
# Devuelve: la desviacion absoluta media de Benford.

benford.mad <- function(x) {
  
  x <- x[!is.na(x) & x > 0]
  
  digito <- floor(x / 10^floor(log10(x)))
  
  frecuencia <- table(factor(digito, levels = 1:9))
  
  p <- as.numeric(frecuencia) / length(x)
  
  benford <- log10(1 + 1 / (1:9))
  
  mad <- mean(abs(p - benford))
  
  return(mad)
}


# Recibe: transacciones, canal y catalogo de cantones.
# Devuelve: tabla por canton con n, MAD y clasificacion.

benford.canton <- function(df, canal, cantones) {
  
  datos.canal <- df[df$canal == canal &
                      !is.na(df$monto_crc) &
                      df$monto_crc > 0, ]
  
  grupos <- split(datos.canal$monto_crc,
                  datos.canal$cod_canton)
  
  resultado <- data.frame(
    cod_canton = as.integer(names(grupos)),
    n = lengths(grupos),
    mad = sapply(grupos, benford.mad)
  )
  
  resultado <- merge(resultado, cantones,
                     by = "cod_canton")
  
  resultado$clase <- ifelse(
    resultado$mad <= 0.006, "conformidad cercana",
    ifelse(resultado$mad <= 0.012, "conformidad aceptable",
           ifelse(resultado$mad <= 0.015, "conformidad marginal",
                  "no conformidad"))
  )
  
  resultado <- resultado[order(-resultado$mad),
                         c("cod_canton", "canton",
                           "n", "mad", "clase")]
  
  rownames(resultado) <- NULL
  
  return(resultado)
}

