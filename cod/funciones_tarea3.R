
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
