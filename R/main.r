source("R/data.r")
source("R/funciones.r")
source("R/variablesGlobales.r")

#Proyecto de final de curso PyE 2026
separador()
cat("    Proyecto de final de curso PyE 2026")

#Con el archivo que contiene 10.000 registros correspondientes a 4 semanas de lunes a viernes,

#calcular:

#1. Valor promedio de las horas solicitadas de uso de estacioniento

    separador()

    ##Pendiente, no se encuentran datos de horas solicitadas.
    cat("1-1) Promedio de horas solicitadas: Pendiente")
    separador()

#2. Promedio de hora de inicio del uso del estacionamiento.

  promedio <- mean(minutosInicio)

  datosFormateados <- formatoHoraMinutos(promedio)

  cat("1-2) Hora inicio promedio del uso del estacionamiento:", datosFormateados)
  separador()

#3. Promedio de las extensiones de hora, los multados por finalizacion de tiempo y los montos de las multas.

  #prom de ext de hora

    sumaExtensiones <- sum(MA$extension_horas)

    promedioExt <- sumaExtensiones / cantidadRegistros

    cat("1-3a) Promedio de extensión de hora:", promedioExt, "(aprox", promedioExt * 30, "minutos por usuario )")
    separador()


  #prom de multas por finalizacion

    promedioMulta <-cantidadMultados / cantidadRegistros

    cat("1-3b) Promedio de multas por finalizacion de hora:", promedioMulta, " o ", promedioMulta*100, "%")
    separador()

  #prom de monto de multas

    sumaMontos <- sum(MA$valor_multa)
    promedioMontoMulta <- sumaMontos / cantidadMultados

    cat("1-3c) Promedio de monto de multa: $",promedioMontoMulta)
    separador()

#4. Usuarios promedios por semana y por dia de la semana.


  #usuarios prom por semana

    promUsuariosSemana <- cantidadRegistros / cantidadSemanas

    cat("1-4a) Promedio de usuarios por semana:", promUsuariosSemana)
    separador()

  #usuarios prom por dia de semana

    usuariosPorDiaSemana <- numeric(diasPorSemana)  #vector vacio con espacios para cada semana

    for(diaIterado in 1:diasPorSemana)
    {
      usuariosPorDiaSemana[diaIterado] <- sum(diaSemana == diaIterado)  #suma dias correspondients a cada semana iterada (d)
    }

    promUsuariosDiaSemana <- usuariosPorDiaSemana / cantidadSemanas

    cat("1-4b) Promedio de usuarios por dia de la semana: \n\nLunes:     ", promUsuariosDiaSemana[1], "\nMartes:    ", promUsuariosDiaSemana[2], "\nMiercoles: ", promUsuariosDiaSemana[3], "\nJueves:    ", promUsuariosDiaSemana[4], "\nViernes:   ", promUsuariosDiaSemana[5])

    graficarPorDiaSemana(promUsuariosDiaSemana, "Usuarios promedio por dia")

    separador()



#5. Luego realice los calculo discriminando dias de la semana, horario(manana/tarde)

    totalesDiaTurno <- matrix(0, nrow = diasPorSemana, ncol = 2)   #filas = dia, columnas: 1 = manana, 2 = tarde

    for (diaIterado in 1:diasPorSemana)
    {
      totalesDiaTurno[diaIterado, 1] <- sum(diaSemana == diaIterado & minutosInicio < cambioTurno)    #si es maniana se almacena en la columna 1 de la matriz si es tarde en la 2
      totalesDiaTurno[diaIterado, 2] <- sum(diaSemana == diaIterado & minutosInicio >= cambioTurno)
    }

    promUsuariosDiaTurno <- totalesDiaTurno / cantidadSemanas

    cat("1-5a) Promedio de usuarios por dia de la semana (Maniana): \n \nLunes:     ", promUsuariosDiaTurno[1, 1], "\nMartes:    ", promUsuariosDiaTurno[2, 1],"\nMiercoles: ", promUsuariosDiaTurno[3, 1], "\nJueves:    ", promUsuariosDiaTurno[4, 1], "\nViernes:   ", promUsuariosDiaTurno[5, 1])

    #el 2 en la casilla 2 = tarde, 1 = maniana
    cat("\n\n1-5b) Promedio de usuarios por dia de la semana (Tarde): \n\nLunes:     ", promUsuariosDiaTurno[1, 2], "\nMartes:    ", promUsuariosDiaTurno[2, 2],"\nMiercoles: ", promUsuariosDiaTurno[3, 2],"\nJueves:    ", promUsuariosDiaTurno[4, 2],"\nViernes:   ", promUsuariosDiaTurno[5, 2])

    alturaMaxGrafica <- 300
    graficarPorDiaSemana(promUsuariosDiaTurno[, 1], "Usuarios promedio en la mañana", alturaMaxGrafica)
    graficarPorDiaSemana(promUsuariosDiaTurno[, 2], "Usuarios promedio en la tarde", alturaMaxGrafica)

    separador()


#6. Realizar un muestreo sistematico uno de cada 10 datos, y hacer un intervalo de confianza para proporciones, para compararlo con la proporcion poblacional de autos multados.



#7. Hacer un intervalo de confianza al 95 % para proporciones y determinar si representa el total de los datos.



#Se busca contestar:

#1. Es conveniente ofrecer planes para pagar online y que inicie a las 10:00 am.
#2. Ventajas y desventajas de ofrecer planes de 5 unidades por el precio de 4.
#3. Ventajas y desventajas de ofrecer planes de por 4 unidades 4 dias a las semana, te exoneran 4 unidades el 5to dia.
#4. Ventajas y desventajas de ofrecer planes de por 4 unidades 4 dias consecutivos, te exoneran 4 unidades el 5to dia/ otro dia cualquiera.
#5. Los datos de uso por dia siguen una distribucion normal?
#6. Proporcion de autos multados por total de usuarios, mensual, semanal y por dia de la semana.
#7. Estimar intervalos de recaudación por cada de los conceptos, con una confianza del 90 %, 95 % y 99 %.


