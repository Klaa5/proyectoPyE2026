source("R/data.r")
source("R/funciones.r")

#Proyecto de final de curso PyE 2026



#Con el archivo que contiene 10.000 registros correspondientes a 4 semanas de lunes a viernes,

#calcular:

#1. Valor promedio de las horas solicitadas de uso de estacioniento

    separador()

#2. Promedio de hora de inicio del uso del estacionamiento.

  datosHoras <- convertirAMinutos(MA$hora_inicio)

  promedio <- mean(datosHoras)

  datosFormateados <- formatoHoraMinutos(promedio)

  cat("1-2) Hora inicio promedio del uso del estacionamiento:", datosFormateados)
  separador()

#3. Promedio de las extensiones de hora, los multados por finalizacion de tiempo y los montos de las multas.

  #prom de ext de hora

    datosExtensionHora <- MA$extension_horas

    promedioExt <- mean(datosExtensionHora)

    cat("1-3a) Promedio de extensión de hora:", promedioExt)
    separador()


  #prom de multas por finalizacion

    datosMultas <- MA$multa
    promedioMulta <-mean(datosMultas)

    cat("1-3b) Promedio de multas por finalizacion de hora:", promedioMulta)
    separador()

  #prom de monto de multas

    datosMontoMultas <- MA$valor_multa
    promedioMontoMulta <-mean(datosMontoMultas)

    cat("1-3c) Promedio de monto de multa:", promedioMontoMulta)
    separador()

#4. Usuarios promedios por semana y por dia de la semana.

    diasDeLaSemana <-5      #lunes a vieenes por defecto

  #usuarios prom por semana

    usuariosPorSemana <- table(ceiling(MA$dia / 5))  #tabla con sumatoria de usuarios separados por semana
    promUsuariosSemana <- mean(usuariosPorSemana)

    cat("1-4a) Promedio de usuarios por semana:", promUsuariosSemana)
    separador()

  #usuarios prom por dia de semana


    diaSemana <- (MA$dia - 1) %%  diasDeLaSemana + 1            #dia dentro de la semana (1 a x), se reinicia cada semana
    nSemanas <- ceiling(max(MA$dia) /  diasDeLaSemana)          #cantidad de semanas, se adapta a los datos

    usuariosPorDiaSemana <- table(factor(diaSemana, levels = 1: diasDeLaSemana))  #total de usuarios de todos los lunes, martes, etc.
    promUsuariosDiaSemana <- usuariosPorDiaSemana / nSemanas

    cat("1-4b) Promedio de usuarios por dia de la semana: \n\nLunes:     ", promUsuariosDiaSemana[1], "\nMartes:    ", promUsuariosDiaSemana[2], "\nMiercoles: ", promUsuariosDiaSemana[3], "\nJueves:    ", promUsuariosDiaSemana[4], "\nViernes:   ", promUsuariosDiaSemana[5])
    separador()



#5. Luego realice los calculo discriminando dias de la semana, horario(manana/tarde)
#6. Realizar un muestreo sistemático uno de cada 10 datos, y hacer un intervalo de confianza para proporciones, para compararlo con la proporción poblacional de autos multados.
#7. Hacer un intervalo de confianza al 95 % para proporciones y determinar si representa el total de los datos.


#Se busca contestar:

#1. Es conveniente ofrecer planes para pagar online y que inicie a las 10:00 am.
#2. Ventajas y desventajas de ofrecer planes de 5 unidades por el precio de 4.
#3. Ventajas y desventajas de ofrecer planes de por 4 unidades 4 dias a las semana, te exoneran 4 unidades el 5to dia.
#4. Ventajas y desventajas de ofrecer planes de por 4 unidades 4 dias consecutivos, te exoneran 4 unidades el 5to dia/ otro dia cualquiera.
#5. Los datos de uso por dia siguen una distribucion normal?
#6. Proporcion de autos multados por total de usuarios, mensual, semanal y por dia de la semana.
#7. Estimar intervalos de recaudación por cada de los conceptos, con una confianza del 90 %, 95 % y 99 %.


