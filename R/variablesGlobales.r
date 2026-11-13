#variables globales, se reutilizan mucho

#Mod

diasPorSemana <-5                   #por defecto de lun a vie

horaCambioTurno <- 13             #el cambio de turno por defecto a las 13hs



#NMod

cantidadRegistros <-nrow(MA)          #total de reg

cantidadMultados <-sum(MA$multa == 1)   #total users multados

cambioTurno <- horaCambioTurno * 60                            #calculo basico y var en minutos para el cambio de turno

cantidadSemanas <- ceiling(max(MA$dia) /  diasPorSemana)                #calculo y var cantidad de semanas

minutosInicio <- convertirAMinutos(MA$hora_inicio)                     #horas de inicio de uso de estacionamiento pasadas a minutos

diaSemana <- factor((MA$dia - 1) %% diasPorSemana + 1, levels = 1:diasPorSemana)          #dia dentro de la semana (1 a x), se reinicia cada semana

nombresDias <- c("Lun", "Mar", "Mie", "Jue", "Vie")

tamanoMuestraAleatoria <- 1000
