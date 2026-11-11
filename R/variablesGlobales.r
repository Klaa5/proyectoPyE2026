#variables globales, se reutilizan mucho

diasPorSemana <-5                                                     #por defecto de lun a vie

cantidadSemanas <- ceiling(max(MA$dia) /  diasPorSemana)                #cantidad de semanas, se adapta a los datos

cambioTurno <- 13 * 60                                                 #hora de pasaje de maniana a tarde, seteado por default a las 13HS

minutosInicio <- convertirAMinutos(MA$hora_inicio)                     #las horas de inicio de uso de estacionamiento pasadas a minutos

diaSemana <- factor((MA$dia - 1) %% diasPorSemana + 1, levels = 1:diasPorSemana)  #dia dentro de la semana (1 a x), se reinicia cada semana

turno <- factor(ifelse(minutosInicio < cambioTurno, "Manana", "Tarde"), levels = c("Manana", "Tarde"))
