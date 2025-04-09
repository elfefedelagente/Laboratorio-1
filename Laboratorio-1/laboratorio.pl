requiere(repartir_urbano, [fuerza]).
requiere(repartir_larga_distancia, [fuerza, movilidad]).
requiere(clasificar_paquetes, [disponibilidad_horaria]).
requiere(realizar_logistica, [logistica]).
requiere(atencion_publico, [buena_atencion]).

% Empleados y sus capacidades
empleado(carlos, [disponibilidad_horaria, fuerza]).
empleado(marisa, [fuerza, buena_atencion]).
empleado(juan, [movilidad, disponibilidad_horaria]).
empleado(jimena, [logistica, disponibilidad_horaria]).
empleado(hector, [buena_atencion, logistica, fuerza]).
empleado(betty, [movilidad, buena_atencion, disponibilidad_horaria]).
empleado(lucia, [buena_atencion]).
empleado(axel, [fuerza, movilidad]).
empleado(eva, [movilidad, logistica]).
empleado(miguel, [buena_atencion, disponibilidad_horaria]).
empleado(clara, [fuerza]).
empleado(luis, [logistica]).

