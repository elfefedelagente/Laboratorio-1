% Hechos 
requiere(1,repartir_urbano, [fuerza]).
requiere(2, repartir_larga_distancia, [fuerza, movilidad]).
requiere(3, clasificar_paquetes, [disponibilidad_horaria]).
requiere(4, realizar_logistica, [logistica]).
requiere(5, atencion_publico, [buena_atencion]).

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

empleados([carlos, marisa, juan, jimena, hector, betty, lucia, axel, eva, miguel, clara, luis]). % Consultar hoy.
/*
    verifica(Empleado, TipoTrabajo) tal que dado un empleado y un tipo de 
    trabajo verifica que el empleado está capacitado para realizarlo.
*/
verifica(Empleado, TipoTrabajo) :-
    requiere(_,TipoTrabajo, ListaHabilidades),
    empleado(Empleado, CapacidadEmpleado),
    verificar_capacidad(ListaHabilidades, CapacidadEmpleado).

verificar_capacidad([], _) :- !.
verificar_capacidad([NHabilidad|RestoHabilidades], Capacidades) :-
    member(NHabilidad, Capacidades), 
    verificar_capacidad(RestoHabilidades, Capacidades).
/*############################################################################################*/
/*############################################################################################*/
/*
    puedenRealizar(idTrabajo, Empleados) tal que dado un identificador de trabajo 
    a realizar permite determinar la lista de empleados capacitados para realizarlo
*/
comprobar_empleados([],_, []).

comprobar_empleados([Empleado | Resto], TipoTrabajo, [Empleado | VerificadosResto]) :-
    verifica(Empleado, TipoTrabajo),!,
    comprobar_empleados(Resto, TipoTrabajo, VerificadosResto).

comprobar_empleados([_| Resto], TipoTrabajo, VerificadosResto) :-
    comprobar_empleados(Resto, TipoTrabajo, VerificadosResto).

pueden_realizar(Id,Verificados):-
    requiere(Id,TipoTrabajo,_),
    empleados(Empleados),
    comprobar_empleados(Empleados,TipoTrabajo,Verificados).
/*############################################################################################*/

/*############################################################################################*/
/*
    asignarTrabajos(Trabajos, Asignaciones)
    Tal que dada una lista de trabajos a realizar, permite obtener todas las combinaciones posibles de lista de asignaciones, 
    donde cada elemento relaciona cada trabajo con un empleado que puede realizarlo, 
    teniendo en cuenta sus capacidades y que se le puede asignar sólo un trabajo por 
    vez.
*/
asignar_trabajos(Trabajos, Asignaciones) :-
    asignar_trabajos(Trabajos, [], Asignaciones).

asignar_trabajos([], _, []) :- !.
asignar_trabajos([Trabajo|Resto], Asignados, [Empleado|Asignacion]) :-
    empleado(Empleado, Capacidades),
    requiere(Trabajo, _, HabilidadesNecesarias),
    verificar_capacidad(HabilidadesNecesarias, Capacidades),
    \+ member(Empleado, Asignados), 
    asignar_trabajos(Resto, [Empleado|Asignados], Asignacion).

/*############################################################################################*/
/*############################################################################################*/
/*
    asignarTrabajosGrupales(TrabajosGrupales, GruposAsignados, TrabajosARechazar) 

    *Tal que dada una lista de trabajos grupales a realizar, donde cada 
    uno determina además la cantidad de empleados requeridos para su realización, permite 
    obtener todas las combinaciones posibles de la lista de grupos asignados. Cada elemento 
    de la lista de grupos asignados relaciona cada trabajo con los empleados que pueden 
    realizarlo teniendo en cuenta sus capacidades, que se le puede asignar sólo un trabajo por 
    vez a cada empleado, y que se debe contar con la cantidad de empleados necesaria. La 
    lista de trabajos a rechazar determina los trabajos que no sería posible realizar.
*/
trabajo_grupal(1, 2, repartir_urbano).
trabajo_grupal(2, 4, repartir_larga_distancia).
trabajo_grupal(3, 5, clasificar_paquetes).
trabajo_grupal(4, 3, realizar_logistica).
trabajo_grupal(5, 6, atencion_publico).

asignar_trabajos_grupales([], [], []):-!.
asignar_trabajos_grupales([Trabajo|RestoTrabajos], [Asignacion|Asignaciones], Rechazados) :-
    trabajo_grupal(_, CantidadNecesaria, Trabajo),  
    pueden_realizar_grupal(Trabajo, CantidadNecesaria, [], Asignacion),
    asignar_trabajos_grupales(RestoTrabajos, Asignaciones, Rechazados).
asignar_trabajos_grupales([Trabajo|RestoTrabajos], Asignaciones, [Trabajo|Rechazados]) :-
    asignar_trabajos_grupales(RestoTrabajos, Asignaciones, Rechazados).

% Helper para encontrar suficientes empleados para un trabajo grupal
pueden_realizar_grupal(TipoTrabajo, CantidadNecesaria, AsignadosActuales, Grupo) :-
    CantidadNecesaria > 0,
    empleado(Empleado, Capacidades),
    requiere(_, TipoTrabajo, HabilidadesNecesarias),
    verificar_capacidad(HabilidadesNecesarias, Capacidades),
    \+ member(Empleado, AsignadosActuales),
    NuevaCantidad is CantidadNecesaria - 1,
    pueden_realizar_grupal(TipoTrabajo, NuevaCantidad, [Empleado|AsignadosActuales], Grupo).
pueden_realizar_grupal(_, 0, Grupo, Grupo).

%  asignar_trabajos_grupales([repartir_urbano, realizar_logistica, atencion_publico], Asignaciones, Rechazados).