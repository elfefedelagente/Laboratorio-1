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

% Lista de empleados
empleados([carlos, marisa, juan, jimena, hector, betty, lucia, axel, eva, miguel, clara, luis]). % Consultar hoy.

% Lista de trabajos grupales, con la cantidad de empleados necesarios
trabajo_grupal(1, 2, repartir_urbano).
trabajo_grupal(2, 4, repartir_larga_distancia).
trabajo_grupal(3, 5, clasificar_paquetes).
trabajo_grupal(4, 3, realizar_logistica).
trabajo_grupal(5, 6, atencion_publico).
/* Verifica que el empleado esta capacitado para realizarl determinado trabajo. */
verifica(Empleado, TipoTrabajo) :-
    requiere(_,TipoTrabajo, ListaHabilidades),
    empleado(Empleado, CapacidadEmpleado),
    verificar_capacidad(ListaHabilidades, CapacidadEmpleado).

verificar_capacidad([], _) :- !.
verificar_capacidad([NHabilidad|RestoHabilidades], Capacidades) :-
    member(NHabilidad, Capacidades), 
    verificar_capacidad(RestoHabilidades, Capacidades).

/* ------------------------------------------------------------------ */

comprobar_empleados([],_, []).

comprobar_empleados([Empleado | Resto], TipoTrabajo, [Empleado | VerificadosResto]) :-
    verifica(Empleado, TipoTrabajo),!,
    comprobar_empleados(Resto, TipoTrabajo, VerificadosResto).

comprobar_empleados([_| Resto], TipoTrabajo, VerificadosResto) :-
    comprobar_empleados(Resto, TipoTrabajo, VerificadosResto).
/* Permite determinar la lista de empleados capacitados para realizar un trabajo dado su id */
pueden_realizar(Id,Verificados):-
    requiere(Id,TipoTrabajo,_),
    empleados(Empleados),
    comprobar_empleados(Empleados,TipoTrabajo,Verificados).

/* Permite obtener todas las combinaciones posibles de lista de asignaciones*/
asignar_trabajos(Trabajos, Asignaciones) :-
    asignar_trabajos(Trabajos, [], [], Asignaciones).

/* Cambia el acumulador para construir pares Trabajo-Empleado */
asignar_trabajos([], _, Asignaciones, Asignaciones) :- !.

asignar_trabajos([Trabajo|RestoTrabajos], Asignados, Parcial, Asignaciones) :-
    empleado(Empleado, Capacidades),
    requiere(Trabajo, TipoTrabajo, HabilidadesNecesarias),
    verificar_capacidad(HabilidadesNecesarias, Capacidades),
    \+ member(Empleado, Asignados),
    NuevoParcial = [trabajo(TipoTrabajo, Empleado)|Parcial],
    asignar_trabajos(RestoTrabajos, [Empleado|Asignados], NuevoParcial, Asignaciones).

/* Permite obtener todas las combinaciones posibles de la lista de grupos asignados */

asignar_trabajos_grupales(Trabajos, Asignaciones, Rechazados) :-
    asignar_trabajos_grupales(Trabajos, [], [], Asignaciones, Rechazados).

asignar_trabajos_grupales([], _, AsignacionesAcum, AsignacionesAcum, []).
asignar_trabajos_grupales([TrabajoID|Resto], Ocupados, AsigAcum, AsignacionesFinales, RechazadosFinales) :-
    trabajo_grupal(TrabajoID, Cantidad, TipoTrabajo),
    buscar_empleados_disponibles(TipoTrabajo, Cantidad, Ocupados, Grupo),
    actualizar_ocupados(Grupo, Ocupados, NuevosOcupados),
    asignar_trabajos_grupales(Resto, NuevosOcupados, [grupo(TipoTrabajo, Grupo)|AsigAcum], AsignacionesFinales, RechazadosFinales).
asignar_trabajos_grupales([TrabajoID|Resto], Ocupados, AsigAcum, AsignacionesFinales, [TrabajoID|RechazadosFinales]) :-
    asignar_trabajos_grupales(Resto, Ocupados, AsigAcum, AsignacionesFinales, RechazadosFinales).

/* Agrega empleados nuevos a la lista de ocupados */
actualizar_ocupados([], Ocupados, Ocupados).
actualizar_ocupados([E|Es], Ocupados, [E|Nuevos]) :-
    actualizar_ocupados(Es, Ocupados, Nuevos).

/* Encuentra grupo de empleados disponibles y válidos */
buscar_empleados_disponibles(_, 0, _, []) :- !.
buscar_empleados_disponibles(TipoTrabajo, Cantidad, Ocupados, [Empleado|Resto]) :-
    Cantidad > 0,
    empleado(Empleado, Capacidades),
    \+ member(Empleado, Ocupados),
    requiere(_, TipoTrabajo, Habilidades),
    verificar_capacidad(Habilidades, Capacidades),
    NuevaCantidad is Cantidad - 1,
    buscar_empleados_disponibles(TipoTrabajo, NuevaCantidad, [Empleado|Ocupados], Resto).

