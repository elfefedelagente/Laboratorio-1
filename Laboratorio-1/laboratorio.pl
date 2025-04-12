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
esta_capacitado(Empleado, TipoTrabajo) :-
    requiere(_,TipoTrabajo, ListaHabilidades),
    empleado(Empleado, CapacidadEmpleado),
    verificar_capacidad(ListaHabilidades, CapacidadEmpleado).

% Obtiene True si el empleado tiene la capacidad ingresada.
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
pueden_realizar(IdTrabajo, ListaEmpleados) :-
    requiere(IdTrabajo, _, CapacidadesNecesarias),    
    empleados(Empleados),
    determinar_empleados(CapacidadesNecesarias, Empleados, ListaEmpleados).

determinar_empleados(_, _,[]).

determinar_empleados(CapacidadesNecesarias, [Emp|RestoEmpleados], [Emp|Resto]) :-
    empleado(Emp, CapacidadesEmp),
    verificar_capacidadades(CapacidadesNecesarias, CapacidadesEmp),
    determinar_empleados(CapacidadesNecesarias, RestoEmpleados, Resto).

determinar_empleados(CapacidadesNecesarias, [Emp|RestoEmpleados], Resto) :-
    empleado(Emp, CapacidadesEmp),
    \+ verificar_capacidadades(CapacidadesNecesarias, CapacidadesEmp),  %El operador \+ significa "no se puede probar" (negación lógica). Esto verifica que el empleado actual no tenga las capacidades necesarias.
    determinar_empleados(CapacidadesNecesarias, RestoEmpleados, Resto).

verificar_capacidadades([], _).
verificar_capacidadades([X|Xs], Capacidades) :-
    member(X, Capacidades),
    verificar_capacidadades(Xs, Capacidades).

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
    verificar_capacidadades(HabilidadesNecesarias, Capacidades),
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
%trabajos_grupales(1, ).
%trabajos_grupales(3, ).
%trabajos_grupales(6, ).


asignar_trabajos_grupales(TrabajosGrupales, GruposAsignados, TrabajosARechazar) :-
    


