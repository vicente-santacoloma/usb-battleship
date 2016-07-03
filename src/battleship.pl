%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Battleship                   %
% Autores: Andreth Salazar     %
%          Vicente Santacoloma %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic barco/4.
:- dynamic acierto/2.
:- dynamic aciertoRealizado/2.
:- dynamic fallo/2.
:- dynamic tableros/1.
:- dynamic porHundir/1.
:- dynamic numProyectiles/1.
:- dynamic sol/1.

%% jugar
%
% Da inicio al juego Battleship.
%

jugar :-
  cargarJuego(Filas, Columnas),
  tableroInicial(Filas, Columnas, T),
  interactuar(Filas, Columnas, T).

%% CargarJuego(-Filas:integer, -Columnas:integer)
%
% Carga del usuario los datos necesarios para dar inicio al juego.
% Devuelve el numero de filas y de columnas para ser utilizado en
% en la creacion del tablero.
%

cargarJuego(Filas, Columnas) :-
  cargarDatos(Filas, 'Num. de Filas',0),
  cargarDatos(Columnas, 'Num. de Columnas',0),
  cargarDatos(NumBarcos, 'Cant. de Barcos',0),
  cargarBarcos(NumBarcos, 0, PorHundir),
  assert(porHundir(PorHundir)),
  cargarDatos(NumProyectiles, 'Cant. de proyectiles disponibles', 0),
  assert(numProyectiles(NumProyectiles)),
  verificarJuegoValido,
  assert(tableros([])),
  assert(acierto(-1, -1)),
  assert(aciertoRealizado(-1, -1)),
  assert(fallo(-1, -1)),
  assert(sol(no)).

% cargarBarcos(-N:integer, -Acumulado:integer, +Final:integer)
%
% Carga todos los barcos en la BD.
% Devuelve el numero de posiciones en la cuales hay barcos.
%

cargarBarcos(0, Final, Final) :- !.

cargarBarcos(N, Acumulado, Final) :-
  N1 is N-1,
  write('Informacion de Barco:'), nl,
  cargarDatos(Tamano, 'Tamano',0),
  cargarDireccion(Direccion),
  cargarDatos(FilaInicial, 'Fila Inicial', -1),
  cargarDatos(ColumnaInicial, 'Columna Inicial', -1),
  assert(barco(FilaInicial, ColumnaInicial, Direccion, Tamano)),
  PorHundir0 is Acumulado + Tamano,
  cargarBarcos(N1, PorHundir0 ,Final).

% cargarDatos(-Datos:integer, +M:atom, +N:integer)
%
% Carga un dato en particular ingresado por el usuario, tal como: Filas, Columnas,
% NumBarcos, NumProyectiles, Tamano, FilaInicial o ColumnaInicial ingresados
% por el usuario. Devuelve dicho dato.
%

cargarDatos(Datos, M, N) :-
  concat(M, ': ', M0),
  write(M0),
  read(D),
  concat(M, ' Invalido, Reintroduzca: ', M1),
  verificarDatos(D, Datos, M1, N).

% verificarDatos(_,+Datos:integer, -M:atom, -N:integer)
%
% Verifica que el dato ingresado por el usuario sea correcto. En caso contrario
% le pide al usuario el reingreso del mismo. Devuelve dicho dato.
%

verificarDatos(D, Datos, _, N) :- D > N, Datos is D,!.

verificarDatos(_, Datos, M, N) :-
  write(M),
  read(X),
  verificarDatos(X, Datos, M, N).

% cargarDireccion(-Direccion:atom)
%
% Carga la Direccion de un barco ingresada por el usuario. Devuelve dicho dato.
%

cargarDireccion(Direccion) :-
  write('Direccion (v|h): '),
  read(D),
  verificarDireccion(D, Direccion).


% verificarDireccion(-D:atom, +Direccion:atom)
%
% Verifica que la Direccion del barco ingresada por el usuario sea correcta. En
% caso contrario le pide al usuario el reingreso del mismo. Devuelve dicha Direccion.
%

verificarDireccion(D, Direccion) :- D == h, Direccion = D, !.

verificarDireccion(D, Direccion) :- D == v, Direccion = D, !.

verificarDireccion(_, Direccion) :-
  write('Direccion (v|h) Invalido, Reintroduzca: '),
  read(X),
  verificarDireccion(X, Direccion).

%% verificarJuegoValido
%
% Verifica que un juego se pueda resolver. Esto es que se dispongan de los
% proyectiles necesarios para poder destruir todos los barcos.
%

verificarJuegoValido:-
  porHundir(X),
  numProyectiles(Y),
  X =< Y.

%% tableroInicial(+F:integer, +C:integer, -T:list)
%
% Crea el tablero del juego Battleship a partir del numero de Filas y de
% Columnas. Devuelve dicho tablero.
%

tableroInicial(1, C, [T]) :- tableroIniFila(C, T), !.

tableroInicial(F, C, [T|T2]) :- %recorrido por filas
  F0 is F-1,
  tableroIniFila(C, T),
  tableroInicial(F0, C, T2).

% tableroIniFila(+C:integer, -T:list)
%
% Crea una Fila con tanta a's como columnas se de.
%

tableroIniFila(1, ['a']) :- !. %caso base por fila

tableroIniFila(C, ['a'|Row]) :- %recorrido por columnas
  Tam is C-1,
  tableroIniFila(Tam, Row).

% imprimirTableros(-T:list)
%
% Manda a imprimir la lista de todos los tableros que tiene los diferentes
% estados de la solucion del problema.
%

imprimirTableros([]).

imprimirTableros([T|R]) :-
  mostrarTablero(T), nl,
  imprimirTableros(R).

% limpiarRegistros
%
% Elimina toda la informacion dinamica ingresa a la BD durante el juego actual.
% Esto para que dicha informacion no afecta al proximo juego en caso del usuario
% solicitarlo.
%

limpiarRegistros :-
  abolish(barco/4),
  abolish(acierto/2),
  abolish(aciertoRealizado/2),
  abolish(fallo/2),
  abolish(tableros/1),
  abolish(porHundir/1),
  abolish(numProyectiles/1),
  abolish(sol/1).

% nuevoJuego(opcion:atom)
%
% Inicia un nuevo juego en caso de ser la opcion 'y'.
% Finaliza el juego en caso de ser 'n'.
%

nuevoJuego('y') :-
  jugar.

nuevoJuego('n') :-
  write('Gracias por jugar Batalla Naval'),nl.


%% estadoFinal(+T:list)
%
% Evalua si un tablero esta en estado final, esto es
% revisa si en la posiciones donde estan los barcos hay
% hundido.
%

estadoFinal(T) :- estadoFinalIndex(T, 0, 0), !.

% estadoFinalIndex(+T:list, +F:integer, +C:integer)
%
% Realiza un recorrido por todas la filas del tablero,
% para la verificacion de si se hundieron todos los
% barcos.
%

estadoFinalIndex([T|T2], F, C) :-
  F0 is F+1,
  estadoFinalFila(T, F, C),
  estadoFinalIndex(T2, F0, C).

estadoFinalIndex([], _, _) :- !.

% estadoFinalFila(+T:list, +F:integer, +C:integer)
%
% Realiza un recorrido por todas las columnas de una fila
% del tablero para la verificacion de si se hundieron
% todos los barcos.
%

estadoFinalFila([T|T2], F, C):-
  C0 is C+1,
  estadoFinalCelda(T, F, C),
  estadoFinalFila(T2, F, C0).

estadoFinalFila([], _, _).

% estadoFinalCelda(+E:atom, +F:integer, +C:integer)
%
% Valida si ya en una celda en particular hay un barco o no, y en caso de
% haberlo si este esta hundido.
%

estadoFinalCelda(_, F, C) :- %Cuando no hay barco ya estas en estado final
  not(hayBarco(F, C)).

estadoFinalCelda(E, F, C) :- % Si hay un barco debe tener una h de hundido
  hayBarco(F, C),
  E = 'h'.

%% hayBarco(+F:integer, +C:integer)
%
% Verifica si hay un barco en una posicion del tablero
%

hayBarco(F, C) :-
  barco(F, C, _, _). % Hay un barco en la misma fila columna.

hayBarco(F, C) :-
  barco(F, C1, 'h', K),
  C < C1+K,
  C > C1.

hayBarco(F, C) :-
  barco(F1, C, 'v', K),
  F < F1+K,
  F > F1.

%% mostrarTablero(+T:list)
%
% Imprimie un tablero por la salida estandar.
%

mostrarTablero([]) :- !.
mostrarTablero([T|T2]):- mostrarTableroFila(T) , nl , mostrarTablero(T2).

mostrarTableroFila([]) :- !.
mostrarTableroFila([E|Resto]):- write(E),tab(1), mostrarTableroFila(Resto).

%% interactuar(+F:integer, +C:integer, +T:list)
%
% El sistema interactua con el tablero dado para encontrar su solucion.
% Al encontrarla imprime todos los tableros y da la posibilidad de volver
% a jugar.
%

interactuar(_, _, _) :-
  sol(X),
  X == si,
  tableros(T),
  imprimirTableros(T),
  write('Juego Teminado. Desea Jugar de nuevo [y/n]: '), nl,
  read(NG),
  limpiarRegistros,
  nuevoJuego(NG),!.

interactuar(F, C, T) :-
  numProyectiles(N),
  atacar(T, _, F, C, N),
  abolish(aciertoRealizado/2),
  assert(aciertoRealizado(-1, -1)),
  interactuar(F, C, T),!.

%% obtenerAtaque(-R1:integer, -R2:integer, +F:integer, +C:integer)
%
% Devuelve una posicion para atacar donde ya se haya explorado en un backtracking
% que hay un barco, por haberlo golpeado. En caso de no haber golpeado ningun barco
% todavia o ya haber golpeado todos los que se exploro, devuelve una posicion aleatoria
% en donde no se hubiese cometido un fallo. Para ambos casos no se repite una posicion
% jugada.
%

obtenerAtaque(R1, R2, _, _) :-
  acierto(R1, R2),
  R1 \= -1,
  R2 \= -1,
  not(aciertoRealizado(R1, R2)),
  retractall(acierto(R1, R2)),
  !.

obtenerAtaque(R1, R2, F, C) :-
  repeat,
  random(0, F, R1),
  random(0, C, R2),
  not(fallo(R1, R2)),
  not(aciertoRealizado(R1, R2)).

%% atacar(+T0:list, -T1:list, +F:integer, +C:integer, ?N:integer)
%
% Realiza una serie de ataques sobre el tablero hasta que se llegue a un estado final,
% o hasta que se acaben las balas.
%

atacar(T0, _, _, _, _) :-
  estadoFinal(T0),
  retract(sol(_)),
  assert(sol('si')),
  !.

atacar(_, _, _, _, 0) :-
  retract(tableros(_)),
  assert(tableros([])),
  !.

atacar(T0, T1, F, C, N) :-
  obtenerAtaque(F0, C0, F, C),
  ataque(F0, C0, T0, T),
  retract(tableros(Z)),
  append(Z, [T], X),
  assert(tableros(X)),
  N0 is N-1,
  atacar(T, T1, F, C, N0).

%% ataque(F0, C0, T0, T1)
%
% Realiza un ataque en una posicion en particular. En caso de haber barco en dicha
% la marco con una 'g' de golpe o con una 'h' si lo hundio. En caso de no haber barco
% la marca con una 'f' de fallo.
%

ataque(F0, C0, T0, T1) :-
  hayBarco(F0, C0),
  copiarBarco(F0, C0, T0, T, 'g'),
  assert(acierto(F0, C0)),
  assert(aciertoRealizado(F0, C0)),
  retract(porHundir(X)),
  X0 is X-1,
  assert(porHundir(X0)),
  hundido(T, T1),
  !.

ataque(F0, C0, T0, T1) :-
  copiarBarco(F0, C0, T0, T1, 'f'),
  assert(fallo(F0, C0)).

%% copiarBarco(+Fila:integer, +Columna:integer, +T0:list, -T1:list, +E:atom)
%
% Copia un caracter en una posicion de un tablero y devuelve el nuevo tablero.
%

copiarBarco(0, Columna, [F1|R], [F2|R], E):-
  copiarBarcoColumna(Columna, F1, F2, E), !.

copiarBarco(Fila, Columna, [X|R1], [X|R2],E) :-
  F0 is Fila-1,
  copiarBarco(F0, Columna, R1, R2, E).

% copiarBarcoColumna(+C:integer, +F0:list, -F1:list, +E:atom)
%
% Copia un caracter en una columna de una fila de un tablero, y devuelve
% la nueva fila modificada.
%

copiarBarcoColumna(0, [_|R1], [E|R1], E) :- !.

copiarBarcoColumna(C, [X|R1], [X|R2], E):-
  C0 is C-1,
  copiarBarcoColumna(C0, R1, R2, E).

%% hundido(+T0:list, -T1:list)
%
% Revisa en el tablero si hay un barco hundido. En caso de haberlo marca todas
% sus casillas con una 'h' de hundido. Si no lo hay no retorna ninguna modificacion.
% Retorna el tablero modificado.
%

hundido(T0, T1) :-
  barco(Fila, Columna, Orientacion, Tamano),
  golpeBarco(Fila, Columna, Orientacion, Tamano, T0),
  marcarHundido(0, 0, Fila, Columna, Orientacion, Tamano, T0, T1), !.

hundido(T, T).

% marcaHundido(+Fila:integer, +C:integer, +Columna:integer, +Direccion:atom,
% +Tamano:integer, +T0:list, -T1:list)
%
% Marca como hundido las posiciones de un barco que haya sido destruido en su
% totalidad.
%

marcarHundido(Fila, C, Fila, Columna, h, Tamano, [T0|R0], [T1|R0]) :-
  marcarGolpe(C, Columna, h, Tamano, T0, T1), !.

marcarHundido(F, C, Fila, Columna, h, Tamano, [T0|R0], [T0|R1]) :-
  F \= Fila,
  F0 is F+1,
  marcarHundido(F0, C, Fila, Columna, h, Tamano, R0, R1).

marcarHundido(_, _, _, _, v, _, [], []).

marcarHundido(F, C, Fila, Columna, v, Tamano, [T0|R0], [T1|R1]) :-
  F >= Fila,
  F =< Fila+Tamano-1,
  F0 is F+1,
  marcarGolpe(C, Columna, v, Tamano, T0, T1),
  marcarHundido(F0, C, Fila, Columna, v, Tamano, R0, R1),
  !.

marcarHundido(F, C, Fila, Columna, v, Tamano, [T0|R0], [T0|R1]) :-
  F0 is F+1,
  marcarHundido(F0, C, Fila, Columna, v, Tamano, R0, R1).

% marcaGolpe(+C:integer, +Columna:integer, +Direccion:atom, +Tamano:integer,
% +T0:list, -T1:list)
%
% Marca con h's todas las casillas en las cuales se encuentra un barco.
%

marcarGolpe(_, _, _, _, [], []).

marcarGolpe(C, Columna, h, Tamano, [_|R0], ['h'|R1]) :-
  C >= Columna,
  C =< Columna+Tamano-1,
  C0 is C+1,
  marcarGolpe(C0, Columna, h, Tamano, R0, R1),
  !.

marcarGolpe(C, Columna, h, Tamano, [T|R0], [T|R1]) :-
  C0 is C+1,
  marcarGolpe(C0, Columna, h, Tamano, R0, R1).


marcarGolpe(Columna, Columna, v, Tamano, [_|R0], ['h'|R1]) :-
  C0 is Columna+1,
  marcarGolpe(C0, Columna, v, Tamano, R0, R1),
  !.

marcarGolpe(C, Columna, v, Tamano, [T0|R0], [T0|R1]) :-
  C0 is C+1,
  marcarGolpe(C0, Columna, v, Tamano, R0, R1).

% golpeBarco(+Fila:integer, +Columna:integer, +Direccion:atom, +Tamano:integer,
% +T0:list)
%
% Revisa si hay un golpe en todas las casillas de un barco.
%

golpeBarco(_, _, _, 0, _) :- !.

golpeBarco(Fila, Columna, h, Tamano, T0) :-
  verificarGolpe(0, 0, Fila, Columna, T0),
  C0 is Columna+1,
  Tam is Tamano-1,
  golpeBarco(Fila, C0, h, Tam, T0).

golpeBarco(Fila, Columna, v, Tamano, T0) :-
  verificarGolpe(0, 0, Fila, Columna, T0),
  F0 is Fila+1,
  Tam is Tamano-1,
  golpeBarco(F0, Columna, v, Tam, T0).

% verificarGolpe(+F:integer, +C:integer, +Fila:integer, +Columna:integer, +T:list)
%
% Verifica si hay un golpe en un casilla de una tablero.
%

verificarGolpe(Fila, C, Fila, Columna, [T|_]) :-
  verificarGolpeFila(C, Columna, T),!.

verificarGolpe(F, C, Fila, Columna, [_|R]) :-
  F0 is F+1,
  verificarGolpe(F0, C, Fila, Columna, R).

% verificarGolpeFila(+C:integer, +Columna:integer, +T:list)
%
% Revisa si hay un golpe en todas las columnas de una fila del tablero.
%

verificarGolpeFila(Columna, Columna, ['g'|_]).

verificarGolpeFila(C, Columna, [_|R]) :-
  C0 is C+1,
  verificarGolpeFila(C0, Columna, R).
