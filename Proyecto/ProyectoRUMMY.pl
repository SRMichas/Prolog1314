/*algo:-write("hola").
prueba :- write("huehueue").
[[5,negro],[1,rojo],[12,rojo],[3,rojo],[4,verde]]
*/
%empieza.

:- dynamic turno/1.
:- dynamic mesa/1.
:- dynamic mazoJugador/2.
:- dynamic mesaJugadas/2.
:- dynamic bandera1raJugada/1.
:- dynamic mesaJugada/1.
:- dynamic contador/1.


mesaJugadas(tercias,[]).
mesaJugadas(escaleras,[]).
colores([rojo,azul,negro,verde]).

jugador(j1).
jugador(j2).

/*mazoJugador(j1,[[1,azul],[2,rojo],[1,negro],[1,verde]]).
mazoJugador(j2,[[11,azul],[2,rojo],[11,negro],[11,verde]]).*/

mazoJugador(j1,[[1,azul],[2,rojo],[1,negro],[1,verde]]).
mazoJugador(j2,[[11,azul],[2,rojo],[11,negro],[11,verde]]).

%mazoJugador(jp,[[1,verde],[1,rojo],[1,azul],[7,rojo],[7,rojo]]).

turno(_).

mesa([]).
mesaJugada([]).

bandera1raJugada(false).
contador(0).



empieza :-
        reseteaJuego,
        colores(ListaColores),
        generaFichas(ListaColores,MazoSC),                               %generamos todas las fichas de colores en una sola lista
        append(MazoSC,[[0,comodin],[0,comodin]],ListaBasica),            %agregarmos los 2 comodinesem
        %write(Revuelta).
        random_permutation(ListaBasica, Revuelta),                      %revolvemos la lista
        llenaMesa(Revuelta),                                            %ponemos la baraja sobre la mesa
        revisaEmpieza2(Revuelta),                                        %verificamos quien empieza
        random_permutation(Revuelta, RevueltaFinal),                    %revolvemos nuevamente
        llenaMesa(RevueltaFinal),                                       %volvemos a poner la baraja sobre la mesa
        %otroImprime(RevueltaFinal),
        writeln("\n\n"),
        reparte,
        /*mesa(LL),
        otroImprime(LL).*/
        primeraJugada.
        %retract(turno(_)).

reparte :-
        mesa(Mazo),
        sublista(Mazo,14,ListaJ1), agregaMazo(j1,ListaJ1),
        quitaCartas(ListaJ1,Mazo,SemiRepartida),
        write("----------MAZO JUGADOR-------------- "),write("j1 \n"),
        otroImprime(ListaJ1),
        sublista(SemiRepartida,14,ListaJ2), agregaMazo(j2,ListaJ2),
        quitaCartas(ListaJ2,SemiRepartida,Repartidas),
        write("----------MAZO JUGADOR-------------- "),write("j2 \n"),
        otroImprime(ListaJ2),       %irrelabante solo con fines de mostrar
        write("\n"),                %irrebalante solo con fines de mostrar
        llenaMesa(Repartidas).


primeraJugada :- turno(J),mazoJugador(J,Mazo),
        /*write("ESTE CONTADOR PERTENECE A LA ITERACION ====> "),
        contador(X),write(X),nl,nl,*/
        ponerFichas2(Mazo).

reseteaJuego :-
        modificaBandera1(false),
        retract(turno(_)),assert(turno(_)), %<-----------
        retractall(mazoJugador(j1,_)),assertz(mazoJugador(j1,[])),
        retractall(mazoJugador(j2,_)),assertz(mazoJugador(j2,[])),
        retractall(mesa(_)),assertz(mesa([])),
        retractall(mesaJugadas(tercias,_)),assertz(mesaJugadas(tercias,[])),
        retractall(mesaJugadas(escaleras,_)),assertz(mesaJugadas(escaleras,[])). 

ifChido(Mazo) :-
        (
                Mazo =:= 1 -> write("este es un UNO");
                Mazo =:= 2 -> write("este es un DOS");
                write("este NO se que nuemro es")
        ).

%==============================================================================================================================
ponerFichas2(Mazo) :-
        jugar1(Mazo,Jugada) ->
                                turno(J),
                                intermedio(Jugada),            % elimina las cartas de la jugada del mazo del jugador en turno
                                mazoJugador(J,Cartitas),
                                modificaBandera1(true),
                                write("==================================================================[ "),
                                print(J),
                                write(" ]=================================================================="),
                                nl,
                                write("Jugada Realizada ---> "),write(Jugada),nl,
                                write("--------Cartas modificadas de "),write(J),
                                write("----------"),nl,
                                write(Cartitas),nl;
        %bandera1raJugada(V),V = false,
                                comerFicha, pasar.


%==============================================================================================================================


modificaBandera1(V) :- retract(bandera1raJugada(_)), asserta(bandera1raJugada(V)).

pasar:- cambiarTurno, primeraJugada.
                %turno(J),mazoJugador(J,Mazo),


cambiarTurno:-
                jugador(NuevoJugador),
                turno(JEnTurno),
                bandera1raJugada(B), B == false,
                NuevoJugador \= JEnTurno,
                retract(turno(_)), assert(turno(NuevoJugador)).

jugar1(Mazo,Res):- tercia(Mazo,Mazo,[H|_]),
                        [N,Nombres] = H,
                        convertir(N,Nombres,Res),nl.
comerFicha:-
                turno(Jugador),
                mazoJugador(Jugador,MazoActual),
                mesa([H|T]),
                append(MazoActual,[H],MazoModificado),
                write("________________________________________________"),
                write("Este mazo pertenece a [ "),write(Jugador),write(" ] y acaba de comer porque no tiene juego"),
                write("________________________________________________"),nl,
                write(MazoModificado),nl,nl,
                retractall(mazoJugador(Jugador,_)),
                assertz(mazoJugador(Jugador,MazoModificado)),
                reescribeMesaPila(T).

reescribeMesaPila(NuevaMesa):-
            retractall(mesa(_)),
            assertz(mesa(NuevaMesa)).

quitaCartas(L1,L2,R) :-
        borrar(L1,L2,R).

borrar([],X,X).
borrar([_|T],[_|Xs],R) :- borrar(T,Xs,R).


borraMaz([],Lista,Lista).
borraMaz([HJ|TJ],ListaMano,MazJugador):-
         remover(HJ,ListaMano,ListaSin),
         borraMaz(TJ,ListaSin,MazJugador),
         write(MazJugador),nl,nl.

borraMaz2([],Lista,Lista).
borraMaz2([HJ|TJ],ListaMano,MazJugador):-
        remover2(HJ,ListaMano,ListaSin),
        borraMaz2(TJ,ListaSin,MazJugador).
        %write(MazJugador),nl,nl.

remover2(A,[A|X],X).     %A = Carta = B , X -> []
remover2(A,[B|X],[H|Y]) :-
        dif(A,B),
        H = B,
        remover2(A,X,Y).
% mano


intermedio(Jugada):-
        turno(J),
        mazoJugador(J,ManoActual),
        borraMaz2(Jugada,ManoActual,ManoModificada),
        retractall(mazoJugador(J,_)),
        assertz(mazoJugador(J,ManoModificada)),
        agregarJugada(tercias,Jugada).

agregarJugada(TipoJugada,Jugada):- mesaJugadas(TipoJugada,TempJugada),
                                   append(TempJugada,[Jugada],RespJugada),
                                   retractall(mesaJugadas(TipoJugada,_)),
                                   assertz(mesaJugadas(TipoJugada,RespJugada)).


remover(A,[A|X],X).     %A = Carta = B , X -> []
remover(A,[B|X],[B|Y]) :- remover(A,X,Y).

/*remover(Elemento,Lista):-
    remover(Elemento,Lista,_).  %Remueve dicho elemento de la lista
    %reescribeMesa(Resultado).*/

agregaMazo(Jugador,Cartas) :-
        retractall(mazoJugador(Jugador,_)),
        assertz(mazoJugador(Jugador,Cartas)).

revisaEmpieza([Carta1,Carta2|_]) :-   %en caso de que el J1 sea el que gane
            [Num1,_] = Carta1,
            [Num2,_] = Carta2,
            Num1 > Num2,
            cambiaTurno(j1),
            write(j1),writeln(" tu empiezas").

revisaEmpieza([Carta1,Carta2|_]) :-         %en caso de que el J2 sea el que gane
           [Num1,_] = Carta1,
           [Num2,_] = Carta2,
           Num1 < Num2,
           cambiaTurno(j2),
           write(j2),writeln(" tu empiezas").

revisaEmpieza([Carta1,Carta2|T]) :-         %en caso de que sean iguales, volvemos a tirar
            [Num1,_] = Carta1,
            [Num2,_] = Carta2,
            Num1 =:= Num2,
            revisaEmpieza(T).
revisaEmpieza2([Carta1,Carta2|T]) :-
                [Num1,_] = Carta1,
                [Num2,_] = Carta2,
                (
                  Num1 > Num2 -> cambiaTurno(j1), write(j1), writeln(" tu empiezas");
                  Num1 < Num2 -> cambiaTurno(j2), write(j2), writeln(" tu empiezas");
                  revisaEmpieza2(T)
                ).

%Llenar el mazo que hay sobre de la mesa para poder jalar
llenaMesa(X) :-
        retractall(mesa(_)),
        assertz(mesa(X)).

cambiaTurno(X) :-
        retract(turno(_)),
        asserta(turno(X)).

generaFichas([],[]).
generaFichas([Color|T],L) :-
        fichasXcolor(Color,13,ListaParcial),        % genera lista por el color designado
        generaFichas(T,ListaAux),
        append(ListaParcial,ListaAux,L).            % fusionamos la lista 1 con la lista 2 y creamos una nueva lista


fichasXcolor(_,0,[]).
fichasXcolor(Color,N,L) :-
        N > 0,
        Naux is N - 1,
        A = [N,Color],          %creamos una carta
        L = [A,A|Taux],         %añadimos 2 veces la carta en el mazo
        fichasXcolor(Color, Naux, Taux).

otroImprime([]).
otroImprime([H|T]) :-
            writeln(H),
            otroImprime(T).

sublista(_,0,[]).       
sublista([H|T],N,[H|RT]) :-     %
        N > 0,                  %
        Naux is N - 1,          %
        sublista(T,Naux,RT).    


imprime :-
       fichasXcolor(rojo,13,X),
       otroImprime(X).
%------Nueva Seccion 07/04/2020 Martes ?Intento Tercia?---------------------------------------------------------------------------

unicos(Lin, Lout) :-    unicos(Lin, [], Lout).
unicos([], ACC, OUT) :- reverse(ACC, OUT).
unicos([X|Z], ACC, OUT) :- member(X, ACC),!,unicos(Z, ACC, OUT).
unicos([X|Z], ACC, OUT) :- unicos(Z, [X|ACC], OUT).

tercia([],_,[]).
tercia([H|T],Mazo,[HR|TR]):-                    % si hay tercia en el mazo
    dameCartasSinRepetir(H,Mazo,Rspa,L),        % devuelve la tercia
    (L >= 3 , L =< 4),                          % longitud de 3 0 4 ?
    HR = Rspa,                                  % smn, agregamos a la lista
    tercia(T,Mazo,TR).

tercia([H|T],Mazo,TR):-                 % no hay tercia
    dameCartasSinRepetir(H,Mazo,_,L),   % devuelve la "tercia"
    not(L=3),not(L=4),                  % longitud de que no sea 3 o 4
    tercia(T,Mazo,TR).                   

dameCartasSinRepetir(Carta,Mazo,Rsp,L) :-
        [N,_] = Carta,
        dameCartasRepetidas(Carta,Mazo,Repetidas),      % devolver la lista aunque haya repetidos
        unicos(Repetidas,Unicos),                       % devuelve lista de colores sin repetir
        length(Unicos,L),                               % obtenemos la longitud de la lista
        Rsp = [N,Unicos].

convertir(_,[],[]).                             %convierte lista en fichas de nuevo xDDDDDDDD
convertir(Numero,[CH1|CT1],[RH2|RT2]):-         
                RH2 = [Numero,CH1],             % carta
                convertir(Numero,CT1,RT2).

dameCartasRepetidas(_,[],[]).                           
dameCartasRepetidas(Carta,[Mazo1|Mazo2],[Col2|T]) :-    % verifica si hay cartas con el mismo nuemro
                [Num,_] = Carta,
                [Num1,Col2] = Mazo1,
                Num =:= Num1,                           %si son del mismo numero, guardamos la carta
                dameCartasRepetidas(Carta,Mazo2,T).

dameCartasRepetidas(Carta,[Mazo1|Mazo2],T) :-           % verifica si hay cartas con el mismo nuemro
                [Num,_] = Carta,
                [Num1,_] = Mazo1,
                not(Num =:= Num1),                      %si son diferentes, ignoramos y avanzamos
                dameCartasRepetidas(Carta,Mazo2,T).

%------Nueva Seccion 10/04/2020 Viernes ? Intento Escalera---------------------------------------------------------------------------
