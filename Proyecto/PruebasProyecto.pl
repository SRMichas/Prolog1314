
:- dynamic turno/1.
:- dynamic mesa/1.
:- dynamic mazoJugador/2.

colores([rojo,azul,negro,verde]). 

jugador(j1).
jugador(j2).

mazoJugador(j1,[]).
mazoJugador(j2,[]).

turno(_).

mesa([]).

empieza :-
    colores(NombresColores),
    generaFichas(NombresColores,ListaColoresSinComodin),                                   %generamos todas las fichas de colores en una sola lista
    append(ListaColoresSinComodin,[[0,comodin],[0,comodin]],ListaBasica),     %agregarmos los 2 comodines
    %write(Revuelta).
    random_permutation(ListaBasica, Revuelta),                      %revolvemos la lista
    llenaMesa(Revuelta),                                            %ponemos la baraja sobre la mesa
    revisaEmpieza(Revuelta),                                        %verificamos quien empieza
    random_permutation(Revuelta, RevueltaFinal),                    %revolvemos nuevamente
    llenaMesa(RevueltaFinal),                                       %volvemos a poner la baraja sobre la mesa
    otroImprime(RevueltaFinal),                     
    writeln("\n\n"),
    reparte,                    
    mesa(LL),                                           
    otroImprime(LL). 

reparte :-
        mesa(Mazo),
        sublista(Mazo,14,ListaJ1), agregaMazo(j1,ListaJ1), 
        quitaCartas(ListaJ1,Mazo,SemiRepartida),
        sublista(SemiRepartida,14,ListaJ2), agregaMazo(j2,ListaJ2),
        quitaCartas(ListaJ2,SemiRepartida,Repartidas),
        llenaMesa(Repartidas). 

quitaCartas(L1,L2,R) :-
        borrar(L1,L2,R).

borrar([],X,X).
borrar([_|T],[_|Xs],R) :- borrar(T,Xs,R).


agregaMazo(Jugador,Cartas) :-
        retractall(mazoJugador(Jugador,_)),
        assertz(mazoJugador(Jugador,Cartas)).

revisaEmpieza([Carta1,Carta2|T]) :-         %en caso de que el J1 sea el que gane
            [Num1,Nombre1] = Carta1,
            [Num2,Nombre2] = Carta2,
            Num1 > Num2,
            cambiaTurno(j1),
            write(j1),writeln(" tu empiezas").

revisaEmpieza([Carta1,Carta2|T]) :-         %en caso de que el J2 sea el que gane
           [Num1,Nombre1] = Carta1,
           [Num2,Nombre2] = Carta2,
           Num1 < Num2,
           cambiaTurno(j2),
           write(j2),writeln(" tu empiezas").

revisaEmpieza([Carta1,Carta2|T]) :-         %en caso de que sean iguales, volvemos a tirar
            [Num1,Nombre1] = Carta1,
            [Num2,Nombre2] = Carta2,
            Num1 =:= Num2,
            revisaEmpieza(T).

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
sublista([H|T],N,[H|RT]) :-
        N > 0,
        Naux is N - 1,
        sublista(T,Naux,RT).


imprime :- 
       fichasXcolor(rojo,13,X),
       otroImprime(X).