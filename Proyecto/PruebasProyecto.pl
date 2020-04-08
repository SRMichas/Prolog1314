/*algo:-write("hola").

prueba :- write("huehueue").*/

:- dynamic turno/1.
:- dynamic mesa/1.
:- dynamic mazoJugador/2.
:- dynamic mesaJugada/1.
:- dynamic bandera1raJugada/1.

colores([rojo,azul,negro,verde]). 

jugador(j1).
jugador(j2).

mazoJugador(j1,[]).
mazoJugador(j2,[]).

turno(_).

mesa([]).
mesaJugada([]).

bandera1raJugada(falsalse).

empieza :-
    colores(ListaColores),
    generaFichas(ListaColores,MazoSC),                                   %generamos todas las fichas de colores en una sola lista
    append(MazoSC,[[0,comodin],[0,comodin]],ListaBasica),     %agregarmos los 2 comodines
    %write(Revuelta).
    random_permutation(ListaBasica, Revuelta),                      %revolvemos la lista
    llenaMesa(Revuelta),                                            %ponemos la baraja sobre la mesa
    revisaEmpieza(Revuelta),                                        %verificamos quien empieza
    random_permutation(Revuelta, RevueltaFinal),                    %revolvemos nuevamente
    llenaMesa(RevueltaFinal),                                       %volvemos a poner la baraja sobre la mesa
    %otroImprime(RevueltaFinal),                     
    writeln("\n\n"),
    reparte,                    
    mesa(LL),                                           
    %otroImprime(LL). 
    primeraJugada.
    
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


primeraJugada :- turno(J),mazoJugador(J,Mazo),ponerFichas(Mazo).


ponerFichas(Mazo):- jugar1(Mazo,Jugada),write(Jugada),intermedio(Jugada),turno(J)
                    ,mazoJugador(J,Cartitas),
                        writeln("--------Cartas modificadas de "),write(J),
                        writeln("----------"),
                    writeln(Cartitas),
                    modificaBandera1(true).
                    
                    %, ponerEnmesa(Jugada).
modificaBandera1(V) :- retract(bandera1raJugada(_)),
asserta(bandera1raJugada(V)).

                    
ponerFichas(Mazo):-bandera1raJugada(V),V = false,pasar.

pasar:-cambiarTurno().
cambiarTurno():-
                jugador(NuevoJugador),
                turno(JEnTurno),
                dif(JEnTurno,NuevoJugador),
                retract(turno(_)), assert(turno(NuevoJugador)).

%ntermedio([[1,rojo],[1,negro],[1,azul],[7,azul],[7,rojo]],[[1,rojo],[1,negro],[1,azul],[7,azul],[7,rojo]],X)
% ponerFichas([[1,rojo],[1,negro],[1,azul],[7,azul],[7,rojo]]).
jugar1(Mazo,Res):- tercia(Mazo,Mazo,[H|_]),
                        [N,Nombres] = H,
                        convertir(N,Nombres,Res).




% R1 , R2 , 
% [1,rojo],[1,azul],[1,verde]
%regla([Numero,Color],[Cabeza|Cola]):- ,Color =,Cabeza /= [Numero,Color],regla([Numero,Color],Cola).
% [[1,rojo],[1,rojo],[1,azul],[1,verde]]
% 1 = [rojo,rojo,azul,verde]
% [[1,rojo],[1,azul],[1,verde]]
% 
% ponerFichas([H|T]):-verificarCorrida().


% verificarTercia(Ficha,Rsp):-
        /*tiene que ser igual el numero,comparado con 
        la ultima que pusimos, y que sea de diferente color.
        Ficha = [Numero,Color]
        Respuesta = [H|T]
        */
        %,cuentaCartas(Ficha,)

quitaCartas(L1,L2,R) :-
        borrar(L1,L2,R).

borrar([],X,X).
borrar([_|T],[_|Xs],R) :- borrar(T,Xs,R).

/*      H|T     

borraMaz([[1,rojo],[1,verde]],[[2,azul],[1,rojo],[4,negro],[1,verde],[10,negro]],X)

borraMaz([[1,verde],[1,rojo],[1,azul],[1,verde],[1,rojo],[1,azul],[7,rojo],[7,rojo]],X)
*/
borraMaz([],Lista,Lista).
borraMaz([HJ|TJ],ListaMano,MazJugador):-
         remover(HJ,ListaMano,ListaSin),
         borraMaz(TJ,ListaSin,MazJugador).
         
% mano
mazoJugador(jp,[[1,verde],[1,rojo],[1,azul],[7,rojo],[7,rojo]]).
intermedio(Jugada):-
        turno(J),
        mazoJugador(J,ManoActual),
        borraMaz(Jugada,ManoActual,ManoModificada),
        retractall(mazoJugador(J,_)),
        assertz(mazoJugador(J,ManoModificada)).

% Remover un objeto de una lista
% remover(+Elemento,+Lista, ?Resultado).
% remover(1,[1,2,3,4],X)  
remover(A,[A|X],X).
remover(A,[B|X],[B|Y]) :- remover(A,X,Y).

remover(Elemento,Lista):-
    remover(Elemento,Lista,Resultado),  %Remueve dicho elemento de la lista
    reescribeMesa(Resultado).
        
/*
jugada(tercia,[[[1,rojo],[1,negro],[1,azul]]]).
jugada(escalera,[[11,rojo],[12,rojo],[13,rojo]]).
*/

% [[1,rojo],[1,negro],[1,azul]]
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

%Llenar el mazo que hay sobre de la mesa para poder jalar
llenaMesa(X) :-
        retractall(mesa(_)),
        assertz(mesa(X)).

%Ponemos las cartas jugadas 
reescribeMesa(NuevaMesa):-
        retractall(mesaJugada(_)),
        assertz(mesaJugada(NuevaMesa)).

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
%------Nueva Seccion 07/04/2020 Martes ?Intento Tercia?---------------------------------------------------------------------------

unicos(Lin, Lout) :-    unicos(Lin, [], Lout).
unicos([], ACC, OUT) :- reverse(ACC, OUT).
unicos([X|Z], ACC, OUT) :- member(X, ACC),!,unicos(Z, ACC, OUT).
unicos([X|Z], ACC, OUT) :- unicos(Z, [X|ACC], OUT).

tercia([],_,[]).
tercia([H|T],Mazo,[HR|TR]):-
    dameCartasSinRepetir(H,Mazo,Rspa,L),
    (L >= 3 , L =< 4),
    HR = Rspa,
    tercia(T,Mazo,TR).

tercia([H|T],Mazo,TR):-   
    dameCartasSinRepetir(H,Mazo,Rsp,L),
    not(L=3),not(L=4),
    tercia(T,Mazo,TR).

dameCartasSinRepetir(Carta,Mazo,Rsp,L) :-
        [N,_] = Carta,
        dameCartasRepetidas(Carta,Mazo,Repetidas),   %devolver la lista aunqye haya repetidos
        unicos(Repetidas,Unicos),                       %devuelve lista sin repetir
        length(Unicos,L),
        Rsp = [N,Unicos].

convertir(Numero,[],[]).        %convierte lista en fichas de nuevo xDDDDDDDD
convertir(Numero,[CH1|CT1],[RH2|RT2]):- RH2 = [Numero,CH1],
                            convertir(Numero,CT1,RT2).

dameCartasRepetidas(_,[],[]).
dameCartasRepetidas(Carta,[Mazo1|Mazo2],[Col2|T]) :-
[Num,_] = Carta,
[Num1,Col2] = Mazo1,
Num =:= Num1,
dameCartasRepetidas(Carta,Mazo2,T).

dameCartasRepetidas(Carta,[Mazo1|Mazo2],T) :-
    [Num,_] = Carta,
    [Num1,_] = Mazo1,
    not(Num =:= Num1),
    dameCartasRepetidas(Carta,Mazo2,T).

/*intermedio(L1,L2,R) :-
        tercia(L1,L2,[H|T]),
        [N,Nombres] = H,
        convertir(N,Nombres,R).
*/

