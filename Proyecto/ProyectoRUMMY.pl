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

mazoJugador(jp,[[1,verde],[1,rojo],[1,azul],[7,rojo],[7,rojo]]).

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
    random_permutation(ListaBasica, Revuelta),                      %revolvemos la lista
    llenaMesa(Revuelta),                                            %ponemos la baraja sobre la mesa
    revisaEmpieza2(Revuelta),                                        %verificamos quien empieza
    random_permutation(Revuelta, RevueltaFinal),                    %revolvemos nuevamente
    llenaMesa(RevueltaFinal),                                       %volvemos a poner la baraja sobre la mesa
    writeln("\n\n"),
    reparte,
    primeraJugada,nl,
    %write("paso lo primero"),nl,
    miniMenu.
    

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
                 ponerFichas2(Mazo),cambiarTurnoNormal.

revisaEscribe :-
        read(X),writeln(X).

imprimeCabezeraMiniMenu :- 
                turno(J), 
                nl,write("Jugador en turno ~~~~ ["),write(J),write("] ~~~~"),nl,
                mazoJugador(J,Mazo),write(Mazo),nl,nl,
                mesaJugadas(tercias,Tercias),
                mesaJugadas(escaleras,Escaleras),
                write("-----Mesa de Jugadas----- "),nl,
                write("Tercias ====>"),write(Tercias),nl,nl,
                write("Escaleras ====>"),write(Escaleras),nl,nl.

miniMenu :-     
                imprimeCabezeraMiniMenu,        
                write("Que quieres hacer? {f} para poner fichas o {p} para pasar --> "),read(Opcion),
                (
                        Opcion == f -> juegoNormal ;
                        Opcion == p -> pasarConComer ;
                        write("Chamaco ingrato, haz caso a lo que se te esta pidiendo >:c .... "),nl,nl,
                        miniMenu
                ).

juegoNormal:-   
                turno(J),
                mazoJugador(J,Mazo),
                write("En donde quieres aplicar la Jugada {1} -> Tercias , {2} -> Ecaleras "),read(Opcion),
                (
                        Opcion =:= 1 -> TipoJugada=tercias;
                        Opcion=:=2 -> TipoJugada = escaleras ;
                        write(" Che pendejo no sabes leer "),nl,nl,miniMenu
                ),
                write("Que ficha quieres pones -->[[Ficha],[FIcha]. . .[Ficha]] --> "),read(FichasElegidas),
                (checarInt(FichasElegidas,Mazo)->
                                proc(FichasElegidas,TipoJugada),cambiarTurnoNormal,miniMenu;
                                write("NO se puede morro(a), checa la baraja que pusiste!!!!"),nl,
                                miniMenu
                ).
/*
si , las fichas elegidas, encajan en una jugada existente.
caele al discord.... --> 5:00 pm

*/

%================================================================================================================================================



%================================================================================================================================================

pertenece(E,[E|_]).
pertenece(E,[_|T]):- pertenece(E,T).

checarInt([],_).
checarInt([H|T],Mazo):-
                pertenece(H,Mazo), %[1,negro] -->
                checarInt(T,Mazo).

proc(Lista,_):- length(Lista,L),L>=3,jugar1(Lista,Rsp,TipoJ),intermedio(Lista),
                write("Jugada Valida ---> "),write(Rsp),
                write(" y fue una : "),write(TipoJ),nl,nl,
                agregarJugada(TipoJ,Rsp).
%[[1,rojo],[1,rojo]]
/*proc(Jugada) :- 
        [FichaH|T] = Jugada,
        [Numero,Color] = FichaH,
        mesaJugadas(tercias,Tercias),
        length(Jugada,L),L>0,L<3,write("Fichas ->"),write(Jugada),nl,nl,
        filtroNumero(Numero,Tercias,ListaFiltrada),
        checarColores(Color,ListaFiltrada,JugadaDefinitiva).*/
% Aqui nos quedamos

/* ?- checarColores(negro,[[[1,azul],[1,verde],[1,negro]],[[1,verde],[1,rojo],[1,azul]]],R).      
        R = [[1, verde], [1, rojo], [1, azul]].*/

proc(Jugada,TipoJugada) :-
        [FichaH|T] = Jugada,
        [Numero,Color] = FichaH,
        mesaJugadas(TipoJugada,JugadasMesa),
        length(Jugada,L),L>0,L<3,write("Fichas ->"),write(Jugada),nl,nl,   % []  TERCIAS ==> [[[1,azul],[1,negro],[1,rojo]],[[1,negro],[1,verde],[1,rojo]]]
        (TipoJugada == tercias ->
                (
                        
                        (
                                (L =:= 1) ->
                                        filtroNumero(Numero,JugadasMesa,ListaFiltrada),
                                        write("Lista FILTRADA ==>"),write(ListaFiltrada),nl,nl, 
                                        checarColores2(FichaH,ListaFiltrada,JugadaDefinitiva,JugadaAbuscar),
                                        buscarReemplazar(TipoJugada,JugadaAbuscar,JugadaDefinitiva),
                                        write("Lista DEFINITIVA ==>"),print(JugadaDefinitiva),nl,nl,
                                        intermedio(Jugada);
                                        filtroNumeroCortar(Numero,JugadasMesa,ListaFiltrada),
                                        write("Lista FILTRADA ==>"),write(ListaFiltrada),nl,nl,
                                        cortarTercia(Jugada,ListaFiltrada,JugadaNueva),
                                        agregarJugada(TipoJugada,JugadaNueva),
                                        write("Lista Definitiva Cortar"),print(JugadaNueva),nl,nl,
                                        write("nueva seccion en construccion"),
                                        intermedio(Jugada)
                        )
                );
        TipoJugada == escaleras ->
                        filtroColor(Color,JugadasMesa,ListaFiltradaNum),
                        write(ListaFiltradaNum),nl,nl,
                        checarNumeros(FichaH,ListaFiltradaNum,JugadaDefinitiva,JugadaAbuscar),
                        buscarReemplazar(TipoJugada,JugadaAbuscar,JugadaDefinitiva),
                        print(JugadaDefinitiva),nl,nl,
                        intermedio(Jugada) 
        ).

        
        /*Ficha = [[9,negro]]                                             TRUE = 1
        ListaFiltrada = [[[9, verde], [9, azul], [9, negro]], [[9, verde], [9, azul], [9, rojo]]]
        JugadaDefinitiva = [[9, verde], [9, azul], [9, rojo],[9,negro]]
        TipoJ = tercias.*/

% Aqui nos quedamos
cortarTercia(_,[],[]).
cortarTercia([CARTA1,CARTA2],[HM|TM],Resp):-
        length(HM,L),
        L =:= 4 -> encuentraTerciaCortar([CARTA1,CARTA2],HM,Resp,FichaAquitar),
        mesaJugadas(tercias,MesaTercias),remover2(HM,MesaTercias,MesaSinJugadaAquitar),
        write("MESA SIN JUGADA A QUITAR --------->"),write(MesaSinJugadaAquitar),nl,nl,
        remover2(FichaAquitar,HM,JugadaSinFicha),
        write("JUGADA SIN LA FICHA CULERA ----->"),write(JugadaSinFicha),nl,nl,
        actualizaMesaJugadas(tercias,MesaSinJugadaAquitar),
        agregarJugada(tercias,JugadaSinFicha);
            cortarTercia([CARTA1,CARTA2],TM,Resp).


        
encuentraTerciaCortar(_,[],[],_).
encuentraTerciaCortar([[N,Color1],[_,Color2]],[[_,ColorM]|_],Resp,FichaAquitar):-
        dif(Color1,ColorM),dif(Color2,ColorM),
        append([[N,Color1],[N,Color2]],[[N,ColorM]],Resp),
        FichaAquitar = [N,ColorM].

encuentraTerciaCortar([[N,Color1],[_,Color2]],[[_,ColorM]|T],Resp,FichaAquitar):-
    (not(dif(ColorM,Color1));not(dif(ColorM,Color2))),
    encuentraTerciaCortar([[N,Color1],[_,Color2]],T,Resp,FichaAquitar).
        

checarColores2(_,[],_,_).
checarColores2(Ficha,[H|T],JugadaDefinitiva,JugadaAbuscar):-
                %write(H),nl,nl,
                predicadoSoria2(Ficha,H,Resp),
                (
                    Resp =:= 1 -> append(H,[Ficha],JugadaDefinitiva),JugadaAbuscar = H,/*write(JugadaDefinitiva),*/nl,nl;
                    checarColores2(Ficha,T,JugadaDefinitiva,JugadaAbuscar)
                ).

checarNumeros(_,[],_,_).
checarNumeros(Ficha,[H|T],JugadaDefinitiva,JugadaAbuscar):-
                %write(H),nl,nl,
                predicadoSoria3(Ficha,H,Resp),
                (
                    Resp =:= 1 -> append([Ficha],H,JugadaDefinitiva),JugadaAbuscar = H,write(Lista),nl,nl;
                    Resp =:= 2 -> append(H,[Ficha],JugadaDefinitiva),JugadaAbuscar = H,write(Lista),nl,nl;
                    checarNumeros(Ficha,T,_,JugadaAbuscar)
                    
                ).

predicadoSoria2(_,[],1).
predicadoSoria2([N,Color],[H|T],Resp) :-
        [_,ColorC|_] = H,
        (
            not(Color == ColorC) -> predicadoSoria2([N,Color],T,Resp) ; Resp = 0
        ).

predicadoSoria3(_,[],0).
predicadoSoria3([N,Color],Lista,Resp) :-
    [H|T] = Lista,
    %write(Lista),nl,nl,
    nth1(1,Lista,Primero),length(Lista,L),nth1(L,Lista,Ultimo),
    [NPr,_] = Primero,[NUlt,_] = Ultimo,
    (  N =:= NPr-1 -> Resp = 1 ;
       N =:= NUlt+1 -> Resp = 2 ;    
       predicadoSoria3([N,Color],T,Resp)
    ).

buscarReemplazar(TipoJ,JugadaBuscar,JugadaReemplazar) :-
                        %[JugadaBu|_] = JugadaB,
                        mesaJugadas(TipoJ,Tercias),
                        reemplaza(JugadaBuscar,Tercias,JugadaReemplazar,Rsp),
                        retractall(mesaJugadas(TipoJ,_)),
                        asserta(mesaJugadas(TipoJ,Rsp)).

reemplaza(_,[],_,[]).
reemplaza(Jugada,[H|T],JugadaR,[HR|TR]) :-
                Jugada == H ->  HR = JugadaR,  TR = T ;
                                HR = H, reemplaza(Jugada,T,JugadaR,TR).


filtroNumero(_,[],[]).
filtroNumero(Numero,[H|T],[H|TR]) :-
        [[NumeroC,_]|_] = H,
        Numero =:= NumeroC,
        length(H,L),
        L =:= 3,
        filtroNumero(Numero,T,TR).

filtroNumero(Numero,[H|T],Resp) :-
        [[NumeroC,_]|_] = H,
        Numero =:= NumeroC,
        length(H,L),
        not(L =:= 3),
        filtroNumero(Numero,T,Resp).

filtroNumero(Numero,[H|T],Resp) :-
        [[NumeroC,_]|_] = H,
        not(Numero =:= NumeroC),
        filtroNumero(Numero,T,Resp).

filtroNumeroCortar(_,[],[]).
filtroNumeroCortar(Numero,[H|T],[H|TR]) :-
        [[NumeroC,_]|_] = H,
        Numero =:= NumeroC,
        length(H,L),
        L =:= 4,
        filtroNumero(Numero,T,TR).
filtroNumeroCortar(Numero,[H|T],Resp) :-
        [[NumeroC,_]|_] = H,
        not(Numero =:= NumeroC),
        filtroNumero(Numero,T,Resp).


filtroColor(_,[],[]).
filtroColor(Color,[H|T],[H|TR]) :-
        [[_,ColorC]|_] = H,
        Color == ColorC,
        filtroColor(Color,T,TR).

filtroColor(Color,[H|T],Resp) :-
        [[_,ColorC]|_] = H,
        not(Color == ColorC),
        filtroColor(Color,T,Resp).



% ==========================================APARTADO DE RESETEO DE VARIABLES DEL JUEGO============================
reseteaJuego :-
        modificaBandera1(false),
        retract(turno(_)),assert(turno(_)), %<-----------
        retractall(mazoJugador(j1,_)),assertz(mazoJugador(j1,[])),
        retractall(mazoJugador(j2,_)),assertz(mazoJugador(j2,[])),
        retractall(mesa(_)),assertz(mesa([])),
        retractall(mesaJugadas(tercias,_)),assertz(mesaJugadas(tercias,[])),
        retractall(mesaJugadas(escaleras,_)),assertz(mesaJugadas(escaleras,[])). 

%==============================================================================================================================
ponerFichas2(Mazo) :-
        jugar1(Mazo,Jugada,TipoJugada) ->
                                turno(J),
                                intermedio(Jugada),            % elimina las cartas de la jugada del mazo del jugador en turno
                                agregarJugada(TipoJugada,Jugada),        
                                mazoJugador(J,Cartitas),
                                modificaBandera1(true),
                                write("==================================================================[ "),
                                print(J),
                                write(" ]=================================================================="),
                                nl,
                                write("Jugada Realizada {"),write(TipoJugada),write("} --->  "),write(Jugada),nl,
                                write("--------Cartas modificadas de "),write(J),
                                write("----------"),nl,
                                write(Cartitas),nl;
                                comerFicha, pasar.
%==============================================================================================================================


modificaBandera1(V) :- retract(bandera1raJugada(_)), asserta(bandera1raJugada(V)).

pasar:- cambiarTurno, primeraJugada.
                %turno(J),mazoJugador(J,Mazo),
pasarConComer :- comerFicha,cambiarTurnoNormal,miniMenu.

cambiarTurno:-
                jugador(NuevoJugador),
                turno(JEnTurno),
                bandera1raJugada(B), B == false,
                NuevoJugador \= JEnTurno,
                retract(turno(_)), assert(turno(NuevoJugador)).

cambiarTurnoNormal:-
        jugador(NuevoJugador),
        turno(JEnTurno),
        bandera1raJugada(B), B == true,
        NuevoJugador \= JEnTurno,
        retract(turno(_)), assert(turno(NuevoJugador)).

jugar1(Mazo,Res,TipoJugada):- tercia(Mazo,Mazo,[H|_]),
                        [N,Nombres] = H,
                        convertir(N,Nombres,Res),nl,nl,
                        TipoJugada = tercias.

jugar1(Mazo,Res,TipoJugada):- escalera(Mazo,Mazo,[H|_]),
                        %write("Entro T u Sde"),
                        [Color,Numeros] = H,
                        write(Numeros),
                        intermedioE(Numeros,Resp),
                        convertir(Color,Resp,Res),nl,nl,
                        TipoJugada = escaleras.
                                        
comerFicha:-
                turno(Jugador),
                mazoJugador(Jugador,MazoActual),
                mesa([H|T]),
                append(MazoActual,[H],MazoModificado),
                /*write("________________________________________________"),
                write("Este mazo pertenece a [ "),write(Jugador),write(" ] y acaba de comer porque no tiene juego"),
                write("________________________________________________"),*/nl,
                write(MazoModificado),nl,nl,
                retractall(mazoJugador(Jugador,_)),
                assertz(mazoJugador(Jugador,MazoModificado)),
                reescribeMesaPila(T).

reescribeMesaPila(NuevaMesa):-
            retractall(mesa(_)),
            assertz(mesa(NuevaMesa)).

actualizaMesaJugadas(TipoJugada,NuevaMesaJugadas):-
                retractall(mesaJugadas(TipoJugada,_)),
                assertz(mesaJugadas(TipoJugada,NuevaMesaJugadas)).   

      

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

intermedio(Jugada):-
        turno(J),
        mazoJugador(J,ManoActual),
        borraMaz2(Jugada,ManoActual,ManoModificada),
        write("Mano Actual --> "),write(ManoActual),nl,
        write("Mano Nueva --> "),write(ManoModificada),nl,nl,
        retractall(mazoJugador(J,_)),
        assertz(mazoJugador(J,ManoModificada)).

agregarJugada(TipoJugada,Jugada):- mesaJugadas(TipoJugada,TempJugada),
                                   append(TempJugada,[Jugada],RespJugada),
                                   retractall(mesaJugadas(TipoJugada,_)),
                                   assertz(mesaJugadas(TipoJugada,RespJugada)).


remover(A,[A|X],X).     %A = Carta = B , X -> []
remover(A,[B|X],[B|Y]) :- remover(A,X,Y).

remover(Elemento,Lista):-
    remover(Elemento,Lista,_).  %Remueve dicho elemento de la lista
    %reescribeMesa(Resultado).

agregaMazo(Jugador,Cartas) :-
        retractall(mazoJugador(Jugador,_)),
        assertz(mazoJugador(Jugador,Cartas)).

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

unicos(Lin, Lout) :-    unicos(Lin, [], Lout).
unicos([], ACC, OUT) :- reverse(ACC, OUT).
unicos([X|Z], ACC, OUT) :- member(X, ACC),!,unicos(Z, ACC, OUT).
unicos([X|Z], ACC, OUT) :- unicos(Z, [X|ACC], OUT).


convertir(Numero,[],[]).
convertir(Numero,[CH1|CT1],[RH2|RT2]):- 
                    number(Numero),
                    RH2 = [Numero,CH1],
                    convertir(Numero,CT1,RT2).
convertir(Color,[Numero|CT1],[RH2|RT2]):-
                not(number(Color)),
                RH2 = [Numero,Color],             % carta
                convertir(Color,CT1,RT2).
% ==============================================APARTADO PARA VER QUIEN EMPIEZA===========================
revisaEmpieza2([Carta1,Carta2|T]) :- /*IF Revisa Empieza*/
                [Num1,_] = Carta1,
                [Num2,_] = Carta2,
                (
                  Num1 > Num2 -> cambiaTurno(j1), write(j1), writeln(" tu empiezas");
                  Num1 < Num2 -> cambiaTurno(j2), write(j2), writeln(" tu empiezas");
                  revisaEmpieza2(T)
                ).
% ==============================================APARTADO TERCIAS==========================================
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

% ========================================APARTADO ESCALERA===========================
escalera([],_,[]).
escalera([H|T],Mazo,[HR|TR]):-
    dameCartasSinRepetirEsc(H,Mazo,Rspa,L),
    L >= 3,
    HR = Rspa,
    escalera(T,Mazo,TR).

escalera([H|T],Mazo,JugaSinproce):-%Mazo = [H|T],
        dameCartasSinRepetirEsc(H,Mazo,_,L),
        L < 3,
        escalera(T, Mazo, JugaSinproce).

intermedioE([],Res).
intermedioE([H,J|T],Res):-
    validaEscalera([H,J|T],[],Res),
    length(Res,L),
    L >= 3.

intermedioE([_,J|T],Res):-
    append([J],T,ListaCon),
    intermedioE(ListaCon,Res).
                
dameCartasSinRepetirEsc(Carta,Mazo,Rsp,L) :-
        [_,C] = Carta,
        dameCartasRepetidasEsc(Carta,Mazo,Repetidas),   %devolver la lista aunqye haya repetidos
        unicos(Repetidas,Unicos), 
        sort(Unicos, UnicosOrd),         %devuelve lista sin repetir
        length(UnicosOrd,L),
        L>=3,
        Rsp = [C,UnicosOrd].

dameCartasRepetidasEsc(_,[],[]).
dameCartasRepetidasEsc(Carta,[Mazo1|Mazo2],[Num2|T]) :-
    [_,C] = Carta,
    [Num2,Col2] = Mazo1,
    C == Col2,
    dameCartasRepetidasEsc(Carta,Mazo2,T).

dameCartasRepetidasEsc(Carta,[Mazo1|Mazo2],T) :-
    [_,C] = Carta,
    [_,Col2] = Mazo1,
    not(C == Col2),
    dameCartasRepetidasEsc(Carta,Mazo2,T).

validaEscalera([X],ListaA,ListaRsp):- append(ListaA,[X],ListaRsp).
validaEscalera([Actual,SigN|T],ListaA,Rsp) :- 
    Actual =:= SigN-1,
    append(ListaA,[Actual],ListaAU),
    append([SigN],T,Lista),
    validaEscalera(Lista,ListaAU,Rsp).

validaEscalera([Actual,SigN|_],ListaA,Rsp) :-
    not(Actual =:= SigN-1),
    append(ListaA,[Actual],Rsp).

/*
jugador 1 = 10
jugador 2 = 12

1 2 3 - 5 6 7 - 4 5 6

1 1 1 1 - 3 3 3 - 3 3 3 

*/