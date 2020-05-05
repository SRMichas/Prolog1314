:- dynamic turno/1.
:- dynamic mesa/1.
:- dynamic mazoJugador/2.
:- dynamic mesaJugadas/2.
:- dynamic bandera1raJugada/1.
:- dynamic mesaJugada/1.
:- dynamic ambosPasan/1.


mesaJugadas(tercias,[]).
mesaJugadas(escaleras,[]).
colores([rojo,azul,negro,verde]).

jugador(j1).
jugador(j2).

turno(_).
mesa([]).
mesaJugada([]).

bandera1raJugada(false).

ambosPasan(0).

% Empieza el juego, LLena la mesa y reparte mazo a jugadores.
empieza :-
    reseteaJuego,
    colores(ListaColores),
    generaFichas(ListaColores,MazoSC),                               %generamos todas las fichas de colores en una sola lista
    append(MazoSC,[[0,comodin],[0,comodin]],ListaBasica),            %agregarmos los 2 comodinesem
    random_permutation(ListaBasica, Revuelta),                      %revolvemos la lista
    llenaMesa(Revuelta),                                            %ponemos la baraja sobre la mesa
    revisaEmpieza(Revuelta),                                        %verificamos quien empieza
    random_permutation(Revuelta, RevueltaFinal),                    %revolvemos nuevamente
    llenaMesa(RevueltaFinal),                                       %volvemos a poner la baraja sobre la mesa
    writeln("\n\n"),
    reparte,
    primeraJugada,nl,
    miniMenu.

% Reparte Las cartar a los mazos de los jugadores    
reparte :-
        mesa(Mazo),
        sublista(Mazo,50,ListaJ1), agregaMazo(j1,ListaJ1),
        quitaCartas(ListaJ1,Mazo,SemiRepartida),
        write("----------MAZO JUGADOR-------------- "),write("j1 \n"),
        otroImprime(ListaJ1),
        sublista(SemiRepartida,52,ListaJ2), agregaMazo(j2,ListaJ2),
        quitaCartas(ListaJ2,SemiRepartida,Repartidas),
        write("----------MAZO JUGADOR-------------- "),write("j2 \n"),
        otroImprime(ListaJ2),       %irrelabante solo con fines de mostrar
        write("\n"),                %irrebalante solo con fines de mostrar
        llenaMesa(Repartidas).

% Se valida la primera jugada
primeraJugada :- turno(J),mazoJugador(J,Mazo),
                 ponerFichas(1,Mazo),cambiarTurnoNormal.
        
% Cabezera de menu.
imprimeCabezeraMiniMenu :- 
                turno(J), 
                nl,write("Jugador en turno ~~~~ ["),write(J),write("] ~~~~"),nl,
                mesa(Pila),length(Pila,L),
                write("Restantes ====>> "),write(L),nl,nl,
                mazoJugador(J,Mazo),
                imprimeMazoBonito(Mazo),nl,
                mesaJugadas(tercias,Tercias),
                mesaJugadas(escaleras,Escaleras),
                write("-----Mesa de Jugadas----- "),nl,
                write("Tercias ====>"),imprimeBonito(0,_,_,Tercias),nl,nl,
                write("Escaleras ====>"),imprimeBonito(0,_,_,Escaleras),nl,nl.

% Imprime menu principal, {P} para pasar, {F} para insertar una ficha o jugada y valida el ganador
miniMenu :-     
                imprimeCabezeraMiniMenu,        
                write("Que quieres hacer? {f} para poner fichas o {p} para pasar --> "),read(Opcion),
                (
                        Opcion == f -> reiniciarPasan,juegoNormal;
                        Opcion == p -> 
                                mesa(MesaPila),
                                length(MesaPila,L),
                                        (L =:= 0 ->
                                        
                                        ambosPasan(V), A is V+1,modificarPasan(A),
                                        nl,validarGanador(G),nl,
                                        (G =:= 0 -> miniMenu ; !)
                                        ; 
                                        pasarConComer
                                        )
                                ;
                        write("Chamaco ingrato, haz caso a lo que se te esta pidiendo >:c .... "),nl,nl,
                        miniMenu
                ).

%  Inicia el juego normal despues de la primera Jugada, acompletar o crear nueva jugada
juegoNormal:-   
                turno(J),
                mazoJugador(J,Mazo),
                write("En donde quieres aplicar la Jugada {1} -> Tercias , {2} -> Ecaleras "),read(Opcion),
                (
                        Opcion =:= 1 -> TipoJugada=tercias;
                        Opcion =:=2 -> TipoJugada = escaleras ;
                        write(" Haga caso a las intrucciones por favor"),nl,nl,miniMenu
                ),
                write("Que ficha quieres pones -->[[Ficha],[FIcha]. . .[Ficha]] --> "),read(FichasElegidas),
                (
                        checarFichasEnMazo(FichasElegidas,Mazo)->
                        validaJugada(FichasElegidas,TipoJugada),
                                validarGanador(Valor),
                                (
                                Valor =:= 0 -> cambiarTurnoNormal, miniMenu;
                                Valor =:= 1 -> write("Se acabo el Juego")
                                )
                                ;
                                write("NO se puede morro(a), checa la baraja que pusiste!!!!"),nl,
                                miniMenu
                ).

%================================================================================================================================================
% Modifica bandera de que pasen los dos cuando la pila de cartas este vacia y ya no puedan comer
modificarPasan(A):-
        retract(ambosPasan(_)),
        asserta(ambosPasan(A)).

% reinicia la bandera ambosPasan
reiniciarPasan:-
        retract(ambosPasan(_)),
        asserta(ambosPasan(0)).
%================================================================================================================================================

/*
Se verifica que el elemento dado exista en la lista
        pertenece(?Elemento,?Lista)
*/
pertenece(E,[E|_]).
pertenece(E,[_|T]):- pertenece(E,T).
/*
Se vefifica que las fichas que se mandaron existan en el mazo 
        checarFichasEnMazo(?Jugada,?MazoJugador).
*/
checarFichasEnMazo([],_).
checarFichasEnMazo([H|T],Mazo):-
                pertenece(H,Mazo), %[1,negro] -->
                checarFichasEnMazo(T,Mazo).
        
% 
validaJugada(Lista,_):- length(Lista,L),L>=3,jugar(0,Lista,Rsp,TipoJ),remueveFichasJugador(Lista),
                write("Jugada Valida ---> "),write(Rsp),
                write(" y fue una : "),write(TipoJ),nl,nl,
                agregarJugada(TipoJ,Rsp).
/*
 Revisa y procesa la jugada en base a su tipo de jugada
                        validaJugada(?Jugada,?TipodeJugada)
*/
validaJugada(Jugada,TipoJugada) :-
        ordenaMayor(Jugada,JugadaOrd),
        [FichaH|_] = JugadaOrd,
        [Numero,Color] = FichaH,
        mesaJugadas(TipoJugada,JugadasMesa),
        length(Jugada,L),L>0,L<3,write("Fichas ->"),write(Jugada),nl,nl,   % []  TERCIAS ==> [[[1,azul],[1,negro],[1,rojo]],[[1,negro],[1,verde],[1,rojo]]]
        (TipoJugada == tercias ->
                (
                        
                        (
                                (L =:= 1) ->
                                        filtroNumeroAcompletarTercia(Numero,JugadasMesa,ListaFiltrada),
                                        write("Lista FILTRADA ==>"),write(ListaFiltrada),nl,nl, 
                                        checarColores(FichaH,ListaFiltrada,JugadaDefinitiva,JugadaAbuscar),
                                        buscarReemplazar(TipoJugada,JugadaAbuscar,JugadaDefinitiva),
                                        write("Lista DEFINITIVA ==>"),print(JugadaDefinitiva),nl,nl,
                                        remueveFichasJugador(Jugada);
                                        filtroNumeroCortarTercia(Numero,JugadasMesa,ListaFiltrada),
                                        write("Lista FILTRADA ==>"),write(ListaFiltrada),nl,nl,
                                        cortarTercia(JugadaOrd,ListaFiltrada,JugadaNueva),
                                        agregarJugada(TipoJugada,JugadaNueva),
                                        write("Lista Definitiva Cortar"),print(JugadaNueva),nl,nl,
                                        write("nueva seccion en construccion"),
                                        remueveFichasJugador(Jugada)
                        )
                );
        TipoJugada == escaleras ->
                        filtroColor(Color,JugadasMesa,ListaFiltradaNum),
                        write(ListaFiltradaNum),nl,nl,
                        sort(Jugada,JugadaOrd),
                        write("Cortar {1}  o  Acompletar {2} ==> "),read(Opcion),
                        (       Opcion =:=1 -> 
                                
                                cortarEscaleras(JugadaOrd,ListaFiltradaNum,JugadaNueva,OtraJugada),
                                write("Jugada 1 :: "),write(JugadaNueva),nl,nl,
                                write("Jugada 2 :: "),write(OtraJugada),nl,nl;
                                Opcion =:= 2 -> 
                                checarNumeros(JugadaOrd,ListaFiltradaNum,JugadaDefinitiva,JugadaAbuscar),
                                buscarReemplazar(TipoJugada,JugadaAbuscar,JugadaDefinitiva),
                                print(JugadaDefinitiva),nl,nl,
                                remueveFichasJugador(JugadaOrd)
                                
                        )
        ).

/*
 Si buscar y cortar arroja resultados positivos saca la longitud de las listas 
 der y izq y revisa si se puede cortar, se da a elegir si quieres 
 cortar hacia el lado izq o der
 recursivo
 cortarEscaleras(?ListaFichas,?MesaJugadasFiltrada,?ListaCortadaIzq,?ListaCortadaDer)
 */
cortarEscaleras([],_,_,_).
cortarEscaleras([],[],_,_).
cortarEscaleras([H|T],[HJ|TJ],Resp1,Resp2) :-
        TipoJugada = escaleras,
        length([H|T],L),
        (
                L=:= 1 -> 
                        buscarYcortar(H,HJ,R1,R2),
                        length(R1,L1),length(R2,L2),
                        (
                                L1 >= 2 , L2 >= 2 ->
                                        write("Cortar a la Izquierda {1} o a la derecha {2} "),
                                        read(X),
                                        (
                                                X =:= 1 -> 
                                                    append(R1,[H],Resp1),append([H],R2,Resp2),
                                                    mesaJugadas(TipoJugada,Escaleras),
                                                    remover(HJ,Escaleras,NuevaEscalera),
                                                    write(NuevaEscalera),nl,nl,
                                                    actualizaMesaJugadas(TipoJugada,NuevaEscalera),
                                                    agregarJugada(TipoJugada,Resp1),
                                                    agregarJugada(TipoJugada,Resp2)
                                                ;
                                                X =:= 2 -> append(R1,[H],Resp1),append([H],R2,Resp2),
                                                mesaJugadas(TipoJugada,Escaleras),
                                                    remover(HJ,Escaleras,NuevaEscalera),
                                                    actualizaMesaJugadas(TipoJugada,NuevaEscalera),
                                                    agregarJugada(TipoJugada,Resp1),
                                                    agregarJugada(TipoJugada,Resp2)
                                        ) ;
                                cortarEscaleras([H|T],TJ,Resp1,Resp2)
                                
                        )

        ).
/*
   recursivo
   busca con la ficha si se encuentra en la lista de la jugada 
   y te saca las listas izq y derecha de esa ficha.
*/
buscarYcortar(_,[],[],[]).
buscarYcortar(Ficha,Lista,[],T) :-
        [H|T] = Lista,
        Ficha == H.

buscarYcortar(Ficha,Lista,[H|T2],Rsp) :-
        [H|T] = Lista,
        not(Ficha == H),
        buscarYcortar(Ficha,T,T2,Rsp).


% Recibe una lista con dos fichas y checa en la mesa de jugadas de tercias si hay alguna jugada con
% cuatro fichas para cortar una ficha y acompletar la jugada mandada y agrega la jugada modificada
% sin la ficha cortada
% cortarTercia(?ListaCartas,?ListaJugadasTerciaFiltrada,?JugadaNueva) 
cortarTercia(_,[],[]).
cortarTercia([CARTA1,CARTA2],[HM|TM],Resp):-
        length(HM,L),
        L =:= 4 -> encuentraTerciaCortar([CARTA1,CARTA2],HM,Resp,FichaAquitar),
        mesaJugadas(tercias,MesaTercias),remover(HM,MesaTercias,MesaSinJugadaAquitar),
        write("MESA SIN JUGADA A QUITAR --------->"),write(MesaSinJugadaAquitar),nl,nl,
        remover(FichaAquitar,HM,JugadaSinFicha),
        write("JUGADA SIN LA FICHA QUITADA ----->"),write(JugadaSinFicha),nl,nl,
        actualizaMesaJugadas(tercias,MesaSinJugadaAquitar),
        agregarJugada(tercias,JugadaSinFicha);

        cortarTercia([CARTA1,CARTA2],TM,Resp).
/*
Se  encarga de buscar la primera ficha que le hace falta a la jugada para completar
una tercia minima (3 elementos).
        encuentraTerciaCortar(?JugadaEscritaJugador,?JugadaMesa,?NuevaJugada,?FichaQueSeQuitara).
*/
encuentraTerciaCortar(_,[],[],_).
encuentraTerciaCortar([[N,Color1],[_,Color2]],[[_,ColorM]|_],Resp,FichaAquitar):-
        dif(Color1,ColorM),dif(Color2,ColorM),
        (
          Color2 == comodin -> NA is 0 ;
          NA = N
        ),
        append([[N,Color1],[NA,Color2]],[[N,ColorM]],Resp),
        FichaAquitar = [N,ColorM].

encuentraTerciaCortar([[N,Color1],[_,Color2]],[[_,ColorM]|T],Resp,FichaAquitar):-
    (not(dif(ColorM,Color1));not(dif(ColorM,Color2))),
    encuentraTerciaCortar([[N,Color1],[_,Color2]],T,Resp,FichaAquitar).
/*
Busca entre todas las jugadas de Tercias para averiguar donde puede entrar la ficha
que se mando buscando en Base al color
        checarColores(?Ficha,?JugadasTerciasDeLaMesa,?JugadaDeReemplazo,?JugadaAReemplazar)
*/
checarColores(_,[],_,_).
checarColores(Ficha,[H|T],JugadaDefinitiva,JugadaAbuscar):-
                verificaTerciaCompleta(Ficha,H,Resp),
                (
                    Resp =:= 1 -> append(H,[Ficha],JugadaDefinitiva),JugadaAbuscar = H,nl,nl;
                    checarColores(Ficha,T,JugadaDefinitiva,JugadaAbuscar)
                ).
/*
Busca entre todas las jugadas de Escaleras para averiguar donde puede entrar la ficha
que se mando ya sea al inicio o al final de la jugada buscando en Base al Numero
        checarNumeros(?ListaJugada,?JugadasDeEscalera,?JugadaBuena,?JugadaAQuitar)
*/
checarNumeros([],_,_,_).
checarNumeros([],[],_,_).
/*checarNumeros([[6,verde],[10,verde]],[[[7,verde],[8,verde],[9,verde]]]*/
checarNumeros([Ficha|Resto],[H|T],JugadaDefinitiva,JugadaAbuscar):-
                %write(H),nl,nl,
                verificaAcompletarEscalera(Ficha,H,Resp),
                (
                    Resp =:= 1 -> 
                    append([Ficha],H,LAux),
                    sort(LAux,LOrdAux),
                    checarNumeros(Resto,[LOrdAux],A,_),
                    append([Ficha|Resto],H,Auxiliar),JugadaAbuscar = H,
                    JugadaDefinitiva  = A,
                    sort(Auxiliar,JugadaDefinitiva),
                    write(JugadaDefinitiva),nl,nl;

                    checarNumeros([Ficha|Resto],T,_,JugadaAbuscar)
                    
                ).

/* 
Se asegura que la tercia puede acompletarse (max 4 elementos)
Regresa 1 si no existe la ficha, 0 en caso de que exista
        verificaTerciaCompleta(?Ficha,?Jugada,?Bandera)
*/
verificaTerciaCompleta(_,[],1).
verificaTerciaCompleta([N,Color],[H|T],Resp) :-
        [_,ColorC|_] = H,
        (
            not(Color == ColorC) -> verificaTerciaCompleta([N,Color],T,Resp) ; Resp = 0
        ).
/*
Se asegura que se puede acompletar una escalera por los extremos
Regresa 1 si es posible agregar, 0 en caso de que no pueda
        verificaAcompletarEscalera(?Ficha,?Jugada,?Bandera)
*/
verificaAcompletarEscalera(_,[],0).
verificaAcompletarEscalera([N,Color],Lista,Resp) :-
    [H|T] = Lista,
    %write(Lista),nl,nl,
    nth1(1,Lista,Primero),length(Lista,L),nth1(L,Lista,Ultimo),
    [NPr,_] = Primero,[NUlt,_] = Ultimo,
    (  N =:= NPr-1 -> Resp = 1 ;
       N =:= NUlt+1 -> Resp = 1;    
       verificaAcompletarEscalera([N,Color],T,Resp)
    ).
/*
Busca una Jugada dada y reemplaza dicha jugada por otra que se proporciono
       buscarReemplazar(?TipoJugada,?JugadaABuscar,?JugadaDeReemplazo)
*/
buscarReemplazar(TipoJ,JugadaBuscar,JugadaReemplazar) :-
                        %[JugadaBu|_] = JugadaB,
                        mesaJugadas(TipoJ,Tercias),
                        reemplaza(JugadaBuscar,Tercias,JugadaReemplazar,Rsp),
                        retractall(mesaJugadas(TipoJ,_)),
                        asserta(mesaJugadas(TipoJ,Rsp)).
/*
Recorre una lista de jugadas y solo reemplaza la que se le indique
        reemplaza(?JugadaABuscar,?ListaDondeSeBusca,?JugadaDeReemplazo,?ListaConLaJugadaReemplazada)
*/
reemplaza(_,[],_,[]).
reemplaza(Jugada,[H|T],JugadaR,[HR|TR]) :-
                Jugada == H ->  HR = JugadaR,  TR = T ;
                                HR = H, reemplaza(Jugada,T,JugadaR,TR).
/*
   Se envia un numero, se filtra la lista de fichas de la jugada por ese numero
   y revisa si la lista filtrada tiene almenos 3 fichas para poder acompletar con otra
   devuelve ejemplo: [1,[rojo,azul,verde]]
   filtroNumeroAcompletarTerciaa(?Numero,?ListaJugadaDeLaMesa,?ListaFiltradaPorNumero)
   
*/
% ===============================================FILTROS======================================
filtroNumeroAcompletarTercia(_,[],[]).
filtroNumeroAcompletarTercia(Numero,[H|T],[H|TR]) :-
        [[NumeroC,_]|_] = H,
        ( Numero =:= NumeroC;
          Numero =:= 0;
          NumeroC =:= 0),
        length(H,L),
        L =:= 3,
        filtroNumeroAcompletarTercia(Numero,T,TR).

filtroNumeroAcompletarTercia(Numero,[H|T],Resp) :-
        [[NumeroC,_]|_] = H,
        Numero =:= NumeroC,
        length(H,L),
        not(L =:= 3),
        filtroNumeroAcompletarTercia(Numero,T,Resp).

filtroNumeroAcompletarTercia(Numero,[H|T],Resp) :-
        [[NumeroC,_]|_] = H,
        not(Numero =:= NumeroC),
        filtroNumeroAcompletarTercia(Numero,T,Resp).

filtroNumeroCortarTercia(_,[],[]).
filtroNumeroCortarTercia(Numero,[H|T],[H|TR]) :-
        [[NumeroC,_]|_] = H,
        ( Numero =:= NumeroC;
          Numero =:= 0;
          NumeroC =:= 0),
        length(H,L),
        L =:= 4,
        filtroNumeroCortarTercia(Numero,T,TR).
filtroNumeroCortarTercia(Numero,[H|T],Resp) :-
        [[NumeroC,_]|_] = H,
        not(Numero =:= NumeroC),
        filtroNumeroCortarTercia(Numero,T,Resp).


/*
   Se envia un color y se filtra la lista de fichas de la jugada por ese color
   devuelve ejemplo: [rojo,[1,2,3,4]]
   filtroColor(?Color,?ListaJugadaDeLaMesa,?ListaFiltradaPorColor) 
*/
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
        retractall(mesaJugadas(escaleras,_)),assertz(mesaJugadas(escaleras,[])),
        reiniciarPasan.

%==============================================================================================================================

%========================================PRIMERA JUGADA===================================================================================
ponerFichas(Bandera,Mazo) :-
        jugar(Bandera,Mazo,Jugada,TipoJugada) ->
                                turno(J),
                                remueveFichasJugador(Jugada),            % elimina las cartas de la jugada del mazo del jugador en turno
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
                                %imprimeCartas(Cartitas),nl;
                                comerFicha, pasar.

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

jugar(Bandera,Mazo,Res,TipoJugada):-
                        tercia(Bandera,Mazo,Mazo,[H|_]),
                        [N,Nombres] = H,
                        convertir(N,Nombres,Res),nl,nl,
                        TipoJugada = tercias.

jugar(Bandera,Mazo,Res,TipoJugada):- 
                        escalera(Mazo,Mazo,[H|_]),
                        %write("Entro T u Sde"),
                        [Color,Numeros] = H,
                        write(Numeros),
                        recorreListasEscaleras(Numeros,Resp),
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

% reescribe la mesa de la pila de cartas para comer, cuando comen alguna carta se modifica
reescribeMesaPila(NuevaMesa):-
            retractall(mesa(_)),
            assertz(mesa(NuevaMesa)).
% reescribe la mesa de jugadas, cuando hacen jugadas o modifican se inserta la nueva mesa
% TipoJugada => tercias O escaleras
% actualizaMesaJugadas(?TipoJugada,?NuevaMesaJugadas)
actualizaMesaJugadas(TipoJugada,NuevaMesaJugadas):-
                retractall(mesaJugadas(TipoJugada,_)),
                assertz(mesaJugadas(TipoJugada,NuevaMesaJugadas)).   

      
/*
Quita las cartas de una lista de otro maso y responde con la lista de diferencia
        quitaCartas(?CartasAQuitar,?Mazo,?ListaDiferencia)
*/
quitaCartas(L1,L2,R) :-
        borrar(L1,L2,R).
/*
Quita los elementos de una lista de otra y responde con la lista de diferencia
        quitaCartas(?ListaDeElementosARemover,?ListaObjetivo,?ListaDiferencia)
*/
borrar([],X,X).
borrar([_|T],[_|Xs],R) :- borrar(T,Xs,R).

/*
Quita las cartas de un maso y responde con la lista de diferencia
        borraMazo(?CartasAQuitar,?Mazo,?ListaDiferencia)
*/
borraMazo([],Lista,Lista).
borraMazo([HJ|TJ],ListaMano,MazJugador):-
        remover(HJ,ListaMano,ListaSin),
        borraMazo(TJ,ListaSin,MazJugador).
        %write(MazJugador),nl,nl.

/*
Quita un elemeto de una lista y responde con la lista sin dicho elemento
        quitaCaremoverrtas(?Elemento,?Lista,?ListaSinElemento)
*/
remover(A,[A|X],X).     %A = Carta = B , X -> []
remover(A,[B|X],[H|Y]) :-
        dif(A,B),
        H = B,
        remover(A,X,Y).
/*
Quita las cartas de la jugada entrante del mazo del jugador en turno
*/
remueveFichasJugador(Jugada):-
        turno(J),
        mazoJugador(J,ManoActual),
        borraMazo(Jugada,ManoActual,ManoModificada),
        retractall(mazoJugador(J,_)),
        assertz(mazoJugador(J,ManoModificada)).


% Agrega Jugada a la mesa de Tipo correspondiente
% TipoJugada => tercias O escaleras
% agregarJugada(?TipoJugada,?ListaFichasJugada)
agregarJugada(TipoJugada,Jugada):- mesaJugadas(TipoJugada,TempJugada),
                                   append(TempJugada,[Jugada],RespJugada),
                                   retractall(mesaJugadas(TipoJugada,_)),
                                   assertz(mesaJugadas(TipoJugada,RespJugada)).

% Agrega un nuevo mazo al mazo del jugador 
% Jugador => jugador en turno
% agregarJugada(?JugadorEnTurno,?MazoNuevo)
agregaMazo(Jugador,Cartas) :-
        retractall(mazoJugador(Jugador,_)),
        assertz(mazoJugador(Jugador,Cartas)).

%Llenar el mazo que hay sobre de la mesa para poder comer
llenaMesa(X) :-
        retractall(mesa(_)),
        assertz(mesa(X)).

% cambia el turno de jugador
cambiaTurno(X) :-
        retract(turno(_)),
        asserta(turno(X)).

% Genera fichas para la pila de fichas para comer
generaFichas([],[]).
generaFichas([Color|T],L) :-
        fichasXcolor(Color,13,ListaParcial),        % genera lista por el color designado
        generaFichas(T,ListaAux),
        append(ListaParcial,ListaAux,L).            % fusionamos la lista 1 con la lista 2 y creamos una nueva lista

% genera la ficha por color

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
/*
Regresa una lista de elementos sin Repetir
        unicos(?ListaElemntosRepetidos,?ListaSinElementosRepetidos)
*/
unicos(Lin, Lout) :-    unicos(Lin, [], Lout).
unicos([], ACC, OUT) :- reverse(ACC, OUT).
unicos([X|Z], ACC, OUT) :- member(X, ACC),!,unicos(Z, ACC, OUT).
unicos([X|Z], ACC, OUT) :- unicos(Z, [X|ACC], OUT).

/*
        combierte una lista filtrada por color o por numero y la combierte en forma Habitual de fichas
        convertir(?numeroOColor,?ListaFiltradaPorColorONumero,?ListaHabitual)
        [1,[rojo,verde,azul]] ==> [[1,rojo],[1,verde],[1,azul]]
*/
convertir(Numero,[],[]).
convertir(Numero,[CH1|CT1],[RH2|RT2]):- 
                    number(Numero),
                    (
                        CH1 == comodin -> NumAux is 0 ;
                        NumAux = Numero
                    ),
                    RH2 = [NumAux,CH1],
                    convertir(Numero,CT1,RT2).
convertir(Color,[Numero|CT1],[RH2|RT2]):-
                not(number(Color)),
                RH2 = [Numero,Color],             % carta
                convertir(Color,CT1,RT2).
% ==============================================APARTADO PARA VER QUIEN EMPIEZA===========================
% Revisa quien empieza al principio del juego sacando las primeras ficha, el que tenga la ficha
% mas grande va primero
revisaEmpieza([Carta1,Carta2|T]) :- /*IF Revisa Empieza*/
                [Num1,_] = Carta1,
                [Num2,_] = Carta2,
                (
                  Num1 > Num2 -> cambiaTurno(j1), write(j1), writeln(" tu empiezas");
                  Num1 < Num2 -> cambiaTurno(j2), write(j2), writeln(" tu empiezas");
                  revisaEmpieza(T)
                ).
% ==============================================APARTADO TERCIAS==========================================
mayor(Lista,Rsp) :- sort(0, @>, Lista, Rsp). %divideColor(rojo,[[3,rojo],[1,rojo],[2,rojo]],X).
ordenaMayor(Lista,Respuesta) :- sort(0, @>, Lista, Respuesta).

tercia(_,[],_,[]).
tercia(Bandera,[H|T],Mazo,[HR|TR]):-                   % si hay tercia en el mazo
    (
        Bandera =:= 0 -> Jugada = [H|T],     
        sort(0,@>=,Jugada,Nueva),
        [HA|_] = Nueva;
        Bandera =:= 1 -> HA = H
    ),
    dameCartasSinRepetir(Bandera,HA,Mazo,Rspa,L),        % devuelve la tercia
    (L >= 3 , L =< 4),                          % longitud de 3 0 4 ?
    HR = Rspa,                                  % smn, agregamos a la lista
    tercia(Bandera,T,Mazo,TR).

tercia(Bandera,[H|T],Mazo,TR):-                 % no hay tercia
    (
        Bandera =:= 0 -> Jugada = [H|T],     
        sort(0,@>=,Jugada,Nueva),
        [HA|_] = Nueva;
        Bandera =:= 1 -> HA = H
    ),
    dameCartasSinRepetir(Bandera,HA,Mazo,_,L),   % devuelve la "tercia"
    not(L=3),not(L=4),                  % longitud de que no sea 3 o 4
    tercia(Bandera,T,Mazo,TR).                   

dameCartasSinRepetir(Bandera,Carta,Mazo,Rsp,L) :-
        [N,_] = Carta,
        dameCartasRepetidas(Bandera,Carta,Mazo,Repetidas),      % devolver la lista aunque haya repetidos
        unicos(Repetidas,Unicos),                       % devuelve lista de colores sin repetir
        length(Unicos,L),                               % obtenemos la longitud de la lista
        Rsp = [N,Unicos].

dameCartasRepetidas(_,_,[],[]).                           
dameCartasRepetidas(Bandera,Carta,[Mazo1|Mazo2],[Col2|T]) :-    % verifica si hay cartas con el mismo nuemro
                [Num,_] = Carta,
                [Num1,Col2] = Mazo1,
                (
                  Bandera =:= 0 ->                                              
                        ( Num =:= Num1 -> dameCartasRepetidas(Bandera,Carta,Mazo2,T) %si son del mismo numero o es cero, guardamos la carta
                                ;                    
                          Num1 =:= 0 -> dameCartasRepetidas(Bandera,Carta,Mazo2,T) )
                ;
                  Bandera =:= 1 -> 
                        Num =:= Num1 -> dameCartasRepetidas(Bandera,Carta,Mazo2,T)
                ).

dameCartasRepetidas(Bandera,Carta,[Mazo1|Mazo2],T) :-           % verifica si hay cartas con el mismo nuemro
                [Num,_] = Carta,
                [Num1,_] = Mazo1,
                (
                  Bandera =:= 0 ->
                        ( not(Num =:= Num1) -> dameCartasRepetidas(Bandera,Carta,Mazo2,T) %si son del mismo numero o es cero, guardamos la carta
                        ;                    
                          not(Num1 =:= 0) -> dameCartasRepetidas(Bandera,Carta,Mazo2,T) )
                ;
                  Bandera =:= 1 ->
                        not(Num =:= Num1) -> dameCartasRepetidas(Bandera,Carta,Mazo2,T)
                ).                   %si son diferentes, ignoramos y avanzamos


% ========================================APARTADO ESCALERA===========================
% verifica si es una escalera valida .
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

% recursivo
% recorre las el mazo del jugador en busca de escaleras validas
% recorreListasEscaleras(?mazoJugador,?ListaEscalerasValidas)
recorreListasEscaleras([],Res).
recorreListasEscaleras([H,J|T],Res):-
    validaEscalera([H,J|T],[],Res),
    length(Res,L),
    L >= 3.

recorreListasEscaleras([_,J|T],Res):-
    append([J],T,ListaCon),
    recorreListasEscaleras(ListaCon,Res).


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

/*Valida si los elementos de una lista son numeros consecutivos
*/
validaEscalera([X],ListaA,ListaRsp):- append(ListaA,[X],ListaRsp).
validaEscalera([Actual,SigN|T],ListaA,Rsp) :- 
    Actual =:= SigN-1,
    append(ListaA,[Actual],ListaAU),
    append([SigN],T,Lista),
    validaEscalera(Lista,ListaAU,Rsp).

validaEscalera([Actual,SigN|_],ListaA,Rsp) :-
    not(Actual =:= SigN-1),
    append(ListaA,[Actual],Rsp).

divideColor(_,[],[]).
divideColor(Color,[Carta|Resto],Respuesta) :-
        [_,Col] = Carta,
        Color == Col ->
                [H|T] = Respuesta,
                H = Carta,
                divideColor(Color,Resto,T);
        divideColor(Color,Resto,Respuesta).

imprimeBonito(_,_,_,[]).
imprimeBonito(Bandera,Limite,Contador,[H|T]) :-
        Bandera =:=  1 ->
        (Contador > 0 ->
                CA is Contador - 1,
                write(H),write(" ");
                CA = Limite,
                writeln(H)
        ),
        imprimeBonito(Bandera,Limite,CA,T);
        write(H),write("  "),
        imprimeBonito(Bandera,_,_,T).
imprimeMazoBonito(Mazo) :-
        nl,write("Rojo -> "),
        divideColor(rojo,Mazo,SinOrdenarRoj),sort(SinOrdenarRoj,OrdRojo),
        imprimeBonito(0,_,_,OrdRojo),nl,nl,
        write("Azul -> "),
        divideColor(azul,Mazo,SinOrdenarAzu),sort(SinOrdenarAzu,OrdAZul),
        imprimeBonito(0,_,_,OrdAZul),nl,nl,
        write("Verde -> "),
        divideColor(verde,Mazo,SinOrdenarVer),sort(SinOrdenarVer,OrdVerde),
        imprimeBonito(0,_,_,OrdVerde),nl,nl,
        write("Negro -> "),
        divideColor(negro,Mazo,SinOrdenarNeg),sort(SinOrdenarNeg,OrdNegro),
        imprimeBonito(0,_,_,OrdNegro),nl,nl,
        write("Comodin -> "),
        divideColor(comodin,Mazo,Comodines),imprimeBonito(0,_,_,Comodines),nl.

% ==== ===========================================================APARTADO A VER QUIEN GANA========================

% valida al ganador , el que se quede sin fichas gana, o si la pila no tiene fichas para comer,
% Si los dos pasanse valida el que tenga menos fichas en su mazo y ese gana.
validarGanador(Valor) :-
    turno(J),
    mazoJugador(J,Mazo),
    mesa(PilaCartas),
    ambosPasan(V1),
    length(Mazo,LMazo),
    length(PilaCartas,LPila),
    (
            LMazo =:= 0 -> 
                    write("Se Acabo el Juego Sin cartas..... "),nl,
                    write(J), write(" A ganado el juego"),
                    Valor = 1       ;
            LPila =:= 0 ->
            (
                    V1 =:= 2 -> 
                            mazoJugador(j1,MazoJ1),
                            mazoJugador(j2,MazoJ2),
                            length(MazoJ1,LMJ1),
                            length(MazoJ2,LMJ2),
                    (       
                            LMJ1 < LMJ2 -> 
                                    write("Se Acabo el Juego sin pila..... "),nl,write("j1"),
                                    write(" A ganado el juego"),Valor = 1 ;
                            LMJ1 > LMJ2 -> 
                                    write("Se Acabo el Juego sin pila..... "),nl,write("j2"),
                                    write(" A ganado el juego"), Valor = 1
                    ); 
                    Valor = 0
            );
            Valor = 0
    ).