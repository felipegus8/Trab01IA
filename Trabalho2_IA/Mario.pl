:- dynamic([mario_location/1,
loc_powerup/1,
loc_inimigo/1,
loc_teletransporte/1,
loc_ouro/1,
loc_poco/1,
energy/1,
tamanho_mundo/1,
casas_visitadas/1
]).

%% Inicializando o mapa
tile(1,1).
tile(1,2).
tile(1,3).
tile(1,4).
tile(1,5).
tile(1,6).
tile(1,7).
tile(1,8).
tile(1,9).
tile(1,10).
tile(1,11).
tile(1,12).
tile(2,1).
tile(2,2).
tile(2,2).
tile(2,3).
tile(2,4).
tile(2,5).
tile(2,6).
tile(2,7).
tile(2,8).
tile(2,9).
tile(2,10).
tile(2,11).
tile(3,1).
tile(3,2).
tile(3,3).
tile(3,4).
tile(3,5).
tile(3,6).
tile(3,7).
tile(3,8).
tile(3,9).
tile(3,10).
tile(3,11).
tile(3,12).
tile(4,1).
tile(4,2).
tile(4,3).
tile(4,4).
tile(4,5).
tile(4,6).
tile(4,7).
tile(4,8).
tile(4,9).
tile(4,10).
tile(4,11).
tile(4,12).
tile(5,1).
tile(5,2).
tile(5,3).
tile(5,4).
tile(5,5).
tile(5,6).
tile(5,7).
tile(5,8).
tile(5,9).
tile(5,10).
tile(5,11).
tile(5,12).
tile(6,1).
tile(6,2).
tile(6,3).
tile(6,4).
tile(6,5).
tile(6,6).
tile(6,7).
tile(6,8).
tile(6,9).
tile(6,10).
tile(6,11).
tile(7,1).
tile(7,2).
tile(7,3).
tile(7,4).
tile(7,5).
tile(7,6).
tile(7,7).
tile(7,8).
tile(7,9).
tile(7,10).
tile(7,11).
tile(7,12).
tile(8,1).
tile(8,2).
tile(8,3).
tile(8,4).
tile(8,5).
tile(8,6).
tile(8,7).
tile(8,8).
tile(8,9).
tile(8,10).
tile(8,11).
tile(8,12).
tile(9,1).
tile(9,2).
tile(9,3).
tile(9,4).
tile(9,5).
tile(9,6).
tile(9,7).
tile(9,8).
tile(9,9).
tile(9,10).
tile(9,11).
tile(9,12).
tile(10,1).
tile(10,2).
tile(10,3).
tile(10,4).
tile(10,5).
tile(10,6).
tile(10,7).
tile(10,8).
tile(10,9).
tile(10,10).
tile(10,11).
tile(10,12).
tile(11,1).
tile(11,2).
tile(11,3).
tile(11,4).
tile(11,5).
tile(11,6).
tile(11,7).
tile(11,8).
tile(11,9).
tile(11,10).
tile(11,11).
tile(11,12).
tile(12,1).
tile(12,2).
tile(12,3).
tile(12,4).
tile(12,5).
tile(12,6).
tile(12,7).
tile(12,8).
tile(12,9).
tile(12,10).
tile(12,11).
tile(12,12).

adj(1,2).
adj(2,1).
adj(2,3).
adj(3,2).
adj(3,4).
adj(4,3).
adj(5,4).
adj(4,5).
adj(6,5).
adj(5,6).
adj(7,6).
adj(6,7).
adj(8,7).
adj(7,8).
adj(9,8).
adj(8,9).
adj(10,9).
adj(9,10).
adj(10,11).
adj(11,10).
adj(12,11).
adj(11,12).



tamanho_mundo(12).

%Só pra poder testar sem o java ainda

loc_ouro([9,2]).
loc_ouro([3,8]).
loc_ouro([11,11]).

loc_inimigo([4,2]).
loc_inimigo([7,5]).
loc_inimigo([10,11]).
loc_inimigo([11,5]).

loc_teletransporte([10,12]).
loc_teletransporte([4,2]).
loc_teletransporte([1,5]).
loc_teletransporte([7,10]).

loc_poco([2,2]).
loc_poco([10,3]).
loc_poco([4,5]).
loc_poco([7,4]).
loc_poco([10,7]).
loc_poco([11,10]).
loc_poco([3,10]).
loc_poco([5,11]).

loc_powerup([1,1]).
loc_powerup([7,6]).
loc_powerup([2,11]).

%Init

init_jogo:-
retractall(mario_location(_,_)),assert(mario_location([1,1]))
retractall(energy(_)),assert(energy(100)),
retractall(visitados(_)),assert(visitados(1)),
retractall(einimigo(_,_)),retractall(eouro(_,_)),
retractall(eteletransporte(_,_)),retractall(epoco(_,_)),
retractall(casas_visitadas(_)),assert(casas_visitadas([])).


visitar(Xs):-casas_visitadas(Ys),retractall(casas_visitadas(_)),assert(casas_visitadas([Ys|Xs])).

adjacent( [X1, Y1], [X2, Y2] ) :-
( X1 = X2, adj( Y1, Y2 )
; Y1 = Y2, adj( X1, X2 )
).



%Percepções
formar_percepcao([_pas,_bris,_flas,_bri]) :- mario_location([X,Y]),ouviu_passos([X,Y]),sentiu_brisa([X,Y]),percebeu_flash([X,Y]),percebeu_brilho([X,Y]).

ouviu_passos(L1) :- loc_inimigo(L2),adjacent(L1,L2).

sentiu_brisa(L1) :- loc_poco(L2),adjacent(L1,L2).

percebeu_flash(L1) :-  loc_teletransporte(L2),adjacent(L1,L2).

percebeu_brilho([X1,Y1]) :- loc_ouro([X2.Y2]),X1 = X2,Y1 = Y2.


passos(yes):- position([X,Y]),ouviu_passos([X,Y]).
passos(no).

brisa(yes):- position([X,Y]),sentiu_brisa([X.Y]).
brisa(no).

flash(yes):- position([X,Y]),percebeu_flash([X,Y]).
flash(no).

brilho(yes):- position([X,Y]),percebeu_brilho([X,Y]).
brilho(no).

%Atualiza pontuação

update_energy :-
mario_location(ML),
loc_ouro(GL),
loc_poco(PL),
loc_teletransporte(TL),
loc_inimigo(EL),
loc_powerup(UL),
update_energy(ML, GL,PL,TL,EL,UL).

update_energy(P) :- energy(E),
newEnergy is E+P,
retractall(energy(_)),
assert(energy(newEnergy)).



update_energy(ML,ML,_,_,_,_) :- update_energy(1000).

update_energy(_,_,_,_,_,_) :- update_energy(-1).

update_energy(ML,_,ML,_,_,_) :- update_energy(-1000).

update_energy(ML,_,_,_,ML,_) :- random_between(20,50,X),update_energy(-X).

update_energy(ML,_,_,_,_,ML) :- update_energy(20).

%Base de conhecimento

adiciona_inimigo(no):- mario_location([X,Y]),
Z1 is Y + 1,assumir_inimigo(no,[X,Z1]),
Z2 is Y - 1,assumir_inimigo(no,[X,Z2]),
Z3 is X + 1,assumir_inimigo(no,[Z3,Y]),
Z4 is X - 1,assumir_inimigo(no,[Z4,Y]).

adiciona_inimigo(yes):- mario_location([X,Y]),
Z1 is Y + 1,assumir_inimigo50(yes,[X,Z1]),
Z2 is Y - 1,assumir_inimigo50(yes,[X,Z2]),
Z3 is X + 1,assumir_inimigo50(yes,[Z3,Y]),
Z4 is X - 1,assumir_inimigo50(yes,[Z4,Y]).

assumir_inimigo(no,L):-retractall(einimigo(_,L)),assert(einimigo(no,L)).

assumir_inimigo(yes,L):-retractall(einimigo(_,L)),assert(einimigo(yes,L)).

adiciona_teletransporte(no):- mario_location([X,Y]),
Z1 is Y + 1,assumir_teletransporte(no,[X,Z1]),
Z2 is Y - 1,assumir_teletransporte(no,[X,Z2]),
Z3 is X + 1,assumir_teletransporte(no,[Z3,Y]),
Z4 is X - 1,assumir_teletransporte(no,[Z4,Y]).

adiciona_teletransporte(yes):- mario_location([X,Y]),
Z1 is Y + 1,assumir_teletransporte(yes,[X,Z1]),
Z2 is Y - 1,assumir_teletransporte(yes,[X,Z2]),
Z3 is X + 1,assumir_teletransporte(yes,[Z3,Y]),
Z4 is X - 1,assumir_teletransporte(yes,[Z4,Y]).

assumir_teletransporte(no,L):-retractall(eteletransporte(_,L)),assert(eteletransporte(no,L)).

assumir_teletransporte(yes,L):-retractall(eteletransporte(_,L)),assert(eteletransporte(yes,L)).

adiciona_poco(no):- mario_location([X,Y]),
Z1 is Y + 1,assumir_poco(no,[X,Z1]),
Z2 is Y - 1,assumir_poco(no,[X,Z2]),
Z3 is X + 1,assumir_poco(no,[Z3,Y]),
Z4 is X - 1,assumir_poco(no,[Z4,Y]).

adiciona_poco(yes):- mario_location([X,Y]),
Z1 is Y + 1,assumir_poco(yes,[X,Z1]),
Z2 is Y - 1,assumir_poco(yes,[X,Z2]),
Z3 is X + 1,assumir_poco(yes,[Z3,Y]),
Z4 is X - 1,assumir_poco(yes,[Z4,Y]).


assumir_poco(no,L):-retractall(epoco(_,L)),assert(epoco(no,L)).

assumir_poco(yes,L):-retractall(epoco(_,L)),assert(epoco(yes,L)).

adiciona_ouro(no):- loc_ouro(GL),
assumir_ouro(no,GL).

adiciona_ouro(yes):- mario_location([X,Y]),
loc_ouro([X1,Y1]),
X = X1, Y = Y1,
assumir_ouro(yes,[X1,Y1]).

assumir_ouro(no,L):-retractall(eouro(_,L)),assert(eouro(no,L)).
assumir_ouro(yes,L):-retractall(eouro(_,L)),assert(eouro(yes,L)).

posicoes_permitidas([X,Y]) :-
    tamanho_mundo(TM),
    X > 0, X < WS+1,
    Y > 0 , Y < WS+1.
	
	
pergunta_base(ListaVisitados,Acao) :- 
einimigo20(no,L),
einimigo50(no,L),
eteletransporte(no,L),
epoco(no,L),
naopertence(L,ListaVisitados),
atualiza_marioloc(L),
Action = L.


naopertence(X,[]).
naopertence([X,Y],[[U,V]|Ys]):- 
(X = U,Y = V -> fail
;naopertence([X,Y],Ys)
).
























































step(VisitedTilesList) :-
    check_current_Perception(Perception),
    mario_location(ML),
    format("Estou em ~p, vendo: ~p~n", [ML,Perception]),
    
    update_base(Perception),
    pergunta_base(VisitedTilesList, Acao),
    format("Estou indo para: ~p~n", [Acao]),

    update_score,
    
    mario_location(Mloc),
    VL = [Mloc|VisitedList],
    standing,
step_pre(VL).


