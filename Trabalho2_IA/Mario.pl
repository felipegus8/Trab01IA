:- dynamic([inicio/2,
parede/2,
vazia/2,
inimigo/2,
ouro/2,
power_up/2,
poco/2,
teletransporte/2,
inimigo/4,
energia/1,
score/1,
municao/1,
mario_location/3,
visitadas/2,
saida/1,
path_atual/1,
pode_ter_poco/2,
pode_ter_teletransporte/2,
pode_ter_inimigo/2,
nao_tem_poco/2,
nao_tem_inimigo/2,
nao_tem_teletransporte/2,
tem_poco/2,
tem_inimigo/2,
tem_teletransporte/2,
rpath/2,
dijkstra_opcao_permite_perigo/1
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
ourosencontrados(0).
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

%Dijkstra

%Implementação de Dijkstra em prolog retirada de http://rosettacode.org/wiki/Dijkstra's_algorithm#Prolog usando peso 1

path([X, Y],[X2, Y2], 1) :- visited(X, Y), visitados(X2, Y2), adjacente(X, Y, X2, Y2),
(
dijkstra_opcao_permite_perigo(1);
(
not(pode_ter_inimigo(X2, Y2)), not(pode_ter_teletransporte(X2, Y2)),
not(pode_ter_teletransporte(X, Y)), not(pode_ter_inimigo(X, Y)),
not(tem_teletransporte(X2, Y2)), not(tem_inimigo(X, Y)),
not(tem_inimigo(X2, Y2)), not(tem_teletransporte(X, Y))
)
). /* From, To, Weight=1 */

shorterPath([H|Path], Dist) :-
rpath([H|T], D), !, Dist < D,
retract(rpath([H|_],_)),
/*writef('%w is closer than %w\n', [[H|Path], [H|T]]),*/
assert(rpath([H|Path], Dist)).
shorterPath(Path, Dist) :-
/*writef('New path:%w\n', [Path]),*/
assert(rpath(Path,Dist)).

traverse([X, Y], Path, Dist) :-
path([X, Y], T, D),
not(memberchk(T, Path)),
shorterPath([T,[X, Y]|Path], Dist+D),
traverse(T,[[X, Y]|Path],Dist+D).

traverse([X, Y]) :-
retractall(rpath(_,_)),
traverse([X, Y],[],0).
traverse(_).

go([X, Y], [X2, Y2]) :-
traverse([X, Y]),
rpath([[X2, Y2]|RPath], Dist)->
reverse([[X2, Y2]|RPath], Path),
Distance is round(Dist),
(writef('Shortest path is %w with distance %w = %w\n',
[Path, Dist, Distance]), Path = [_|T], assert(curPath(T)));
writef('There is no route from %w to %w\n', [[X, Y], [X2, Y2]]).



%Init
init_jogo_free() :-
retractall(mario_location(_,_)),
retractall(energy(_)),
retractall(score()),assert(score(0)),
retractall(visitados(_)),assert(visitados(1)),
retractall(einimigo(_,_)),retractall(eouro(_,_)),
retractall(eteletransporte(_,_)),retractall(epoco(_,_)),
retractall(casas_visitadas(_)),assert(casas_visitadas([])),
format("Passou pelo init").



adjacente(X,Y,X2,Y) :- X2 is X + 1,pode_ser_acessada(X2,Y).
adjacente(X,Y,X2,Y) :- X2 is X - 1,pode_ser_acessada(X2,Y).
adjacente(X,Y,X,Y2) :- X2 is Y + 1,pode_ser_acessada(X2,Y).
adjacente(X,Y,X,Y2) :- X2 is Y - 1,pode_ser_acessada(X2,Y).

pode_ser_acessada(X,Y) :- inicio(X,Y);poco(X,Y);vazia(X,Y);ouro(X,Y);teletransporte(X,Y);power_up(X,Y);inimigo(_,_,X,Y),!.


mark_visited_position(Position) :-
	assert(agent_knowledge(Position, visited)).

%Movimento
estado_atual_mario(X,Y,Direcao,Score,Energia,Municao) :- mario_location(X,Y,Direcao),score(Score),energia(Energia),municao(Municao).

mario_andar() :- ((mario_location(_,_,direita),Prox = baixo);(mario_location(_,_,esquerda),Prox = cima);(mario_location(_,_,baixo),Prox = esquerda);(mario_location(_,_,cima),Prox = direita)),ir_Para(Prox).

mario_andar_esquerda :- ((mario_location(_,_,direita),Prox = cima);(mario_location(_,_,esquerda),Prox = baixo);(mario_location(_,_,baixo),Prox = direita);(mario_location(_,_,cima),Prox = esquerda)),ir_Para(Prox).

ir_Para(Prox) :-  mario_location(X,Y,_),retractall(mario_location(X,Y,_)),assert(mario_location(X,Y,Prox)),atualiza_score(-1).

mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),X is X2 + 1,Y2 = Y,Posicao = direita,!.
mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),X is X2 - 1,Y2 = Y,Posicao = esquerda,!.
mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),Y is Y2 - 1,X2 = X,Posicao = cima,!.
mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),Y is Y2 + 1,X2 = X,Posicao = baixo,!.

mario_andar_para(X,Y):-pode_ser_acessada(X,Y),mario_location(_,_,Posicao),retractall(mario_location(_,_,_)),assert(mario_location(X,Y,Posicao)),(visitadas(X,Y);assert(visitadas(X,Y))),update_score(-1),atualizar_incertezas(),
    ((poco(X,Y),assert(tem_poco(X,Y)),update_score(-1000),retractall(energia(_)),assert(energia(0)));0=0),
    ((inimigo(E2,_,X2,Y2),atualiza_energia(-E2),assert(tem_inimigo(X,Y)),retractall(pode_ter_inimigo(X,Y)),atualizar_incertezas(),assert(dijkstra_opcao_permite_perigo(1)),tomar_decisao_voltar_para_mais_proxima(),retractall(dijkstra_opcao_permite_perigo(_)));0=0),!.

%Percepções

ouviu_passos_inimigo(X,Y) :-adjacente(X,Y,X2,Y2),inimigo(_,_,X2,Y2),!.
ouviu_passos_inimigo() :- mario_location(X,Y,_),ouviu_passos_inimigo(X,Y),!.

sentiu_brisa_poco(X,Y) :- adjacente(X,Y,X2,Y2),poco(X2,Y2),!.
sentiu_brisa_poco() :- mario_location(X,Y,_),sentiu_brisa_poco(X,Y),!.

percebeu_flash_teletransporte(X,Y) :- adjacente(X,Y,X2,Y2),teletransporte(X2,Y2),!.
percebeu_flash_teletransporte() :- mario_location(X,Y,_),percebeu_flash_teletransporte(X,Y),!.

percebeu_algum_perigo(X,Y):- (ouviu_passos_inimigo(X,Y);sentiu_brisa_poco(X,Y);percebeu_flash_teletransporte(X,Y)).
percebeu_algum_perigo() :- mario_location(X,Y,_),percebeu_algum_perigo(X,Y).

percebeu_brilho([X1,Y1]) :- loc_ouro([X2.Y2]),X1 = X2,Y1 = Y2.



%Atualiza pontuação

update_energy :-
mario_location(ML),
loc_ouro(GL),
loc_poco(PL),
loc_teletransporte(TL),
loc_inimigo(EL),
loc_powerup(UL),
update_energy(ML, GL,PL,TL,EL,UL).

atualiza_energia(P) :- energia(E),
novaEnergia is E+P,
retractall(energia(_)),
assert(energia(novaEnergia)).


update_energy(ML,ML,_,_,_,_) :- update_score(1000),update_ouros(1).

update_energy(_,_,_,_,_,_) :- update_score(-1).

update_energy(ML,_,ML,_,_,_) :- update_score(-1000).

update_energy(ML,_,_,_,ML,_) :- random_between(20,50,X),update_energy(-X).

update_energy(ML,_,_,_,_,ML) :- update_energy(20).

update_energy(ML,_,_,ML,_,_) :- random_between(1,12,X),random_between(1,12,Y),retractall(mario_location(_,_)),assert(mario_location([X,Y])).

atualiza_score(N) :- score(S),
Novoscore is S+N,
retractall(score(_)),
assert(score(Novoscore)).

atualiza_municao() :- municao(Municao),NovaMunicao is Municao - 1,retractall(municao(Municao)),assert(municao(NovaMunicao)).

update_ouros(X) :- ourosencontrados(O),
newO is O+X,
retractall(ourosencontrados(_)),
assert(ourosencontrados(newO)).

%Base de conhecimento

%Atualiza todas as certezas do Mario
atualizar_certezas_teletransporte() :- teletransporte(X,Y),pode_ter_teletransporte(X,Y),X2 is X+1,X3 is X-1,Y2 is Y + 1,Y3 is Y - 1,
(
	((parede(X2,Y));visitadas(X2,Y)),
	((parede(X3,Y));visitadas(X3,Y)),
	((parede(X,Y2));visitadas(X,Y2)),
	((parede(X,Y3));visitadas(X,Y3))
),
retractall(pode_ter_teletransporte(X,Y)),assert(tem_teletransporte(X,Y)).

atualizar_certezas_poco() :- poco(X,Y),pode_ter_poco(X,Y),X2 is X+1,X3 is X-1,Y2 is Y + 1,Y3 is Y - 1,
(
	((parede(X2,Y));visitadas(X2,Y)),
	((parede(X3,Y));visitadas(X3,Y)),
	((parede(X,Y2));visitadas(X,Y2)),
	((parede(X,Y3));visitadas(X,Y3))
),
retractall(pode_ter_poco(X,Y)),assert(tem_poco(X,Y)).

atualizar_certezas_inimigo() :- inimigo(X,Y),pode_ter_inimigo(X,Y),X2 is X+1,X3 is X-1,Y2 is Y + 1,Y3 is Y - 1,
(
	((parede(X2,Y));visitadas(X2,Y)),
	((parede(X3,Y));visitadas(X3,Y)),
	((parede(X,Y2));visitadas(X,Y2)),
	((parede(X,Y3));visitadas(X,Y3))
),
retractall(pode_ter_inimigo(X,Y)),assert(tem_inimigo(X,Y)).

atualizar_certezas() :- (atualizar_certezas_teletransporte();0=0),(atualizar_certezas_poco();0=0),(atualizar_certezas_inimigo();0=0),!. 

%Remover incertezas

remover_incertezas_casa_atual():- mario_location(X,Y,_),
(
		((not(poco(X,Y)),assert(nao_tem_poco(X,Y)),retractall(pode_ter_poco(X,Y)));0=0),
		((not(inimigo(X,Y)),assert(nao_tem_inimigo(X,Y)),retractall(pode_ter_inimigo(X,Y)));0=0),
		((not(teletransporte(X,Y)),assert(nao_tem_teletransporte(X,Y)),retractall(pode_ter_teletransporte(X,Y)));0 = 0)
		
).

atualizar_incertezas():-
(
	(remover_incertezas_casa_atual();0=0),
	((ouviu_passos_inimigo(),pergunta_pode_ter_inimigo());0=0),
	((percebeu_flash_teletransporte(),pergunta_pode_ter_teletransporte());0=0),
	((sentiu_brisa_poco(),pergunta_pode_ter_poco());0=0),
	((not(ouviu_passos_inimigo()),remover_incerteza_inimigo_adjacente());0=0),
	((not(percebeu_flash_teletransporte()),remover_incerteza_teletransporte_adjacente());0=0),
	((not(sentiu_brisa_poco()),remover_incerteza_poco_adjacente());0=0),
	((atualizar_certezas();0=0))
).




remover_incerteza_poco_adjacente(X,Y):- assert(nao_tem_poco(X,Y)),retractall(pode_ter_poco(X,Y)).
remover_incerteza_poco_adjacente():- mario_location(X,Y,_),
((X2 is X  + 1,adjacente(X,Y,X2,Y),remover_incerteza_poco_adjacente(X2,Y));0=0),
((X3 is X  - 1,adjacente(X,Y,X3,Y),remover_incerteza_poco_adjacente(X3,Y));0=0),
((Y2 is Y  + 1,adjacente(X,Y,X,Y2),remover_incerteza_poco_adjacente(X,Y2));0=0),
((Y3 is Y  - 1,adjacente(X,Y,X,Y3),remover_incerteza_poco_adjacente(X,Y3));0=0).

remover_incerteza_inimigo_adjacente(X,Y):- assert(nao_tem_inimigo(X,Y)),retractall(pode_ter_inimigo(X,Y)).
remover_incerteza_inimigo_adjacente():- mario_location(X,Y,_),
((X2 is X  + 1,adjacente(X,Y,X2,Y),remover_incerteza_inimigo_adjacente(X2,Y));0=0),
((X3 is X  - 1,adjacente(X,Y,X3,Y),remover_incerteza_inimigo_adjacente(X3,Y));0=0),
((Y2 is Y  + 1,adjacente(X,Y,X,Y2),remover_incerteza_inimigo_adjacente(X,Y2));0=0),
((Y3 is Y  - 1,adjacente(X,Y,X,Y3),remover_incerteza_inimigo_adjacente(X,Y3));0=0).

remover_incerteza_teletransporte_adjacente(X,Y):- assert(nao_tem_teletransporte(X,Y)),retractall(pode_ter_teletransporte(X,Y)).
remover_incerteza_teletransporte_adjacente():- mario_location(X,Y,_),
((X2 is X  + 1,adjacente(X,Y,X2,Y),remover_incerteza_teletransporte_adjacente(X2,Y));0=0),
((X3 is X  - 1,adjacente(X,Y,X3,Y),remover_incerteza_teletransporte_adjacente(X3,Y));0=0),
((Y2 is Y  + 1,adjacente(X,Y,X,Y2),remover_incerteza_teletransporte_adjacente(X,Y2));0=0),
((Y3 is Y  - 1,adjacente(X,Y,X,Y3),remover_incerteza_teletransporte_adjacente(X,Y3));0=0).

%Vendo o que pode ter em cada posicao adjacente a posição atual.

pergunta_pode_ter_poco(X,Y) :- not(visitadas(X,Y)),not(nao_tem_poco(X,Y)),assert(pode_ter_poco(X,Y)),!.
adjacente_pode_ter_poco() :- mario_location(X,Y,_),
((X2 is X  + 1,adjacente(X,Y,X2,Y),pode_ter_poco(X2,Y));0=0),
((X3 is X  - 1,adjacente(X,Y,X3,Y),pode_ter_poco(X3,Y));0=0),
((Y2 is Y  + 1,adjacente(X,Y,X,Y2),pode_ter_poco(X,Y2));0=0),
((Y3 is Y  - 1,adjacente(X,Y,X,Y3),pode_ter_poco(X,Y3));0=0).

pergunta_pode_ter_teletransporte(X,Y) :- not(visitadas(X,Y)),not(nao_tem_teletransporte(X,Y)),assert(pode_ter_teletransporte(X,Y)),!.
adjacente_pode_ter_teletransporte() :- mario_location(X,Y,_),
((X2 is X  + 1,adjacente(X,Y,X2,Y),pode_ter_teletransporte(X2,Y));0=0),
((X3 is X  - 1,adjacente(X,Y,X3,Y),pode_ter_teletransporte(X3,Y));0=0),
((Y2 is Y  + 1,adjacente(X,Y,X,Y2),pode_ter_teletransporte(X,Y2));0=0),
((Y3 is Y  - 1,adjacente(X,Y,X,Y3),pode_ter_teletransporte(X,Y3));0=0).

pergunta_pode_ter_inimigo(X,Y) :- not(visitadas(X,Y)),not(nao_tem_inimigo(X,Y)),assert(pode_ter_inimigo(X,Y)),!.
adjacente_pode_ter_inimigo() :- mario_location(X,Y,_),
((X2 is X  + 1,adjacente(X,Y,X2,Y),pode_ter_inimigo(X2,Y));0=0),
((X3 is X  - 1,adjacente(X,Y,X3,Y),pode_ter_inimigo(X3,Y));0=0),
((Y2 is Y  + 1,adjacente(X,Y,X,Y2),pode_ter_inimigo(X,Y2));0=0),
((Y3 is Y  - 1,adjacente(X,Y,X,Y3),pode_ter_inimigo(X,Y3));0=0).

posicoes_permitidas([X,Y]) :-
    tamanho_mundo(TM),
    X > 0, X < WS+1,
    Y > 0 , Y < WS+1.

%Preenchendo o Path

tomar_decisao_segura():-visitadas(X,Y),not(percebeu_algum_perigo(X,Y)),not(tem_inimigo(X,Y)),not(tem_teletransporte(X,Y)),adjacente(X,Y,X2,Y2),not(visitadas(X2,Y2)),mario_location(LocX,LocY,_),go([LocX,LocY],[X,Y]),!.

tomar_decisao_inimigo():-visitadas(X,Y),adjacente(X,Y,X2,Y2),not(visitadas(X2,Y2)),inimigo(_,_,X2,Y2),!.

tomar_decisao_teletransporte():-visitadas(X,Y),adjacente(X,Y,X2,Y2),not(visitadas(X2,Y2)),teletransporte(X2,Y2),!.

tomar_decisao_powerup():-power_up(X,Y),visitadas(X,Y),mario_location(LocX,LocY,_),not((X = LocX,Y=LocY)),go([LocX,LocY],[X,Y]),!.

tomar_decisao_poco():-mario_location(LocX,LocY,_),pode_ter_poco(X,Y),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),not((Y2=LocY,X2=LocX)),go([LocX,LocY],[X2,Y2]),!.

%Exceções
tomar_decisao_voltar_para_mais_proxima:-mario_location(X,Y,_),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),go[X,Y],[X2,Y2],!.

tomar_decisao_saida:-mario_location(X,Y,_),inicio(X2,Y2),go([X,Y],[X2,Y2]),!.

tomar_decisao_lutar:-tem_inimigo(X,Y),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),mario_location(LocX,LocY,_),not((Y2=LocY,x2=LocX)),go([LocX,LocY],[X2,Y2]),!.

tomar_decisao_pode_ter_inimigo:-pode_ter_inimigo(X,Y),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),mario_location(LocX,LocY),not((X2=LocX,Y2=LocY)),go([LocX,LocY],[X2,Y2]),!.

%Atirar
atirar(Resultado_tiro):-mario_location(X,Y,_),adjacente(X,Y,X2,Y2),tem_inimigo(X2,Y2),random_between(20,50,Dano),
(
    (inimigo(Dano_Inimigo_provoca,Energia_Inimigo,X2,Y2),
        Energia_Inimigo_apos_dano is Energia_Inimigo - Dano,
    (
     (Energia_Inimigo_apos_dano < 1,retractall(inimigo(_,_,X2,Y2)),retractall(tem_inimigo(X2,Y2)),assert(vazia(X2,Y2)),Resultado_tiro = matou,writef('Inimigo morreu'));
     (retractall(inimigo(_,_,X2,Y2)),assert(inimigo(Dano_Inimigo_provoca,Energia_Inimigo_apos_dano,X2,Y2)),Resultado_tiro = atacou_nao_matou,writef('Inimigo atacado mas não morto'))
    )
)
),
atualiza_energia(-10),
atualiza_municao(),!.

%Ações possíveis

proximo_movimento(sair):-mario_location(X,Y,_),inicio(X,Y),saida(1),!.

proximo_movimento(pegar_ouro):-mario_location(X,Y,_),ouro(X,Y),retract(ouro(X,Y)),assert(vazia(X,Y)),atualiza_score(1000),!.

proximo_movimento(morreu):-energia(E),E<1,!.

proximo_movimento(Seguro):-tomar_decisao_segura(),proximo_movimento(Seguro),writef('Indo para uma casa segura'),!.

proximo_movimento(Andar_livremente):-mario_vai_para(X,Y),pode_ser_acessada(X,Y),not(percebeu_algum_perigo()),not(visitadas(X,Y)),mario_andar_para(X,Y),!.

proximo_movimento(Andar_livremente_rodando):-mario_location(X,Y,_),adjacente(X,Y,X2,Y2),not(percebeu_algum_perigo()),not(visitadas(X2,Y2),not(mario_vai_para(X2,Y2)),mario_andar(),!.

proximo_movimento(Rodar_se_nao_permitido):-mario_vai_para(X,Y),not(pode_ser_acessada(X,Y)),mario_andar(),!.

proximo_movimento(Path):-curPath([]),retract(curPath([])),proximo_movimento(Path),!.

proximo_movimento(Rodar):-curPath([X,Y]|_),not(mario_vai_para(X,Y)),mario_andar(),!.

proximo_movimento(Andar):-curPath([X,Y]|N),mario_vai_para(X,Y),retractall(curPath([])),assert(curPath(N)),mario_andar_para(X,Y),!.

proximo_movimento(Atirar):-municao(M),M>0,mario_location(X,Y,_),adjacente(X,Y,X2,Y2),tem_inimigo(X2,Y2),mario_vai_para(X2,Y2),energia(E),E>50,writef('Vai atirar'),atirar(Atirar),!.

proximo_movimento(Rodar):-municao(M),M>0,mario_location(X,Y,_),adjacente(X,Y,X2,Y2),tem_inimigo(X2,Y2),not((mario_vai_para(X2,Y2))),energia(E),E>50,mario_andar(),!.

proximo_movimento(Andar):-municao(M),M>0,tomar_decisao_lutar(),proximo_movimento(Andar),!.

proximo_movimento(Andar_Pode_Ter_Inimigo):-energia(E),E>50,mario_vai_para(X,Y),pode_ter_inimigo(X,Y),mario_andar_para(X,Y),!.

proximo_movimento(Rodar):-energia(E),E>50,mario_location(LocX,LocY,_),adjacente(LocX,LocY,X2,Y2),pode_ter_inimigo(X2,Y2),not(mario_vai_para(X2,Y2)),mario_vai_para().

proximo_movimento(Acao):-energia(E),E>50,tomar_decisao_pode_ter_inimigo(),proximo_movimento(Acao),writef('Andar em direcao ao inimigo'),!.

proximo_movimento(pegar_power_up):-energia(E),mario_location(X,Y,_),power_up(X,Y),retract(power_up(X,Y)),assert(vazia(X,Y)),update_score(-1),update_energy(20),!.

proximo_movimento(Acao):-energia(E),tomar_decisao_powerup(),proximo_movimento(Acao),writef('Pegar power up'),!.

proximo_movimento(Acao):-assert(saida(1)),tomar_decisao_saida(),proximo_movimento(Acao),writef('Vai sair'),!.



naopertence(X,[]).
naopertence([X,Y],[[U,V]|Ys]):- 
(X = U,Y = V -> fail
;naopertence([X,Y],Ys)
).

