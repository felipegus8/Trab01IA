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
curPath/1,
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
inicio(1,1).
vazia(1,2).
vazia(1,3).
vazia(1,4).
vazia(1,5).
vazia(1,6).
vazia(1,8).
vazia(1,9).
vazia(1,10).
vazia(1,11).
vazia(2,1).
vazia(2,3).
vazia(2,4).
vazia(2,5).
vazia(2,6).
vazia(2,7).
vazia(2,8).
vazia(2,10).
vazia(2,12).
vazia(3,1).
vazia(3,2).
vazia(3,4).
vazia(3,6).
vazia(3,7).
vazia(3,8).
vazia(3,9).
vazia(3,10).
vazia(3,11).
vazia(3,12).
vazia(4,1).
vazia(4,2).
vazia(4,3).
vazia(4,4).
vazia(4,5).
vazia(4,6).
vazia(4,7).
vazia(4,9).
vazia(4,10).
vazia(4,12).
vazia(5,1).
vazia(5,3).
vazia(5,4).
vazia(5,5).
vazia(5,7).
vazia(5,8).
vazia(5,9).
vazia(5,10).
vazia(5,11).
vazia(5,12).
vazia(6,1).
vazia(6,2).
vazia(6,3).
vazia(6,4).
vazia(6,5).
vazia(6,6).
vazia(6,7).
vazia(6,8).
vazia(6,9).
vazia(6,10).
vazia(6,11).
vazia(6,12).
vazia(7,1).
vazia(7,2).
vazia(7,4).
vazia(7,5).
vazia(7,6).
vazia(7,8).
vazia(7,10).
vazia(7,11).
vazia(7,12).
vazia(8,1).
vazia(8,2).
vazia(8,3).
vazia(8,4).
vazia(8,5).
vazia(8,6).
vazia(8,7).
vazia(8,8).
vazia(8,9).
vazia(8,10).
vazia(8,11).
vazia(8,12).
vazia(9,1).
vazia(9,2).
vazia(9,3).
vazia(9,4).
vazia(9,5).
vazia(9,6).
vazia(9,7).
vazia(9,8).
vazia(9,9).
vazia(9,10).
vazia(9,12).
vazia(10,3).
vazia(10,4).
vazia(10,5).
vazia(10,7).
vazia(10,8).
vazia(10,9).
vazia(10,11).
vazia(10,12).
vazia(11,1).
vazia(11,4).
vazia(11,5).
vazia(11,6).
vazia(11,7).
vazia(11,9).
vazia(11,10).
vazia(11,11).
vazia(11,12).
vazia(12,1).
vazia(12,2).
vazia(12,3).
vazia(12,4).
vazia(12,5).
vazia(12,6).
vazia(12,7).
vazia(12,8).
vazia(12,9).
vazia(12,10).
vazia(12,11).
vazia(12,12).


%Só pra poder testar sem o java ainda
parede(0,1).
parede(0,2).
parede(0,3).
parede(0,4).
parede(0,5).
parede(0,6).
parede(0,7).
parede(0,8).
parede(0,9).
parede(0,10).
parede(0,11).
parede(0,12).
parede(13,1).
parede(13,2).
parede(13,3).
parede(13,4).
parede(13,5).
parede(13,6).
parede(13,7).
parede(13,8).
parede(13,9).
parede(13,10).
parede(13,11).
parede(13,12).
parede(1,0).
parede(2,0).
parede(3,0).
parede(4,0).
parede(5,0).
parede(6,0).
parede(7,0).
parede(8,0).
parede(9,0).
parede(10,0).
parede(11,0).
parede(12,0).
parede(1,13).
parede(2,13).
parede(3,13).
parede(4,13).
parede(5,13).
parede(6,13).
parede(7,13).
parede(8,13).
parede(9,13).
parede(10,13).
parede(11,13).
parede(12,13).


ouro(11,2).
ouro(3,5).
ouro(9,11).

inimigo(10,2).
inimigo(5,6).
inimigo(2,9).
inimigo(11,8).

teletransporte(10,1).
teletransporte(7,3).
teletransporte(1,7).
teletransporte(4,11).

poco(5,2).
poco(3,3).
poco(11,3).
poco(10,6).
poco(4,8).
poco(7,9).
poco(10,10).
poco(2,11).

powerup(2,2).
powerup(7,7).
powerup(1,12).

adjacente(X,Y,X2,Y) :- X2 is X + 1,pode_ser_acessada(X2,Y).
adjacente(X,Y,X2,Y) :- X2 is X - 1,pode_ser_acessada(X2,Y).
adjacente(X,Y,X,Y2) :- Y2 is Y + 1,pode_ser_acessada(X,Y2).
adjacente(X,Y,X,Y2) :- Y2 is Y - 1,pode_ser_acessada(X,Y2).

pode_ser_acessada(X,Y) :- inicio(X,Y);poco(X,Y);vazia(X,Y);ouro(X,Y);teletransporte(X,Y);power_up(X,Y);inimigo(_,_,X,Y),!.

mario_reset() :- retractall(energia(_)),retractall(score(_)),retractall(municao(_)),retractall(mario_location(_,_,_)),retractall(visitadas(_,_)),retractall(saida(_)),retractall(inicio(StartX,StartY)),
retractall(dijkstra_opcao_permite_perigo(_)),retractall(curPath(_)),retractall(pode_ter_inimigo(_,_)),retractall(tem_inimigo(_,_)),retractall(nao_tem_inimigo(_,_)),
retractall(pode_ter_poco(_,_)),retractall(nao_tem_poco(_,_)),retractall(tem_poco(_,_)),retractall(pode_ter_teletransporte(_,_)),retractall(nao_tem_teletransporte(_,_)),retractall(tem_teletransporte(_,_)),
assert(energia(100)),assert(score(0)),assert(municao(5)),assert(mario_location(1,1,cima)),assert(visitadas(1,1)),assert(saida(0)),assert(inicio(1,1)).

%Movimento
estado_atual_mario(X,Y,Direcao,Score,Energia,Municao) :- mario_location(X,Y,Direcao),score(Score),energia(Energia),municao(Municao),!.

mario_andar() :- ((mario_location(_,_,direita),Prox = baixo);(mario_location(_,_,esquerda),Prox = cima);(mario_location(_,_,baixo),Prox = esquerda);(mario_location(_,_,cima),Prox = direita)),ir_Para(Prox).

mario_andar_esquerda :- ((mario_location(_,_,direita),Prox = cima);(mario_location(_,_,esquerda),Prox = baixo);(mario_location(_,_,baixo),Prox = direita);(mario_location(_,_,cima),Prox = esquerda)),ir_Para(Prox).

ir_Para(Prox) :-  mario_location(X,Y,_),retractall(mario_location(X,Y,_)),assert(mario_location(X,Y,Prox)),atualizar_score(-1),!.

mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),X is X2 + 1,Y2 = Y,Posicao = direita,!.
mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),X is X2 - 1,Y2 = Y,Posicao = esquerda,!.
mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),Y is Y2 - 1,X2 = X,Posicao = cima,!.
mario_vai_para(X,Y) :- mario_location(X2,Y2,Posicao),Y is Y2 + 1,X2 = X,Posicao = baixo,!.

mario_andar_para(X,Y):-pode_ser_acessada(X,Y),mario_location(_,_,Posicao),retractall(mario_location(_,_,_)),assert(mario_location(X,Y,Posicao)),(visitadas(X,Y);assert(visitadas(X,Y))),atualizar_score(-1),atualizar_incertezas(),
    ((poco(X,Y),assert(tem_poco(X,Y)),atualizar_score(-1000),retractall(energia(_)),assert(energia(0)));0=0),
    ((inimigo(E2,_,X2,Y2),atualizar_energia(-E2),assert(tem_inimigo(X,Y)),retractall(pode_ter_inimigo(X,Y)),atualizar_incertezas(),assert(dijkstra_opcao_permite_perigo(1)),tomar_decisao_voltar_para_mais_proxima(),retractall(dijkstra_opcao_permite_perigo(_)));0=0),!.
	
%Dijkstra

%Implementação de Dijkstra em prolog retirada de http://rosettacode.org/wiki/Dijkstras_algorithm#Prolog usando peso 1

path([X, Y],[X2, Y2], 1) :- visitadas(X, Y), visitadas(X2, Y2), adjacente(X, Y, X2, Y2),
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
	((ouviu_passos_inimigo(),adjacente_pode_ter_inimigo());0=0),
	((percebeu_flash_teletransporte(),adjacente_pode_ter_teletransporte());0=0),
	((sentiu_brisa_poco(),adjacente_pode_ter_poco());0=0),
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

%Atualiza pontuação

atualizar_energia(P) :- energia(E),
novaEnergia is E+P,
retractall(energia(_)),
assert(energia(novaEnergia)).

atualizar_score(N) :- score(S),
Novoscore is S+N,
retractall(score(_)),
assert(score(Novoscore)).

atualizar_municao() :- municao(Municao),NovaMunicao is Municao - 1,retractall(municao(Municao)),assert(municao(NovaMunicao)).

%Preenchendo o Path

tomar_decisao_segura():-visitadas(X,Y),not(percebeu_algum_perigo(X,Y)),not(tem_inimigo(X,Y)),not(tem_teletransporte(X,Y)),adjacente(X,Y,X2,Y2),not(visitadas(X2,Y2)),mario_location(LocX,LocY,_),go([LocX,LocY],[X,Y]),!.

tomar_decisao_inimigo():-visitadas(X,Y),adjacente(X,Y,X2,Y2),not(visitadas(X2,Y2)),inimigo(_,_,X2,Y2),!.

tomar_decisao_teletransporte():-visitadas(X,Y),adjacente(X,Y,X2,Y2),not(visitadas(X2,Y2)),teletransporte(X2,Y2),!.

tomar_decisao_powerup():-power_up(X,Y),visitadas(X,Y),mario_location(LocX,LocY,_),not((X = LocX,Y=LocY)),go([LocX,LocY],[X,Y]),!.

tomar_decisao_poco():-mario_location(LocX,LocY,_),pode_ter_poco(X,Y),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),not((Y2=LocY,X2=LocX)),go([LocX,LocY],[X2,Y2]),!.

%Exceções
tomar_decisao_voltar_para_mais_proxima() :- mario_location(X,Y,_),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),go([X,Y],[X2,Y2]),!.

tomar_decisao_saida() :- mario_location(X,Y,_),inicio(X2,Y2),go([X,Y],[X2,Y2]),!.

tomar_decisao_lutar() :- tem_inimigo(X,Y),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),mario_location(LocX,LocY,_),not((Y2=LocY,x2=LocX)),go([LocX,LocY],[X2,Y2]),!.

tomar_decisao_pode_ter_inimigo() :-pode_ter_inimigo(X,Y),adjacente(X,Y,X2,Y2),visitadas(X2,Y2),mario_location(LocX,LocY),not((X2=LocX,Y2=LocY)),go([LocX,LocY],[X2,Y2]),!.



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
atualizar_energia(-10),
atualizar_municao(),!.

%Ações possíveis

proximo_movimento(sair):-mario_location(X,Y,_),inicio(X,Y),saida(1),!.

proximo_movimento(pegar_ouro):-mario_location(X,Y,_),ouro(X,Y),retract(ouro(X,Y)),assert(vazia(X,Y)),atualizar_score(1000),!.

proximo_movimento(morreu):-energia(E),E<1,!.

proximo_movimento(Path):-curPath([]),retract(curPath([])),proximo_movimento(Path),!.

proximo_movimento(Rodar):-curPath([X,Y]|_),not(mario_vai_para(X,Y)),mario_andar(),!.

proximo_movimento(Andar):-curPath([X,Y]|N),mario_vai_para(X,Y),retractall(curPath([])),assert(curPath(N)),mario_andar_para(X,Y),!.

proximo_movimento(Andar):-mario_vai_para(X,Y),pode_ser_acessada(X,Y),not(percebeu_algum_perigo()),not(visitadas(X,Y)),mario_andar_para(X,Y),!.

proximo_movimento(Rodar) :- mario_location(X,Y,_),adjacente(X,Y,X2,Y2),not(percebeu_algum_perigo()),not(visitadas(X2,Y2)),not(mario_vai_para(X2,Y2)),format("Mandou andar"),mario_andar(),!.

proximo_movimento(Rodar):-mario_vai_para(X,Y),not(pode_ser_acessada(X,Y)),mario_andar(),!.

proximo_movimento(Acao):-tomar_decisao_segura(),format("Passou"),proximo_movimento(Acao),writef('Indo para uma casa segura'),!.

proximo_movimento(Atirar):-municao(M),M>0,mario_location(X,Y,_),adjacente(X,Y,X2,Y2),tem_inimigo(X2,Y2),mario_vai_para(X2,Y2),energia(E),E>50,writef('Vai atirar'),atirar(Atirar),!.

proximo_movimento(Rodar):-municao(M),M>0,mario_location(X,Y,_),adjacente(X,Y,X2,Y2),tem_inimigo(X2,Y2),not((mario_vai_para(X2,Y2))),energia(E),E>50,mario_andar(),!.

proximo_movimento(Andar):-municao(M),M>0,tomar_decisao_lutar(),proximo_movimento(Andar),!.

proximo_movimento(Andar):-energia(E),E>50,mario_vai_para(X,Y),pode_ter_inimigo(X,Y),mario_andar_para(X,Y),!.

proximo_movimento(Rodar):-energia(E),E>50,mario_location(LocX,LocY,_),adjacente(LocX,LocY,X2,Y2),pode_ter_inimigo(X2,Y2),not(mario_vai_para(X2,Y2)),mario_vai_para(),!.

proximo_movimento(Acao):-energia(E),E>50,tomar_decisao_pode_ter_inimigo(),proximo_movimento(Acao),writef('Andar em direcao ao inimigo'),!.

proximo_movimento(pegar_power_up):-energia(E),mario_location(X,Y,_),power_up(X,Y),retract(power_up(X,Y)),assert(vazia(X,Y)),atualizar_score(-1),atualizar_energia(20),!.

proximo_movimento(Acao):-energia(E),tomar_decisao_powerup(),proximo_movimento(Acao),writef('Pegar power up'),!.

proximo_movimento(Acao):-assert(saida(1)),tomar_decisao_saida(),proximo_movimento(Acao),writef('Vai sair'),!.

proximo_movimento(Andar) :- score(S), S < 1, mario_vai_para(X, Y), pode_ter_poco(X, Y), mario_andar_para(X, Y), writef('Pode cair no poco\n'), !.

proximo_movimento(Rodar) :- score(S), S < 1, mario_location(X, Y, _), adjacente(X, Y, X2, Y2),pode_ter_poco(X2, Y2), not(mario_vai_para(X2, Y2)), mario_andar(), writef('Rodando podendo cair no poco!\n'), !.

proximo_movimento(Acao) :- score(S), S < 1, tomar_decisao_poco(), proximo_movimento(Acao), writef('Acao para poder cair no poco!\n'), !.

%proximo_movimento(Teletransporte):-mario_location(X,Y,_),adjacente(X,Y,X2,Y2),tem_teletransporte(X2,Y2),random_between(1,12,XT),random_between(1,12,YT),mario_vai_para(XT,YT),!.

