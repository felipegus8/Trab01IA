
:- dynamic inicio/2.
:- dynamic  parede/2.
:- dynamic  vazia/2.
:- dynamic  ouro/2.
:- dynamic  power_up/2.
:- dynamic  inimigo/2.
:- dynamic poco/2.
:- dynamic  teletransporte/2.
:- dynamic  inimigo/4.
:- dynamic  energia/1.
:- dynamic  score/1.
:- dynamic municao/1.
:- dynamic  mario_location/3.
:- dynamic  visitadas/2.
:- dynamic  pode_ter_teletransporte/2.
:- dynamic  pode_ter_inimigo/2.
:- dynamic pode_ter_poco/2.
:- dynamic  nao_tem_poco/2.
:- dynamic  nao_tem_inimigo/2.
:- dynamic  nao_tem_teletransporte/2.
:- dynamic tem_poco/2.
:- dynamic tem_inimigo/2.
:- dynamic tem_teletransporte/2.
:- dynamic  saida/1.
:- dynamic  curPath/1.
:- dynamic rpath/2.
:- dynamic dijkstra_opcao_permite_perigo/1.
:- dynamic ouros_encontrados/1.

ouros_encontrados(0).

adjacente(X, Y, X2, Y) :- X2 is X+1, pode_ser_acessada(X2, Y).
adjacente(X, Y, X2, Y) :- X2 is X-1, pode_ser_acessada(X2, Y).
adjacente(X, Y, X, Y2) :- Y2 is Y+1, pode_ser_acessada(X, Y2).
adjacente(X, Y, X, Y2) :- Y2 is Y-1, pode_ser_acessada(X, Y2).

pode_ser_acessada(X, Y) :- inicio(X, Y); vazia(X, Y); inimigo(_, _, X, Y); ouro(X, Y); powerup(X, Y); poco(X, Y); teletransporte(X, Y), !. 



estado_atual_mario(X,Y,Direcao,Score,Energia,Municao) :- mario_location(X, Y, Direcao), energia(Energia), score(Score), municao(Municao), !.
%Para iniciar o jogo

mario_esvaziamapa() :- retractall(parede(_,_)), retractall(vazia(_,_)), retractall(inimigo(_,_,_,_)),
                    retractall(ouro(_,_)), retractall(powerup(_,_)), 
                    retractall(poco(_,_)), retractall(teletransporte(_,_)), retractall(inicio(_,_)).

mario_reset() :- retractall(energia(_)), retractall(score(_)), retractall(municao(_)), retractall(mario_location(_,_,_)),
			    retractall(visitadas(_,_)), inicio(StartX, StartY),
			    retractall(curPath(_)), retractall(saida(_)), retractall(dijkstra_opcao_permite_perigo(_)),
			    retractall(nao_tem_poco(_,_)), retractall(nao_tem_inimigo(_,_)), retractall(nao_tem_teletransporte(_,_)),
			    retractall(pode_ter_poco(_,_)), retractall(pode_ter_inimigo(_,_)), retractall(pode_ter_teletransporte(_,_)),
			    retractall(tem_poco(_,_)), retractall(tem_inimigo(_,_)), retractall(tem_teletransporte(_,_)),
			    assert(energia(100)), assert(score(0)), assert(municao(5)), assert(mario_location(StartX, StartY, up)), assert(visitadas(StartX, StartY)).

%Atualiza pontuação

atualizar_energia(P) :- energia(E),
novaEnergia is E+P,
retractall(energia(_)),
assert(energia(novaEnergia)),!.

atualizar_score(N) :- score(S),
Novoscore is S+N,
retractall(score(_)),
assert(score(Novoscore)),!.

atualizar_municao() :- municao(Municao),NovaMunicao is Municao - 1,retractall(municao(Municao)),assert(municao(NovaMunicao)),!.

%Movimento

mario_andar() :- ((mario_location(_,_,right), Prox = down);
				  (mario_location(_,_,down), Prox = left);
				  (mario_location(_,_,left), Prox = up);
				  (mario_location(_,_,up), Prox = right)
				 ), irPara(Prox).

mario_andar_esquerda() :- ((mario_location(_,_,right), Prox = up);
				  (mario_location(_,_,up), Prox = left);
				  (mario_location(_,_,left), Prox = down);
				  (mario_location(_,_,down), Prox = right)
				 ), irPara(Prox).

irPara(Prox) :- mario_location(X,Y,_), retract(mario_location(X,Y,_)), assert(mario_location(X,Y,Prox)), atualizar_score(-1), !.



mario_vai_para(X, Y) :- mario_location(X2, Y2, Position), Y is Y2-1, X2 = X, Position=up, !.
mario_vai_para(X, Y) :- mario_location(X2, Y2, Position), X is X2+1, Y2 = Y, Position=right, !.
mario_vai_para(X, Y) :- mario_location(X2, Y2, Position), Y is Y2+1, X2 = X, Position=down, !.
mario_vai_para(X, Y) :- mario_location(X2, Y2, Position), X is X2-1, Y2 = Y, Position=left, !.



mario_andar_para(X, Y) :- pode_ser_acessada(X, Y), mario_location(_, _, Position), retractall(mario_location(_, _, _)), assert(mario_location(X, Y, Position)), (visitadas(X, Y);assert(visitadas(X, Y))),
                      atualizar_score(-1),
                      atualizar_incertezas(),
                      ((poco(X, Y), assert(tem_poco(X, Y)), atualizar_score(-1000), E is 0, retractall(energia(_)), assert(energia(E)));1=1),
                      ((inimigo(EE,_, X, Y), writef('Encontrou inimigo :(\n'), atualizar_energia(-EE), assert(tem_inimigo(X, Y)), retractall(pode_ter_inimigo(X, Y)), atualizar_incertezas(), assert(dijkstra_opcao_permite_perigo(1)), tomar_decisao_voltar_visitadas(), retractall(dijkstra_opcao_permite_perigo(_)));1=1), !.

					  
%Dijkstra

%Implementação de Dijkstra em prolog retirada de http://rosettacode.org/wiki/Dijkstras_algorithm#Prolog usando peso 1

path([X, Y],[X2, Y2], 1) :- visitadas(X, Y), visitadas(X2, Y2), adjacente(X, Y, X2, Y2),
							(
								dijkstra_opcao_permite_perigo(1);
								(
									not(pode_ter_inimigo(X2, Y2)), not(pode_ter_teletransporte(X2, Y2)),
									not(pode_ter_inimigo(X, Y)), not(pode_ter_teletransporte(X, Y)),
									not(tem_teletransporte(X2, Y2)), not(tem_teletransporte(X, Y)),
									not(tem_inimigo(X2, Y2)), not(tem_inimigo(X, Y))
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
	
%Percepcoes
ouviu_passos_inimigo(X,Y) :-adjacente(X,Y,X2,Y2),inimigo(_,_,X2,Y2),!.
ouviu_passos_inimigo() :- mario_location(X,Y,_),ouviu_passos_inimigo(X,Y),!.

sentiu_brisa_poco(X,Y) :- adjacente(X,Y,X2,Y2),poco(X2,Y2),!.
sentiu_brisa_poco() :- mario_location(X,Y,_),sentiu_brisa_poco(X,Y),!.

percebeu_flash_teletransporte(X,Y) :- adjacente(X,Y,X2,Y2),teletransporte(X2,Y2),!.
percebeu_flash_teletransporte() :- mario_location(X,Y,_),percebeu_flash_teletransporte(X,Y),!.

percebeu_algum_perigo(X,Y):- (ouviu_passos_inimigo(X,Y);sentiu_brisa_poco(X,Y);percebeu_flash_teletransporte(X,Y)),!.
percebeu_algum_perigo() :- mario_location(X,Y,_),percebeu_algum_perigo(X,Y),!.

percebeu_brilho([X1,Y1]) :- loc_ouro([X2.Y2]),X1 = X2,Y1 = Y2,!.

%Base de conhecimento

%Atualiza todas as certezas do Mario

atualizar_certezas_teletransporte() :-
	teletransporte(X, Y),
	pode_ter_teletransporte(X, Y),
	XA is X+1, XB is X-1, YA is Y+1, YB is Y-1,
	(
		((parede(XA, Y));visitadas(XA,Y)),
		((parede(XB, Y));visitadas(XB,Y)),
		((parede(X, YA));visitadas(X,YA)),
		((parede(X, YB));visitadas(X,YB))
	),
	retractall(pode_ter_teletransporte(X, Y)), assert(tem_teletransporte(X, Y)).
	
atualizar_certezas_inimigo() :- 
	inimigo(_, _, X, Y),
	pode_ter_inimigo(X, Y),
	XA is X+1, XB is X-1, YA is Y+1, YB is Y-1,
	(
		((parede(XA, Y));visitadas(XA,Y)),
		((parede(XB, Y));visitadas(XB,Y)),
		((parede(X, YA));visitadas(X,YA)),
		((parede(X, YB));visitadas(X,YB))
	),
	retractall(pode_ter_inimigo(X, Y)), assert(tem_inimigo(X, Y)).
	

atualizar_certezas_poco() :-
	poco(X, Y),
	pode_ter_poco(X, Y),
	XA is X+1, XB is X-1, YA is Y+1, YB is Y-1,
	(
		((parede(XA, Y));visitadas(XA,Y)),
		((parede(XB, Y));visitadas(XB,Y)),
		((parede(X, YA));visitadas(X,YA)),
		((parede(X, YB));visitadas(X,YB))
	),
	retractall(pode_ter_poco(X, Y)), assert(tem_poco(X, Y)).
	
atualizar_certezas() :-
	(atualizar_certezas_inimigo();1=1),
	(atualizar_certezas_teletransporte();1=1),
	(atualizar_certezas_poco();1=1), !.

		
%Remover incertezas

remover_incertezas_casa_atual() :- 
		mario_location(X, Y, _),
		(
			((not(poco(X, Y)), assert(nao_tem_poco(X, Y)), retractall(pode_ter_poco(X, Y)));1=1),
			((not(inimigo(_, _, X, Y)), assert(nao_tem_inimigo(X, Y)), retractall(pode_ter_inimigo(X, Y)));1=1),
			((not(teletransporte(X, Y)), assert(nao_tem_teletransporte(X, Y)), retractall(pode_ter_teletransporte(X, Y)));1=1)
		), !.

atualizar_incertezas():-
(
	(remover_incertezas_casa_atual();0=0),
	((ouviu_passos_inimigo(),adjacente_pode_ter_inimigo());0=0),
	((percebeu_flash_teletransporte(),adjacente_pode_ter_teletransporte());0=0),
	((sentiu_brisa_poco(),adjacente_pode_ter_poco());0=0),
	((not(ouviu_passos_inimigo()),remover_incerteza_inimigo_adjacente());0=0),
	((not(percebeu_flash_teletransporte()),remover_incertezas_teletransporte_adjacente());0=0),
	((not(sentiu_brisa_poco()),remover_incertezas_poco_adjacente());0=0),
	((atualizar_certezas();0=0))
),!.



remover_incerteza_poco_adjacente(X, Y) :- assert(nao_tem_poco(X, Y)), retractall(pode_ter_poco(X, Y)), !.	
remover_incertezas_poco_adjacente() :-
	mario_location(X, Y, _),
	(( X2 is X+1, adjacente(X, Y, X2, Y), remover_incerteza_poco_adjacente(X2, Y) );1=1),
	(( X2X is X-1, adjacente(X, Y, X2X, Y), remover_incerteza_poco_adjacente(X2X, Y) );1=1),
	(( Y2 is Y+1, adjacente(X, Y, X, Y2), remover_incerteza_poco_adjacente(X, Y2) );1=1),
	(( Y2Y is Y-1, adjacente(X, Y, X, Y2Y), remover_incerteza_poco_adjacente(X, Y2Y) );1=1), !.

remover_incerteza_teletransporte_adjacente(X, Y) :- assert(nao_tem_teletransporte(X, Y)), retractall(pode_ter_teletransporte(X, Y)), !.							   
remover_incertezas_teletransporte_adjacente() :-
	mario_location(X, Y, _),
	(( X2 is X+1, adjacente(X, Y, X2, Y), remover_incerteza_teletransporte_adjacente(X2, Y) );1=1),
	(( X2X is X-1, adjacente(X, Y, X2X, Y), remover_incerteza_teletransporte_adjacente(X2X, Y) );1=1),
	(( Y2 is Y+1, adjacente(X, Y, X, Y2), remover_incerteza_teletransporte_adjacente(X, Y2) );1=1),
	(( Y2Y is Y-1, adjacente(X, Y, X, Y2Y), remover_incerteza_teletransporte_adjacente(X, Y2Y) );1=1), !.

remover_incerteza_inimigo_adjacente(X, Y) :- assert(nao_tem_inimigo(X, Y)), retractall(pode_ter_inimigo(X, Y)), !.			   
remover_incerteza_inimigo_adjacente() :-
	mario_location(X, Y, _),
	(( X2 is X+1, adjacente(X, Y, X2, Y), remover_incerteza_inimigo_adjacente(X2, Y) );1=1),
	(( X2X is X-1, adjacente(X, Y, X2X, Y), remover_incerteza_inimigo_adjacente(X2X, Y) );1=1),
	(( Y2 is Y+1, adjacente(X, Y, X, Y2), remover_incerteza_inimigo_adjacente(X, Y2) );1=1),
	(( Y2Y is Y-1, adjacente(X, Y, X, Y2Y), remover_incerteza_inimigo_adjacente(X, Y2Y) );1=1), !.



pergunta_pode_ter_poco(X, Y) :- not(visitadas(X, Y)), not(nao_tem_poco(X, Y)), assert(pode_ter_poco(X, Y)), !.
adjacente_pode_ter_poco() :-
	mario_location(X, Y, _),
	(( X2 is X+1, adjacente(X, Y, X2, Y), pergunta_pode_ter_poco(X2, Y) );1=1),
	(( X2X is X-1, adjacente(X, Y, X2X, Y), pergunta_pode_ter_poco(X2X, Y) );1=1),
	(( Y2 is Y+1, adjacente(X, Y, X, Y2), pergunta_pode_ter_poco(X, Y2) );1=1),
	(( Y2Y is Y-1, adjacente(X, Y, X, Y2Y), pergunta_pode_ter_poco(X, Y2Y) );1=1), !.


pergunta_pode_ter_inimigo(X, Y) :- not(visitadas(X, Y)), not(tem_inimigo(X, Y)), not(nao_tem_inimigo(X, Y)), assert(pode_ter_inimigo(X, Y)), !.
adjacente_pode_ter_inimigo() :-
	mario_location(X, Y, _),
	(( X2 is X+1, adjacente(X, Y, X2, Y), pergunta_pode_ter_inimigo(X2, Y) );1=1),
	(( X2X is X-1, adjacente(X, Y, X2X, Y), pergunta_pode_ter_inimigo(X2X, Y) );1=1),
	(( Y2 is Y+1, adjacente(X, Y, X, Y2), pergunta_pode_ter_inimigo(X, Y2) );1=1),
	(( Y2Y is Y-1, adjacente(X, Y, X, Y2Y), pergunta_pode_ter_inimigo(X, Y2Y) );1=1), !.



pergunta_pode_ter_teletransporte(X, Y) :- not(visitadas(X, Y)), not(tem_teletransporte(X, Y)), not(nao_tem_teletransporte(X, Y)), assert(pode_ter_teletransporte(X, Y)), !.
adjacente_pode_ter_teletransporte() :-
	mario_location(X, Y, _),
	not(tem_teletransporte(X, Y)), 
	(( X2 is X+1, adjacente(X, Y, X2, Y), pergunta_pode_ter_teletransporte(X2, Y) );1=1),
	(( X2X is X-1, adjacente(X, Y, X2X, Y), pergunta_pode_ter_teletransporte(X2X, Y) );1=1),
	(( Y2 is Y+1, adjacente(X, Y, X, Y2), pergunta_pode_ter_teletransporte(X, Y2) );1=1),
	(( Y2Y is Y-1, adjacente(X, Y, X, Y2Y), pergunta_pode_ter_teletransporte(X, Y2Y) );1=1), !.
	

%Preenchendo o path

tomar_decisao_segura() :- visitadas(X, Y), not(percebeu_algum_perigo(X, Y)), 
						    not(tem_inimigo(X, Y)), not(tem_teletransporte(X, Y)),
							adjacente(X, Y, X2, Y2), not(visitadas(X2, Y2)),
							mario_location(CurX, CurY, _),
							go([CurX, CurY], [X, Y]), !. 

tomar_decisao_inimigoOuTeletransporte() :- visitadas(X, Y), adjacente(X, Y, X2, Y2), not(visitadas(X2, Y2)),
							      (inimigo(_, _, X2, Y2); teletransporte(X2, Y2)), !.

tomar_decisao_powerup() :- powerup(X, Y), visitadas(X, Y), mario_location(CurX, CurY, _),
								 not((X = CurX, Y = CurY)), go([CurX, CurY], [X, Y]), !.

tomar_decisao_lutar() :- tem_inimigo(X, Y), adjacente(X, Y, X2, Y2), visitadas(X2, Y2), mario_location(CurX, CurY, _),  
							   not((X2 = CurX, Y2 = CurY)), go([CurX, CurY], [X2, Y2]), !.
							   
tomar_decisao_pode_encontrar_inimigo() :- pode_ter_inimigo(X, Y), adjacente(X, Y, X2, Y2), visitadas(X2, Y2), mario_location(CurX, CurY, _),  
							       not((X2 = CurX, Y2 = CurY)), go([CurX, CurY], [X2, Y2]), !.

tomar_decisao_pode_cair_poco() :- mario_location(CurX, CurY, _), pode_ter_poco(X, Y), adjacente(X, Y, X2, Y2),
							visitadas(X2, Y2), not((X2 = CurX, Y2 = CurY)), go([CurX, CurY], [X2, Y2]), !.
							
tomar_decisao_voltar_visitadas() :- mario_location(X, Y, _), adjacente(X, Y, X2, Y2), visitadas(X2, Y2),
								  go([X, Y], [X2, Y2]), writef('Will run away from enemy\n'), !.
							
tomar_decisao_sair() :- mario_location(X, Y, _), inicio(X2, Y2), go([X, Y], [X2, Y2]), !.

atirar(Resultado) :- 
			     mario_location(X, Y, _), adjacente(X, Y, X2, Y2), tem_inimigo(X2, Y2),
			     random_between(20, 50, DanoInimigo),
				 (
				 	(inimigo(DanoInimigoProvoca, Energia_Inimigo, X2, Y2),
				 		Energia_Inimigo_Apos is Energia_Inimigo - DanoInimigo,
				 		(
				 			(Energia_Inimigo_Apos < 1, retractall(inimigo(_, _, X2, Y2)), retractall(tem_inimigo(X2, Y2)), assert(vazia(X2, Y2)), Resultado = matou, writef('Inimigo morreu!\n'));
				 			(retractall(inimigo(_, _, X2, Y2)), assert(inimigo(DanoInimigoProvoca, Energia_Inimigo_Apos, X2, Y2)), Resultado = atacou_nao_matou, writef('Acertou inimigo (ENERGIA = %w)!\n', [Energia_Inimigo_Apos]))
				 		)
				 	)
				 ),
				 atualizar_score(-10),
				 atualizar_municao(),!.
				 


proximo_movimento(sair) :- mario_location(X, Y, _), inicio(X, Y), saida(1), !.


proximo_movimento(morreu) :- energia(E), E < 1, !.


proximo_movimento(pegar_ouro) :- mario_location(X, Y, _), ouro(X, Y), retract(ouro(X, Y)), assert(vazia(X, Y)),
					     atualizar_score(1000), !. 

proximo_movimento(Acao) :- curPath([]), retract(curPath([])), proximo_movimento(Acao), !.

proximo_movimento(girar) :- curPath([[X, Y]|_]), not(mario_vai_para(X, Y)), mario_andar(), !.

proximo_movimento(andar) :- curPath([[X, Y]|T]), mario_vai_para(X, Y),
					 retractall(curPath(_)), assert(curPath(T)), mario_andar_para(X, Y), !.



proximo_movimento(girar) :- mario_vai_para(X, Y), not(pode_ser_acessada(X, Y)), mario_andar(), !.



proximo_movimento(andar) :- mario_vai_para(X, Y), pode_ser_acessada(X, Y), not(percebeu_algum_perigo()),
					 not(visitadas(X, Y)), mario_andar_para(X, Y), !.

proximo_movimento(girar) :- mario_location(X, Y, _), adjacente(X, Y, X2, Y2), not(percebeu_algum_perigo()),
					   not(visitadas(X2,Y2)), not(mario_vai_para(X2, Y2)), mario_andar(), !.



proximo_movimento(Acao) :- tomar_decisao_segura(), proximo_movimento(Acao), writef('Vai para uma casa segura\n'), !.


proximo_movimento(Resultado) :- municao(M), M > 0, mario_location(X, Y, _), adjacente(X, Y, X2, Y2), tem_inimigo(X2, Y2), mario_vai_para(X2, Y2),
					   energia(E), E > 50, writef('Vai atirar no inimigo\n'), atirar(Resultado), !.
				   
proximo_movimento(girar) :- municao(M), M > 0, mario_location(X, Y, _), adjacente(X, Y, X2, Y2), tem_inimigo(X2, Y2), not(mario_vai_para(X2, Y2)),
					   energia(E), E > 50, mario_andar(), !.
					   
proximo_movimento(Acao) :- municao(M), M > 0, energia(E), E > 50, tomar_decisao_lutar(), proximo_movimento(Acao), !.



proximo_movimento(andar) :- energia(E), E > 50, mario_vai_para(X, Y), pode_ter_inimigo(X, Y),
					 mario_andar_para(X, Y), !.

proximo_movimento(girar) :- energia(E), E > 50, mario_location(CurX, CurY, _), adjacente(CurX, CurY, X2, Y2),
					   pode_ter_inimigo(X2, Y2), not(mario_vai_para(X2, Y2)), mario_andar(),!.
					   	 
proximo_movimento(Acao) :- energia(E), E > 50, tomar_decisao_pode_encontrar_inimigo(), proximo_movimento(Acao), !.
					   



proximo_movimento(pegar_power_up) :- energia(E), E < 51, mario_location(X, Y, _), powerup(X, Y), retract(powerup(X, Y)), assert(vazia(X, Y)),
					        atualizar_score(-1),atualizar_energia(20), !. 

proximo_movimento(Acao) :- energia(E), E < 51, tomar_decisao_powerup(), proximo_movimento(Acao), writef('Vai pegar um power up!\n'), !.



proximo_movimento(andar) :- score(C), C < 1, mario_vai_para(X, Y), pode_ter_poco(X, Y), mario_andar_para(X, Y), writef('Pode cair no poco\n'), !.
proximo_movimento(girar) :- score(C), C < 1, mario_location(X, Y, _), adjacente(X, Y, X2, Y2), 
					   pode_ter_poco(X2, Y2), not(mario_vai_para(X2, Y2)), mario_andar(), !.
proximo_movimento(Acao) :- score(C), C < 1, tomar_decisao_pode_cair_poco(), proximo_movimento(Acao),!.



proximo_movimento(Acao) :- assert(saida(1)), tomar_decisao_sair(), proximo_movimento(Acao), writef('Vai tentar sair!\n'), !.

