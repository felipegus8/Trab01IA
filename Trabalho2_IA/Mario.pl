:- dynamic inicio/2.
:- dynamic parede/2.
:- dynamic vazia/2.
:- dynamic inimigo/4. 
:- dynamic ouro/2.
:- dynamic powerup/2.
:- dynamic poco/2.
:- dynamic teletransporte/2.
:- dynamic energia/1.
:- dynamic score/1.
:- dynamic municao/1.
:- dynamic mario_location/3.
:- dynamic visitadas/2.
:- dynamic exiting/1.
:- dynamic curPath/1.
:- dynamic pode_ter_teletransporte/2.
:- dynamic pode_ter_inimigo/2.
:- dynamic pode_ter_poco/2.
:- dynamic nao_tem_teletransporte/2.
:- dynamic nao_tem_inimigo/2.
:- dynamic nao_tem_poco/2.
:- dynamic tem_teletransporte/2.
:- dynamic tem_inimigo/2.
:- dynamic tem_poco/2.
:-dynamic rpath/2.      
:-dynamic dijkstra_option_allowDanger/1. 


adjacente(X, Y, XX, Y) :- XX is X+1, pode_ser_acessada(XX, Y).
adjacente(X, Y, XX, Y) :- XX is X-1, pode_ser_acessada(XX, Y).
adjacente(X, Y, X, YY) :- YY is Y+1, pode_ser_acessada(X, YY).
adjacente(X, Y, X, YY) :- YY is Y-1, pode_ser_acessada(X, YY).

pode_ser_acessada(X, Y) :- inicio(X, Y); vazia(X, Y); inimigo(_, _, X, Y); ouro(X, Y); powerup(X, Y); poco(X, Y); teletransporte(X, Y), !. 



estado_atual_mario(X, Y, Direcao, Energia, Score, Municao) :- mario_location(X, Y, Direcao), energia(Energia), score(Score), municao(Municao), !.


mario_esvaziamapa() :- retractall(parede(_,_)), retractall(vazia(_,_)), retractall(inimigo(_,_,_,_)),
                    retractall(ouro(_,_)), retractall(powerup(_,_)), 
retractall(poco(_,_)), retractall(teletransporte(_,_)), retractall(inicio(_,_)).

mario_reset() :- retractall(energia(_)), retractall(score(_)), retractall(municao(_)), retractall(mario_location(_,_,_)),
			    retractall(visitadas(_,_)), inicio(StartX, StartY),
			    retractall(curPath(_)), retractall(exiting(_)), retractall(dijkstra_option_allowDanger(_)),
			    retractall(nao_tem_poco(_,_)), retractall(nao_tem_inimigo(_,_)), retractall(nao_tem_teletransporte(_,_)),
			    retractall(pode_ter_poco(_,_)), retractall(pode_ter_inimigo(_,_)), retractall(pode_ter_teletransporte(_,_)),
			    retractall(tem_poco(_,_)), retractall(tem_inimigo(_,_)), retractall(tem_teletransporte(_,_)),
assert(energia(100)), assert(score(0)), assert(municao(5)), assert(mario_location(StartX, StartY, up)), assert(visitadas(StartX, StartY)).


%Atualiza pontuação

mario_somar_score(SOM) :- score(C), CC is C+SOM, retract(score(C)), assert(score(CC)).
mario_subtrair_score(SUB) :- SOM is SUB * -1, mario_somar_score(SOM).

mario_somar_energia(SOM) :- energia(E), EE is E+SOM, retract(energia(E)), assert(energia(EE)).
mario_subtrair_energia(SUB) :- SOM is SUB * -1, mario_somar_energia(SOM).


mario_perdeu_municao() :- municao(Municao), NewAmmo is Municao - 1, retract(municao(Municao)), assert(municao(NewAmmo)), !.


%Movimento

mario_girar() :- ((mario_location(_,_,right), Prox = down);
				  (mario_location(_,_,down), Prox = left);
				  (mario_location(_,_,left), Prox = up);
				  (mario_location(_,_,up), Prox = right)
				 ), mario_girarPara(Prox).

mario_girar_esquerda() :- ((mario_location(_,_,right), Prox = up);
				  (mario_location(_,_,up), Prox = left);
				  (mario_location(_,_,left), Prox = down);
				  (mario_location(_,_,down), Prox = right)
				 ), mario_girarPara(Prox).

mario_girarPara(Prox) :- mario_location(X,Y,_), retract(mario_location(X,Y,_)), assert(mario_location(X,Y,Prox)), mario_subtrair_score(1), !.



mario_andar_para(X, Y) :- mario_location(XX, YY, Direcao), Y is YY-1, XX = X, Direcao=up, !.
mario_andar_para(X, Y) :- mario_location(XX, YY, Direcao), X is XX+1, YY = Y, Direcao=right, !.
mario_andar_para(X, Y) :- mario_location(XX, YY, Direcao), Y is YY+1, XX = X, Direcao=down, !.
mario_andar_para(X, Y) :- mario_location(XX, YY, Direcao), X is XX-1, YY = Y, Direcao=left, !.



mario_andar(X, Y) :- pode_ser_acessada(X, Y), mario_location(_, _, Direcao), retractall(mario_location(_, _, _)), assert(mario_location(X, Y, Direcao)), (visitadas(X, Y);assert(visitadas(X, Y))),
                      mario_subtrair_score(1),
                      atualizar_incertezas(),
                      ((poco(X, Y), assert(tem_poco(X, Y)), mario_subtrair_score(1000), EE is 0, retractall(energia(_)), assert(energia(EE)));1=1),
                      ((inimigo(EEE,_, X, Y), writef('Caiu no inimigo.Já era\n'), mario_subtrair_energia(EEE), assert(tem_inimigo(X, Y)), retractall(pode_ter_inimigo(X, Y)), atualizar_incertezas(), assert(dijkstra_option_allowDanger(1)), tomar_decisao_voltar_visitadas(), retractall(dijkstra_option_allowDanger(_)));1=1), !.


 
%Dijkstra

%Implementação de Dijkstra em prolog retirada de(http://rosettacode.org/wiki/Dijkstra's_algorithm#Prolog)      

path([X, Y],[XX, YY], 1) :- visitadas(X, Y), visitadas(XX, YY), adjacente(X, Y, XX, YY),
							(
								dijkstra_option_allowDanger(1);
								(
									not(pode_ter_inimigo(XX, YY)), not(pode_ter_teletransporte(XX, YY)),
									not(pode_ter_inimigo(X, Y)), not(pode_ter_teletransporte(X, Y)),
									not(tem_teletransporte(XX, YY)), not(tem_teletransporte(X, Y)),
									not(tem_inimigo(XX, YY)), not(tem_inimigo(X, Y))
								)
							). 

shorterPath([H|Path], Dist) :-		      
	rpath([H|T], D), !, Dist < D,          
	retract(rpath([H|_],_)),
	
	assert(rpath([H|Path], Dist)).
shorterPath(Path, Dist) :-		      
	
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
 
go([X, Y], [XX, YY]) :-
	traverse([X, Y]),                   
	rpath([[XX, YY]|RPath], Dist)->         
	  reverse([[XX, YY]|RPath], Path),      
	  Distance is round(Dist),
	  (writef('Shortest path is %w with distance %w = %w\n',
	       [Path, Dist, Distance]), Path = [_|T], assert(curPath(T)));
	writef('There is no route from %w to %w\n', [[X, Y], [XX, YY]]).
	
%Percepcoes
sentiu_brisa_poco(X, Y) :- adjacente(X, Y, XX, YY), poco(XX, YY), !.
sentiu_brisa_poco() :- mario_location(X, Y, _), sentiu_brisa_poco(X, Y), !.

ouviu_passos_inimigo(X, Y) :- adjacente(X, Y, XX, YY), inimigo(_, _, XX, YY), !.
ouviu_passos_inimigo() :- mario_location(X, Y, _), ouviu_passos_inimigo(X, Y), !.

percebeu_flash_teletransporte(X, Y) :- adjacente(X, Y, XX, YY), teletransporte(XX, YY), !.
percebeu_flash_teletransporte() :- mario_location(X, Y, _), percebeu_flash_teletransporte(X, Y), !.

percebeu_algum_perigo(X, Y) :- (sentiu_brisa_poco(X, Y); ouviu_passos_inimigo(X, Y); percebeu_flash_teletransporte(X, Y)), !.
percebeu_algum_perigo() :- mario_location(X, Y, _), percebeu_algum_perigo(X, Y), !.

%Base de conhecimento

%Atualiza todas as certezas do Mario

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
	
atualizar_certezas_geral() :-
	(atualizar_certezas_inimigo();1=1),
	(atualizar_certezas_teletransporte();1=1),
	(atualizar_certezas_poco();1=1), !.

remover_incertezas_casa_atual() :- 
		mario_location(X, Y, _),
		(
			((not(poco(X, Y)), assert(nao_tem_poco(X, Y)), retractall(pode_ter_poco(X, Y)));1=1),
			((not(inimigo(_, _, X, Y)), assert(nao_tem_inimigo(X, Y)), retractall(pode_ter_inimigo(X, Y)));1=1),
			((not(teletransporte(X, Y)), assert(nao_tem_teletransporte(X, Y)), retractall(pode_ter_teletransporte(X, Y)));1=1)
		), !.

atualizar_incertezas() :- (
		(remover_incertezas_casa_atual();1=1),
		((sentiu_brisa_poco(), pergunta_adjacente_pode_ter_poco());1=1),
		((ouviu_passos_inimigo(), pergunta_adjacente_pode_ter_inimigo());1=1),
		((percebeu_flash_teletransporte(), pergunta_adjacente_pode_ter_teletransporte());1=1),
		(( not(sentiu_brisa_poco()), remover_incertezas_pocos_adjacentes() );1=1),
		(( not(ouviu_passos_inimigo()), remover_incertezas_inimigos_adjacentes() );1=1),
		(( not(percebeu_flash_teletransporte()), remover_incertezas_teletransportes_adjacentes() );1=1),
		((atualizar_certezas_geral());1=1)
	), !.
	
%Remove incertezas

remover_incerteza_poco_adjacente(X, Y) :- assert(nao_tem_poco(X, Y)), retractall(pode_ter_poco(X, Y)), !.	
remover_incertezas_pocos_adjacentes() :-
	mario_location(X, Y, _),
	(( XX is X+1, adjacente(X, Y, XX, Y), remover_incerteza_poco_adjacente(XX, Y) );1=1),
	(( XXX is X-1, adjacente(X, Y, XXX, Y), remover_incerteza_poco_adjacente(XXX, Y) );1=1),
	(( YY is Y+1, adjacente(X, Y, X, YY), remover_incerteza_poco_adjacente(X, YY) );1=1),
	(( YYY is Y-1, adjacente(X, Y, X, YYY), remover_incerteza_poco_adjacente(X, YYY) );1=1), !.



remover_incertezas_inimigos_adjacentes(X, Y) :- assert(nao_tem_inimigo(X, Y)), retractall(pode_ter_inimigo(X, Y)), !.			   
remover_incertezas_inimigos_adjacentes() :-
	mario_location(X, Y, _),
	(( XX is X+1, adjacente(X, Y, XX, Y), remover_incertezas_inimigos_adjacentes(XX, Y) );1=1),
	(( XXX is X-1, adjacente(X, Y, XXX, Y), remover_incertezas_inimigos_adjacentes(XXX, Y) );1=1),
	(( YY is Y+1, adjacente(X, Y, X, YY), remover_incertezas_inimigos_adjacentes(X, YY) );1=1),
	(( YYY is Y-1, adjacente(X, Y, X, YYY), remover_incertezas_inimigos_adjacentes(X, YYY) );1=1), !.


remover_incerteza_teletransporte_adjacente(X, Y) :- assert(nao_tem_teletransporte(X, Y)), retractall(pode_ter_teletransporte(X, Y)), !.							   
remover_incertezas_teletransportes_adjacentes() :-
	mario_location(X, Y, _),
	(( XX is X+1, adjacente(X, Y, XX, Y), remover_incerteza_teletransporte_adjacente(XX, Y) );1=1),
	(( XXX is X-1, adjacente(X, Y, XXX, Y), remover_incerteza_teletransporte_adjacente(XXX, Y) );1=1),
	(( YY is Y+1, adjacente(X, Y, X, YY), remover_incerteza_teletransporte_adjacente(X, YY) );1=1),
	(( YYY is Y-1, adjacente(X, Y, X, YYY), remover_incerteza_teletransporte_adjacente(X, YYY) );1=1), !.


	
pergunta_pode_ter_poco(X, Y) :- not(visitadas(X, Y)), not(nao_tem_poco(X, Y)), assert(pode_ter_poco(X, Y)), !.
pergunta_adjacente_pode_ter_poco() :-
	mario_location(X, Y, _),
	(( XX is X+1, adjacente(X, Y, XX, Y), pergunta_pode_ter_poco(XX, Y) );1=1),
	(( XXX is X-1, adjacente(X, Y, XXX, Y), pergunta_pode_ter_poco(XXX, Y) );1=1),
	(( YY is Y+1, adjacente(X, Y, X, YY), pergunta_pode_ter_poco(X, YY) );1=1),
	(( YYY is Y-1, adjacente(X, Y, X, YYY), pergunta_pode_ter_poco(X, YYY) );1=1), !.



pergunta_pode_ter_inimigo(X, Y) :- not(visitadas(X, Y)), not(tem_inimigo(X, Y)), not(nao_tem_inimigo(X, Y)), assert(pode_ter_inimigo(X, Y)), !.
pergunta_adjacente_pode_ter_inimigo() :-
	mario_location(X, Y, _),
	(( XX is X+1, adjacente(X, Y, XX, Y), pergunta_pode_ter_inimigo(XX, Y) );1=1),
	(( XXX is X-1, adjacente(X, Y, XXX, Y), pergunta_pode_ter_inimigo(XXX, Y) );1=1),
	(( YY is Y+1, adjacente(X, Y, X, YY), pergunta_pode_ter_inimigo(X, YY) );1=1),
	(( YYY is Y-1, adjacente(X, Y, X, YYY), pergunta_pode_ter_inimigo(X, YYY) );1=1), !.


pergunta_pode_ter_teletransporte(X, Y) :- not(visitadas(X, Y)), not(tem_teletransporte(X, Y)), not(nao_tem_teletransporte(X, Y)), assert(pode_ter_teletransporte(X, Y)), !.
pergunta_adjacente_pode_ter_teletransporte() :-
	mario_location(X, Y, _),
	not(tem_teletransporte(X, Y)), 
	(( XX is X+1, adjacente(X, Y, XX, Y), pergunta_pode_ter_teletransporte(XX, Y) );1=1),
	(( XXX is X-1, adjacente(X, Y, XXX, Y), pergunta_pode_ter_teletransporte(XXX, Y) );1=1),
	(( YY is Y+1, adjacente(X, Y, X, YY), pergunta_pode_ter_teletransporte(X, YY) );1=1),
	(( YYY is Y-1, adjacente(X, Y, X, YYY), pergunta_pode_ter_teletransporte(X, YYY) );1=1), !.
	

%Preenchendo o path

tomar_decisao_segura() :- visitadas(X, Y), not(percebeu_algum_perigo(X, Y)), 
						    not(tem_inimigo(X, Y)), not(tem_teletransporte(X, Y)),
							adjacente(X, Y, XX, YY), not(visitadas(XX, YY)),
							mario_location(CurX, CurY, _),
							go([CurX, CurY], [X, Y]), !. 

tomar_decisao_inimigoOuTeletransporte() :- visitadas(X, Y), adjacente(X, Y, XX, YY), not(visitadas(XX, YY)),
							      (inimigo(_, _, XX, YY); teletransporte(XX, YY)), !.

tomar_decisao_powerup() :- powerup(X, Y), visitadas(X, Y), mario_location(CurX, CurY, _),
								 not((X = CurX, Y = CurY)), go([CurX, CurY], [X, Y]), !.

tomar_decisao_lutar() :- tem_inimigo(X, Y), adjacente(X, Y, XX, YY), visitadas(XX, YY), mario_location(CurX, CurY, _),  
							   not((XX = CurX, YY = CurY)), go([CurX, CurY], [XX, YY]), !.
							   
tomar_decisao_pode_encontrar_inimigo() :- pode_ter_inimigo(X, Y), adjacente(X, Y, XX, YY), visitadas(XX, YY), mario_location(CurX, CurY, _),  
							       not((XX = CurX, YY = CurY)), go([CurX, CurY], [XX, YY]), !.

tomar_decisao_pode_cair_poco() :- mario_location(CurX, CurY, _), pode_ter_poco(X, Y), adjacente(X, Y, XX, YY),
							visitadas(XX, YY), not((XX = CurX, YY = CurY)), go([CurX, CurY], [XX, YY]), !.
							
tomar_decisao_voltar_visitadas() :- mario_location(X, Y, _), adjacente(X, Y, XX, YY), visitadas(XX, YY),
								  go([X, Y], [XX, YY]), writef('Vai fugir do inimigo\n'), !.
							
tomar_decisao_sair() :- mario_location(X, Y, _), inicio(XX, YY), go([X, Y], [XX, YY]), !.

%Atirar no inimigo
atirar(Resultado) :- 
			     mario_location(X, Y, _), adjacente(X, Y, XX, YY), tem_inimigo(XX, YY),
			     random_between(20, 50, DanoInimigo),
				 (
				 	(inimigo(DanoInimigoProvoca, Energia_Inimigo, XX, YY),
				 		Energia_Inimigo_Apos is Energia_Inimigo - DanoInimigo,
				 		(
				 			(Energia_Inimigo_Apos < 1, retractall(inimigo(_, _, XX, YY)), retractall(tem_inimigo(XX, YY)), assert(vazia(XX, YY)), Resultado = matou, writef('ENIMIGO MORTO!\n'));
				 			(retractall(inimigo(_, _, XX, YY)), assert(inimigo(DanoInimigoProvoca, Energia_Inimigo_Apos, XX, YY)), Resultado = atacou_nao_matou, writef('Inimigo Atingido (ENERGIA = %w)!\n', [Energia_Inimigo_Apos]))
				 		)
				 	)
				 ),
				 mario_subtrair_score(10),
				 mario_perdeu_municao(), !.
				 
			
%Movimentos possíveis

proximo_movimento(sair) :- mario_location(X, Y, _), inicio(X, Y), exiting(1), !.



proximo_movimento(morreu) :- energia(E), E < 1,format('Regra 1'), !.



proximo_movimento(pegar_ouro) :- mario_location(X, Y, _), ouro(X, Y), retract(ouro(X, Y)), assert(vazia(X, Y)),
					     mario_somar_score(1000),format('Regra 2'), !. 


proximo_movimento(Acao) :- curPath([]), retract(curPath([])), proximo_movimento(Acao),format('Regra 3'), !.

proximo_movimento(girar) :- curPath([[X, Y]|_]), not(mario_andar_para(X, Y)), mario_girar(),format('Regra 4'), !.

proximo_movimento(andar) :- curPath([[X, Y]|T]), mario_andar_para(X, Y),
					 retractall(curPath(_)), assert(curPath(T)), mario_andar(X, Y),format('Regra 5'), !.



proximo_movimento(girar) :- mario_andar_para(X, Y), not(pode_ser_acessada(X, Y)), mario_girar(),format('Regra 6'), !.


proximo_movimento(andar) :- mario_andar_para(X, Y), pode_ser_acessada(X, Y), not(percebeu_algum_perigo()),
					 not(visitadas(X, Y)), mario_andar(X, Y),format('Regra 7'), !.
					 

proximo_movimento(girar) :- mario_location(X, Y, _), adjacente(X, Y, XX, YY), not(percebeu_algum_perigo()),
					   not(visitadas(XX,YY)), not(mario_andar_para(XX, YY)), mario_girar(),format('Regra 8'), !.



proximo_movimento(Acao) :- tomar_decisao_segura(), proximo_movimento(Acao), writef('Vai por um caminho seguro!\n'),format('Regra 9'), !.



proximo_movimento(Resultado) :- municao(A), A > 0, mario_location(X, Y, _), adjacente(X, Y, XX, YY), tem_inimigo(XX, YY), mario_andar_para(XX, YY),
					   energia(E), E > 50, writef('Vai atirar no inimigo!\n'), atirar(Resultado),format('Regra 10'), !.
				   
proximo_movimento(girar) :- municao(A), A > 0, mario_location(X, Y, _), adjacente(X, Y, XX, YY), tem_inimigo(XX, YY), not(mario_andar_para(XX, YY)),
					   energia(E), E > 50, mario_girar(),format('Regra 11'), !.
					   
proximo_movimento(Acao) :- municao(A), A > 0, energia(E), E > 50, tomar_decisao_lutar(), proximo_movimento(Acao), writef('Vai atirar no inimigo!\n'),format('Regra 12'), !.

	


proximo_movimento(andar) :- energia(E), E > 50, mario_andar_para(X, Y), pode_ter_inimigo(X, Y),
					 mario_andar(X, Y),format('Regra 13'), !.

proximo_movimento(girar) :- energia(E), E > 50, mario_location(CurX, CurY, _), adjacente(CurX, CurY, XX, YY),
					   pode_ter_inimigo(XX, YY), not(mario_andar_para(XX, YY)), mario_girar(),format('Regra 14'),!.
					   	 
proximo_movimento(Acao) :- energia(E), E > 50, tomar_decisao_pode_encontrar_inimigo(), proximo_movimento(Acao), writef('Vai arriscar inimigo!\n'),format('Regra 15'), !.
					   


proximo_movimento(pegar_power_up) :- mario_location(X, Y, _), powerup(X, Y), retract(powerup(X, Y)), assert(vazia(X, Y)),
					        mario_subtrair_score(1), mario_somar_energia(20),format('Regra 16'), !. 

proximo_movimento(Acao) :- tomar_decisao_powerup(), proximo_movimento(Acao), writef('Going to pick a power up!\n'),format('Regra 17'), !.



proximo_movimento(andar) :- score(C), C < 1, mario_andar_para(X, Y), pode_ter_poco(X, Y), mario_andar(X, Y), writef('Vai arriscar cair no poco!\n'),format('Regra 18'), !.
proximo_movimento(girar) :- score(C), C < 1, mario_location(X, Y, _), adjacente(X, Y, XX, YY), 
					   pode_ter_poco(XX, YY), not(mario_andar_para(XX, YY)), mario_girar(), writef('Vai arriscar cair no poco!\n'),format('Regra 19'), !.
proximo_movimento(Acao) :- score(C), C < 1, tomar_decisao_pode_cair_poco(), proximo_movimento(Acao), writef('Vai arriscar cair no poco!\n'),format('Regra 20'), !.



proximo_movimento(Acao) :- assert(exiting(1)), tomar_decisao_sair(), proximo_movimento(Acao), writef('Vai sair!\n'),format('Regra 21'), !.
