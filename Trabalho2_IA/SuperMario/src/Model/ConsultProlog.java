package Model;

import org.jpl7.*;
import java.util.Map;

public class ConsultProlog {
	
	private static Query q1 = new Query("consult", new Term[] {new Atom("src/logic/main.pl")});
	
	public ConsultProlog() {
		System.out.println( "consult " + (q1.hasSolution() ? "succeeded" : "failed"));
	}

	public Action getNextMove() {
		//TO-DO
		Query q3 = new Query("proximo_movimento(Mov).");
		Map<String, Term> solution = q3.oneSolution();
		
		if(solution == null) {
			return null;
		}
		String action = solution.get("Action").toString();
		
		switch(action) {
			case "Rodar":
				return Action.Virar_a_direita;
			case "atacou_não_matou":
				return Action.Atirar;
			case "Andar":
				return Action.Mover_para_frente;
			case "pegar_ouro":
				return Action.Pegar_ouro;
			case "pegar_power_up":
				return Action.Pegar_powerUp;
			case "morreu":
				return Action.Matar;
			case "sair":
				return Action.Sair;
		}
		
		return null;
		
	}
	

}
