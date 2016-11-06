package Model;

import org.jpl7.*;
import java.util.Map;

public class ConsultProlog {
	
	private static Query q1 = new Query("consult", new Term[] {new Atom("Resources/Mario.pl")});
	
	public ConsultProlog() {
		System.out.println( "consult " + (q1.hasSolution() ? "succeeded" : "failed"));
	}

	public void getNextMove(Mario mario) {

		Query q3 = new Query("proximo_movimento(Mov).");
		Map<String, Term> solution = q3.oneSolution();
		
		if(solution == null) {
			return;
		}
		String action = solution.get("Action").toString();
		
		switch(action) {
			case "rodar":
				mario.orientationManager();
				break;
			case "atacou_não_matou":
				mario.ataca();
				break;
			case "andar":
				mario.andar();
				break;
			case "pegar_ouro":
				mario.pegar(1000);
				break;
			case "pegar_power_up":
				mario.pegar(20);
				break;
			case "morreu":
				System.out.println("BUSTED");
				System.exit(1);
			case "matou":
				mario.matar();
				break;
			case "sair":
				System.out.println("GANHOU");
				System.out.println(mario.energy);
				System.exit(1);
			default:
				System.out.println("ACTION INVALIDA");
				System.out.println(action);
				System.exit(1);
		}
	}
	

}
