package Model;

import org.jpl7.*;
import java.util.Map;

public class ConsultProlog {
	
	private static Query q1 = new Query("consult", new Term[] {new Atom("Resources/Mario.pl")});
	
//	private static Query q2 = new Query("consult", new Term[] {new Atom("Resources/map.pl")});
	
	public ConsultProlog() {
		System.out.println( "consult " + (q1.hasSolution() ? "succeeded" : "failed"));
        
        Map<String, Term> esvaziarMapa = fazQuery("mario_esvaziamapa().");
        Query consultaMapa = new Query("consult", new Term[] {new Atom("map.pl")});
        
        if(!consultaMapa.hasSolution()) {
            return;
        }
        
        Map<String, Term> reset = fazQuery("mario_reset().");
        
        if(esvaziarMapa == null ||reset == null)
        {
            System.out.println("NULL");
            return;
        }
//		System.out.println( "consult " + (q2.hasSolution() ? "succeeded" : "failed"));
	}

	public void getNextMove(Mario mario) {

//		Query q3 = new Query("proximo_movimento(Acao).");
		
		
		
		Map<String, Term> solution = fazQuery("proximo_movimento(Acao).");
		
		if(solution == null) {
			System.out.println("NULL");
			return;
		}
		String action = solution.get("Acao").toString();
		
		System.out.println(action);
		
		switch(action) {
			case "girar":
				mario.orientationManager();
				break;
			case "atacou_nao_matou":
				mario.ataca();
				break;
			case "andar":
				System.out.println("Andou");
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
	
	public Map<String, Term> fazQuery(String query) {
		Query q3 = new Query(query);
		Map<String, Term> solution = q3.oneSolution();
		
		if(solution == null) {
			System.out.println("NULL");
			return null;
		}
		
		return solution;
	}
	

}
