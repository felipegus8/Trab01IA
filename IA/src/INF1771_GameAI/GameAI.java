package INF1771_GameAI;
import INF1771_GameAI.Map.*;
import java.util.ArrayList;
import java.util.List;


public class GameAI {
    Position player = new Position();
    String state = "ready";
    Estado estado = null;
    String dir = "north";
    long score = 0;
    int energy = 0;
    Estado estadoAnterior;
    int i = 0;
    int j = 0;
    int k = 0;
    int camper = 0;
    int blocked = 0;
    int qtdBlocked = 0;

    
    /**
     * Refresh player status
     * @param x			player position x
     * @param y			player position y
     * @param dir		player direction
     * @param state		player state
     * @param score		player score
     * @param energy	player energy
     */
    public void SetStatus(int x, int y, String dir, String state, long score, int energy) {
        player.x = x;
        player.y = y;
        this.dir = dir.toLowerCase();

        this.state = state;
        this.score = score;
        this.energy = energy;
    }

    /**
     * Get list of observable adjacent positions
     * @return List of observable adjacent positions 
     */
    public List<Position> GetObservableAdjacentPositions() {
        List<Position> ret = new ArrayList<Position>();

        ret.add(new Position(player.x - 1, player.y));
        ret.add(new Position(player.x + 1, player.y));
        ret.add(new Position(player.x, player.y - 1));
        ret.add(new Position(player.x, player.y + 1));

        return ret;
    }

    /**
     * Get list of all adjacent positions (including diagonal)
     * @return List of all adjacent positions (including diagonal)
     */
    public List<Position> GetAllAdjacentPositions() {
        List<Position> ret = new ArrayList<Position>();

        ret.add(new Position(player.x - 1, player.y - 1));
        ret.add(new Position(player.x, player.y - 1));
        ret.add(new Position(player.x + 1, player.y - 1));

        ret.add(new Position(player.x - 1, player.y));
        ret.add(new Position(player.x + 1, player.y));

        ret.add(new Position(player.x - 1, player.y + 1));
        ret.add(new Position(player.x, player.y + 1));
        ret.add(new Position(player.x + 1, player.y + 1));

        return ret;
    }

    /**
     * Get next forward position
     * @return next forward position
     */
    public Position NextPosition() {
        Position ret = null;
        if(dir.equals("north"))
                ret = new Position(player.x, player.y - 1);
        else if(dir.equals("east"))
                ret = new Position(player.x + 1, player.y);
        else if(dir.equals("south"))
                ret = new Position(player.x, player.y + 1);
        else if(dir.equals("west"))
                ret = new Position(player.x - 1, player.y);

        return ret;
    }

    /**
     * Player position
     * @return player position
     */
    public Position GetPlayerPosition() {
        return player;
    }
    
    /**
     * Set player position
     * @param x		x position
     * @param y		y position
     */
    public void SetPlayerPosition(int x, int y) {
        player.x = x;
        player.y = y;

    }

    /**
     * Observations received
     * @param o	 list of observations
     */
    public void GetObservations(List<String> o) {

    	if(o.isEmpty()) {
    		estado = null;
    		return;
    	}
    	
        for (String s : o) {
        	System.out.println(s);
        	
           if(s.equals("steps")){
        	    if(this.estado != Estado.PEGAR_OURO && this.estado != Estado.CAMPERMALDITO) {
            	    this.estado = Estado.ENEMY;
        	    }
            } else if(s.equals("breeze")){
            	if(this.estado != Estado.PEGAR_OURO && this.estado != Estado.CAMPERMALDITO) {
            		this.estado = Estado.VOLTAR;
            	}
            } else if(s.equals("flash")){
            	this.estado = Estado.VOLTAR;
            } else if(s.equals("redLight")){
            	System.out.println("entrei no pegar pocao\n");
            	this.estado = Estado.PEGAR_POCAO;
            } else if(s.equals("blueLight")){
            	System.out.println("entrei no pegar\n");
            	this.estado = Estado.PEGAR_OURO;
            } else if(s.equals("greenLight")){
            	this.estado = null;
            } else if(s.equals("weakLight")){
            	this.estado = null;
            } else if(s.equals("damage")) {
            	if(energy<50) {
            	   this.estado = Estado.FUGIR;
            	}
            } else if(s.equals("hit")) {
            	this.estado = Estado.HIT;
            } else if(s.equals("blocked")) {
            	this.estado = Estado.BLOCKED;
            } else if(s.contains("enemy")) {
            	if(this.estado != Estado.PEGAR_OURO && this.estado != Estado.CAMPERMALDITO) {
            		this.estado = Estado.ENEMY;
            	}
            } else {
            	System.out.println("entrei no limbo\n");
            	this.estado = null;
            }
        }
    }

    /**
     * No observations received
     */
    public void GetObservationsClean() {
        
    }

    /**
     * Get Decision
     * @return command string to new decision
     */
    public String GetDecision() {
    	System.out.println("Estado: " + estado);
    	estadoAnterior = null;
    	if(estado == null) {
    		blocked = 0;
    		if(j == 0) {
    			System.out.println("Valor de j" + j);
    			j++;
    			return "virar_direita";
    		}
	    	return "andar";
    	} else {
    		j = 0;
    	}
    	
    	if(estado != Estado.FUGIR) {
    		i = 0;
    	}
    	
    	if(estado != Estado.ENEMY && estado != Estado.HIT) {
    		k = 0;
    	}
    	
    	if(estado != Estado.CAMPERMALDITO) {
    		camper = -1;
    	}
    	
    	if(estado != Estado.BLOCKED) {
    		blocked = 0;
    	}
    	
    	switch(estado) {
    	case PEGAR_OURO:
    		
    		System.out.println("to pegando ouro\n");
    		estado = Estado.CAMPERMALDITO;
    		return "pegar_ouro";
    		
    	case CAMPERMALDITO:
    		camper++;
    		System.out.println("camper "+camper);
    		estadoAnterior = estado;
    		if(camper == 0) {
    			return "atacar";
    		} else if(camper == 1) {
    			camper = -1;
    			return "virar_direita";
    		}
    		
    	case PEGAR_POCAO:
    		estado = null;
    		System.out.println("pocao " + energy);
    		if(energy<50) {
    			return "pegar_powerup";
    		}
    	
    	case PEGAR_ANEL:
    		estado = null;
    		return "pegar_anel";
    		
    	case ENEMY:
    		if(k == 1) {
    			k++;
    			return "virar_direita";
    		} else if(k == 2) {
    			k++;
    			return "atacar";
    		} else if (k == 3) {
    			return "andar";
    		} else {
    			k = 1;
    		}
    		return "atacar";
    	case HIT:
    		estado = estadoAnterior;
    		return "atacar";
    			
    	case FUGIR:
    		String ret = "";
    		
    		if(i == 0) {
    			ret = "andar_re";
    			
    		} else if(i == 1) {
    			ret = "virar_direita";
    			
    		} else {
    			ret = "andar";
    		}
    		
    		i++;
    		return ret;
    		
    	case VOLTAR:
    		estado = null;
    		return "andar_re";
    	
 
    	case BLOCKED:
    		estado = null;
    		blocked++;
    		qtdBlocked++;
    		if(qtdBlocked>10) {
    			java.util.Random rand = new java.util.Random();
        		int  n = rand.nextInt(2);
    	    	switch(n){
    		    	case 0:
    		            return "virar_direita";
    		    	case 1:
    		            return "andar_re";
    	    	}
    			qtdBlocked = 0;
    		}
    		System.out.println("blocked  " +  blocked);
    		if(blocked == 1) {
    			return "andar_re";
    		} else {
    			blocked = 0;
    		}
    	}
    	return "";
    }
}

