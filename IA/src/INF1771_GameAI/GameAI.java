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
    int i = 0;

    
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
        	
            if(s.equals("blocked")){
            	this.estado = Estado.VOLTAR;
            } else if(s.equals("steps")){
            	this.estado = Estado.ATIRAR;
            } else if(s.equals("breeze")){
            	this.estado = Estado.VOLTAR;
            } else if(s.equals("flash")){
            	this.estado = Estado.VOLTAR;
            } else if(s.equals("blueLight")){
            	this.estado = Estado.PEGAR_POCAO;
            } else if(s.equals("redLight")){
            	this.estado = Estado.PEGAR_OURO;
            } else if(s.equals("greenLight")){
            	this.estado = null;
            } else if(s.equals("weakLight")){
            	this.estado = null;
            } else if(s.equals("hit")) {
            	this.estado = Estado.FUGIR;
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
    	if(estado == null) {
    		return "andar";
    	}
    	
    	if(estado != Estado.FUGIR) {
    		i = 0;
    	}
    	
    	switch(estado) {
    	case PEGAR_OURO:
    		return "pegar_ouro";
    		
    	case PEGAR_POCAO:
    		return "pegar_powerup";
    	
    	case PEGAR_ANEL:
    		return "pegar_anel";
    		
    	case ATIRAR:
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
    		return "andar_re";
    	}
    	
    	return "";
    }
}
