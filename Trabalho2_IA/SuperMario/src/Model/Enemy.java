package Model;

public class Enemy {
	
	protected int energy;
	private EnemyType type;
	private int x;
	private int y;
	
	public Enemy(EnemyType type, int x, int y) {
		this.type = type;
		this.energy = 100;
		this.x = x;
		this.y = y;
	}
	
	protected int attack() {
		
		switch (type) {
		case dano_Maior:
			return 50;
		case dano_Menor:
			return 20;
		case Teletransporte:
			return 0;
		}
		
		return -1;
		
	}
}
