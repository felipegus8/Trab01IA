package Model;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Random;

public class Mario {
	
	public int x;
	public int y;
	public Orientation orientation;
	public int energy;
	public int ammo;
	
	public Mario() {
		
		this.x = 0;
		this.y = 11;
		this.orientation = Orientation.up;
		this.energy = 100;
		this.ammo = 5;
	}
	
	protected void orientationManager() {
		
		switch (orientation) {
		case up:
			orientation = Orientation.right;
			break;
		case down:
			orientation = Orientation.left;
			break;
		case left:
			orientation = Orientation.up;
			break;
		case right:
			orientation = Orientation.down;
			break;
		}
	}
	
	@SuppressWarnings("null")
	public void ataca() {
		Random random = new Random();
		int [] enemyCoord = null;
		
		switch (orientation) {
		case up:
			enemyCoord[0] = this.x - 1;
			enemyCoord[1] = this.y;
			break;
			
		case down:
			enemyCoord[0] = this.x + 1;
			enemyCoord[1] = this.y;
			
		case right:
			enemyCoord[0] = this.x;
			enemyCoord[1] = this.y + 1;

		case left:
			enemyCoord[0] = this.x;
			enemyCoord[1] = this.y - 1;
			break;
		}
		
		MapModel.getSingleton().enemies.get(enemyCoord).energy -= random.nextInt(31) + 20;
	}
	
	public void andar() {
		switch (orientation) {
		case up:
			this.x--;
			break;
			
		case down:
			this.x++;
			break;
			
		case right:
			this.y++;
			break;

		case left:
			this.y--;
			break;
		}
	}
	
	public void pegar(int valor) {
		MapModel.getSingleton().matrix.get(this.x)[this.y] = '.';
		this.energy += valor;
	}
	
	public void matar() {
		int [] enemyCoord = null;
		
		switch (orientation) {
		case up:
			enemyCoord[0] = this.x - 1;
			enemyCoord[1] = this.y;
			break;
			
		case down:
			enemyCoord[0] = this.x + 1;
			enemyCoord[1] = this.y;
			
		case right:
			enemyCoord[0] = this.x;
			enemyCoord[1] = this.y + 1;

		case left:
			enemyCoord[0] = this.x;
			enemyCoord[1] = this.y - 1;
			break;
		}
		
		MapModel.getSingleton().enemies.remove(enemyCoord);
		MapModel.getSingleton().matrix.get(enemyCoord[0])[enemyCoord[1]] = '.';
		
	}
	
	

}
