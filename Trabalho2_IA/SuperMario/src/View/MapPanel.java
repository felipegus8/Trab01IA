package View;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

public class MapPanel extends JPanel {
	
	private BufferedImage tile;
	private ArrayList<Character[]> matrix;
	private int marioX;
	private int marioY;
	
	public MapPanel() {
		setBounds(0, 0, 300, 300);
		setLayout(null);
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		criaMapa(g);
		criaTerreno("mario.png", marioX, marioY, g);
		
		
	}
	
	private void criaMapa(Graphics g) {
		int i = 0;
		int j = 0;
		
		for (Iterator iterator = matrix.iterator(); iterator.hasNext();) {
			Character[] c = (Character[]) iterator.next();
				
			for (Character character : c) {
				System.out.print(character);
				switch (character) {
				case'U':
					criaTerreno("powerUp.png", j, i, g);
//					System.out.print(character);
					break;
				case'.':
					criaTerreno("default.png", j, i, g);
//					System.out.print(character);
					break;
				case'P':
					criaTerreno("hole.png", j, i, g);
//					System.out.print(character);
					break;
				case'T':
					criaTerreno("teletransport.png", j, i, g);
//					System.out.print(character);
					break;
				case'D':
					criaTerreno("bigEnemy.png", j, i, g);
//					System.out.print(character);
					break;
				case'O':
					criaTerreno("gold.png", j, i, g);
//					System.out.print(character);
					break;
				case'd':
					criaTerreno("littleEnemy.png", j, i, g);
//					System.out.print(character);
					break;
				default:
					System.out.println("Caracter desconhecido");
					break;
				}
				
				j++;
			}
			
			System.out.println();
			j = 0;
			i++;
		}	
	}
	
	private void criaTerreno(String s, int x, int y, Graphics g) {	
		try {
			tile = ImageIO.read(new File("Images/"+s));
			g.drawImage(tile, x * 25, y * 25, null);
//			System.out.print(x * 25+" ");
//			System.out.print(y * 25+" ");
			
		} catch(IOException e) {
			System.out.println(e.getMessage());
			System.exit(1);
		}
	}
	
	public void setInfo(ArrayList<Character[]> matrix, int x, int y) {
		this.matrix = matrix;
		this.marioX = x;
		this.marioY = y;
	}
	
	

}
