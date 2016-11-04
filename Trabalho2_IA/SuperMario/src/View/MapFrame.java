package View;

import java.util.ArrayList;
import java.util.Iterator;
import java.io.*;
import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;
import javax.swing.*;

public class MapFrame extends JFrame {
	
	private JPanel panel;
	private BufferedImage tile;
	
	public MapFrame(ArrayList<Character[]> matrix) {
		panel = new JPanel();
		panel.setLayout(null);
		panel.setBounds(0, 0, 300, 300);
		getContentPane().add(panel);
		setSize(300, 300);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		criaMapa(matrix); 
	}
	
	private void criaMapa(ArrayList<Character[]> matrix) {
		for (Iterator iterator = matrix.iterator(); iterator.hasNext();) {
			Character[] c = (Character[]) iterator.next();
			for (Character character : c) {
				switch (character) {
				case'U':
					criaTerreno("powerUp.jpg");
					break;
				case'.':
					criaTerreno("default.jpg");
					break;
				case'P':
					criaTerreno("hole.jpg");
					break;
				case'T':
					criaTerreno("teletransport.jpg");
					break;
				case'D':
					criaTerreno("bigEnemy.jpg");
					break;
				case'O':
					criaTerreno("gold.jpg");
					break;
				case'd':
					criaTerreno("littleEnemy.jpg");
					break;
				default:
					System.out.println("Caracter desconhecido");
					break;
				}
			}
		}	
	}
	
	private void criaTerreno(String s) {	
		try {
			tile = ImageIO.read(new File("Images/"+s));
		} catch(IOException e) {
			System.out.println(e.getMessage());
			System.exit(1);
		}
	}
	

}
