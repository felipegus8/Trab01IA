package View;

import java.util.ArrayList;
import java.util.Iterator;
import java.io.*;
import java.awt.image.BufferedImage;

import javax.imageio.ImageIO;
import javax.swing.*;

public class MapFrame extends JFrame {
	
	private MapPanel panel;

	public MapFrame(ArrayList<Character[]> matrix, int x, int y) {
		panel = new MapPanel();
		getContentPane().add(panel);
		setSize(300, 300);
		setDefaultCloseOperation(EXIT_ON_CLOSE);
		setTitle("Mario");
		setVisible(true);
		panel.setInfo(matrix, x, y); 
	}
	
	public void repaintPanel(ArrayList<Character[]> matrix, int x, int y) {
		panel.setInfo(matrix, x, y);
		panel.validate();
		panel.repaint();
	}
	
	
	
}
