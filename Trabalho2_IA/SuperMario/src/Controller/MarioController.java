package Controller;


import javax.swing.Timer;

import Model.*;
import View.MapFrame;

import java.awt.event.ActionListener;
import java.io.IOException;
import java.util.ArrayList;
import java.awt.event.ActionEvent;

public class MarioController {
	
	private Timer timer;
	private ActionListener timerAction;
	private ConsultProlog consulter;
	private MapFrame view;
	private Mario mario;
	
	public MarioController() {
		
//		consulter = new ConsultProlog();
		mario = new Mario();
		try {
			ArrayList<Character[]> matrix = TxtReader.leArquivo();
			view = new MapFrame(matrix, mario.x, mario.y);
			MapModel.getSingleton().setMatrix(matrix);
		} catch (IOException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		consulter = new ConsultProlog();
		timerAction = new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent e) {
				// TODO Auto-generated method stub
				consulter.getNextMove(mario);
				view.repaintPanel(MapModel.getSingleton().getMatrix(), mario.x, mario.y);
			}
		};
		
		timer = new Timer(500, timerAction);
		timer.start();
	}
	
}
