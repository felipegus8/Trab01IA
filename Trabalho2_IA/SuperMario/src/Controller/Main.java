package Controller;

import java.io.IOException;
import java.lang.System;
import java.util.Hashtable;
import org.jpl7.*;

import Model.TxtReader;

public class Main {

	public static void main(String[] args) {

//		Query q1 = new Query("consult", new Term[] {new Atom("D:\\teste.pl")});
//
//		System.out.println("consult " + (q1.hasSolution() ? "succeeded" : "failed"));
//
//		Query q2 = new Query("ancestral(X, jose)");
//
//		Hashtable[] solution = (Hashtable[]) q2.allSolutions();
//
//		if (solution != null) {	
//			for (int i = 0; i < solution.length; i++)
//				System.out.println("X = " + solution[i].get("X"));
//		}
		
		try {
			TxtReader.leArquivo();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	
	}
}
