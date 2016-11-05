package Model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Scanner;

public class TxtReader {
	
	public static ArrayList<Character[]> leArquivo() throws IOException {
		try {
			File file = new File("Resources/mapa.txt");
			FileReader reader = new FileReader(file);
			Character[] arrayChar;
			ArrayList<Character[]> matrix =  new ArrayList<Character[]>();
			Scanner s = new Scanner(new BufferedReader(reader));
			MapModel mapa = MapModel.getSingleton();
			String next;
			int j = 0;
			while(s.hasNext()) {
				next = s.next();
				arrayChar = new Character[next.length()];
				for(int i = 0; i < next.length(); i++) {
					char c = next.charAt(i);
					arrayChar[i] = c;
					if(c == 'T' || c == 'd' || c == 'D') mapa.addEnemy(c, j, i);
				}
				matrix.add(arrayChar);
				j++;
			}
			s.close();
			
			return matrix;
			
		} catch(FileNotFoundException e) {
			System.out.println(e);
		}
		
		return null;
		
	}
}
