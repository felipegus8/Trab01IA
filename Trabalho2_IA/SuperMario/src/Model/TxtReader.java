package Model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;
import java.util.Scanner;

public class TxtReader {
	
	public static void leArquivo() throws IOException {
		try {
			File file = new File("Resources/mapa.txt");
			FileReader reader = new FileReader(file);
			Character[] arrayChar;
			Scanner s = new Scanner(new BufferedReader(reader));
			String next;
			Map map = Map.getSingleton();
			
			while(s.hasNext()) {
				next = s.next();
				arrayChar = new Character[next.length()];
				for(int i = 0; i < next.length(); i++) {
					char c = next.charAt(i);
					arrayChar[i] = c;
				}
				map.matrix.add(arrayChar);
			}
			
			s.close();		
		} catch(FileNotFoundException e) {
			System.out.println(e);
		}
		
		
	}
}
