package Model;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
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
			try {
				generateMapPL(matrix);
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return matrix;
			
		} catch(FileNotFoundException e) {
			System.out.println(e);
		}
		
		return null;	
	}
	
	private static void generateMapPL(ArrayList<Character[]> matrix) throws Exception {
		
		Map<String, StringBuilder> rules = new HashMap<String, StringBuilder>();
		int i = 0;
		int j = 0;
		int jString = 0;
		int iString = 0;
		System.out.println("ENTREI *****");
		for (Iterator iterator = matrix.iterator(); iterator.hasNext();) {
			Character[] c = (Character[]) iterator.next();	
			for (Character character : c) {
				String cellRule = null;
				iString = i + 1;
				jString = j + 1;
				
				switch (character) {
				case'U':
					cellRule = "powerUp(";
					break;
				case'.':
					if(i == 0 && j == 11) {
						StringBuilder start = new StringBuilder();
						start.append("inicio(" + jString + "," + iString + ").\n");
						rules.put("inicio", start);
						cellRule = "inicio(";
					} else {
						cellRule = "vazia(";
					}
					break;
				case'P':
					cellRule = "poco(";
					break;
				case'T':
					cellRule = "teletransporte(";
					break;
				case'D':
					cellRule = "inimigo(";
					break;
				case'O':
					cellRule = "ouro(";
					break;
				case'd':
					cellRule = "inimigo(";
					break;
				default:
					System.out.println("Caracter desconhecido");
					break;
				}
				j++;
				if(cellRule == null) {
					throw new Exception("Error - invalid map rule");
				}
				
				if (character == 'D') {
					if(rules.get("inimigo") == null) rules.put("inimigo", new StringBuilder());
					rules.get("inimigo").append(cellRule + "50,100," + jString + "," + iString + ").\n");
				} else if (character == 'd') {
					if(rules.get("inimigo") == null) rules.put("inimigo", new StringBuilder());
					rules.get("inimigo").append(cellRule + "20,100," + jString + "," + iString + ").\n");
				}
				else {
					if(rules.get(cellRule) == null) rules.put(cellRule, new StringBuilder());
					rules.get(cellRule).append(cellRule + jString + "," + iString + ").\n");
				}
			}
			i++;
			j = 0;
		}	
		
		parede(rules);
		
		StringBuilder plMapBuilder = new StringBuilder();
//		plMapBuilder.append("/* AUTO-GENERATED MAP FILE - CONTENT WILL BE REMOVED AFTER NEXT SEARCH */\n");
		rules.forEach((key, value) -> plMapBuilder.append(value.toString()));
		
		try(PrintWriter out = new PrintWriter("Resources/map.pl")){
			System.out.println("TO PRINTANDO ****");
			System.out.println(plMapBuilder);
		    out.println( plMapBuilder.toString() );
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	private static void parede(Map<String, StringBuilder> rules) {
			for (int i = 1; i < 13; i++) {
				if(rules.get("parede") == null) rules.put("parede", new StringBuilder());
				rules.get("parede").append("parede(" + 0 + "," + i + ").\n");
				rules.get("parede").append("parede(" + 13 + "," + i + ").\n");
				rules.get("parede").append("parede(" + i + "," + 0 + ").\n");
				rules.get("parede").append("parede(" + i + "," + 13 + ").\n");
			}
	}
	
	
}
