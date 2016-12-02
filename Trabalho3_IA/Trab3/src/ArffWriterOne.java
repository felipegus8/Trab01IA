import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

public class ArffWriterOne {
	private List<Character> palavras = new ArrayList<Character>() {{
		add('.'); 
		add(',');
		add('!');
		add('?'); 
		add('*');
		add('$');
		add('/'); 
		add(':'); 
		add(';'); 
		add('-'); 
		add('(');
		add(')');
		add('%'); 
		add('@'); 
		add('#');
		add('"');
	}};
	private List<String> words = new ArrayList<String>();	
	
	public void generateArff() {		
		
	}
	
	public void  makeArrayPaths(String path) {
		try {
			FileWriter writer = null;
			File arffOne = new File(path + "/teste2.arff");
			writer = new FileWriter(arffOne);
			
			writer.write("@relation Movies\n");
			writer.write(getAttributes());
			writer.write("@attribute PouN {P,N}\n");
			writer.write("@data\n");
			
			String[] paths = {"/part1/pos", "/part1/neg", "/part2/pos", "/part2/neg"};
			String frase = "";
			int i = 0;
			
			for(String caminho : paths) {
				File folder = new File(path + caminho);
				File[] listOfFiles = folder.listFiles();
				System.out.println(i);
				for(File file: listOfFiles) {
					frase = getStringFromFile(file);
					
					if(i % 2 == 0) {
						frase += "P";
					} else {
						frase += "N";
					}
					
					writer.write(frase + "\n");
				}
				i++;
			}
			System.out.println("ACABOU");
			
		} catch(IOException e) {
			System.out.println(e);
		}	
	}
	
	
	private String getAttributes() {
		
		String attributes = "";
		FileReader reader;
		try {
			reader = new FileReader(new File("/Users/gabrieloliveira/Desktop/PUC/IA/TRAB01/Trabalho3_IA/words.txt"));
			Scanner s = new Scanner(new BufferedReader(reader));
			
			while(s.hasNext()) {
				String next = s.next();
				attributes += "@attribute " + next + " numeric\n";
				words.add(next);
			}

		} catch (Exception e) {
			// TODO: handle exception
		}
		
		return attributes;
		
	}
	
	private String tupleToString(int[] tuple) {
		
		String stringTuple = "";
		
		for(int element: tuple) {
			stringTuple += element + ",";
		}
		return stringTuple;
	}
	
	private String getStringFromFile(File file) {
		String content = "";
		int[] tuple = new int[words.size()];
		Arrays.fill(tuple, 0);
		
//		List<Integer> tuple = new ArrayList<Integer>(); 
		FileReader reader;
		try {
			reader = new FileReader(file);
			Scanner s = new Scanner(new BufferedReader(reader));
			
			while(s.hasNext()) {
				String next = s.next();
				
				for(Character c: palavras) {
					next = next.replace(c, ' ');
					next = next.replace("<br", " ");
					next = next.replace("/><br", " ");
					next = next.replace("/>", " ");
				}
				
				for(int i = 0; i < words.size(); i++) {
					String w = words.get(i);
					if(w.equals(next)) {
						tuple[i] += 1;
					}				
				}
			}
			
			content += tupleToString(tuple);
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			System.out.println(e.getMessage());
		}
		
//		content += ",";
		
		return content;
	}
}