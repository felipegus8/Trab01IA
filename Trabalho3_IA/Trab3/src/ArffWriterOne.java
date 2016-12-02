import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ArffWriterOne {
	private List<String> palavras = new ArrayList<String>() {{
		add("."); 
		add(",");
		add("!");
		add("?"); 
		add("*");
		add("$");
		add("<br"); 
		add("/><br"); 
		add("/>"); 
		add("/"); 
		add(":"); 
		add(";"); 
		add("-"); 
		add("(");
		add(")");
		add("%"); 
		add("@"); 
		add("#");

	}};
	
	public void generateArff() {		
		
	}
	
	public void  makeArrayPaths(String path) {
		try {
			FileWriter writer = null;
			File arffOne = new File(path + "/teste.arff");
			writer = new FileWriter(arffOne);
			
			writer.write("@relation Movies/n");
			writer.write("@attribute frase string/n");
			writer.write("@attribute PouN {P,N}/n");
			writer.write("@data/n");
			
			String[] paths = {"/part1/pos", "/part1/neg", "/part2/pos", "/part2/neg"};
			String frase = "";
			int i = 0;
			
			for(String caminho : paths) {
				File folder = new File(path + caminho);
				File[] listOfFiles = folder.listFiles();
				
				for(File file: listOfFiles) {
					frase = getStringFromFile(file);
					
					if(i % 2 == 0) {
						frase += "P";
					} else {
						frase += "N";
					}
					
					writer.write(frase + "\n");
				}
			}
			
		} catch(IOException e) {
			System.out.println(e);
		}	
	}
	
	private String getStringFromFile(File file) {
		String content = "\"";
		
		FileReader reader;
		try {
			reader = new FileReader(file);
			Scanner s = new Scanner(new BufferedReader(reader));
			
			while(s.hasNext()) {
				String next = s.next();
				
				if(!palavras.contains(next)) {
					content += next + " ";
				}
			}
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		content += "\", ";
		
		return content;
	}
}