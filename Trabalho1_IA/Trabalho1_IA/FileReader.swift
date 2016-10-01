//
//  FileReader.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 26/09/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

class FileReader {
    
    func readFile() -> ([[Int?]],(Int,Int),(Int,Int))? {
        
        let path = "Mapa"
        var levelMatrix:Array<String>
        if let path = Bundle.main.path(forResource: path, ofType: "txt")
        {
            var data: String!
            
            do {
                data = try String(contentsOfFile: path, encoding: String.Encoding.utf8)
            } catch {
                print("Error reading .txt file.")
            }
            
            if let content = (data){
                levelMatrix = content.components(separatedBy: NSCharacterSet.newlines)
                print("Read data")
                return matrixGen(data: levelMatrix)
            }
            else {
                print("Couldnt read data.")
            }
        }
        else {
            print("Couldnt find txt with path: \(path).")
            return nil
        }
        
        return nil
    }
    
    func matrixGen(data: Array<String>) -> ([[Int?]],(Int,Int),(Int,Int)) {
        var array: [Int?] = []
        var origem, fim: (Int, Int)
        origem = (0,0)
        fim = (0,0)
        var matrix: [[Int?]] = []
        var column = 0
        for valor in data {
            for char in valor.characters {
                
                switch char {
                case "D":
                    // floresta densa
                    array.append(200)
                case ".":
                    // trilha limpa
                    array.append(1)
                case "G":
                    // trilha com galhos
                    array.append(200)
                case "F":
                    fim = (data.index(of: valor)!, column)
                    array.append(1)
                case "I":
                    origem = (data.index(of: valor)!, column)
                    array.append(1)
//                case "C":
//                  TO-DO
//                    
                default:
                    print("*** Caracter invalido: \(char) ***")
                    break
                }
                
                column += 1
            }
            if !array.isEmpty {
                matrix.append(array)
                array.removeAll()
            }
        }
        
        return (matrix,origem,fim)
    }
    
}
