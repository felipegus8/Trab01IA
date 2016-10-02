//
//  FileReader.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 26/09/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

class FileReader {
    
    class func readFile() -> ([[Int]]?,(Int,Int),(Int,Int), [LoboModel])? {
        
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
            
            if let content = (data) {
                levelMatrix = content.components(separatedBy: NSCharacterSet.newlines)
                print(levelMatrix)
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
    
    private static func matrixGen(data: Array<String>) -> ([[Int]]?,(Int,Int),(Int,Int), [LoboModel]) {
        var array = [Int]()
        var origem: (Int, Int) = (0, 0)
        var fim: (Int, Int) = (0, 0)
        var matrix = [[Int]]()
        var y: Int = 0
        var x: Int = 0
        
        let dificuldades: [Int] = [150, 140, 130, 120, 110, 100, 95, 90, 85, 80]
        var baseLidas: Int = 0
        var arrayLobo: [LoboModel] = []
        
        print(data.count)
        
        for valor in data {
            if valor != "" {
                print("x: \(x)")
                y = 0
                for char in valor.characters {
                    //                let x = valor.index(of: valor)!
                    
                    print("\(x) \(y)")
                    
                    switch char {
                    case "D":
                        // floresta densa
                        array.append(200)
                    case ".":
                        // trilha limpa
                        array.append(1)
                    case "G":
                        // trilha com galhos
                        array.append(5)
                    case "F":
                        fim = (x, y)
                        array.append(1)
                    case "I":
                        
                        origem = (x, y)
                        
                        array.append(1)
                    case "C":
                        let lobo = LoboModel(x: x, y: y, dificulty: dificuldades[baseLidas])
                        baseLidas += 1
                        arrayLobo.append(lobo)
                        array.append(1)
                        
                    default:
                        print("*** Caracter invalido: \(char) ***")
                        break
                    }
                    
                    y += 1
                    
                }
                
                if x == 1{
                    print("x = 1")
                }
                
                x += 1
                
                if !array.isEmpty {
                    matrix.append(array)
                    array.removeAll()
                }
            }
        }
        
        print(matrix[0].count)
        
        return (matrix, origem, fim, arrayLobo)
    }
    
}
