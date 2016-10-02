//
//  CandyDistributor .swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 02/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

class CandyDistributor {
    
//    Primeira Casa - Peso 150 - 3 de amora - 33.3 seg
//    Segunda Casa -Peso 140 - 2 de amora e 1 de marshmallow - 31.81 seg
//    Terceira Casa -Peso 130- 3 de marshmallow - 30.95 seg
//    Quarta Casa -Peso 120- 1 de marshmallow e 2 de chocolate - 30 seg
//    Quinta Casa -Peso 110- 2 de chocolate - 42.30 seg
//    Sexta Casa -Peso 100- 1 de chocolate e 1 de brigadeiro - 40 seg
//    Sétima Casa -Peso 95- 2 de brigadeiro - 39.58 seg
//    Oitava Casa -Peso 90- 2 de brigadeiro - 37.5 seg
//    Nona Casa -Peso 85- 2 de coco - 38.63 seg
//    Décima Casa -Peso 80- 2 de coco - 36.36 seg
//    Chega no final com 1 doce de coco.

    
    static private let combinatorial: [[Double]] = [[3,0,0,0,0], [2,1,0,0,0],
                                                    [0,3,0,0,0], [0,1,2,0,0],
                                                    [0,0,2,0,0], [0,0,1,1,0],
                                                    [0,0,0,2,0], [0,0,0,2,0],
                                                    [0,0,0,0,2], [0,0,0,0,2]]
    
    static func wolfCost(id: Int, loboArray: [LoboModel],
                         feedWolf: (CandyType, Int) -> () ) -> Double {
        
        var candiesGiven = combinatorial[id]
        var appreciationSum: Double = 0.0
        
        for i in 0...4 {
            
            switch i {
            case 0:
                appreciationSum += candiesGiven[i] * CandyType.TortaDeAmoras.rawValue
                feedWolf(CandyType.TortaDeAmoras, Int(candiesGiven[i]))
            case 1:
                appreciationSum += candiesGiven[i] * CandyType.CupcakesDeMarshmallow.rawValue
                feedWolf(CandyType.CupcakesDeMarshmallow, Int(candiesGiven[i]))
            case 2:
                appreciationSum += candiesGiven[i] * CandyType.BoloDeChocolate.rawValue
                feedWolf(CandyType.BoloDeChocolate, Int(candiesGiven[i]))
            case 3:
                appreciationSum += candiesGiven[i] * CandyType.Brigadeiro.rawValue
                feedWolf(CandyType.Brigadeiro, Int(candiesGiven[i]))
            case 4:
                appreciationSum += candiesGiven[i] * CandyType.DoceDeCoco.rawValue
                feedWolf(CandyType.DoceDeCoco, Int(candiesGiven[i]))
            default:
                print("error")
                break
            }
            
        }
        
        for lobo in loboArray {
            if lobo.getID() == id {
                let dificuldade = lobo.getDificulty()
                return dificuldade/appreciationSum
            }
        }
        
        return -1.0
        
    }
}
