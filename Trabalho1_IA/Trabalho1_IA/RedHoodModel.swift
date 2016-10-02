//
//  RedHoodModel.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 01/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

class RedHoodModel {
    
    private var position: (Int, Int)!
    private var candies: [Int]!
    
    init(originX: Int, originY: Int) {
        position = (originX, originY)
        candies = [Int]()
        for _ in 0...4 {
            candies.append(5)
        }
    }
    
    func feedWolf(candyType: CandyType) {
        switch candyType {
        case .TortaDeAmoras:
            candies[0] -= 1
        case .CupcakesDeMarshmallow:
            candies[1] -= 1
        case .BoloDeChocolate:
            candies[2] -= 1
        case .Brigadeiro:
            candies[3] -= 1
        case .DoceDeCoco:
            candies[4] -= 1
        }
    }
    
    func setPosition(x: Int,y: Int) {
        position = (x,y)
    }
    
    func getPosition() -> (Int, Int) {
        return position
    }
    
    func getCandies() -> [Int] {
        return candies
    }
    
}
