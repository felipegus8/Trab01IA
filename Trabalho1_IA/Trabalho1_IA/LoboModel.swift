//
//  LoboTileModel.swift
//  Trabalho1_IA
//
//  Created by Gabriel Oliveira on 01/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

class LoboModel {
    private var coordX: Int!
    private var coordY: Int!
    private var dificulty: Int!
    
    init(x: Int, y: Int, dificulty: Int) {
        self.coordX = x
        self.coordY = y
        self.dificulty = dificulty
    }
    
    func getCoordX() -> Int {
        return self.coordX
    }
    
    func getCoordY() -> Int {
        return self.coordY
    }
    
    func getDificulty() -> Int {
        return self.dificulty
    }
}
