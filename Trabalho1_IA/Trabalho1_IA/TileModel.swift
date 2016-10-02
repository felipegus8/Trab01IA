//
//  TileModel.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 01/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

class TileModel {
    
    var col: Int
    var row: Int
    var type: TileType
    
    init(row: Int, col: Int, TileModelType: TileType) {
        self.col = col
        self.row = row
        self.type = TileModelType
    }
    
    func columnNumber() -> Int {
        return self.col
    }
    
    func rowNumber() -> Int {
        return self.row
    }
}

extension TileModel: Equatable {
    static func ==(lhs: TileModel, rhs: TileModel) -> Bool {
        return lhs.col == rhs.col && lhs.row == rhs.row
    }
}
