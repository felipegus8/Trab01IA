//
//  MapaModel.swift
//  Trabalho1_IA
//
//  Created by Gabriel Oliveira on 01/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

class MapaModel {
    private var lines: Int!
    private var columns: Int!
    private var matrix = [[TileModel]]()
    private var initialTile: TileModel!
    private var finalTile: TileModel!
    
    init(costs: [[Int]], initialTile: (Int, Int), finalTile: (Int, Int)) {
        self.lines = costs.count
        self.columns = costs[0].count
        
        for i in 0..<lines {
            var values = [TileModel]()
            
            for j in 0..<columns {
                let tile = TileModel(row: i, col: j, TileModelType: TileType(rawValue: costs[i][j])!)
                values.append(tile)
                
                if i == initialTile.0 && j == initialTile.1 {
                    print("InitialTile")
                    self.initialTile = tile
                    
                } else if i == finalTile.0 && j == finalTile.1 {
                    self.finalTile = tile
                    
                }   
            }
            
            matrix.append(values)
        }
    }
    
    func getTileTop(tile: TileModel) -> TileModel? {
        let y = tile.row - 1
        
        if(y >= 0)
        {
            return self.matrix[y][tile.col]
        }
        
        return nil
    }
    
    func getTileBottom(tile: TileModel) -> TileModel? {
        let y = tile.row + 1
        
        if(y < self.lines)
        {
            return self.matrix[y][tile.col]
        }
        
        return nil
    }
    
    func getTileRight(tile: TileModel) -> TileModel? {
        let x = tile.col + 1
        
        if(x < self.columns)
        {
            return self.matrix[tile.row][x]
        }
        
        return nil
    }
    
    func getTileLeft(tile: TileModel) -> TileModel? {
        let x = tile.col - 1
        
        if(x >= 0)
        {
            return self.matrix[tile.row][x]
        }
        
        return nil
    }
    
    func getAdjacents(tile: TileModel) -> [TileModel] {
        var arrayTiles: [TileModel] = []
        
        if let top = getTileTop(tile: tile) {
            arrayTiles.append(top)
        }
        
        if let bottom = getTileBottom(tile: tile) {
            arrayTiles.append(bottom)
        }
        
        if let left = getTileLeft(tile: tile) {
            arrayTiles.append(left)
        }
        
        if let right = getTileRight(tile: tile) {
            arrayTiles.append(right)
        }
        
        return arrayTiles
    }
    
    func getTilePosition(row: Int, col: Int) -> TileModel {
        return self.matrix[row][col]
    }
    
    func getInitialTile() -> TileModel {
        return self.initialTile
    }
    
    func getFinalTile() -> TileModel {
        return self.finalTile
    }
    
    func getNumberOfRows() -> Int {
        return self.matrix.count
    }
    
    func getNumberOfColumns() -> Int {
        return self.matrix[0].count
    }
}
