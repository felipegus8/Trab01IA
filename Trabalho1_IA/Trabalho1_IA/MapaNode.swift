//
//  MapaNode.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 02/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation
import SpriteKit

class MapaNode: SKSpriteNode {
    
    private var mapa: MapaModel!
    private var tileNodes: [[TileNode]]!
    private var initialNode: TileNode!
    private var finalNode: TileNode!
    
    init(size: CGSize, mapaModel: MapaModel) {
        super.init(texture: nil, color: UIColor.clear, size: size)

        self.mapa = mapaModel
        self.tileNodes = [[TileNode]]()
        
        let rows = self.mapa.getNumberOfRows()
        let columns = self.mapa.getNumberOfColumns()
        
        for i in 0...rows-1 {
            var rowNode = [TileNode]()
            for j in 0...columns-1 {
                let tileModel = self.mapa.getTilePosition(row: i, col: j)
                let sizeNode = CGSize(width: size.width * 2/CGFloat(columns),
                                      height: size.height * 2/CGFloat(rows))
                let tileNode = TileNode(size: sizeNode, tileModel: tileModel)
                tileNode.anchorPoint = CGPoint.zero
                
                if tileModel == self.mapa.getInitialTile() {
                    tileNode.texture = SKTexture(imageNamed: trilhaLimpaTexture)
                    self.initialNode = tileNode
                }
                if tileModel == self.mapa.getFinalTile() {
                    tileNode.texture = SKTexture(imageNamed: "")
                    self.initialNode = tileNode
                }
                
                let position = CGPoint(x: CGFloat(i)*sizeNode.width, y: CGFloat(j)*sizeNode.width)
                tileNode.position = position
                
                rowNode.append(tileNode)
                self.addChild(tileNode)
                
            }
            self.tileNodes.append(rowNode)
        }
    }
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func getTileSize() -> CGSize {
        return (self.children.last as! SKSpriteNode).size
    }
    
    func model() -> MapaModel {
        return self.mapa
    }
    
    func getInitialNode() -> TileNode {
        return self.initialNode
    }
    
    func getFinalNode() -> TileNode {
        return self.finalNode
    }
    
    func getNodeOnTop(tile: TileNode) -> TileNode? {
        let x = tile.model().row - 1
        let y = tile.model().col
        
        if x < self.tileNodes.count {
            return self.tileNodes[x][y]
        }
        else
        {
            return nil
        }
    }
    
    func getNodeOnBottom(tile: TileNode) -> TileNode? {
        let x = tile.model().row + 1
        let y = tile.model().col
        
        if x > 0 {
            return self.tileNodes[x][y]
        }
        else
        {
            return nil
        }
    }
    
    func getNodeOnRight(tile: TileNode) -> TileNode? {
        let x = tile.model().row
        let y = tile.model().col + 1
        
        if y < self.tileNodes.count {
            return self.tileNodes[x][y]
        }
        else
        {
            return nil
        }
    }
    
    func getNodeOnLeft(tile: TileNode) -> TileNode? {
        let x = tile.model().row
        let y = tile.model().col - 1
        
        if y > 0 {
            return self.tileNodes[x][y]
        }
        else
        {
            return nil
        }
    }
    
    func getTilePosition(tile: TileNode) -> CGPoint {
        let position = CGPoint(x: tile.position.x + tile.frame.size.width/2, y: tile.position.y + tile.frame.size.height/2)
        return position
    }
    
    func getAdjacentNodeDirection(currentTile curr: TileNode, toTile adj: TileNode) -> Direction {
        
        if let node = self.getNodeOnTop(tile: curr) {
            if node == adj { return Direction.Up }
        }
        
        if let node = self.getNodeOnBottom(tile: curr) {
            if node == adj { return Direction.Down }
        }
        
        if let node = self.getNodeOnRight(tile: curr) {
            if node == adj { return Direction.Right }
        }
        
        if let node = self.getNodeOnLeft(tile: curr) {
            if node == adj { return Direction.Left }
        }
        
        return Direction.None
        
    }
    
    func getTileOnPosition(row: Int, col: Int) -> TileNode? {
        if row >= 0 && row < self.tileNodes.count && col >= 0 && col < self.tileNodes[0].count {
            return self.tileNodes[row][col]
        }
        else {
            return nil
        }
    }
    
    func modelArrayToNodeArray(models: [TileModel]) -> [TileNode] {
        var tiles = [TileNode]()
        
        for m in models {
            tiles.append(self.getTileOnPosition(row: m.row, col: m.col)!)
        }
        
        return tiles
    }
    
    func getInitialPoint() -> CGPoint {
        return self.initialNode.position
    }
    
    func getFinalPoint() -> CGPoint {
        return self.finalNode.position
    }

}
