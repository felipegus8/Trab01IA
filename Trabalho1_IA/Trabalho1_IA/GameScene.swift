//
//  GameScene.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 26/09/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import SpriteKit

class GameScene: SKScene {
    var mapNode: MapaNode!
    private var mapModel: MapaModel!
//    private var loboNode: LoboNode!
//    private var loboModel: LoboModel!
    var redHoodModel: RedHoodModel!
    private var redHoodNode: RedHoodNode!
    private var pathFinder: AStarPathfinder!
    private var currentNode: TileNode!
    private var pathTiles: [TileModel]!
    private var pathNodes: [TileNode]!
    private var world = SKSpriteNode()
    private var costLabel: SKLabelNode!
    private var candiesLabel: SKLabelNode!
    private let moveDuration = 0.15
    private var totalCost: Double = 0
    private var screenSize: CGRect!
    let (matrix, initial, final, clareiras) = FileReader.readFile()!
    
    override func didMove(to view: SKView) {
        self.backgroundColor = UIColor.clear
        self.screenSize = UIScreen.main.bounds
        self.world.size = CGSize(width: screenSize.width - 200, height: screenSize.width - 200)
        
        self.mapModel = MapaModel(costs: matrix!, initialTile: (36, 4), finalTile: final)
        
        self.mapNode = MapaNode(size: self.world.size, mapaModel: self.mapModel)
        self.mapNode.anchorPoint = CGPoint.zero
        self.currentNode = self.mapNode.getInitialNode()
        
        self.costLabel = SKLabelNode(text: "Cost: \(totalCost)")
        self.costLabel.position = CGPoint(x: 650, y: 10)
        self.costLabel.fontSize = 15
        self.costLabel.fontName = "Helvetica-Bold"
        self.costLabel.fontColor = UIColor.white
        self.costLabel.horizontalAlignmentMode = .left
        
        let tileSize = self.mapNode.getTileSize()
        
        for c in self.clareiras {
            let clareiraNode = LoboNode(size: tileSize, loboModel: c)
            clareiraNode.anchorPoint = CGPoint(x: 0.5, y: 0.5)
            clareiraNode.position = self.mapNode.getTilePosition(tile: self.mapNode.getTileOnPosition(row: c.getCoordX(), col: c.getCoordY())!)
            self.mapNode.addChild(clareiraNode)
        }
        
        self.redHoodModel = RedHoodModel()
        self.redHoodNode = RedHoodNode(size: tileSize, redHoodModel: redHoodModel)
        self.redHoodNode.position = self.mapNode.getInitialPoint()
        
        var candies = redHoodModel.getCandies()
        
        self.candiesLabel = SKLabelNode(text: "Doce 1: \(candies[0]), Doce 2: \(candies[1]), Doce 3: \(candies[2]), Doce 4: \(candies[3]), Doce 5: \(candies[4])")
        self.candiesLabel.position = CGPoint(x: 50, y: 10)
        self.candiesLabel.fontSize = 15
        self.candiesLabel.fontName = "Helvetica-Bold"
        self.candiesLabel.fontColor = UIColor.white
        self.candiesLabel.horizontalAlignmentMode = .left
        
        self.addChild(self.world)
        self.addChild(self.costLabel)
        self.addChild(self.candiesLabel)
        
        world.addChild(self.mapNode)
        mapNode.addChild(self.redHoodNode)
        
        self.pathFinder = AStarPathfinder()
        self.pathFinder.dataSource = self
        
        self.pathTiles = self.pathFinder.shortestPathFromTileModel(fromTileModel: self.mapModel.getInitialTile(), toTileModel: self.mapModel.getFinalTile())
        self.pathNodes = self.mapNode.modelArrayToNodeArray(models: self.pathTiles)
    
        self.movement()
        
    }
    
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?) {
    }
    
    override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent?) {
    }
    
    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
    }
    
    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent?) {
    }
    
    
    override func update(_ currentTime: TimeInterval) {
        costLabel.text = "Cost: \(totalCost.rounded())"
        
        var candies = redHoodModel.getCandies()
        self.candiesLabel.text =  "Doce 1: \(candies[0]), Doce 2: \(candies[1]), Doce 3: \(candies[2]), Doce 4: \(candies[3]), Doce 5: \(candies[4])"
        
        
    }
    
    override func didSimulatePhysics() {
        self.world.position = CGPoint(x: -(self.redHoodNode.position.x-(self.size.width/2)), y: -(self.redHoodNode.position.y-(self.size.height/2)))
    }
    
    func moveUp(completion: (() ->())?) {
        if let node = self.mapNode.getNodeOnTop(tile: self.currentNode) {
            let point = self.mapNode.getTilePosition(tile: node)
            
            let moving = SKAction.move(to: point, duration: moveDuration)
            self.redHoodNode.rotate(direction: .Up)
            
            let paintedNode = self.mapNode.getNodeOnTop(tile: self.currentNode)
            paintedNode?.paintNode(node: paintedNode!)
            
            self.redHoodNode.run(moving, completion: {
                self.currentNode = node
                
                completion?()
            })
        }
    }
    
    func moveDown(completion: (() ->())?) {
        if let node = self.mapNode.getNodeOnBottom(tile: self.currentNode) {
            let point = self.mapNode.getTilePosition(tile: node)
            
            let moving = SKAction.move(to: point, duration: moveDuration)
            self.redHoodNode.rotate(direction: .Down)
            
            let paintedNode = self.mapNode.getNodeOnBottom(tile: self.currentNode)
            paintedNode?.paintNode(node: paintedNode!)
            
            self.redHoodNode.run(moving, completion: {
                self.currentNode = node
                
                completion?()
            })
        }
    }
    
    func moveLeft(completion: (() ->())?) {
        if let node = self.mapNode.getNodeOnLeft(tile: self.currentNode) {
            let point = self.mapNode.getTilePosition(tile: node)
            
            let moving = SKAction.move(to: point, duration: moveDuration)
            self.redHoodNode.rotate(direction: .Left)
            
            let paintedNode = self.mapNode.getNodeOnLeft(tile: self.currentNode)
            paintedNode?.paintNode(node: paintedNode!)
            
            self.redHoodNode.run(moving, completion: {
                self.currentNode = node
                
                completion?()
            })
        }
    }
    
    func moveRight(completion: (() ->())?) {
        if let node = self.mapNode.getNodeOnRight(tile: self.currentNode) {
            let point = self.mapNode.getTilePosition(tile: node)
            
            let moving = SKAction.move(to: point, duration: moveDuration)
            self.redHoodNode.rotate(direction: .Right)
            
            let paintedNode = self.mapNode.getNodeOnRight(tile: self.currentNode)
            paintedNode?.paintNode(node: paintedNode!)
            
            self.redHoodNode.run(moving, completion: {
                self.currentNode = node
                
                completion?()
            })
        }
    }
    
    func movement() {
        if self.pathNodes.count > 0 {
            let node = self.pathNodes.removeFirst()
            let direction = self.mapNode.getAdjacentNodeDirection(currentTile: self.currentNode, toTile: node)
            
            if let clareira = self.getWolfOnTile(x: node.model().row, y: node.model().col) {
                //TO DO
                self.totalCost += CandyDistributor.wolfCost(id: clareira.getID(), loboArray: clareiras, feedWolf: self.redHoodModel.feedWolf)
            } else {
                self.totalCost += Double(node.model().type.rawValue)
            }
            
            switch direction {
            case .Up:
                self.moveUp(completion: self.movement)
                
            case .Down:
                self.moveDown(completion: self.movement)
                
            case .Left:
                self.moveLeft(completion: self.movement)
                
            case .Right:
                self.moveRight(completion: self.movement)
                
            case .None:
                break;
            }
        }
    }
    
    func getWolfOnTile(x : Int, y: Int) -> LoboModel? {
        for c in self.clareiras {
            if c.getCoordX() == x && c.getCoordY() == y {
                return c
            }
        }
        
        return nil
    }
}

extension GameScene: AStarPathfinderDataSource {
    func getWalkableAdjacentsTileModels(tileModel: TileModel) -> [TileModel] {
        return self.mapNode.model().getAdjacents(tile: tileModel)
    }
    
    func getTileModelCost(fromTileModel: TileModel, toAdjacentTileModel toTileModel: TileModel) -> Int {
        for clareira in self.clareiras {
            if clareira.getCoordX() == toTileModel.row && clareira.getCoordY() == toTileModel.col {
                //TO DO
                
                let ret = Int(CandyDistributor.wolfCost(id: clareira.getID(), loboArray: clareiras))
                
                return ret
            }
        }
        
        return toTileModel.type.rawValue
    }
}
