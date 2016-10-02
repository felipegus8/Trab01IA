//
//  GameScene.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 26/09/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import SpriteKit

class GameScene: SKScene {
//    private var mapNode: MapNode!
    private var mapModel: MapaModel!
    private var loboNode: LoboNode!
    private var loboModel: LoboModel!
    private var pathFinder: AStarPathfinder!
    private var currentNode: TileNode!
    private var pathTiles: [TileModel]!
    private var pathNodes: [TileNode]!
    private var world = SKSpriteNode()
    private let moveDuration = 0.15
    private var totalCost: Double = 0
    private var screenSize: CGRect
    
    override func didMove(to view: SKView) {
        self.backgroundColor = UIColor.clear
        self.screenSize = UIScreen.main.bounds
        self.world.size = CGSize(width: screenSize.width - 200, height: screenSize.width - 200)
        
        self.pathFinder = AStarPathfinder()
        self.pathFinder.dataSource = self
        
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
        // Called before each frame is rendered
    }
}

extension GameScene: AStarPathfinderDataSource {
    func getWalkableAdjacentsTileModels(TileModel: TileModel) -> [TileModel] {
        return self.
    }
}
