//
//  TileNode.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 01/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation
import SpriteKit

class TileNode: SKSpriteNode {
    
    private var tile: TileModel!
    
    init(size: CGSize, tileModel: TileModel) {
        var texture: SKTexture!
        
        self.tile = tileModel
        switch self.tile.type {
        case .FlorestaDensa:
            texture = SKTexture(imageNamed: florestaDensaTexture)
        case .TrilhaGalhos:
            texture = SKTexture(imageNamed: trilhaComGalhosTexture)
        case .TrilhaLimpa:
            texture = SKTexture(imageNamed: trilhaLimpaTexture)
        case .LoboMau:
            texture = SKTexture(imageNamed: loboMauTexture)
        }
        super.init(texture: texture, color: UIColor.clear, size: size)
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func model() -> TileModel {
        return tile
    }
    
    func paintNode(node: TileNode) {
        node.color = UIColor.red
        node.texture = nil
    }
    
}
