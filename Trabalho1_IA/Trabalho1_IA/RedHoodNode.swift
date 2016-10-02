//
//  RedHoodNode.swift
//  Trabalho1_IA
//
//  Created by Gabriel Oliveira on 02/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation
import SpriteKit

class RedHoodNode: SKSpriteNode {
    private var redHoodModel: RedHoodModel!
    
    init(size: CGSize, redHoodModel: RedHoodModel) {
        self.redHoodModel = redHoodModel
        
        super.init(texture: SKTexture(image: #imageLiteral(resourceName: "chapeuzinhoCostas")), color: UIColor.clear, size: size)
        self.anchorPoint = CGPoint(x: 0.5, y: 0.5)

    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
    
    func rotate(direction: Direction) {
        switch direction {
        case .Up:
            self.texture = SKTexture(image: #imageLiteral(resourceName: "chapeuzinhoCostas"))
            
        case .Down:
            self.texture = SKTexture(image: #imageLiteral(resourceName: "chapeuzinhoFrente"))
            
        case .Left:
            self.texture = SKTexture(image: #imageLiteral(resourceName: "chapeuzinhoEsquerda"))
            
        case .Right:
            self.texture = SKTexture(image: #imageLiteral(resourceName: "chapeuzinhoDireita"))
            
        }
    }
    
    func model() -> RedHoodModel {
        return self.redHoodModel
    }
}
