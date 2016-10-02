//
//  LoboNode.swift
//  Trabalho1_IA
//
//  Created by Gabriel Oliveira on 02/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation
import SpriteKit

class LoboNode: SKSpriteNode {
    init(size: CGSize, loboModel: LoboModel) {
        super.init(texture: SKTexture(image: #imageLiteral(resourceName: "loboMau")), color: UIColor.clear, size: size)
    }
    
    required init?(coder aDecoder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }
}
