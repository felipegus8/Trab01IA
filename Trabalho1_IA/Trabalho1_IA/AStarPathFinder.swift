//
//  AStarPathFinder.swift
//  Trabalho1_IA
//
//  Created by Guilherme Marques on 01/10/16.
//  Copyright © 2016 Guilherme Marques. All rights reserved.
//

import Foundation

// A single step on the computed path; used by the A* pathfinding algorithm
private class ShortestPathStep: Hashable {
    let position: TileModel
    var parent: ShortestPathStep?
    
    //Custo de movimentacao do tile : tempo em minutos
    var gScore = 0
    
    //Distancia do tile para o tile final
    var hScore = 0
    
    //Soma dos dois custos
    var fScore: Int {
        return gScore + hScore
    }
    
    var hashValue: Int {
        return position.col.hashValue + position.row.hashValue
    }
    
    init(position: TileModel) {
        self.position = position
    }
    
    func setParent(parent: ShortestPathStep, withMoveCost moveCost: Int) {
        // The G score is equal to the parent G score + the cost to move from the parent to it
        self.parent = parent
        self.gScore = parent.gScore + moveCost
    }
}

private func ==(lhs: ShortestPathStep, rhs: ShortestPathStep) -> Bool {
    return lhs.position == rhs.position
}

protocol AStarPathfinderDataSource : NSObjectProtocol {
    func getWalkableAdjacentsTileModels(tileModel: TileModel) -> [TileModel]
    
    func getTileModelCost(fromTileModel: TileModel, toAdjacentTileModel toTileModel: TileModel) -> Int
}

// Algoritimo A*
class AStarPathfinder {
    private var steps : Int = 0
    
    private var cost : Float = 0
    
    weak var dataSource: AStarPathfinderDataSource?
    
    private func insertStep(step: ShortestPathStep, inOpenSteps openSteps: inout [ShortestPathStep]) {
        openSteps.append(step)
        openSteps.sort { $0.fScore <= $1.fScore }
    }
    
    func hScoreFromCoord(fromCoord: TileModel, toCoord: TileModel) -> Int {
        return abs(toCoord.col - fromCoord.col) + abs(toCoord.row - fromCoord.row)
    }
    // Converte o caminho mais curto para a notação do TileModel
    private func convertStepsToShortestPath(lastStep: ShortestPathStep) -> [TileModel] {
        var shortestPath = [TileModel]()
        var currentStep = lastStep
        while let parent = currentStep.parent { // if parent is nil, then it is our starting step, so don't include it
            shortestPath.insert(currentStep.position, at: 0)
            currentStep = parent
        }
        return shortestPath
    }
    
    func shortestPathFromTileModel(fromTileModel: TileModel, toTileModel: TileModel) -> [TileModel]? {
        
        if self.dataSource == nil {
            return nil
        }
        let dataSource = self.dataSource!
        
        // 2 : Instancia os caminhos ja analisados
        var closedSteps = Set<ShortestPathStep>()
        
        // Instancia os caminhos abertos
        var openSteps = [ShortestPathStep(position: fromTileModel)]
        
        while !openSteps.isEmpty {
            // 3 Retira o passo com menor custo F.
            //   O menor custo F é sempre o primeiro pois a lista está ordenada
            let currentStep = openSteps.remove(at: 0)
            closedSteps.insert(currentStep)
            
            // 4 Se o passo atual é o destino. Retorna, chegou ao destino.
            if currentStep.position == toTileModel {
                return convertStepsToShortestPath(lastStep: currentStep)
            }
            
            // 5 Pega os tiles adjacentes ao passo atual
            let adjacentTileModels = dataSource.getWalkableAdjacentsTileModels(tileModel: currentStep.position)
            
            //Verifica todos os tiles adjacents
            for TileModel in adjacentTileModels {
                
                // 6 Pega o passo e checa se ele já está na lista dos passos fechados.
                //   Se não, calcula o custo de movimento
                let step = ShortestPathStep(position: TileModel)
                
                if closedSteps.contains(step) {
                    continue
                }
                let moveCost = dataSource.getTileModelCost(fromTileModel: currentStep.position, toAdjacentTileModel: step.position)
                
                if let existingIndex = openSteps.index(of: step) {
                    // 7 Se esse passo já está na lista aberta pegue-o
                    let step = openSteps[existingIndex]
                    // se o passo atual for melhor que o pai do passo aberto
                    // substitua-o como pai do passo aberto
                    if currentStep.gScore + moveCost < step.gScore {
                        step.setParent(parent: currentStep, withMoveCost: moveCost)
                        
                        openSteps.remove(at: existingIndex)
                        insertStep(step: step, inOpenSteps: &openSteps)
                        self.steps += 1
                    }
                    
                } else {
                    // 8 Se o passo não foi aberto ainda, compute o seu custo e o adicione na lista de abertos
                    step.setParent(parent: currentStep, withMoveCost: moveCost)
                    step.hScore = hScoreFromCoord(fromCoord: step.position, toCoord: toTileModel)
                    
                    insertStep(step: step, inOpenSteps: &openSteps)
                    self.steps += 1
                }
            }
            
        }
        
        return nil
    }
}
