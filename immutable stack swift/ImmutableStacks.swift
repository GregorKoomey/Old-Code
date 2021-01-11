//
//  ImmutableStacks.swift
//
//  Created by Gregor Koomey on 4/27/20.
//  Copyright © 2020 Gregor Koomey. All rights reserved.
//

//  Created by Gregor Koomey on 12/8/19.
//  Copyright © 2019 Gregor Koomey. All rights reserved.
//


import Foundation

public protocol ImmutableStack  {
    
    associatedtype Element : Equatable
    
    var immStackCurrent : Element? {get}
    
    func immGetDelta(toward : Self) -> stackDelta<Element>
    func immGetNextWithDelta(_ delta: stackDelta<Element>) -> Self?
    func immGetResultOfDeltaStream(_ stream : [stackDelta<Element>]) -> Self?
    
    func immStackIndexItem(_ index : Int) -> Element?
    func immStackSetItem(_ item : Element,  atIndex index : Int) -> Array<Element>?
    func immStackInsertItem(_ item : Element,  atIndex index : Int) -> Array<Element>?
    
    func immStackPush(_ newItem: Element) -> Array<Element>
    
    func immStackPushes(_ newItems: Array<Element>) -> Array<Element>
    func immStackPop() -> Array<Element>?
    
    func immStackPops(_ popNum : Int) -> Array<Element>?
    
    func immStackClear() -> Array<Element>
    
    func immStackCopy(_ copyCount: Int) -> Array<Element>?
    
    func immStackDup() -> Array<Element>?
    func immStackRemoveAtIndex(_ index : Int) -> Array<Element>?
    func immStackIndex(_ index : Int) -> Array<Element>?
    
    func immStackExch() -> Array<Element>?
    func immStackRoll(magnitude : Int , spin : Int) -> Array<Element>?
    func immStackSpunTo(_ index: Int) -> Array<Element>?
    func immStackSpunBy (_ spinVal : Int) -> Array<Element>
    
    //    func immStackElementType()-> Element.Type
}


extension Array : ImmutableStack where Element : Equatable {
    
    public var immStackCurrent : Element?
    {get
    {
        if self.count > 0
        {return self[self.count - 1]}
        else
        {return nil}
        }
    }
    
    public func immStackIndexItem(_ index : Int) -> Element?
    {
        var newIndex = index
        if index < 0 {
            newIndex = index + self.count
        }
        if newIndex < self.count {
            return self[newIndex]
        } else
        {
            return nil
        }
    }
    
    public func immStackSetItem(_ item : Element,  atIndex index : Int) -> Array<Element>?
    {
        var returnValue : Array<Element>? = self
        var newIndex = index
        if index < 0 {
            newIndex = index + self.count
        }
        if newIndex < self.count {
            returnValue![newIndex] = item
        } else
        {
            returnValue = nil
        }
        return returnValue
    }
    
    public func immStackInsertItem(_ item : Element,  atIndex index : Int) -> Array<Element>?
    {
        var returnValue = Array<Element>()
        var newIndex = index
        if index < 0 {
            newIndex = index + self.count
        }
        
        if newIndex < self.count {
            for tempIndex in 0..<self.count {
                if tempIndex == newIndex {
                    returnValue.append(item)
                }
                returnValue.append(self[tempIndex])
                
            }
        } else
        {
            return nil
        }
        return returnValue
    }

    public func immStackPush(_ newItem: Element) -> Array<Element>
    {
        var returnValue = self
        returnValue.append(newItem)
        return returnValue
    }
    
    public func immStackPushes(_ newItems: Array<Element>) -> Array<Element>
    {
        var returnValue = self
        for item in newItems {
            returnValue = returnValue.immStackPush(item)
        }
        return returnValue
    }
    
    public func immStackPop() -> Array<Element>?
    {
        var returnValue: Array<Element>? = self
        if self.count > 0 {
            returnValue!.removeLast()
            return returnValue
        } else {
            return nil
        }
    }
    
    public func immStackPops(_ popNum : Int) -> Array<Element>?
    {
        var returnValue: Array<Element>? = self
        if self.count >= popNum {
            for _ in 0..<popNum
            {
                returnValue!.removeLast()
            }
            return returnValue
        } else {
            return nil
        }
    }
    
    public func immStackClear() -> Array<Element>
    {
        var returnValue = self
        
        returnValue.removeAll()
        
        return returnValue
    }
    
    public func immStackCopy(_ copyCount: Int) -> Array<Element>?
    {
        if (self.count >= copyCount) && (copyCount >= 0) {
            var returnValue = self
            
            //extract top count arrayslice
            
            let adjustedIndex = self.count - copyCount
            
            let subStack = self[adjustedIndex...]
            
            for item in subStack
            {
                returnValue = returnValue.immStackPush(item)
            }
            return returnValue
        } else {
            return nil
        }
    }
    
    public func immStackDup() -> Array<Element>?
    {
        if self.count > 0 {
            var returnValue = self
            returnValue = returnValue.immStackPush(returnValue.immStackCurrent!)
            return returnValue
        } else {
            return nil
        }
    }
    
    public func immStackRemoveAtIndex(_ index : Int) -> Array<Element>?
    {
        var returnValue : Array<Element> = Array<Element>()
        var newIndex = index
        if index < 0 {
            newIndex = index + self.count
        }
        
        
        if newIndex < self.count {
            for tempIndex in 0..<self.count {
                if tempIndex != newIndex {
                    returnValue.append(self[tempIndex])
                }
            }
        } else
        {
            return nil
        }
        return returnValue
    }
    
    
    
    
    public func immStackIndex(_ index : Int) -> Array<Element>?
    {
        var returnValue : Array<Element>? = self
        var newIndex = index
        if index < 0 {
            newIndex = index + self.count
        }
        if newIndex < self.count {
            returnValue = returnValue!.immStackPush(returnValue![newIndex])
        } else
        {
            returnValue = nil
        }
        return returnValue
    }
    
    
    public func immStackExch() -> Array<Element>?
    {
        if self.count > 1 {
            var returnValue = self
            let currentCurrent = returnValue.immStackCurrent!
            returnValue = returnValue.immStackPop()!
            let secondCurrent =  returnValue.immStackCurrent!
            returnValue = returnValue.immStackPop()!
            returnValue = returnValue.immStackPushes([currentCurrent, secondCurrent])
            return returnValue
        } else {
            return nil
        }
    }
    
    public func immStackRoll(magnitude : Int , spin : Int) -> Array<Element>?
    {
        if (self.count >= magnitude) && (magnitude >= 0) {
            if (spin != 0 && magnitude > 1) {
                var returnValue = self.immStackClear()
                
                let adjustedIndex = self.count - magnitude
                //extract top count arrayslice
                
                let subStack = self[adjustedIndex...]
                
                for item in subStack
                {
                    returnValue = returnValue.immStackPush(item)
                }
                //do spin on returnValue
                
                let subStackReturn = returnValue.immStackSpunBy(spin)
                
                
                returnValue = self.immStackClear()
                
                
                for index in 0..<adjustedIndex
                {
                    returnValue = returnValue.immStackPush(self[index])
                    
                }
                
                for item in subStackReturn {
                    returnValue = returnValue.immStackPush(item)
                }
                
                return returnValue
            } else
            {
                return self
            }
        } else {
            return nil
        }
    }
    
    
    public func immStackSpunTo(_ index: Int) -> Array<Element>? {
        var returnValue = self.immStackClear()
        
        let selfCount = self.count
        
        var adjustedIndex = index
        
        if index < 0 {
            adjustedIndex = self.count + adjustedIndex
        }
        
        if (adjustedIndex < selfCount) && (adjustedIndex > 0)
        {
            var remainderArray = returnValue
            var initialArray = returnValue
            
            var localIndex: Int = 0
            
            for item in self {
                if localIndex < adjustedIndex {
                    remainderArray.append(item)
                } else
                {
                    initialArray.append(item)
                }
                localIndex += 1
            }
            returnValue = initialArray + remainderArray
            return returnValue
        } else
        {
            if adjustedIndex == 0 {
                return self}
            else {return nil}
        }
    }
    
    public func immStackSpunBy (_ spinVal : Int) -> Array<Element>
    {
        var returnValue = self
        
        let selfCount = self.count
        let spinMag = abs(spinVal)
        
        if (selfCount < 2)||(spinVal == 0)||(spinMag == selfCount)||((spinMag % selfCount == 0))
        {
            return returnValue
        }
        //else
        
        var adjustedSpin = spinVal
        
        if spinMag > selfCount {
            adjustedSpin = adjustedSpin % selfCount
        }
        var splitIndex = adjustedSpin
        if adjustedSpin > 0 {
            splitIndex = selfCount - adjustedSpin
        } else {
            splitIndex = abs(adjustedSpin)
        }
        let firstPart = self[0..<splitIndex]
        let secondPart = self[splitIndex..<endIndex]
        returnValue.removeAll()
        returnValue.append(contentsOf: secondPart)
        returnValue.append(contentsOf: firstPart)
        return returnValue
    }
    
    public func immGetDelta(toward other : Array<Element> ) -> stackDelta<Element>
    {
        var returnValue = stackDelta<Element>.identity
        
        let shorterSource  = self.count <= other.count  ? self : other
        let longerSource = self.count <= other.count  ? other : self
        
        var startMatchCount : Int = 0
        
        for i in 0..<shorterSource.count
        {
            if shorterSource[i] == longerSource[i]
            {
                startMatchCount += 1
            }
        }
        
        let popCount = self.count - startMatchCount
        let pushPayload = other[startMatchCount...]
        
        var payLoad : Array? = nil
        
        if pushPayload.count > 0 {
            payLoad = Array(pushPayload)
        }
        
        if let finalPayload = payLoad
        {
            if finalPayload.count > 0
            {
                if popCount > 0
                {
                    if popCount > 1
                    {
                        returnValue = stackDelta<Element>.popsAndPushes(popCount, finalPayload)
                    } else
                    {
                        returnValue = stackDelta<Element>.popAndPushes(finalPayload)
                    }
                    
                } else
                {
                    returnValue = stackDelta<Element>.pushes(finalPayload)
                    
                }
                
            } else {
                if popCount > 0
                {
                    if popCount > 1
                    {
                        returnValue = stackDelta<Element>.pops(popCount)
                    } else
                    {
                        returnValue = stackDelta<Element>.pop
                    }
                }
            }
        } else
        {
            if popCount > 0
            {
                if popCount > 1
                {
                    returnValue = stackDelta<Element>.pops(popCount)
                } else
                {
                    returnValue = stackDelta<Element>.pop
                }
            }
        }
        
        
        return returnValue
    }
    
    
    
    public func immGetNextWithDelta(_ delta: stackDelta<Element>) -> Array?
    {
        var returnValue : Array? = self.immStackClear()
        
        switch delta {
        case .pop:
            returnValue = self.immStackPop()
        case .pops(let numPops):
            returnValue = self.immStackPops(numPops)
        case .pushes (let payload):
            returnValue = self.immStackPushes(payload)
        case .popAndPushes(let payload):
            returnValue = self.immStackPop()
            returnValue = returnValue!.immStackPushes(payload)
        case let .popsAndPushes( numPops, payload):
            returnValue = self.immStackPops(numPops)
            returnValue = returnValue!.immStackPushes(payload)
        case .identity:
            returnValue = self
        }
        return returnValue
    }
    
    public func immGetResultOfDeltaStream(_ stream : [stackDelta<Element>]) -> Array?
    {
        var returnValue : Array? = self
        
        for delta in stream {
            returnValue = returnValue!.immGetNextWithDelta(delta)
            
            if returnValue == nil
            {
                break
            }
        }
        return returnValue
    }
}

public protocol StackDelta {
    associatedtype Element : Equatable
    var pops : Int {get}
    var payLoad : [Element]? {get}
}

public enum stackDelta<Element : Equatable> : StackDelta {
    case pop
    case pops(Int)
    case pushes ([Element])
    case popAndPushes([Element])
    case popsAndPushes( Int, [Element])
    case identity
    
    
    public var description : String {
        get
        {
            var returnValue  = "error: description not set"
            switch self {
            case .pop:
                returnValue = "pop\r"
            case .pops(let numPops):
                returnValue = "pops: \(numPops)\r"
            case .pushes (let payload):
                returnValue = "pushes: \(payload)\r"
            case .popAndPushes(let payload):
                returnValue = "popAndPushes: \(payload)\r"
            case let .popsAndPushes( numPops, payload):
                returnValue = "pops: \(numPops) andPushes: \(payload)\r"
            case .identity:
                returnValue = "identity"
            }
            return returnValue
        }
    }
    
    public static func == (left: stackDelta, right: stackDelta) -> Bool
    {
        if left.debugDescription == right.debugDescription {
            return true
        } else
        {
            return false
        }
    }
    
    public static func != (left: stackDelta, right: stackDelta) -> Bool
    {
        if (left == right)
        {
            return false
        } else
        {
            return true
        }
    }
    
    public var debugDescription : String {
        get
        {
            return description
        }
    }
    
    public var pops : Int
    { get
    {
        switch self {
        case .pop:
            return 1
        case .pops(let pops):
            return pops
        case .pushes ( _):
            return 0
        case .popAndPushes(_):
            return 1
        case let .popsAndPushes( pops, _):
            return pops
        case .identity:
            return 0
        }
        }
    }
    
    public var payLoad : [Element]?
    { get
    {switch self {
    case .pop:
        return nil
    case .pops(_):
        return nil
    case .pushes (let pushes):
        return pushes
    case .popAndPushes(let pushes):
        return pushes
    case let .popsAndPushes( _, pushes):
        return pushes
    case .identity:
        return nil
        }
        }
    }
    
}
