module Collections

open System

type Stack<'T>(arr: 'a[]) =

    let defaultSize = 10
    let mutable size = arr.Length
    let mutable objects = arr

    new() = Stack(Array.init 10 (fun x -> null))

    member this.Count
        with get() = size
        and set(value) = size <- value

    member this.Push (obj: 'a) =
        if objects.Length = size then
            objects <- Array.append objects (Array.init 1 (fun x -> null))
        objects.[size] <- obj
        size <- size + 1

    member this.Peek() = objects.[size - 1]

    member this.Pop() = 
        let lastElem = objects.[size - 1]
        objects.[size - 1] <- null
        size <- size - 1
        lastElem

    member this.Clear() = 
        objects <- Array.init defaultSize (fun x -> null)
        size <- 0

    member this.Contains(obj: Object) = objects |> Array.filter (fun x -> x = obj) |> Array.length <> 0

    member this.ToList() = Array.toList objects

    member this.ToArray() = objects


type Queue<'T>(arr: 'a[]) = 

    let defaultSize = 10
    let mutable size = arr.Length
    let mutable objects = arr

    new() = Queue(Array.init 10 (fun x -> null))

    member this.Count 
        with get() = size
        and set(value) = size <- value

    member this.Dequeue() = 
         let first = objects.[0]
         objects <- Array.skip 1 objects
         size <- size - 1
         first

    member this.Peek() = objects.[0]

    member this.Enqueue(obj: 'a) = 
        if objects.Length = size then
            objects <- Array.append objects (Array.init 1 (fun x -> null))
        objects.[size] <- obj
        size <- size + 1

    member this.Contains(obj: 'a) = objects |> Array.filter (fun x -> x = obj) |> Array.length <> 0

    member this.ToList() = Array.toList objects

    member this.Clear() = 
        objects <- Array.init defaultSize (fun x -> null)
        size <- 0

    member this.ToArray() = objects
