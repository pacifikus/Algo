module TreeManage

open System

type TreePoint(index: int, parentIndex: int) = 
    let mutable x = 0.0
    let mutable y = 0.0
    let mutable level = 0
    let mutable parent = parentIndex

    member this.Index = index

    member this.X
        with get() = x
        and set(value: double) = x <- value

    member this.Y
        with get() = y
        and set(value: double) = y <- value

    member this.Level
        with get() = level
        and set(value: int) = level <- value

    member this.Parent
        with get() = parent
        and set(value: int) = parent <- value

type Tree() = 

    let mutable size = 1
    let mutable points = List.init size (fun x-> new TreePoint(0, -1))
    let objects = [|0;0;1;1;2;2|]
    let leftPart = [
        for i in 0..objects.Length - 1 do
            yield new TreePoint(i + 1, objects.[i])]

    do points <- List.append points leftPart
    do size <- points.Length

    member this.Count = size

    member this.Add (parent: int) =
        points <- List.append points (List.init 1 (fun x -> TreePoint(points.Length, points.[parent].Index))) 
        size <- size + 1

    member this.Remove(index: int) =  
        let toRemove = 
            points |> 
            List.indexed |> 
            List.filter (fun (i, x) -> x.Parent = index) |>
            List.map snd
        points <- points |> List.filter (fun x -> x.Index <> index)
        for i in 0..toRemove.Length - 1 do
            this.Remove(toRemove.[i].Index)
        size <- points.Length

    member this.ToList() = points

    member this.Merge(index: int) = 
        let add = this.Count
        let tree = new Tree()
        let data = tree.ToList()
        for i in 0..data.Length - 1 do
            if data.[i].Index = 0 then this.Add(index) 
            else this.Add(data.[i].Parent + add)

    member this.GetMostOften() = 
        fst ((snd (points 
        |> Seq.countBy id 
        |> Seq.groupBy snd 
        |> Seq.sortByDescending fst 
        |> Seq.toArray).[0] 
        |> Seq.toArray).[0])
