module GraphManage

type Graph(v: int) =
    let v = v
    let mutable paths = []
    let mutable adjList = [|for i in 0..v-1 -> []|]

    member this.Paths = paths |> List.map (fun x -> Seq.toList x) |> Seq.toList

    member this.AddEdge u v = 
        adjList.[u] <- adjList.[u] 
        |> List.append [v]

    member this.GetAllPaths s d = 
        let mutable isVisited = [|for i in 0..v-1 -> false|]
        let mutable pathList = [s]

        let rec loop (fullPath : int list, u, d) = 
            isVisited.[u] <- true
            if u = d then paths <- paths |> List.append [fullPath]
            else begin
                for i in 0..adjList.[u].Length-1 do
                    if isVisited.[adjList.[u].[i]] <> true then begin
                        let mutable newFullPath = 
                            [adjList.[u].[i]] 
                            |> List.append fullPath
                        loop (newFullPath, adjList.[u].[i], d)
                        pathList <- pathList |> List.filter (fun x-> x <> adjList.[u].[i])
                        end
            end
            isVisited.[u] <- false
        loop (pathList, s, d)

