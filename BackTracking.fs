module BackTracking

open System.Collections.Generic

type BackTrack(n) =
    let n = n
    let mutable pos = [|for i in 1..n -> 0|]
    let mutable solved = false
    let mutable states = Queue()

    let isSafe (queenNum: int, currentRow: int) = 
        pos |> Array.take queenNum
        |> Array.indexed
        |> Array.filter (fun(i, x) -> (x = currentRow || (x = (currentRow - queenNum + i)) || x = (currentRow + queenNum - i)))
        |> Array.length = 0

    let solve k n = 
        let rec loop (k: int, n: int) = 
            if solved = false then
                solved <- k = n
                for i in 0..(n - 1) do
                    if isSafe(k, i) then begin
                        pos.[k] <- i
                        let temp = Array.copy pos
                        if solved <> true then states.Enqueue temp
                        loop (k + 1, n)
                    end
        loop (k, n)

    do solve 0 n

    member this.PosQueue = states

    member this.Solved = solved


