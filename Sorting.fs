module Sorting

open System

type Algo() =

    let mutable steps = []

    let swap i j (arr: 'a[]) = 
        let temp = arr.[i]
        arr.[i] <- arr.[j]
        arr.[j] <- temp  

    let binarySearch (arr: int[]) key l r = 
        let mutable left = l
        let mutable right = r
        while (left < right) do
            let mutable middle = left + (right - left) / 2
            if arr.[middle] < key then left <- (middle + 1) else right <- middle
        if right < 0 then 0 else right

    member this.Steps = steps
            
    member this.BubbleSort(arr) = 
        steps <- List.init 0 (fun x -> [||])
        let rec loop (arr: 'a[]) =
            let mutable swaps = 0
            for i in 0..arr.Length - 2 do
                if(arr.[i] > arr.[i+1]) then
                    swap i (i + 1) arr
                    steps <- (Array.copy arr) :: steps
                    swaps <- swaps + 1

            if swaps > 0 then loop arr else arr
        loop arr

    member this.Cocktail  (arr: int[]) = 
        steps <- List.init 0 (fun x -> [||])
        let mutable count = 0
        let mutable left = 0
        let mutable right = arr.Length - 2
        let rec loop  (arr: 'a[]) = 
            count <- count + 1
            for i in left..right do
                if(arr.[i] > arr.[i+1]) then
                    swap i (i+1) arr
                    steps <- (Array.copy arr) :: steps
            left <- left + 1
            for i = right downto left do
                if(arr.[i] < arr.[i-1]) then
                    swap i (i-1) arr
                    steps <- (Array.copy arr) :: steps
            right <- right - 1
            if left < right then loop arr else arr
        loop arr

    member this.Insertion (arr: int[]) =
        steps <- List.init 0 (fun x -> [||])
        for i in 1..arr.Length - 1 do
            let mutable j = i - 1
            while (j >= 0 && arr.[j] > arr.[j+1]) do
                swap j (j+1) arr
                j <- j-1
                steps <- (Array.copy arr) :: steps
        arr

    member this.BinaryInsertion (arr: int[]) =
        steps <- List.init 0 (fun x -> [||])
        for i in 1..arr.Length - 1 do
            let mutable j = i - 1
            let mutable k = binarySearch arr arr.[i] 0 i
            for m = j downto k do
                swap m (m+1) arr
                steps <- (Array.copy arr) :: steps
        arr


