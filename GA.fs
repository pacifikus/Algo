module GenAlgo

open System
open System.Collections.Generic

type Soln(n: int) = 
    let mutable fit = 0
    let mutable solutionData = [|for i in 0..n-1 -> i|]
    
    member this.SolutionData
        with get() = solutionData
        and set(value) = solutionData <- value

    member this.Fit
        with get() = fit
        and set(value) = fit <- value

type GA(countRow: int) = 
    let maxIter = 100000
    let elitrate = 10 // percent
    let n = countRow
    let populationCount = n*n
    let rnd = new Random()
    let esize = populationCount / elitrate
    let states = new Queue<int[]>()
    let mutable solved = false

    let permute(data:int[]) =
        for i = data.Length - 1 downto 1 do
            let j = rnd.Next(i + 1)
            let temp = data.[j]
            data.[j] <- data.[i]
            data.[i] <- temp
        data

    let generatePopulation() = [
        for i in 0..populationCount-1 do
            let sol = new Soln(n)
            sol.SolutionData <- permute( sol.SolutionData)
            yield sol
        ]

    let gemmation(data: int[]) =
        let array = [|for i in 0..n-1 -> data.[i]|]
        let i = rnd.Next(0, n)
        let j = rnd.Next(0, n)
        let temp = array.[i]
        array.[i] <- array.[j]
        array.[j] <- temp
        array

    let mutable population = generatePopulation()

    let doStep() = 
        for i in esize..(populationCount - esize - 1) do
            population.[i].SolutionData <- gemmation(population.[i].SolutionData)
        for i in (populationCount - esize)..populationCount-1 do
            population.[i].SolutionData <- gemmation(population.[populationCount - i].SolutionData)

    let calcFit() = 
        for k in 0..populationCount-1 do
            let mutable count = 0
            for i in 0..n-1 do
                for j in 0..n-1 do    
                    if population.[k].SolutionData.[j] - population.[k].SolutionData.[i] = Math.Abs(j - i) then count <- count + 1
            population.[k].Fit <- count - n

    let sortByFit() = population <- population |> List.sortBy(fun x -> x.Fit)

    member this.States = states

    member this.Solved = solved

    member this.Start() =
        let mutable num = 0
        while num < maxIter do
            calcFit()
            sortByFit()
            if population.[0].Fit <> 0 then
                states.Enqueue(population.[population.Length - 1].SolutionData)
                doStep()
                num <- num + 1
            else 
                states.Enqueue(population.[0].SolutionData)
                num <- maxIter + 1
                solved <- true



