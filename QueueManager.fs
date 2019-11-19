module QueueManage

open Collections
open System

type QueueManager(arr: 'a[]) = 

    let mutable currentQueue = Queue(arr)
    
    let compareSequences = Seq.compareWith Operators.compare

    member this.CurrentQueue = currentQueue

    /// a. Инициализация очереди.
    member this.Init() = 
        let rnd = Random()
        for i in 0..3 do currentQueue.Enqueue(rnd.Next(1, 1000))       

    /// b. Добавление элемента в начало очереди.
    member this.AddToStart(obj: 'T) = currentQueue.Enqueue(obj)

    /// c.	Добавление элемента в конец очереди.
    member this.AddToEnd(obj: Object) = 
        let count = currentQueue.Count
        currentQueue <- Queue(Array.append [|obj|] (currentQueue.ToArray()))
        currentQueue.Count <- count + 1

    /// d. Сдвиг элементов очерерди на одну позицию.
    member this.Shift() = 
        currentQueue <- Queue(Array.append (currentQueue.ToArray() |> Array.skip 1) [|currentQueue.Dequeue()|] )

    /// e.	Удаление всех элементов очереди.
    member this.ClearAll() = currentQueue.Clear()

    /// f.	Определение количества элементов очереди.
    member this.GetItemsCount = currentQueue.Count

    /// g.	Проверка очереди на пустоту.
    member this.IsEmpty() = currentQueue.Count = 0  

    /// h.	Удаление первого элемента.
    member this.RemoveFirst() = 
        let count = currentQueue.Count
        currentQueue <- Queue(Array.skip 1 (currentQueue.ToArray()))
        currentQueue.Count <- count - 1

    /// i.	Удаление последнего элемента.
    member this.RemoveLast() = currentQueue.Dequeue()

    /// j.	Поиск данного значения в очереди.
    member this.Search(obj: 'T) = currentQueue.Contains(obj)

    /// k1. Поиск наибольшего элемента очереди.
    member this.GetMax() =      
        try
            let elemType = currentQueue.Peek().GetType() |> string
            let takeFunc = currentQueue.ToList() |> List.take currentQueue.Count
            match elemType with 
            | "System.Int32" ->  takeFunc |> Seq.cast<int> |> Seq.max
            | "System.Double" ->  takeFunc |> Seq.cast<double> |> Seq.max |> int
            | "System.Decimal" ->  takeFunc |> Seq.cast<decimal> |> Seq.max |> int
            | "System.Byte" ->  takeFunc |> Seq.cast<byte> |> Seq.max |> int
            | _ -> raise (System.InvalidCastException())
        with
            | :? System.InvalidCastException -> printfn "Ошибка приведения типов"; -1  

    /// k2. Поиск наименьшего элемента очереди.
    member this.GetMin() =     
        try
            currentQueue.ToList() |> List.take currentQueue.Count |> Seq.cast<int> |> Seq.min
        with
            | :? System.InvalidCastException -> printfn "Ошибка приведения типов"; -1    
            
    /// l.	Удаление элемента списка с данным значением
    member this.RemoveFirstMatch(obj: Object) =
        let index = Array.findIndex (fun x -> x = obj) (currentQueue.ToArray())
        let first = Array.take index (currentQueue.ToArray())
        let second = Array.take (index + 1) (currentQueue.ToArray())
        currentQueue <- Queue(Array.append first second)

    /// m .Удаление всех элементов очереди с данным значением.
    member this.RemoveAllMatches(obj: Object) = 
        currentQueue <- Queue(currentQueue.ToArray() |> Array.filter (fun x -> x = obj))

    /// n.	Изменение всех элементов очереди с данным значением на новое.
    member this.ReplaceAll(obj: Object, replaceTo: Object) = 
        currentQueue <- Queue(currentQueue.ToArray() |> Array.map (fun x -> (if x = obj then replaceTo else x)))

    /// p. Определение, можно ли удалить из очереди каких-нибудь два элемента так, чтобы новая очередь оказалась упорядоченной.
    member this.HasOrder() = 
        try
            let temp = currentQueue.ToList() |> List.take currentQueue.Count |> Seq.cast<int> |> Seq.toList
            let mutable countToAsc = 0
            let mutable countToDesc = 0
            for i in 0..currentQueue.Count - 2 do
                if temp.[i] >= temp.[i + 1] then countToAsc <- countToAsc + 1
                if temp.[i] <= temp.[i + 1] then countToDesc <- countToDesc + 1
            countToAsc < 3 || countToDesc < 3
        with
            | :? System.InvalidCastException -> printfn "Ошибка приведения типов"; false
      

    /// q.	Определение, сколько различных значений содержится в очереди.
    member this.DistinctCount() = currentQueue.ToList() |> List.distinct |> List.length

    /// r.	Удаление из списка элементов, значения которых уже встречались в предыдущих элементах.
    member this.Distinct() = currentQueue <- Queue(currentQueue.ToArray() |> Array.distinct)

    /// s.	Изменение порядка элементов на обратный.
    member this.Reverse() = currentQueue <- Queue(currentQueue.ToArray() |> Array.rev)

