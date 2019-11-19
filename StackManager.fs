module StackManage

open Collections
open System

type StackManager(arr: 'a[]) = 
    
    let mutable currentStack = Stack(arr)

    member this.CurrentStack = currentStack

    /// a. Инициализация стека.
    member this.Init() = 
        let rnd = Random()
        for i in 0..3 do currentStack.Push(rnd.Next(1, 1000))

    // b. Добавление элемента в начало стека.
    member this.AddToStart(obj: Object) = 
        let count = currentStack.Count
        currentStack <- Stack(Array.append [|obj|] (currentStack.ToArray()))
        currentStack.Count <- count + 1

    /// c.	Добавление элемента в конец стека.
    member this.AddToEnd(obj: 'T) = currentStack.Push(obj)
    
    /// d. Сдвиг элементов стека на одну позицию.
    member this.Shift() = 
        currentStack <- Stack(Array.append [|currentStack.Pop()|] (currentStack.ToArray() |> Array.take (currentStack.Count)))

    /// e.	Удаление всех элементов стека.
    member this.ClearAll() = currentStack.Clear()

    /// f.	Определение количества элементов стека.
    member this.GetItemsCount = currentStack.Count

    /// g.	Проверка стека на пустоту.
    member this.IsEmpty() = currentStack.Count = 0  

    /// h.	Удаление первого элемента.
    member this.RemoveFirst() = 
        let count = currentStack.Count
        currentStack <- Stack(Array.skip 1 (currentStack.ToArray()))
        currentStack.Count <- count - 1

    /// i.	Удаление последнего элемента.
    member this.RemoveLast() = currentStack.Pop()

    /// j.	Поиск данного значения в стека.
    member this.Search(obj: 'T) = currentStack.Contains(obj)

    /// m .Удаление всех элементов стека с данным значением.
    member this.RemoveAllMatches(obj: Object) = currentStack <- Stack(currentStack.ToArray() |> Array.filter (fun x -> x = obj))

    /// n.	Изменение всех элементов стека с данным значением на новое.
    member this.ReplaceAll(obj: Object, replaceTo: Object) = 
        currentStack <- Stack(currentStack.ToArray() |> Array.map (fun x -> (if x = obj then replaceTo else x)))

    /// q.	Определение, сколько различных значений содержится в стеке.
    member this.DistinctCount() = currentStack.ToList() |> List.distinct |> List.length

    /// r.	Удаление из списка элементов, значения которых уже встречались в предыдущих элементах.
    member this.Reverse() = currentStack <- Stack(currentStack.ToArray() |> Array.rev)

    /// s.	Изменение порядка элементов на обратный.
    member this.Distinct() = currentStack <- Stack(currentStack.ToArray() |> Array.distinct)
