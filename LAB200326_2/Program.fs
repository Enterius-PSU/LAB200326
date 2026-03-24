// Кушев Александр БАС-2
// Лабораторная работа №4 (Задание №2)

open System

// Структура дерева
type Tree = {
    value:int
    left:Tree option
    right:Tree option
}

// Проверки на содержание цифры в числе
let rec check number digit =
    if number = 0 then 
        false
    else if number % 10 = digit then 
        true
    else 
        check (number / 10) digit

// Проверка на частный случай, иначе - проверка на содержание
let isDigitInNumber number digit =
    if number = 0 && digit = 0 then 
        true
    else 
        check (abs number) digit

// Вставка элемента в бинарное дерево
let rec insert tree newValue =
    match tree with 
    | Some t -> 
        if newValue < t.value then 
            {t with left = Some (insert t.left newValue)}
        else if newValue > t.value then 
            {t with right = Some (insert t.right newValue)}
        else t
    | None -> 
        {value = newValue; left = None; right = None}

// tree.Fold()
let rec treeFold funcToApply acc tree =
    match tree with
    | None -> acc
    | Some t ->
        let newAcc = funcToApply acc t.value
        let leftAcc = treeFold funcToApply newAcc t.left
        let rightAcc = treeFold funcToApply leftAcc t.right
        rightAcc

// Вывод бинарного дерева
let rec printTree tree indent side =
    match tree with
    | None -> "" 
    | Some tree ->
        let current = sprintf "%s%s%d" indent side tree.value
        let left = printTree tree.left (indent + "  ") "L:"
        let right = printTree tree.right (indent + "  ") "R:"
        let children = [ 
            if left <> "" then 
                left
            if right <> "" then 
                right
            ]
        if List.isEmpty children then 
            current
        else 
            current + "\n" + String.concat "\n" children

// Функция генерации уникального числа
let rec uniqueNumber usedNumbers =
    let newNumber = System.Random().Next(-100, 100)
    if List.contains newNumber usedNumbers then
        uniqueNumber usedNumbers
    else
        newNumber

// Создание бинарного дерева с уникальными числами
let rec createTree count numbers =
    match count with
    | 0 -> None, numbers
    | _ ->
        let newNumber = uniqueNumber numbers
        let newNumbers = newNumber :: numbers
        let subtree, resultNumbers = createTree (count - 1) newNumbers
        Some (insert subtree newNumber), resultNumbers

// Запрос и проверка цифры
let rec checkDigit() = 
    printf "Введите цифру для поиска: "
    let digit = int(System.Console.ReadLine())
    if digit >= 0 && digit <= 9 then
        digit
    else
        printfn "Цифра вне диапазона!\n"
        checkDigit()
    
// Запрос и проверка количества элементов
let rec checkCount() = 
    printf "Введите количество элементов бинарного дерева: "
    let count = int(System.Console.ReadLine())
    if count < 0 then
        printfn "Количество элементов < 0!\n"
        checkCount()
    else
        count

[<EntryPoint>]
let main args = 
    let count = checkCount()
    let tree, _ = createTree count []
    printf "Исходное дерево: \n%s\n" (printTree tree "" "")

    let digit = checkDigit()

    let result = treeFold (
        fun acc value -> 
            if isDigitInNumber value digit then 
                acc 
            else 
                acc + 1) 
                    0 tree

    printf "Количество элементов, не содержащих цифру %i: %i" digit result

    System.Console.ReadKey() |> ignore
    0
