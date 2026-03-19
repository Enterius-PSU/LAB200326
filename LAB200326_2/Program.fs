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

// Подсчёт элементов, не содержащих заданную цифру
let rec searchDigitInTree tree digit =
    match tree with
    | Some t ->
        let leftCount = searchDigitInTree t.left digit
        let rightCount = searchDigitInTree t.right digit
        if isDigitInNumber t.value digit then
            leftCount + rightCount
        else
            leftCount + rightCount + 1
    | None -> 0

// Вывод бинарного дерева (обратный обход)
let rec printTree tree =
    match tree with
    | Some t ->
        printTree t.left
        printTree t.right
        printf "%i " t.value
    | None -> ()

// Создание бинарного дерева
let rec createTree list =
    match list with
    | [] -> None
    | head :: tail -> 
        let subtree = createTree tail
        Some (insert subtree head)

// Создание списка из случайных чисел
let createList number =
    [1..number]
    |> List.fold (fun acc x ->
        acc @ [x*x - 2*x - 4]
    ) []


// Запрос и проверка цифры
let rec checkDigit() = 
    printf "Введите цифру для поиска: "
    let digit = int(Console.ReadLine())
    if digit >= 0 && digit <= 9 then
        digit
    else
        printfn "Цифра вне диапазона!\n"
        checkDigit()
    
// Запрос и проверка количества элементов
let rec checkElements() = 
    printf "Введите количество элементов бинарного дерева: "
    let elements = int(Console.ReadLine())
    if elements < 0 then
        printfn "Количество элементов < 0!\n"
        checkElements()
    else
        elements

[<EntryPoint>]
let main args = 
    let elements = checkElements()
    let list = createList elements
    let tree = createTree list
    printf "Исходное дерево: "
    printTree tree
    printfn "\n"

    let digit = checkDigit()
    let result = searchDigitInTree tree digit
    printf "Количество элементов, не содержащих цифру %i: %i" digit result

    System.Console.ReadKey() |> ignore
    0

