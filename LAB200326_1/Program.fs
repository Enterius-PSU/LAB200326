// Кушев Александр БАС-2
// Лабораторная работа №4 (Задание №1)

open System

// Структура дерева
type Tree = {
    value:int
    left:Tree option
    right:Tree option
}

// Добавление цифры в конец числа
let transformNumber number digit = 
    if number < 0 then
        -(-number * 10 + digit)
    else if number > 0 then
        number * 10 + digit
    else
        0
    
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

// Создание нового дерева с изменёнными значениями
let rec transformTree tree digit =
    match tree with
    | Some t ->
        let newValue = transformNumber t.value digit
        let newLeft = transformTree t.left digit
        let newRight = transformTree t.right digit
        Some { value = newValue; left = newLeft; right = newRight }
    | None -> None

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
    let numberList = [1..number]
    List.map (fun x -> x*x - 2 * x - 4) numberList

// Запрос и проверка цифры
let rec checkDigit() = 
    printf "Введите цифру для приписывания: "
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
    let newTree = transformTree tree digit
    printf "Измененное дерево: "
    printTree newTree
    printfn "\n"

    System.Console.ReadKey() |> ignore
    0