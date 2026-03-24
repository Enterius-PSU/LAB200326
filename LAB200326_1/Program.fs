// Кушев Александр БАС-2
// Лабораторная работа №4 (Задание №1)

open System

// Структура дерева
type Tree = {
    value: int
    left: Tree option
    right: Tree option
}

// Добавление цифры в конец числа
let transformNumber digit number = 
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
let rec treeMap tree funcToApply =
    match tree with
    | Some t ->
        let newValue = funcToApply t.value
        let newLeft = treeMap t.left funcToApply
        let newRight = treeMap t.right funcToApply
        Some { value = newValue; left = newLeft; right = newRight }
    | None -> None

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
    let newNumber = Random().Next(-100, 100)
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
    printf "Введите цифру для приписывания: "
    let digit = int(Console.ReadLine())
    if digit >= 0 && digit <= 9 then
        digit
    else
        printfn "Цифра вне диапазона!\n"
        checkDigit()
    
// Запрос и проверка количества элементов
let rec checkCount() = 
    printf "Введите количество элементов бинарного дерева: "
    let count = int(Console.ReadLine())
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
    let newTree = treeMap tree (transformNumber digit)
    printf "Изменённое дерево: \n%s\n" (printTree newTree "" "")

    System.Console.ReadKey() |> ignore
    0
