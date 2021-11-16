//
// If you're reading this code, and you're not newbie to F#, then, yes,
// I admit it, this code is disgusting. I wrote it in a few hours without paying
// attention to its cleanliness. It is also commented and written for a French audience.
//

open System
open FParsec
open IndentParser

// On considère l'AST des expressions suivant
type Expression =
    | Constant of Number
    | Variable of string
    | BinOp of Operator * Expression * Expression
    | Derivative of var: Expression * expr: Expression
    | Function of name: Function * expr: Expression
and Number = float
and Operator = Addition | Substraction | Multiplication | Division | Power
and Function = Ln | Log2 | Log10 | Sin | Cos | Tan | Cot | Exp | Abs

type TopLevel =
    | DefFunction of name: Expression * expr: Expression
    | DefError of var: Expression * err: Error
    | DefMeasures of var: Expression * measures: Number list
    | DefConstant of name: Expression * expr: Expression
    | DefSignificantDigits of int32
and Error =
    | ErrConstant of Number
    | ErrPercent of Number

type Assoc = Associativity

let mutable significantDigits = 4

//////////////////
///   Parser   ///
//////////////////

let numberFormat =
    NumberLiteralOptions.AllowBinary
    ||| NumberLiteralOptions.AllowMinusSign
    ||| NumberLiteralOptions.AllowHexadecimal
    ||| NumberLiteralOptions.AllowOctal
    ||| NumberLiteralOptions.AllowPlusSign
    ||| NumberLiteralOptions.AllowFraction

let pnum =
        (numberLiteral numberFormat "number" |>> fun nl ->
                if nl.IsInteger then (float nl.String)
                else (float nl.String))

let comment = pstring "#" >>. skipRestOfLine true <?> ""
let ws = spaces >>? (attempt comment <|> spaces)
let str_ws s = pstring s >>. ws
let opp = new OperatorPrecedenceParser<_,_,_>()
let expr = ws >>. opp.ExpressionParser
let parseNumber = pfloat |>> Constant

// Une variable accepte les lettres majuscules et minusciles, ainsi que des chiffres
// et aussi le wildcard et le "prime" (un guillemet ')
let parseVariable =
    let isAsciiIdStart c =
        isAsciiLetter c || c = '_' || c = '-'

    let isAsciiIdContinue c =
        isAsciiLetter c || isDigit c || c = '_' || c = '\''

    identifier (IdentifierOptions(isAsciiIdStart    = isAsciiIdStart,
                                  isAsciiIdContinue = isAsciiIdContinue))
                                    |>> fun s ->
                                        if s.[0] = '-'
                                        then BinOp(Multiplication, Constant -1., Variable s.[1..])
                                        else Variable s

// La syntaxe de l'opérateur "dérivé" est:  derive[variable] expression
let parseDerivative =
    parse { do! stringReturn "derive" ()
            let! v = between (ws >>. pstring "[") (ws >>. pstring "]") (ws >>. parseVariable .>> ws)
            let! e = expr
            return Derivative (v, e) }

// Les fonctions particulières

let parseLn = pipe2 (pstring "ln") expr (fun _ e -> Function(Ln, e))
let parseLog2 = pipe2 (pstring "log2") expr (fun _ e -> Function(Log2, e))
let parseLog10 = pipe2 (pstring "log10") expr (fun _ e -> Function(Log10, e))
let parseSin = pipe2 (pstring "sin") expr (fun _ e -> Function(Sin, e))
let parseCos = pipe2 (pstring "cos") expr (fun _ e -> Function(Cos, e))
let parseTan = pipe2 (pstring "tan") expr (fun _ e -> Function(Tan, e))
let parseCot = pipe2 (pstring "cot") expr (fun _ e -> Function(Cot, e))
let parseExp = pipe2 (pstring "exp") expr (fun _ e -> Function(Exp, e))
let parseAbs = pipe2 (pstring "abs") expr (fun _ e -> Function(Abs, e))

let parseFunction =
    choice [ parseLn
           ; parseLog2
           ; parseLog10
           ; parseSin
           ; parseCos
           ; parseTan
           ; parseCot
           ; parseExp
           ; parseAbs ]

// Pi est une constante, remplacée par 3.1415...
let parsePi =
    stringReturn "pi" ()
    |>> fun _ -> Constant Math.PI

// La racine carrée est équivalent à la puissance 1/2
let parseSqr =
    pipe2 (pstring "sqrt" <|> pstring "sqr")
          expr
          (fun _ e -> BinOp(Power, e, Constant 0.5))

// Les différentes valeurs parsées
let value =
    parseNumber <|>         // Les nombres
    parseDerivative <|>     // Les dérivées
    parseSqr <|>            // Les racines carrées
    parseFunction <|>       // Les fonctions natives
    parsePi <|>             // La constante PI
    parseVariable           // Les variables

// Un terme est une valeur ou une valeur entre parenthèses
let term =
    (ws >>. value .>> ws) <|>
    between (str_ws "(") (str_ws ")") expr

// Le parser manipule des expressions, les expressions sont composées de termes
// et les termes eux-mêmes peuvent utiliser des expressions
opp.TermParser <- term

// Fonctions utiles au parsing d'opérateurs

let adjustPosition offset (pos: Position) =
    Position(pos.StreamName, pos.Index + int64 offset,
             pos.Line, pos.Column + int64 offset)

let addInfixOperator str prec assoc mapping =
    let op = InfixOperator(str, getPosition .>> ws, prec, assoc, (),
                           fun opPos leftTerm rightTerm ->
                               mapping
                                   (adjustPosition -str.Length opPos)
                                   leftTerm rightTerm)
    opp.AddOperator(op)

// On ajoute les opérateurs principaux

addInfixOperator "+"  6 Assoc.Left (fun _ leftTerm rightTerm -> BinOp(Addition, leftTerm, rightTerm))
addInfixOperator "-"  6 Assoc.Left (fun _ leftTerm rightTerm -> BinOp(Substraction, leftTerm, rightTerm))
addInfixOperator "*"  7 Assoc.Left (fun _ leftTerm rightTerm -> BinOp(Multiplication, leftTerm, rightTerm))
addInfixOperator "/"  7 Assoc.Left (fun _ leftTerm rightTerm -> BinOp(Division, leftTerm, rightTerm))
addInfixOperator "**" 8 Assoc.Left (fun _ leftTerm rightTerm -> BinOp(Power, leftTerm, rightTerm))

// Parse la définition d'une fonction
let parseDefFunction =
    parse { let! pos = getPosition
            do! greater pos <| stringReturn "Function:" ()
            let! var = greater pos parseVariable
            do! greater pos <| stringReturn "=" ()
            let! expr = greater pos <| expr
            return DefFunction(var, expr) }

// Parse la définition d'une attribution d'erreur
let parseDefError =
    parse { let! pos = getPosition
            do! greater pos <| stringReturn "Error:" ()
            let! var = greater pos parseVariable
            do! greater pos <| stringReturn "=" ()
            let! err = greater pos <| pnum
            return DefError(var, ErrConstant err) }

// Parse la définition d'une attribution d'erreur
let parseDefConstant =
    parse { let! pos = getPosition
            do! greater pos <| stringReturn "Constant:" ()
            let! var = greater pos parseVariable
            do! greater pos <| stringReturn "=" ()
            let! expr = greater pos <| expr
            return DefConstant(var, expr) }

// Définit les chiffres significatifs à afficher
let parseSignificantDigits =
    parse { let! pos = getPosition
            do! greater pos <| stringReturn "Digits:" ()
            let! digits = greater pos pint32
            return DefSignificantDigits digits }

// Parse la liste de mesures pour une variable
let parseDefMeasures =
    parse { let! pos = getPosition
            do! greater pos <| stringReturn "Measures:" ()
            let! var = greater pos parseVariable
            do! greater pos <| stringReturn "=" ()
            let! measures = greater pos <| sepBy (ws >>. pnum .>> ws) (pstring ",")
            return DefMeasures(var, measures) }

let parseTopLevel =
    (attempt parseDefFunction <|>
     attempt parseDefError <|>
     attempt parseDefConstant <|>
     attempt parseSignificantDigits <|>
     parseDefMeasures) .>> ws

let parseProgram = blockOf (ws >>. parseTopLevel) .>> eof
    //(sepEndBy parseTopLevel (newline >>. ws)) .>> eof

//////////////////////
///   Evaluation   ///
//////////////////////


// Opération de dériver en fonction d'une variable donnée
let rec derive variable =
    function

    // La dérivée d'une variable est 1
    | Variable x when x = variable ->
        Constant 1.

    // La dérivée d'une constante est 0
    | Constant _ | Variable _ ->
        Constant 0.

    // La dérivée d'une dérivée est la dérivée seconde par rapport à la même variable
    | Derivative(Variable v, e) ->
        derive v e

    // La dérivée de l'addition est l'addition des dérivées
    | BinOp(Addition, e1, e2) ->
        BinOp(Addition, derive variable e1, derive variable e2)

    // La dérivée de la soustraction est la soustraction des dérives
    | BinOp(Substraction, e1, e2) ->
        BinOp(Substraction, derive variable e1, derive variable e2)
    
    // La dérivée d'une multiplication est l'addition du produit de la dérivée pour chaque terme constant
    | BinOp(Multiplication, e1, e2)
        -> BinOp(Addition,
                    BinOp(Multiplication, derive variable e1, e2),
                    BinOp(Multiplication, e1, derive variable e2))

    // La dérivée d'une division est la dérivée du produit inverse
    | BinOp(Division, e1, e2) ->
        derive variable (BinOp(Multiplication, e1, BinOp(Power, e2, Constant -1.)))

    // La dérivée d'une puissance est plus compliqué...
    // Pour le cas général, f(x)^g(x), la dérivée est:
    //      f(x)^(g(x) - 1) (g(x) f'(x) + f(x) ln(f(x)) g'(x))
    | BinOp(Power, e1, e2) ->

        let wnewBase = BinOp(Power, e1, BinOp(Substraction, e2, Constant 1.))
        let term1 = BinOp(Multiplication, e2, derive variable e1)
        let logterm = Function(Ln, e1)
        let term2 = BinOp(Multiplication, e1, logterm)
        let term2' = BinOp(Multiplication, term2, derive variable e2)
        let addition = BinOp(Addition, term1, term2')
        
        BinOp(Multiplication, wnewBase, addition)
    
    // La dérivée du sinus est le cosinus
    | Function(Sin, e) ->
        BinOp(Multiplication, Function(Cos, e), derive variable e)
        
    // La dérivée du cosinus est -sinus
    | Function(Cos, e) ->
        BinOp(Multiplication,
                BinOp(Multiplication, Constant -1., Function(Sin, e)),
                derive variable e)

    // La dérivée de la tangente est la dérivée de sin(x)/cos(x)
    | Function(Tan, e) ->
        derive variable <| BinOp(Division, Function(Sin, e), Function(Cos, e))

    // La dérivée de la cotangente est la dérivée de cos(x)/sin(x)
    | Function(Tan, e) ->
        derive variable <| BinOp(Division, Function(Cos, e), Function(Sin, e))

    // La dérivée du logarithme est l'inverse de son argument par la dérivée de son argument
    | Function(Ln, e) ->
        BinOp(Multiplication,
                BinOp(Division, Constant 1., e),
                derive variable e)

    // La dérivée de log2 est le produit de la dérivée du logarithme
    // par l'inverse du logarithme de la base (ici, ln 2)
    | Function(Log2, e) ->
        BinOp(Multiplication,
                BinOp(Division, Constant 1.,
                        BinOp(Multiplication, Function(Ln, Constant 2.), e)),
                derive variable e)

    // La dérivée de log10 est le produit de la dérivée du logarithme
    // par l'inverse du logarithme de la base (ici, ln 10)
    | Function(Log2, e) ->
        BinOp(Multiplication,
                BinOp(Division, Constant 1.,
                        BinOp(Multiplication, Function(Ln, Constant 10.), e)),
                derive variable e)

    // La dérivée de l'exponentielle est le produit de lui-même avec la dérivée de son argument 
    | Function(Exp, e) ->
        BinOp(Multiplication, Function(Exp, e), derive variable e)

    // La dérivée de la valeur absolue est le quotient entre le produit de l'expression
    // et sa dérivée et de la valeur absolue de l'expression
    | Function(Abs, e) ->
        BinOp(Division, BinOp(Multiplication, e, derive variable e), Function(Abs, e))

    // En cas d'exception...
    | _ -> raise (Exception "Not implemented yet...")

// Converti en chaine de caractère "lisible" une expression de l'AST
let rec pprint =
    function
    | Variable x -> x
    | Constant x -> sprintf "%.3g" x

    // (expr)'
    | Derivative(_, x) ->
        sprintf "(%s)'" (pprint x)
    
    // -1 * x <=> -x
    | BinOp(Multiplication, Constant -1., e) ->
        sprintf "-%s" (pprint e)

    // a + b
    | BinOp(op, e1, e2) when (not <| isBinOp e1) && (not <| isBinOp e2) ->
        sprintf "%s %s %s" (pprint e1) (ppop op) (pprint e2)

    // a + (expr)
    | BinOp(op, e1, e2) when (not <| isBinOp e1) ->
        sprintf "%s %s (%s)" (pprint e1) (ppop op) (pprint e2)

    // (expr) + b
    | BinOp(op, e1, e2) when (not <| isBinOp e2) ->
        sprintf "(%s) %s %s" (pprint e1) (ppop op) (pprint e2)

    // (expr) + (expr)
    | BinOp(op, e1, e2) ->
        sprintf "(%s) %s (%s)" (pprint e1) (ppop op) (pprint e2)

    // f(x)
    | Function(f, e) ->
        sprintf "%s(%s)" (f.ToString ()) (pprint e)

// Les différents opérateurs
and ppop =
    function
    | Addition -> "+"
    | Multiplication -> "*"
    | Division -> "/"
    | Substraction -> "-"
    | Power -> "**"

// Vérifie si une expression est une opération binaire
and isBinOp =
    function
    | BinOp(_,_,_) -> true
    | _ -> false

// Evalue l'expression donnée, calcul son résultat et le renvoie (float)
let rec evaluate expr =
    match expr with
    | Constant x -> x
    | BinOp(Addition, e1, e2) -> evaluate e1 + evaluate e2
    | BinOp(Substraction, e1, e2) -> evaluate e1 - evaluate e2
    | BinOp(Multiplication, e1, e2) -> evaluate e1 * evaluate e2
    | BinOp(Division, e1, e2) -> evaluate e1 / evaluate e2
    | BinOp(Power, e1, e2) -> evaluate e1 ** evaluate e2
    | Function(Sin, e) -> Math.Sin(evaluate e)
    | Function(Cos, e) -> Math.Cos(evaluate e)
    | Function(Tan, e) -> Math.Tan(evaluate e)
    | Function(Cot, e) -> Math.Cos(evaluate e) / Math.Sin(evaluate e)
    | Function(Ln, e) -> Math.Log(evaluate e)
    | Function(Log2, e) -> Math.Log2(evaluate e)
    | Function(Log10, e) -> Math.Log10(evaluate e)
    | Function(Exp, e) -> Math.Exp(evaluate e)
    | Function(Abs, e) -> Math.Abs(evaluate e)
    | x ->
        printfn "%A" x
        raise (Exception "Not computable")

    // Affiche 4 décimales
    |> fun x -> Math.Round (x, significantDigits)

// Applique le parser sur une chaine de caractère
//  let runTaylorParser text =
//      match run expr text with
//      | Success(r, _, _) -> Some r
//      | Failure(m, _, _) ->
//          printfn "Parsing error: %s" m
//          None

// Substitue les variables aux valeurs données
let rec substitute (expr: Expression) ((var, value): string * Number) =
    match expr with
    | Variable x when x = var ->
        Constant value
    
    | BinOp(op, e1, e2) ->
        BinOp(op, substitute e1 (var, value), substitute e2 (var, value))
    
    | Function(fn, e) ->
        Function(fn, substitute e (var, value))

    | e -> e

// Type d'alias, représente une liste de valeurs associées à une variable
type SNL = (string * Number) list

// Calcul le résultat de l'expression pour les valeurs données
let rec compute (expr': Expression) (values: SNL) =
    let mutable expr = expr'
    for v in values do
        expr <- substitute expr v
    evaluate expr

// Applique la formule d'erreur absolue de Taylor sur l'expression
let absoluteError (expr: Expression) (values: SNL) (errors: SNL) =
    let mutable result = 0.

    for (var, err) in errors do
        // Dérivée partielle par rapport à "var"
        let partial = derive var expr
        // Evaluation de la partielle
        let tmp' = compute partial values |> abs // On prend la valeur absolue
        // Prise en compte de l'erreur
        let tmp = tmp' * err
        // Addition au résultat
        result <- result + tmp

    result

// L'erreur relative est un pourcentage de l'erreur sur la valeur absolue
let relativeError (err: Number) (value: Number) = 100. * (err / value |> abs)

// Calcul de la moyenne d'une liste de nombre
let average (xs: Number list) = xs |> List.average

// Affiche les résultats
let runTaylor (quantityName: string) expr (measures: SNL list) (errors: SNL) =
    printfn "\n> Calcul de %d valeurs pour %s = %s\n" (measures.Length) quantityName (pprint expr)

    printfn "\tRésultat\tErr absolue\tErr relative\tEcriture normale\n"

    let mutable finalResults = []
    let mutable finalErrors = []

    for measure in measures do
        let result = compute expr measure
        let abserr = absoluteError expr measure errors
        let relerr = relativeError abserr result
        printfn "\t%.3g\t\t%.3g\t\t%.3g%%\t\t│ %s = (%.3g ± %.3g)" result abserr relerr quantityName result abserr
        finalResults <- finalResults @ [result]
        finalErrors <- finalErrors @ [abserr]

    let averageResult = average finalResults
    let averageError = average finalErrors

    printfn "\n> Moyenne des résultats : %.3g ± %.3g" averageResult averageError

    printfn ""

// Prend une list de mesures et renvoie une liste de tuple contenant la mesure et la variable donnée
let attribMeasuresToVariable (variable: string) (measures: Number list) =
    List.fold (fun acc x -> acc @ [variable, x]) ([variable, measures.[0]]) measures

// Fait une manipulation un peu sorcière (sur matrices) pour établir une bonne liste
let rec linkMeasures (measures: SNL list) =
    let mutable result : SNL list = []

    for i = 0 to measures.[0].Length - 1 do
        result <- result @ [getColumn measures i]

    List.tail result

and getColumn (xs: SNL list) i = [for ys in xs do ys.[i]]

let handle (program: TopLevel list) =
    let mutable function' = None
    let mutable errors = []
    let mutable constants = []
    let mutable measures = []

    for program in program do
        match program with
        | DefFunction(Variable name, e) ->
            if function' <> None
            then printfn "Une seule fonctione a la fois!"; exit 0
            function' <- Some (name, e)

        | DefConstant(Variable name, e) ->
            constants <- constants @ [name, evaluate e]

        | DefError(Variable name, ErrConstant e) ->
            errors <- errors @ [name, e]

        | DefMeasures(Variable name, es) ->
            measures <- measures @ [attribMeasuresToVariable name es]

        | DefSignificantDigits _ ->
            printfn "\nAffichage des resultats avec %d chiffres significatifs" significantDigits

        | _ -> raise (Exception "Not implemented yet...")

    if function' = None
    then printfn "Aucune fonction definie!"; exit 0

    if measures.IsEmpty && constants.IsEmpty
    then printfn "Il n'y a rien a faire."; exit 0

    if not <| List.forall (fun (xs: SNL) -> xs.Length = measures.[0].Length) measures
    then printfn "Toutes les mesures entrees ne sont pas en nombre equivalent!"; exit 0

    for cst in constants do
        function' <- Some ((fst function'.Value), (substitute (snd function'.Value) cst))

    let linked = (linkMeasures measures)

    runTaylor (fst function'.Value) (snd function'.Value) linked errors

[<EntryPoint>]
let main argv =

    // let test1 = let rand = Random() in [for _ in 1..2500 do yield rand.NextDouble() + 11.]
    // let test2 = let rand = Random() in [for _ in 1..2500 do yield rand.NextDouble() + 11.]


    let p = runParserOnFile parseProgram () "hello.txt"
    
    match p with
    | Success(program, _, _) -> handle program
    | Failure(msg, _, _) -> printfn "Erreur: %s" msg

    //let p = runParserOnFile parseProgram () "hello.txt" System.Text.Encoding.UTF8
    // printfn "%A" p

    //let expr = "n * R * T"
    //let parsed = (runTaylorParser expr).Value
    //let s = compute parsed ["p", 10.; "V", 300.; "n", 1.; "T", 300.; "R", 8.3145]
//
    //printfn "%g" s

    // let expr = "33 * a + b * sin(a + c)"
    // let m1 = [for i in 1..10 do yield float i]
    // let m2 = [for i in 1..10 do yield float i]
    // let m3 = [for i in 1..10 do yield float i]
    // let errors = ["a", 0.1; "b", 0.8; "c", 0.8]
// 
    // let m1' = attribMeasuresToVariable "a" m1
    // let m2' = attribMeasuresToVariable "b" m1
    // let m3' = attribMeasuresToVariable "c" m1
// 
    // let linked = linkMeasures [m1'; m2'; m3']
// 
    // let parsed = runTaylorParser expr
    // 
    // runTaylor "j" (parsed.Value) linked errors

    0
