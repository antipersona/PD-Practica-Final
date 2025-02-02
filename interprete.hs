import System.Environment
------------------
----- MAIN -------
------------------

main :: IO ()
main = do
  args <- getArgs
  r <- if null args then modoInteractivo else modoArgs args
  print r

modoInteractivo :: IO Int
modoInteractivo = do
    putStrLn "Modo interactivo:"
    putStrLn "Lista de funciones:"
    putStrLn showFunciones
    putStrLn "Ingrese el nombre de la funcion que desea ejecutar (sin parentesis):"
    nombreFun <- getLine
    let (programa, e_ini) = funcion nombreFun
    if null programa then do
        putStrLn "La funcion no existe"
        return 0
    else do
        putStrLn "¿Desea definir su propio estado inicial? (s/n)"
        respuesta <- getLine
        if respuesta == "s" then do
            putStrLn "Ingrese los nombres de las variables en el siguiente formato: nombre valor nombre valor..."
            variables <- getLine
            let variables' = words variables
            let v_ini = ("R", 0) : procesarArgs variables'
            putStrLn "Ejecutando..."
            let v_fin = ejecutar programa (v_ini, [])
            -- print v_fin
            return (valor "R" v_fin)
        else do
            putStrLn "Ejecutando..."
            let v_fin = ejecutar programa e_ini
            -- print v_fin
            return (valor "R" v_fin)


modoArgs :: [String] -> IO Int
modoArgs args = let (programa, (v_ini, v_fun)) = funcion (head args) in
                if null programa then do
                    putStrLn "La funcion no existe"
                    return 0
                else do
                if length args == 1 then -- solo se pasa el nombre de la funcion
                    let v_fin = ejecutar programa (v_ini, v_fun)
                    in return (valor "R" v_fin)
                else -- se pasa el nombre de la funcion y el estado inicial
                    let v_ini' = ("R", 0) : procesarArgs (tail args)
                        v_fin = ejecutar programa (v_ini', [])
                    in return (valor "R" v_fin)


procesarArgs :: [String] -> [Variable]
procesarArgs [] = [] -- si el array esta vacio, devolvemos un array vacio
procesarArgs [_] = [] -- si el array tiene un solo elemento, devolvemos un array vacio (no hay valor para el ultimo nombre)
procesarArgs (x : y : xs) = (x, (read y)) : procesarArgs xs


----------------------------------------------------------------
--- EJECUTAR ---------------------------------------------------
----------------------------------------------------------------

ejecutar :: Programa -> Estado -> Estado
ejecutar [] s = s
ejecutar (i:resto) s = let s' = ejecutarInstruccion i s
    in ejecutar resto s'

ejecutarInstruccion :: Instruccion -> Estado -> Estado
-- v := e
ejecutarInstruccion (Asignacion v e) s = insertar (v, evaluarA e s) s

-- If e p1 p2
ejecutarInstruccion (If e p1 p2) (v_ini, v_fun)
    | evaluarL e (v_ini, v_fun) = let (v_ret, _) = ejecutar p1 (v_ini++v_fun, []) in separarVariables v_ret v_ini v_fun
    | otherwise = let (v_ret, _) = ejecutar p2 (v_ini++v_fun, []) in separarVariables v_ret v_ini v_fun

-- While e p
ejecutarInstruccion (While e p) (v_ini, v_fun)
    | evaluarL e (v_ini, v_fun) = let   (v_ret, _) = ejecutar p (v_ini++v_fun, [])
                                        s = separarVariables v_ret v_ini v_fun in
                                ejecutarInstruccion (While e p) s -- volvemos a ejecutar el while con v_ini cambiado
    | otherwise = (v_ini, v_fun)



-- separar las variables internas de la funcion de las variables iniciales ya que las funciones las devuelven juntas
separarVariables :: [Variable] -> [Variable] -> [Variable] -> Estado
separarVariables v_ret v_ini v_fun = let v_fun' = filter (\(n, _) -> pertenece n v_fun) v_ret   -- variables que estaban en v_fun
                                         v_ini' = filter (\(n, _) -> pertenece n v_ini) v_ret   -- variables que estaban en v_ini
                                     in (v_ini', v_fun')

-- TODO podria implementar llamada a funcion


-- un programa es una lista de instrucciones
-- una instruccion es un comando y sus argumentos
-- tipos de instrucciones:
    -- asignacion/declaracion de variables
        -- variable =: expresion_aritmetica
    -- bucles (while)
        -- while expresion_logica programa
    -- condiciones (if)
        -- if expresion_logica programa_true programa_falsev_fun

-- expresion_aritmetica
    -- numero
    -- variable
    -- expresion_aritmetica operador_aritmetico expresion_aritmetica

-- expresion_logica
    -- expresion_aritmetica operador_logico expresion_aritmetica
    -- booleano (true/false)
    -- expresion_logica operador_logico expresion_logica
    -- negacion expresion_logica

----------------------------------------------------------------
--- VARIABLES Y ESTADO -----------------------------------------
----------------------------------------------------------------
    -- nombre, valor

type Variable = (String, Int)

-- devuelve el valor de una variable
valor :: String -> Estado -> Int
valor n (v_ini, v_fun)  | pertenece n v_ini = valorAux n v_ini -- si es una variable inicial
                        | pertenece n v_fun = valorAux n v_fun -- si es una variable de la funcion
                        | otherwise = error ("la variable " ++ n ++ " no existe")
    where
        valorAux :: String -> [Variable] -> Int
        valorAux n vs = snd (head (filter (\(n', _) -> n' == n) vs))

-- inserta una variable en el estado
insertar :: Variable -> Estado -> Estado
insertar (n, v) (v_ini, v_fun)  | elem n (map fst v_ini) = (insertarAux (n,v) v_ini, v_fun) -- si es una variable inicial
                                | otherwise = (v_ini, insertarAux (n,v) v_fun) -- si es una variable de la funcion
    where
        insertarAux :: Variable -> [Variable] -> [Variable]
        insertarAux var vs = filter (\(n', _) -> n' /= n) vs ++ [var]

pertenece :: String -> [Variable] -> Bool
pertenece _ [] = False
pertenece n vs = elem n (map fst vs)

-- variables_iniciales, variables_de_la_funcion
type Estado = ([Variable], [Variable])

----------------------------------------------------------------
--- EXPRESIONES ARITMETICAS ------------------------------------
----------------------------------------------------------------
    -- numero
    -- variable
    -- expresion_aritmetica operador_aritmetico expresion_aritmetica

type OperadorAritmetico = Int -> Int -> Int

type E_A = ExpresionAritmetica
data ExpresionAritmetica = I Int
                         | V String
                         | E_A OperadorAritmetico E_A E_A

-- devuelve el valor de una expresion aritmetica
evaluarA :: E_A -> Estado -> Int
evaluarA (I n) s = n
evaluarA (V v) s = valor v s
evaluarA (E_A op a b) s = op (evaluarA a s) (evaluarA b s)

----------------------------------------------------------------
--- OPERADORES ARITMETICOS--------------------------------------
----------------------------------------------------------------

infixl 6 +:, -:

(+:) :: E_A -> E_A -> E_A
(+:) = E_A (+) -- (+:) a b = E_A (+) a b

(-:) :: E_A -> E_A -> E_A
(-:) = E_A (-)

infixl 7 *:, /:

(*:) :: E_A -> E_A -> E_A
(*:) = E_A (*)

(/:) :: E_A -> E_A -> E_A
(/:) = E_A div


----------------------------------------------------------------
--- EXPRESIONES LÓGICAS ----------------------------------------
----------------------------------------------------------------
    -- expresion_aritmetica operador_logico expresion_aritmetica
    -- booleano (true/false)
    -- expresion_logica conjuncion expresion_logica
    -- negacion expresion_logica

type Conjuncion = Bool -> Bool -> Bool
type OperadorLogico = Int -> Int -> Bool
type E_L = ExpresionLogica

data ExpresionLogica = Comp OperadorLogico E_A E_A
                     | B Bool
                     | E_L Conjuncion E_L E_L
                     | Not E_L

-- devuelve el valor de una expresion logica
evaluarL :: E_L -> Estado -> Bool
evaluarL (Comp op a b) s = op (evaluarA a s) (evaluarA b s)
evaluarL (B a) s = a
evaluarL (E_L conj a b) s = conj (evaluarL a s) (evaluarL b s)
evaluarL (Not a) s = not (evaluarL a s)


----------------------------------------------------------------
--- OPERADORES LOGICOS------------------------------------------
----------------------------------------------------------------

infixl 4 <:, >:, <=:, >=:, ==:, !=:

(<:) :: E_A -> E_A -> E_L
(<:) = Comp (<)

(>:) :: E_A -> E_A -> E_L
(>:) = Comp (>)

(<=:) :: E_A -> E_A -> E_L
(<=:) = Comp (<=)

(>=:) :: E_A -> E_A -> E_L
(>=:) = Comp (>=)

(==:) :: E_A -> E_A -> E_L
(==:) = Comp (==)

(!=:) :: E_A -> E_A -> E_L
(!=:) = Comp (/=)

----------------------------------------------------------------
--- INSTRUCCION ------------------------------------------------
----------------------------------------------------------------

data Instruccion = Asignacion String E_A
                 | While E_L Programa
                 | If E_L Programa Programa

type Programa = [Instruccion]

----------------------------------------------------------------
--- ASIGNACION -------------------------------------------------
----------------------------------------------------------------
    -- variable =: expresion_aritmetica
infix 1 =:
(=:) :: String -> E_A -> Instruccion
v =: e = Asignacion v e

----------------------------------------------------------------
--- BUCLE ------------------------------------------------------
----------------------------------------------------------------
    -- While expresion_logica programa

----------------------------------------------------------------
--- CONDICION --------------------------------------------------
----------------------------------------------------------------
    -- If expresion_logica programa_true programa_false

----------------------------------------------------------------
--- FUNCIONES --------------------------------------------------
----------------------------------------------------------------

listaDeFunciones :: [(String, String, Programa, Estado)]
listaDeFunciones = [("suma", "suma(a,b)", suma, estadoSuma),
                    ("suma2","suma2()", suma2, estadoSuma2),
                    ("asignarPi", "asignarPi()", asignarPi, estadoAsignarPi),
                    ("asignar", "asignar(i)", asignarI, estadoAsignar),
                    ("mayor", "mayor(a,b)", mayor, estadoMayor),
                    ("factorial", "factorial(X)", factorial, estadoFactorial)
                   ]

nombreFuncion :: (String, String, Programa, Estado) -> String
nombreFuncion (_, n, _, _) = n

-- devuelve una funcion por su nombre
funcion :: String -> (Programa, Estado)
funcion n = let lista = filter (\(n', _, _, _) -> n' == n) listaDeFunciones in
            if null lista then ([], ([], [])) -- si no existe la funcion
            else let (_, _, p, (e1, e2)) = head lista in (p, (("R", 0):e1, e2)) -- añadimos tmabien el return (R, 0)

-- imprime una lita con los nombres() de las funciones
showFunciones :: String
showFunciones = let nombres = map nombreFuncion listaDeFunciones in show nombres

-- lista de funiones
suma :: Programa
suma = ["R" =: V "a" +: V "b"]
estadoSuma :: Estado
estadoSuma = ([("a", 3), ("b", 5)], [])

suma2 :: Programa
suma2 = ["R" =: V "a" +: V "b"]
estadoSuma2 :: Estado
estadoSuma2 = ([("b", 5), ("a", 3)], [])

asignarPi :: Programa
asignarPi = ["R" =: I 3]
estadoAsignarPi :: Estado
estadoAsignarPi = ([], [])

asignarI :: Programa
asignarI = ["R" =: V "i"]
estadoAsignar :: Estado
estadoAsignar = ([("i",8)], [])

mayor :: Programa -- if (a > b) return a; else return b;
mayor = [ If (V "a" >: V "b")
                ["R" =: V "a"]
                ["R" =: V "b"]
            ]
estadoMayor :: Estado
estadoMayor = ([("a", 3), ("b", 5)], [])

factorial :: Programa
factorial = [   "Y" =: V "X",
                "R" =: I 1,
                While (I 0 <: V "Y") [
                    "R" =: V "R" *: V "Y",
                    "Y" =: V "Y" -: I 1
                    ]
            ]
estadoFactorial :: Estado
estadoFactorial = ([("X", 5)], [])