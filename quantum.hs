-- Librerias
import System.Environment (getArgs)
import System.Process (callCommand)
import Data.Aeson (ToJSON, toJSON, encode, object, (.=))
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

-- Operaciones con matrices y números complejos

-- Definimos los tipos de datos
data C = Complex Double Double deriving (Eq)
data M = Matrix [[C]] deriving (Eq)
data Circuit = Circuit {
    qubits :: Int,
    gates :: [(Int, String)]
}

-- Instancia Show para formatear la impresión de números complejos
instance Show C where
    show (Complex a b)
        | a /= 0 && b > 0 = show a ++ " + " ++ show b ++ "i"
        | a /= 0 && b < 0 = show a ++ " - " ++ show (abs b) ++ "i"
        | b == 0 = show a
        | otherwise = show b ++ "i"

-- Instancia Show para formatear la impresión de matrices
instance Show M where
    show (Matrix rows) = unlines [unwords [show elem | elem <- row] | row <- rows]

-- Instancia Show para fomrmatear la impresión de un circuito cuantico
instance Show Circuit where
    show (Circuit qubits gates) = unlines (map renderQubit [0 .. qubits -1])
        where
            renderQubit qubit = "q_" ++ show qubit ++ ": " ++ renderGates qubit
            renderGates qubit = concatMap (renderGate qubit) gates
            renderGate qubit (target, gate)
                | qubit == target = "─┤ " ++ gate ++ " ├─"
                | otherwise = "─────"

-- Instancia para serializar números complejos a JSON
instance ToJSON C where
    toJSON (Complex a b) = object [T.pack "real" .= a, T.pack "imag" .= b]

-- Instancia ToJSON para el tipo Matrix
instance ToJSON M where
    toJSON (Matrix rows) = object [T.pack "state" .= rows]

-- Constantes comunes
zeroC, realPos, realNeg, imgPos, imgNeg :: C
zeroC = Complex 0 0
realPos = Complex 1 0
realNeg = Complex (-1) 0
imgPos = Complex 0 1
imgNeg = Complex 0 (-1)

-- Definición de compuertas cuanticas
pauliX, pauliY, pauliZ, hadamard :: M
pauliX = (Matrix [[zeroC, realPos], [realPos, zeroC]])
pauliY = (Matrix [[zeroC, imgNeg], [imgPos, zeroC]])
pauliZ = (Matrix [[realPos, zeroC], [zeroC, realNeg]])    
hadamard = scalarMultiplyMatrix (1/(sqrt 2)) (Matrix [[realPos, realPos], [realPos, realNeg]])
phase = (Matrix [[realPos, zeroC], [zeroC, imgPos]])
-- piOverEight = (Matrix [[realPos, zeroC], [zeroC, (eulerExp (pi / 4))]])

-- Qubits
qubitZero = Matrix [[realPos],[zeroC]]

-- Puertas cuánticas avanzadas
cnotGate :: M
cnotGate = Matrix [[realPos, zeroC, zeroC, zeroC], 
                   [zeroC, realPos, zeroC, zeroC], 
                   [zeroC, zeroC, zeroC, realPos], 
                   [zeroC, zeroC, realPos, zeroC]]

swapGate :: M 
swapGate = Matrix [[realPos, zeroC, zeroC, zeroC], 
                   [zeroC, zeroC, realPos, zeroC], 
                   [zeroC, realPos, zeroC, zeroC], 
                   [zeroC, zeroC, zeroC, realPos]]

toffoliGate :: M 
toffoliGate = Matrix [[realPos, zeroC, zeroC, zeroC, zeroC, zeroC, zeroC, zeroC],
                      [zeroC, realPos, zeroC, zeroC, zeroC, zeroC, zeroC, zeroC],
                      [zeroC, zeroC, realPos, zeroC, zeroC, zeroC, zeroC, zeroC],
                      [zeroC, zeroC, zeroC, realPos, zeroC, zeroC, zeroC, zeroC],
                      [zeroC, zeroC, zeroC, zeroC, realPos, zeroC, zeroC, zeroC],
                      [zeroC, zeroC, zeroC, zeroC, zeroC, realPos, zeroC, zeroC],
                      [zeroC, zeroC, zeroC, zeroC, zeroC, zeroC, zeroC, realPos],
                      [zeroC, zeroC, zeroC, zeroC, zeroC, zeroC, realPos, zeroC]]

-- Operaciones básicas con números complejos

-- Expresión de Euler para complejos
eulerExp :: Double -> C
eulerExp x = Complex (cos x) (sin x)

-- Parte real
realC :: C -> Double
realC (Complex a b) = a

-- Parte imaginaria
imgC :: C -> Double
imgC (Complex a b) = b

-- Norma
normC :: C -> Double
normC (Complex a b) = sqrt (a ** 2 + b ** 2)

-- Conjugado
conjugateC :: C -> C
conjugateC (Complex a b) = Complex a (-1 *  b)

-- Suma
addC :: C -> C -> C
addC (Complex a b) (Complex c d) = Complex (a + c) (b + d)

-- Multiplicación
multiplyC :: C -> C -> C
multiplyC (Complex a b) (Complex c d) = Complex (a * c - b * d) (a * d + b * c)

-- Multiplicación por un escalar
scalarMultiplyC :: Double -> C -> C
scalarMultiplyC x (Complex a b) = Complex (x * a) (x * b)

-- División
divisionC :: C -> C -> C
divisionC z1 z2 = scalarMultiplyC (1 / normC z2 ** 2) (multiplyC z1 (conjugateC z2))

-- Funciones para determinar propiedades de matrices

-- Número de filas
countRows :: M -> Int
countRows (Matrix rows) = length rows

-- Número de columnas
countColumns :: M -> Int
countColumns (Matrix rows) = length (head rows)

-- Validación de una matriz
isMatrix :: M -> Bool
isMatrix (Matrix [[]]) = True
isMatrix (Matrix[ls]) = True
isMatrix (Matrix (row1:row2:rows)) = (length row1) == (length row2) && isMatrix (Matrix (row2:rows))

-- Verificación de una matriz cuadrada
isSquareMatrix :: M -> Bool
isSquareMatrix (Matrix rows)
    | isMatrix (Matrix rows) = (length rows) == (length (head rows))

-- Operaciones con matrices complejas

-- Multiplicación por un escalar 
scalarMultiplyMatrix :: Double -> M -> M
scalarMultiplyMatrix x (Matrix rows) = Matrix [[scalarMultiplyC x elem | elem <- row] | row <- rows]

-- Suma de matrices complejas
addMatrix :: M -> M -> M
addMatrix (Matrix rows1) (Matrix rows2) =
    Matrix (zipWith (zipWith addC) rows1 rows2)

-- Transposición de matrices
transposeMatrix :: M -> M
transposeMatrix (Matrix rows) = Matrix (transposeHelper rows)
    where
        transposeHelper [] = []
        transposeHelper ([]:_) = []
        transposeHelper rows = (map head rows) : transposeHelper (map tail rows)
    
-- Producto de matrices
matrixProduct :: M -> M -> M
matrixProduct (Matrix rows1) (Matrix rows2) =
    let transposed = case transposeMatrix (Matrix rows2) of Matrix t -> t
    in Matrix [[foldl1 addC (zipWith multiplyC row col) | col <- transposed] | row <- rows1]

-- Producto tensorial
tensorProduct :: M -> M -> M
tensorProduct (Matrix a) (Matrix b) = 
    Matrix [[multiplyC x y | y <- rowB] | rowA <- a, x <- rowA, rowB <- b]

-- Traza de una matriz
traceMatrix :: M -> C
traceMatrix (Matrix rows) 
    | isSquareMatrix (Matrix rows) = foldl1 addC [rows !! i !! i | i <- [0 .. (length rows) - 1]]

-- Simetria
isSymmetric :: M -> Bool
isSymmetric m = m == transposeMatrix m

-- Circuito cuántico

-- Agrega una puerta al circuito en un quibit específico
addGate :: Int -> String -> Circuit -> Circuit
addGate targetQubit gate (Circuit totalQubits gates)
    | targetQubit >= totalQubits = error "El índice del qubit es mayor al número de qubits en el circuito."
    | otherwise = Circuit totalQubits (gates ++ [(targetQubit, gate)])

-- Ejecuta el circuito
runCircuit :: Circuit -> M
runCircuit (Circuit totalQubits gates)
    | totalQubits <= 0 = error "El circuito no contiene qubits"
    | otherwise = foldl1 tensorProduct finalStates
        where
            finalStates = map (foldl1 matrixProduct) addQubits
            addQubits = reverse (map (\ x -> x++[qubitZero]) gateList)
            gateList = map (map gateMatrix) (map reverse [(filter (\ x -> x /= "Null") [if target == qubit then gate else "Null" | (target, gate) <- gates]) | qubit <- [0 .. totalQubits - 1]])
            gateMatrix "H" = hadamard
            gateMatrix "X" = pauliX
            gateMatrix "Y" = pauliY
            gateMatrix "Z" = pauliZ
            gateMatrix "P" = phase
            gateMatrix _ = error "Puerta no definida"

-- Medición de los estados mediante Prolog
measure :: M -> IO String
measure finalState = do
    let jsonFile = "state.json"
    B.writeFile jsonFile (encode finalState)
    callCommand "swipl -s measure.pl -g \"measure_from_file, halt.\""
    return "Medición completada en Prolog"

-- Ejemplo de construcción y uso del circuito
main :: IO ()
main = do
    putStrLn "Construcción de un circuito cuántico con diseño visual:"
    -- let circuit = addGate 0 "H" (addGate 0 "X" (Circuit 1 []))
    -- let circuit = addGate 0 "X" (addGate 0 "H" (Circuit 2 []))
    let circuit = addGate 2 "Y" (addGate 2 "Z" (addGate 1 "P" (addGate 0 "H" (addGate 0 "X" (Circuit 3 [])))))
    print circuit

    -- Ejecución del circuito
    putStrLn "Ejecución del circuito:"
    let finalState = runCircuit circuit
    print finalState

    -- Medición del estado final
    result <- measure finalState
    putStrLn result