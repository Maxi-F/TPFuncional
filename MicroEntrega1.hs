data Microprocesador = Microprocesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String
} deriving Show

type Instruccion = Microprocesador -> Microprocesador

creacionMemoriaVacia :: [Int]
creacionMemoriaVacia = replicate 1024 0

xt8088 = Microprocesador {
  memoria = creacionMemoriaVacia,
  acumuladorA = 0,
  acumuladorB = 0,
  programCounter = 0,
  mensajeError = ""
}

siguienteInstruccion :: Int -> Int
siguienteInstruccion pc = pc + 1

intercambioValorDeLista :: [Int] -> Int -> Int -> [Int]
intercambioValorDeLista memoria addr val = take (addr-1) memoria ++ [val] ++ drop addr memoria

nop :: Instruccion
nop micro = micro {programCounter = siguienteInstruccion (programCounter micro)}

str :: Int -> Int -> Instruccion
str addr val micro | addr > 1024 = micro{mensajeError = "ERROR: Posicion de memoria no existente"}
                   | otherwise = micro{memoria = intercambioValorDeLista (memoria micro) addr val, programCounter = siguienteInstruccion (programCounter micro)}

lod :: Int -> Instruccion
lod addr micro | addr > 1024 = micro{mensajeError = "ERROR: Posicion de memoria no existente"}
               | otherwise = micro{acumuladorA = (memoria micro)!!(addr-1), programCounter = siguienteInstruccion (programCounter micro)}

lodv :: Int -> Instruccion
lodv val micro = micro{acumuladorA = val, programCounter = siguienteInstruccion (programCounter micro)}

add :: Instruccion
add micro = micro{acumuladorA = (acumuladorA micro) + (acumuladorB micro), acumuladorB = 0, programCounter = siguienteInstruccion (programCounter micro)}

swap :: Instruccion
swap micro = micro{acumuladorA = acumuladorB micro, acumuladorB = acumuladorA micro, programCounter = siguienteInstruccion (programCounter micro)}

divide :: Microprocesador -> Microprocesador
divide micro | (acumuladorB micro)==0 = micro{mensajeError = "DIVISION BY ZERO"}
             | otherwise = micro{acumuladorA = div (acumuladorA micro) (acumuladorB micro), acumuladorB = 0, programCounter = siguienteInstruccion (programCounter micro)}
