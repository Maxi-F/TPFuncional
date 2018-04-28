data Microprocesador = Microprocesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String
} deriving Show

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

nop :: Microprocesador -> Microprocesador
nop micro = micro {programCounter = siguienteInstruccion (programCounter micro)}

str :: Int -> Int -> Microprocesador -> Microprocesador
str addr val micro | addr > 1024 = micro{mensajeError = "ERROR: Posicion de memoria no existente"}
                   | otherwise = micro{memoria = intercambioValorDeLista (memoria micro) addr val, programCounter = siguienteInstruccion (programCounter micro)}

lod :: Int -> Microprocesador -> Microprocesador
lod addr micro | addr > 1024 = micro{mensajeError = "ERROR: Posicion de memoria no existente"}
               | otherwise = micro{acumuladorA = (memoria micro)!!(addr-1), programCounter = siguienteInstruccion (programCounter micro)}

lodv :: Int -> Microprocesador -> Microprocesador
lodv val micro = micro{acumuladorA = val, programCounter = siguienteInstruccion (programCounter micro)}

add :: Microprocesador -> Microprocesador
add micro = micro{acumuladorA = acumuladorA + acumuladorB, acumuladorB = 0, programCounter = siguienteInstruccion (programCounter micro)}

divide :: Microprocesador -> Microprocesador
divide micro | acumuladorB == 0 = micro{mensajeError = "DIVISION BY ZERO”}
             | otherwise = micro{acumuladorA = acumuladorA / acumuladorB, acumuladorB = 0, programCounter = siguienteInstruccion (programCounter micro)}

swap :: Microprocesador -> Microprocesador
swap micro = micro{acumuladorA = acumuladorB, acumuladorB = acumuladorA, programCounter = siguienteInstruccion (programCounter micro)}

