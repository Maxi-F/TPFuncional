module MicroEntrega1 where

import Text.Show.Functions

creacionMemoriaVacia :: Int -> [Int]
creacionMemoriaVacia espaciosVacios = replicate espaciosVacios 0

data Microprocesador = Microprocesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String,
  programas :: [Instruccion]
} deriving Show

type Instruccion = Microprocesador -> Microprocesador
type Programa = Microprocesador -> Microprocesador

xt8088 = Microprocesador {
  memoria = creacionMemoriaVacia 1024,
  acumuladorA = 0,
  acumuladorB = 0,
  programCounter = 0,
  mensajeError = "",
  programas = []
}

fp20 = Microprocesador {
  memoria = creacionMemoriaVacia 1024,
  acumuladorA = 7,
  acumuladorB = 24,
  programCounter = 0,
  mensajeError = "",
  programas = []
}

at8086 = Microprocesador {
  memoria = [1..20] ++ (creacionMemoriaVacia 1004),
  acumuladorA = 0,
  acumuladorB = 0,
  programCounter = 0,
  mensajeError = "",
  programas = []
}

intercambioValorDeLista :: [Int] -> Int -> Int -> [Int]
intercambioValorDeLista memoria addr val = take (addr-1) memoria ++ [val] ++ drop addr memoria

nop :: Instruccion
nop micro = micro {programCounter = (programCounter micro) + 1}

str :: Int -> Int -> Instruccion
str addr val micro | addr > 1024 = micro{mensajeError = "ERROR: Posicion de memoria no existente"}
                   | otherwise = nop micro{memoria = intercambioValorDeLista (memoria micro) addr val}

lod :: Int -> Instruccion
lod addr micro | addr > 1024 = micro{mensajeError = "ERROR: Posicion de memoria no existente"}
               | otherwise = nop micro{acumuladorA = (memoria micro)!!(addr-1)}

lodv :: Int -> Instruccion
lodv val micro = nop micro{acumuladorA = val}

add :: Instruccion
add micro = nop micro{acumuladorA = (acumuladorA micro) + (acumuladorB micro), acumuladorB = 0}

swap :: Instruccion
swap micro = nop micro{acumuladorA = acumuladorB micro, acumuladorB = acumuladorA micro}

divide :: Instruccion
divide micro | (acumuladorB micro)==0 = micro{mensajeError = "DIVISION BY ZERO"}
             | otherwise = nop micro{acumuladorA = div (acumuladorA micro) (acumuladorB micro), acumuladorB = 0}

puntoDeCorteIFNZ :: [Instruccion] -> Microprocesador -> Bool
puntoDeCorteIFNZ cola micro =  (length cola) == 0 || ((mensajeError micro) /= []) || (acumuladorA micro) == 0

ifnz :: [Instruccion] -> Instruccion
ifnz (instruccion:cola) micro | puntoDeCorteIFNZ cola micro = micro
                              | otherwise = ifnz cola (instruccion micro)

cargarProgramas :: [Instruccion] -> Programa
cargarProgramas instrucciones micro = micro{programas = (programas micro) ++ instrucciones}

programaSumador :: Programa
programaSumador = add.(lodv 22).swap.(lodv 10)

programaDivisionCero :: Programa
programaDivisionCero = divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)

programaDivisionDocePorCuatro :: Programa
programaDivisionDocePorCuatro = divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12)
