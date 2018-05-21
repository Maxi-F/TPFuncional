--     CUANDO ES DOMINGO A LA NOCHE Y TODAVIA
--        TE FALTA TERMINAR EL TP DE PDEP
--
--                        _ ,___,-'",-=-. 
--           __,-- _ _,-'_)_  (""`'-._\ `. 
--        _,'  __ |,' ,-' __)  ,-     /. | 
--      ,'_,--'   |     -'  _)/         `\ 
--    ,','      ,'       ,-'_,`           : 
--    ,'     ,-'       ,(,-(              : 
--         ,'       ,-' ,    _            ; 
--        /        ,-._/`---'            / 
--       /        (____)(----. )       ,' 
--      /         (      `.__,     /\ /, 
--     :           ;-.___         /__\/| 
--     |         ,'      `--.      -,\ | 
--     :        /            \    .__/ 
--      \      (__            \    |_ 
--       \       ,`-, *       /   _|,\ 
--        \    ,'   `-.     ,'_,-'    \ 
--       (_\,-'    ,'\")--,'-'       __\ 
--        \       /  // ,'|      ,--'  `-. 
--         `-.    `-/ \'  |   _,'         `. 
--            `-._ /      `--'/             \ 
--               ,'           |              \ 
--              /             |               \ 
--           ,-'              |               / 
--          /                 |             -' 


module MicroEntrega where

import Text.Show.Functions

creacionMemoriaVacia :: Int -> [Int]
creacionMemoriaVacia espaciosVacios = replicate espaciosVacios 0

creacionMemoriaInfEnCero :: [Int]
creacionMemoriaInfEnCero = repeat 0

data Microprocesador = Microprocesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String,
  programas :: [Instruccion]
} deriving Show

type Instruccion = Microprocesador -> Microprocesador
type Programa = [Instruccion]

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

microMemoriaInfinita = Microprocesador {
  memoria = creacionMemoriaInfEnCero,
  acumuladorA = 2,
  acumuladorB = 3,
  programCounter = 0,
  mensajeError = "",
  programas = []
 }

microDesorden = Microprocesador{
  memoria = [2, 5, 1, 0, 6, 9] ++ (creacionMemoriaVacia 1018),
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
divide micro | (acumuladorB micro)==0 = nop micro{mensajeError = "DIVISION BY ZERO"}
             | otherwise = nop micro{acumuladorA = div (acumuladorA micro) (acumuladorB micro), acumuladorB = 0}

ifnz :: Programa -> Instruccion
ifnz instrucciones micro | (acumuladorA micro) == 0 = micro
                         | otherwise = foldl (ejecutarInstruccion.validacionAcumulador) micro instrucciones

validacionAcumulador :: Instruccion
validacionAcumulador micro | (acumuladorA micro) == 0 = micro{mensajeError = "ERROR: AcumuladorA en 0."}
                           | otherwise = micro

ejecutarPrograma :: Programa -> Microprocesador -> Microprocesador
ejecutarPrograma instrucciones micro = foldl (ejecutarInstruccion) micro instrucciones

ejecutarInstruccion :: Microprocesador -> Instruccion -> Microprocesador
ejecutarInstruccion micro instruccion | (mensajeError micro) /= [] = micro
                                      | otherwise = instruccion micro

cargarProgramas :: Programa -> Instruccion
cargarProgramas instrucciones micro = micro{programas = (programas micro) ++ instrucciones}

depurar :: Programa -> Microprocesador -> Programa
depurar instrucciones micro = filter (instruccionNecesaria micro) instrucciones

instruccionNecesaria :: Microprocesador -> Instruccion -> Bool
instruccionNecesaria micro instruccion =  any (/=0).microVacio.instruccion $ micro

microVacio :: Microprocesador -> [Int]
microVacio micro = [acumuladorA micro, acumuladorB micro] ++ memoria micro

estaOrdenadaLaMemoria :: Microprocesador -> Bool
estaOrdenadaLaMemoria = memoriaOrdenada.memoria

memoriaOrdenada :: [Int] -> Bool
memoriaOrdenada [] = True
memoriaOrdenada [_] = True
memoriaOrdenada (x1:x2:xs) = (x2 >= x1) && memoriaOrdenada (x2:xs)

programaSumador :: Programa
programaSumador = [(lodv 10),swap,(lodv 22),add]

programaDivisionCero :: Programa
programaDivisionCero = [(str 1 2),(str 2 0),(lod 2),swap,(lod 1),divide]

--programaDivisionDocePorCuatro :: Programa
--programaDivisionDocePorCuatro = divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12)
