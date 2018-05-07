--            ,,d00KKKKK0KKKKKKKKKKKKKKKKKKKKKk:,
--          ,,':OK0K0KK00KKKKKKKKKKKKKKKKKKKKK0Oxxxxxxddoollc:;
--       ;:clodO0KK00K00KKKKKKK0000OO0KKKKKKKKKKKKKKKKKKXKKKOxc
--  :cloxkO0KKKKKKKKK0Okkkxxxdddddddx0KKKKKKKKKKKKKK00Okxolc:,'
-- xO0KKKKKKKKKKKKKKKK0000OkOOOO0000KKKKKK0OOkkxxol::;,''..
-- 000KKKKKKKKKKKKKKKKK00000OOOkkkxxxxdddoollclll:'.
--  ::ccclllccccc:::;;;,,coolllllcclllllloooooolol:'.
--          .           .:oooollllllllloolllllclcll;
--         .            'cocclllll;,clccloolcclll;;:'.
--         .           .:oc:clooll:;llcclooolc:clcc:'
--          .         .;lllllccllllllllloloooll:clc,.
--          .  .';;;..;lllllollollllllloolloloolc;'.                PUNTITO MÁS POR PONER UN ASCII ART??
--          '..clccllcllllllloolooollllllllllolll;.
--          '.;ll::cloolooloooooooooooooooooolloll;.                      TUTOR USTED ES DIABÓLICO
--          '.,lolloolooloooooooooooooooooooooloooc,.
--           '.,cllcllooooloooooooooooooooolloooool;.
--                  ,colloololooooooooolooolccccc:;
--                 .'colooolloolooollooooooolllc:;,
--                 .'coloooooooollllllloooooooollollc;
--                 .'coolloool:.........',::;::cccccc;
--               .'';loollolll:...       .;'.
--              .':;:loooolllllllcc::;;;;:c;
--             ..:oc:cooloololllllllloool:,,


module MicroEntrega1 where

creacionMemoriaVacia :: Int -> [Int]
creacionMemoriaVacia espaciosVacios = replicate espaciosVacios 0

data Microprocesador = Microprocesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String
} deriving Show

type Instruccion = Microprocesador -> Microprocesador
type Programa = Microprocesador -> Microprocesador

xt8088 = Microprocesador {
  memoria = creacionMemoriaVacia 1024,
  acumuladorA = 0,
  acumuladorB = 0,
  programCounter = 0,
  mensajeError = ""
}

fp20 = Microprocesador {
  memoria = creacionMemoriaVacia 1024,
  acumuladorA = 7,
  acumuladorB = 24,
  programCounter = 0,
  mensajeError = ""
}

at8086 = Microprocesador {
  memoria = [1..20] ++ (creacionMemoriaVacia 1004),
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
str addr val micro | addr > 1024 = nop micro{mensajeError = "ERROR: Posicion de memoria no existente"}
                   | otherwise = nop micro{memoria = intercambioValorDeLista (memoria micro) addr val}

lod :: Int -> Instruccion
lod addr micro | addr > 1024 = nop micro{mensajeError = "ERROR: Posicion de memoria no existente"}
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

programaSumador :: Programa
programaSumador = add.(lodv 22).swap.(lodv 10)

programaDivisionCero :: Programa
programaDivisionCero = divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)

programaDivisionDocePorCuatro :: Programa
programaDivisionDocePorCuatro = divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12)
