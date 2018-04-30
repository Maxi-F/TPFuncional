data Microprocesador = Microprocesador {
  memoria :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String
} deriving Show

type Instruccion = Microprocesador -> Microprocesador
type Programa = Microprocesador -> Microprocesador

creacionMemoriaVacia :: Int -> [Int]
creacionMemoriaVacia espaciosVacios = replicate espaciosVacios 0

xt8088 = Microprocesador {
  memoria = creacionMemoriaVacia 1024,
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

divide :: Instruccion
divide micro | (acumuladorB micro)==0 = micro{mensajeError = "DIVISION BY ZERO", programCounter = siguienteInstruccion (programCounter micro)}
             | otherwise = micro{acumuladorA = div (acumuladorA micro) (acumuladorB micro), acumuladorB = 0, programCounter = siguienteInstruccion (programCounter micro)}

fp20 = Microprocesador {
  memoria = creacionMemoriaVacia 1024,
  acumuladorA = 7,
  acumuladorB = 24,
  programCounter = 0,
  mensajeError = ""
}

programaSumador :: Programa
programaSumador = add.(lodv 22).swap.(lodv 10)

at8086 = Microprocesador {
  memoria = [1..20],
  acumuladorA = 0,
  acumuladorB = 0,
  programCounter = 0,
  mensajeError = ""
}

programaDivisionCero :: Programa
programaDivisionCero = divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)

programaDivisionDocePorCuatro :: Programa
programaDivisionDocePorCuatro = divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12)
