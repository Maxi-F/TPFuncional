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


nop :: Microprocesador -> Microprocesador
nop micro = micro {programCounter = siguienteInstruccion (programCounter micro)}

str :: Int -> Int -> Microprocesador -> Microprocesador
str addr val micro | addr > 1024 = micro{mensajeError = "ERROR: Posicion de memoria no existente"}
                   | otherwise = micro{memoria = take (addr-1) (memoria micro) ++ [val] ++ drop (addr) (memoria micro), programCounter = siguienteInstruccion (programCounter micro)}
