type Posicion = Int
type Dato = Int

data Microprocesador = Microprocesador {
  memoria :: [(Posicion, Dato)],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String
} deriving Show

xt8088 = Microprocesador {
  memoria = [],
  acumuladorA = 0,
  acumuladorB = 0,
  programCounter = 0,
  mensajeError = ""
}

nop microprocesador = microprocesador{programCounter = (programCounter microprocesador) + 1}
