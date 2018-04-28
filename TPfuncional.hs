type Memoria = [(Int, Int)]

data Microprocesador = Microprocesador {
  posiciones :: [Int],
  acumuladorA :: Int,
  acumuladorB :: Int,
  programCounter :: Int,
  mensajeError :: String
}
