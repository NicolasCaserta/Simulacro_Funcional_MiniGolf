module Library where
import PdePreludat

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = UnaHabilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro
} deriving (Eq)

---------------- Jugadores de ejemplo ---------------- 

bart = UnJugador "Bart" "Homero" (UnaHabilidad 25 60)
todd = UnJugador "Todd" "Ned" (UnaHabilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (UnaHabilidad 10 1)

------------ Funciones Útiles Auxiliares -------------

between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun f lista = foldl1 (mayorSegun f) lista

mayorSegun :: Ord x => (t -> x) -> (t -> t -> t)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-------------------------------------------------------------- PUNTO 1 --------------------------------------------------------------

type Palo = Habilidad -> Tiro

putter :: Palo
putter habilidad = UnTiro {velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0}

madera :: Palo
madera habilidad = UnTiro {velocidad = 100, precision = precisionJugador habilidad `div` 2, altura = 5}

hierro :: Number -> Palo
hierro n habilidad = UnTiro {velocidad = fuerzaJugador habilidad * n, precision = precisionJugador habilidad `div` n, altura = (n - 3) `max` 0}

-- Constante con todos los palos posibles --

palos :: [Palo]
palos = [putter , madera] ++ map hierro [1..10]

-------------------------------------------------------------- PUNTO 2 --------------------------------------------------------------

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)

-------------------------------------------------------------- PUNTO 3 --------------------------------------------------------------

tiroDetenido = UnTiro 0 0 0

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal | (puedeSuperar obstaculo) tiroOriginal = (efectoLuegoDeSuperar obstaculo) tiroOriginal
                                                | otherwise = tiroDetenido

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && ((== 0).altura) tiro
efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiroOriginal = UnTiro {velocidad = velocidad tiroOriginal *2, precision = 100, altura = 0}

laguna :: Number -> Obstaculo
laguna largo = UnObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro
efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal `div` largo}

hoyo :: Obstaculo
hoyo = UnObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo tiro = (between 5 20 . velocidad) tiro && ((== 0).altura) tiro && precision tiro > 95
efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = tiroDetenido

-------------------------------------------------------------- PUNTO 4 --------------------------------------------------------------

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (leSirveParaSuperar jugador obstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar jugador obstaculo = puedeSuperar obstaculo . golpe jugador

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Number
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos) | puedeSuperar obstaculo tiro = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos
                                                                  | otherwise = 0

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos.golpe jugador) palos

-------------------------------------------------------------- PUNTO 5 --------------------------------------------------------------

type Puntos = Number

jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = (map (padre . jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = 
    (all ((< puntosGanados puntosDeUnJugador).puntosGanados) . filter (/= puntosDeUnJugador)) puntosDeTorneo