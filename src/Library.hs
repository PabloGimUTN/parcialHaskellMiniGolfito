module Library where
import PdePreludat
import GHC.Generics (prec)

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = x `elem` [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--punto 1
type Palo = Jugador -> Tiro

putter :: Palo
putter  jugador = UnTiro{ velocidad = 10 , precision = precisionJugador ( habilidad jugador) * 2 , altura = 0}

madera :: Palo
madera  jugador = UnTiro{ velocidad = 100 , precision = precisionJugador (habilidad jugador) /2 , altura = 5}

hierros :: Number -> Palo
hierros n jugador = UnTiro { velocidad = fuerzaJugador (habilidad jugador) * n ,
precision = precisionJugador (habilidad jugador)/n, altura = max 0 (n-3)}

--punto1 comentado b

palos = [putter,madera]++ map hierros [1..10]


-- punto 2

golpe palo = palo 


mapTiro tiro velocidad precision altura = tiro {
    velocidad = velocidad,
    precision = precision,
    altura = altura
}

--obstaculo tiro 

tunelConRampita :: Tiro -> Tiro
tunelConRampita tiro 
    | ((>90).precision $ tiro)  && ((==0).altura $ tiro)  = mapTiro tiro  (velocidad tiro * 2)  100  0
    | otherwise = mapTiro tiro 0 0 0

laguna largoLaguna tiro 
    | ((>80).velocidad $ tiro)  &&  altura tiro >= 1 && altura tiro <= 5  = tiro{altura = altura tiro /largoLaguna}
    | otherwise = mapTiro tiro 0 0 0

hoyo tiro 
    | ((>=5).velocidad $ tiro) && ((<=20).velocidad $ tiro) && ((>95).precision $ tiro) && ((==0).altura $ tiro) 
    = mapTiro tiro 0 0 0
    | otherwise = mapTiro tiro 0 0 0
    


superaObstaculo obstaculo  jugador palo 
    | obstaculo (palo jugador) == UnTiro 0 0 0 = False
    | otherwise = True


--4 a
palosUtiles obstaculo jugador = filter (superaObstaculo obstaculo jugador) palos


--4 b 
superaTiro obstaculo tiro 
  | obstaculo tiro == UnTiro 0 0 0 = False
  | otherwise = True



--hago un lenh de los obstaculos que me filtre
--tengo que hacer una funcion que me agarre todos los obstaculos
--tengo una lista de obstaculos y un tiro , yo quiero que este tiro pase a medida de que su
--tiro no sea 0 , entonces lo que puedo hacer es



--5


jugadorGanador jugadorConPuntos = foldl1 (mayorSegun snd)  jugadorConPuntos


padreGanadorDelTorneo   jugadorConPuntos =  padre . fst. jugadorGanador $ jugadorConPuntos 