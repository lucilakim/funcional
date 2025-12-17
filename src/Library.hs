module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{--Recuperatorio pdep - Aventureros
En un lugar muy lejano, existen aventureros que recorren un peligroso camino lleno de
encuentros especiales. Cada encuentro es con un ser o situación que promete cambiar el
estado del aventurero: aliviar su carga, aumentar su salud o modificar su coraje. Cada
aventurero tiene un criterio para decidir si acepta o no los cambios propuestos. Queremos
modelar todo este proceso.

Punto 1 - Una aventura es más divertida si huele a peligro…
De cada aventurero nos interesa conocer:
●Nombre
●Carga un número que indica la carga en kilos que lleva el aventurero.
●Salud un número entre 0 y 100 que representa su nivel de salud actual.
●Coraje que indica si el aventurero conserva el coraje o lo perdió.
●Criterio de Selección de Encuentros que dado el estado resultante del aventurero
tras un encuentro, determina si el aventurero está conforme.--}

data Aventurero = Aventurero {
    nombre :: String,
    carga :: Number,
    salud :: Number,
    coraje :: Bool, 
    criterioDeEleccion :: CriterioDeEleccion 
} deriving Show

type CriterioDeEleccion = Aventurero -> Bool 

{--Criterios de Elección:
●Conformista: Le viene bien cualquier resultado posible.
●Valiente: Acepta si después del encuentro el aventurero tiene coraje o si su salud
es mayor que 50.
●LightPacker: Acepta si la carga final es menor a un valor umbral configurable. Por
ejemplo puede pretender quedar con una carga menor a 15 kilos. Otros pueden ser
más exigentes y preferir quedar con menos de 12 kilos
Se pide definir un aventurero y sus criterios--}

-- Conformista: Le viene bien cualquier resultado posible.
conformista :: CriterioDeEleccion
conformista _ = True

-- Valiente: Acepta si después del encuentro el aventurero tiene coraje o si su salud es mayor que 50.
valiente :: CriterioDeEleccion
valiente aventurero = 
    coraje aventurero || tieneSaludMayorA 50 aventurero

tieneSaludMayorA :: Number -> Aventurero -> Bool
tieneSaludMayorA valor = (> valor) . salud

{--LightPacker: Acepta si la carga final es menor a un valor umbral configurable. Por
ejemplo puede pretender quedar con una carga menor a 15 kilos. Otros pueden ser
más exigentes y preferir quedar con menos de 12 kilos
Se pide definir un aventurero y sus criterios--}
lightPacker :: Number -> CriterioDeEleccion
lightPacker umbral = (< umbral) . carga  


{-- =====================================
Punto 2 - Casi Raiders of the lost Ark
Dada una lista de aventureros y utilizando exclusivamente funciones de orden superior y
aplicación parcial (sin recursividad) se pide:

a) Determinar si existe algún aventurero cuyo nombre contenga más de 5 letras.

b) Sumar la carga total de todos los aventureros cuya carga sea un número par.--}

-- a) Determinar si existe algún aventurero cuyo nombre contenga más de 5 letras.
algunoConNombreLargo :: [Aventurero] -> Bool
algunoConNombreLargo = any nombreLargo

nombreLargo :: Aventurero -> Bool
nombreLargo = (>5) . length . nombre

-- b) Sumar la carga total de todos los aventureros cuya carga sea un número par.
cargaTotalConCargaPar :: [Aventurero] -> Number
cargaTotalConCargaPar  =
        sum . map carga . conCargaNumeroPar

conCargaNumeroPar :: [Aventurero] -> [Aventurero]
conCargaNumeroPar = filter (even . carga) 

{-- ========================
    Punto 3 Ke personajes
Un encuentro con un personaje promete alterar el estado del aventurero. Todos los encuentros con personajes le descarga 1 kilo de su carga dado que siempre les deja un souvenir y además cuando se encuentra con:

● Curandero (Healer): Reduce la carga a la mitad y aumenta la salud un 20%.

● Inspirador (Inspirer): Otorga coraje y aumenta la salud en un 10% sobre su valor
actual.

● Embaucador (Trickster): Quita el coraje, suma 10 a la carga, lo deja con la mitad de
la salud y lo convence de que su criterio para los próximos encuentros tienen que
ser de LightPacker con un máximo de 10 kilos.
Debe evitar la repetición de lógica y respetar los límites mencionados anteriormente.--}
{--
encuentro :: Aventurero -> Personaje -> Aventurero
encuentro aventurero personaje = 
     cambioParticular personaje . cambioGeneral aventurero 

cambioGeneral :: Aventurero -> Aventurero 
cambioGeneral = (+ (-1)) carga

cambioParticular :: Aventurero -> Personaje -> Aventurero
cambioParticular aventurero personaje = aventurero
--}

-- Personajes (encuentros)
type Encuentro = Aventurero -> Aventurero
encuentroCurandero :: Encuentro 
encuentroCurandero = darSouvenir . accionCurandero 

encuentroInspirador :: Encuentro 
encuentroInspirador = darSouvenir . accionInspirador 

encuentroEmbaucador :: Encuentro 
encuentroEmbaucador = darSouvenir . accionEmbaucador 

darSouvenir :: Aventurero -> Aventurero 
darSouvenir = modificarCarga (+ (-1))  

modificarCarga :: (Number -> Number) -> Aventurero -> Aventurero
modificarCarga ajusteCarga aventurero = 
    aventurero {carga = ajusteCarga . carga $ aventurero}

accionCurandero ::  Aventurero -> Aventurero
accionCurandero =  reducirCarga (/2) . modificarSaludPorc 20

accionInspirador ::  Aventurero -> Aventurero
accionInspirador  = modificarCoraje True . modificarSaludPorc 10 

accionEmbaucador ::  Aventurero -> Aventurero
accionEmbaucador = 
    modificarCoraje False 
    . modificarCarga (+10) 
    . modificarSalud (/2) 
    . modificarCriterio (lightPacker 10)

modificarCriterio :: CriterioDeEleccion -> Aventurero  -> Aventurero
modificarCriterio nuevoCriterio aventurero =
    aventurero {criterioDeEleccion = nuevoCriterio}

modificarCoraje :: Bool -> Aventurero -> Aventurero
modificarCoraje valor aventurero = aventurero {coraje = valor}

reducirCarga :: (Number -> Number) -> Aventurero -> Aventurero
reducirCarga ajusteCarga aventurero = 
    aventurero { carga = ajusteCarga $ carga aventurero}

limitesSalud :: Number -> Number
limitesSalud  = min 100 . max 0 

modificarSalud :: (Number -> Number) -> Aventurero -> Aventurero
modificarSalud ajusteSalud aventurero =
    aventurero {salud = limitesSalud . ajusteSalud . salud $ aventurero}

modificarSaludPorc :: Number -> Aventurero -> Aventurero 
modificarSaludPorc porcentaje = 
    modificarSalud (* (1 + porcentaje/100)) 

{-- =====================================
    Punto 4 ¿A qué encuentros se enfrentaría un aventurero? (resolver
utilizando recursividad)

Dada una lista de encuentros, queremos determinar a cuáles de ellos se enfrentaría un
aventurero. La lógica es:

● Tras cada encuentro, el aventurero evalúa su criterio.

● Si el resultado no le satisface, no continúa con los encuentros siguientes.

● Si le satisface se produce el encuentro y pasa al siguiente.


Por ejemplo, si un aventurero con carga 6, salud 50, sin coraje, y criterio Valiente se
enfrenta a la lista [Curandero, Inspirador, Embaucador, Curandero]:

● Con Curandero: reduce la carga a 3 y queda con energía en 60 ⇒ cumple el criterio
(energía > 50).

● Con Inspirador: tiene coraje, salud aumenta a 66 ⇒ cumple el criterio.

● Con Embaucador: quita el coraje y suma 10 a la carga y su salúd queda en 33⇒ no
cumple criterio, entonces no se aplica y se descarta de la solución.
Automáticamente corta la ejecución y no evalúa los siguientes resultados.
Dada una persona (aventurero) y una lista de encuentros debe determinar la lista de
encuentros que realmente enfrentaría mientras cumple su criterio.--}

encuentrosPosibles ::  Aventurero -> [Encuentro] -> [Encuentro]
encuentrosPosibles _ [] = []
encuentrosPosibles aventurero (e:es)
    | aceptaEncuentro aventurero e  = e : encuentrosPosibles (e aventurero) es
    | otherwise = []

aceptaEncuentro :: Aventurero -> Encuentro -> Bool
aceptaEncuentro aventurero encuentro =
    criterioDeEleccion aventurero (encuentro aventurero)
    