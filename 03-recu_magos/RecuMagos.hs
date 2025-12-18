module RecuAventureros where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{--Harry Potter y el Examen de Funcional

Nos piden modelar un programa que permita analizar los conflictos entre magos y, obviamente, nuestro
primer impulso es hacerlo en funcional. De los magos, sabemos que tienen un nombre, una edad y
cierta cantidad de salud. Además, cada mago conoce un conjunto de hechizos el cual puede usar para
hacerle cosas raras a otros magos. 

Se pide resolver los siguientes puntos, aprovechando al máximo los conceptos del paradigma funcional:

1. Elegir un tipo de dato con el que representar a los Magos y los Hechizos (justificando
brevemente la elección) de forma tal que se respete la descripción previa del dominio--}

data Mago = Mago {
    nombre :: String,
    edad :: Number,
    salud :: Number,
    hechizos ::[Hechizo]
} deriving Show
-- Elijo modelar Mago con un data con la notacion Record Syntax 
-- ya que me permite acceder mas facilmente 
-- setear sus atributos cuando defino un nuevo mago, sin importar el orden de los parametros
-- y acceder y modificar los atributos del mago de una forma mas sencilla
-- y nos brinda las funciones de sus atributos

type Hechizo = Mago -> Mago
-- Elijo modelar Hechico como una funcion, que toma un mago 
-- le realiza una trasformacion al mago
-- devuelve a un nuevo mago con la transformacion
-- Ya que en el dominio se menciona que el hechizo le hace cosas un mago


{--  y sea
posible modelar los siguientes hechizos:

a. curar: Este hechizo hace que el mago sobre el que lanzamos el hechizo recupere una
cierta cantidad de salud. Este hechizo puede usarse para curar distintas cantidades de
vida.

b. lanzarRayo: Este hechizo le hace daño al mago sobre el que se lanza. Si la salud de
dicho mago es mayor a 10, le hace 10 puntos de daño, de lo contrario le quita la mitad de
su vida actual.

c. amnesia: El mago objetivo olvida los primeros N hechizos que conozca. Se puede lanzar
este hechizo con diferentes valores de N.

d. confundir: El mago objetivo se ataca a sí mismo con su primer hechizo--}

-- a. curar: Este hechizo hace que el mago sobre el que lanzamos el hechizo recupere una cierta cantidad de salud. 
-- Este hechizo puede usarse para curar distintas cantidades de vida.

curar :: Number -> Hechizo
curar ajusteSalud  = modificarSalud  (+ ajusteSalud)

modificarSalud :: (Number -> Number)  -> Mago -> Mago
modificarSalud ajusteSalud mago =
    mago {salud = ajusteSalud . salud $ mago}

-- b. lanzarRayo: Este hechizo le hace daño al mago sobre el que se lanza. 
--Si la salud de dicho mago es mayor a 10, le hace 10 puntos de daño, 
-- de lo contrario le quita la mitad de su vida actual.    
lanzarRayo :: Hechizo
lanzarRayo mago 
    | saludMayorA 10 mago = modificarSalud (subtract 10) $ mago
    | otherwise = modificarSalud (/2) mago

saludMayorA :: Number -> Mago -> Bool
saludMayorA valor = (> valor) . salud 

-- d. confundir: El mago objetivo se ataca a sí mismo con su primer hechizo
confundir :: Hechizo
confundir  mago =  primerHechizo mago mago

primerHechizo :: Mago -> Hechizo
primerHechizo = head . hechizos 

{-- ===============================
    2. Modelar las siguientes funciones:

a. poder
El poder de un mago es su salud sumada al resultado de multiplicar su edad por la
cantidad de hechizos que conoce.

b. daño
Esta función retorna la cantidad de vida que un mago pierde si le lanzan un hechizo.

c. diferenciaDePoder
La diferencia de poder entre dos magos es el valor absoluto de la resta del poder de cada uno. Esto siempre retorna un número positivo.--}

-- a. poder
--El poder de un mago es su salud 
--sumada al resultado de multiplicar su edad 
--por la cantidad de hechizos que conoce.

poder :: Mago -> Number
poder mago =  (+ ((* cantidadHechizos mago) . edad $ mago)) .salud $ mago

cantidadHechizos :: Mago -> Number
cantidadHechizos  = length . hechizos

--b. daño
--Esta función retorna la cantidad de vida que un mago pierde si le lanzan un hechizo.
danio :: Mago -> Hechizo -> Number
danio mago hechizo = subtract (salud (hechizo mago)) . salud $ mago
--danio mago hechizo = (+ (- salud (hechizo mago))) . salud $ mago

-- c. diferenciaDePoder
-- La diferencia de poder entre dos magos es el valor absoluto de la resta del poder de cada uno. Esto siempre retorna un número positivo.--}
diferenciaPoder :: Mago -> Mago -> Number
diferenciaPoder mago1 = abs .  subtract (poder mago1) . poder 

{-- =========================
    3. Dada una Academia, la cual representamos con el siguiente tipo de dato:

data Academia = Academia {
magos :: [Mago],
examenDeIngreso :: Mago -> Bool
}

Se pide escribir el código necesario para realizar las siguientes consultas sin usar recursividad:

a. Saber si hay algún mago sin hechizos cuyo nombre sea “Rincenwind”.

b. Saber si todos los magos viejos (>50) son ñoños. 
Esto ocurre si tienen más hechizos que salud.

c. Conocer la cantidad de magos que no pasarían el examen de ingreso si lo volvieran a rendir.

d. Averiguar la sumatoria de la edad de los magos que saben más de 10 hechizos. --}

data Academia = Academia {
magos :: [Mago],
examenDeIngreso :: Mago -> Bool
} deriving Show

-- Se pide escribir el código necesario para realizar las siguientes consultas sin usar recursividad:
-- a. Saber si hay algún mago sin hechizos cuyo nombre sea “Rincenwind”.
algunRincenwindSinHechizo :: Academia -> Bool
algunRincenwindSinHechizo  =
    any esRincenwindSinHechizo . magos 

esRincenwindSinHechizo :: Mago -> Bool
esRincenwindSinHechizo mago =  esRincenwind mago && sinHechizo mago

sinHechizo :: Mago -> Bool
sinHechizo = null . hechizos

esRincenwind :: Mago -> Bool
esRincenwind = (== "Rincenwind") . nombre

-- b. Saber si todos los magos viejos (>50) son ñoños. 
--Esto ocurre si tienen más hechizos que salud.
sonTodosViejosYNionios :: Academia -> Bool
sonTodosViejosYNionios = all esViejoYNionio . magos

esViejoYNionio :: Mago -> Bool
esViejoYNionio mago = esViejo mago && esNioNio mago

esViejo :: Mago -> Bool
esViejo = (>50) . edad

esNioNio :: Mago -> Bool
esNioNio mago =  (> salud mago) . length . hechizos $ mago

-- c. Conocer la cantidad de magos que no pasarían el examen de ingreso si lo volvieran a rendir.
quienesNoPasarianExamenIngreso :: Academia -> Number
quienesNoPasarianExamenIngreso academia =
    length . filter (noApruebaExamen academia) $ magos academia

noApruebaExamen :: Academia -> Mago -> Bool
noApruebaExamen academia  = not . examenDeIngreso academia

-- d. Averiguar la sumatoria de la edad de los magos que saben más de 10 hechizos. 
sumaEdadMagosSabios :: Academia -> Number
sumaEdadMagosSabios academia =
        sum . map edad . filter (sabeMasHechizosQue 10) $ magos academia

sabeMasHechizosQue :: Number -> Mago -> Bool
sabeMasHechizosQue cantHechizos = 
    (> cantHechizos) . length . hechizos 

{-- ==============================
4. Dadas las siguientes funciones:

maximoSegun criterio valor comparables =
foldl1 (mayorSegun $ criterio valor) comparables

mayorSegun evaluador comparable1 comparable2
| evaluador comparable1 >= evaluador comparable2 = comparable1
| otherwise = comparable2

Usar la función maximoSegun para definir las siguientes funciones, 
sin repetir código ni definir funciones auxiliares:

i. mejorHechizoContra
Dados dos magos, retorna el hechizo del segundo que le haga más daño al
primero.

ii. mejorOponente
Dado un mago y una academia, retorna el mago de la academia que tenga la
mayor diferencia de poder con el mago recibido. --}

maximoSegun criterio valor comparables =
    foldl1 (mayorSegun $ criterio valor) comparables

mayorSegun evaluador comparable1 comparable2
    | evaluador comparable1 >= evaluador comparable2 = comparable1
    | otherwise = comparable2

-- i. mejorHechizoContra
-- Dados dos magos, retorna el hechizo del segundo que le haga más daño al
-- primero.
mejorHechizoContra :: Mago -> Mago -> Hechizo
mejorHechizoContra mago1 mago2 =
    maximoSegun danio mago1 (hechizos mago2)

-- ii. mejorOponente
-- Dado un mago y una academia, retorna el mago de la academia que tenga la
-- mayor diferencia de poder con el mago recibido. 
mejorOponente :: Mago -> Academia -> Mago
mejorOponente magoRecibido academia = 
    maximoSegun diferenciaPoder magoRecibido (magos academia)


{-- ================================================
5. Definir la siguiente función sin utilizar recursividad:
noPuedeGanarle
Decimos que el segundo mago no puede ganarle al primero si, 
luego de hechizarlo con todos los hechizos que conoce (uno atrás del otro) 
la salud del primer mago sigue siendo la misma. --}    
noPuedeGanarle :: Mago -> Mago -> Bool
noPuedeGanarle mago1 =
    (== salud mago1) . (salud . foldl aplicarHechizo mago1 . hechizos) 

aplicarHechizo :: Mago -> Hechizo -> Mago
aplicarHechizo mago hechizo = hechizo mago 
