module RecuHolidays where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


{--Haskell Holidays!
Es tiempo de vacaciones... no importa cuando leas esto. Así que vamos a recrearnos la imaginación a
partir de un negocio que modela tomarse un descanso visitando diferentes lugares.

El dominio
Una persona tiene un nivel de stress que es numérico, un nombre, las preferencias (le puede gustar el
“mar”
, la “gastronomía”
, etc.), tiene una determinada cantidad de amigues. Existen los contingentes, que
representan personas que quieren encarar unas vacaciones juntes.--}

data Persona = Persona {
    nombre :: String,
    nivelDeEstress :: Number,
    preferencias :: [String],
    cantAmigos :: Number
} deriving (Show)

type Contingentes = [Persona]

{-- ===============================
Punto 1: Vacaciones aplicadas
Total de stress de la gente glotona
Queremos saber el total de stress de la gente glotona de un contingente, que suma el nivel de stress de
todas las personas a los que les gusta la gastronomía. 

Contingente raro
Un contingente raro es aquel en el que todas las personas tienen cantidad par de amigos.

Ambos puntos debe resolverlos utilizando composición, aplicación parcial y funciones de orden superior.
No puede utilizar funciones auxiliares. --}

-------------
-- Total de stress de la gente glotona
--Queremos saber el total de stress de la gente glotona de un contingente, que suma el --nivel de stress de
--todas las personas a los que les gusta la gastronomía. 
stressGenteGlotona :: Contingentes -> Number
stressGenteGlotona = 
    sum . map nivelDeEstress .filter (elem "gastronomia" . preferencias) 

-------------
--Contingente raro
--Un contingente raro es aquel en el que todas las personas tienen cantidad par de amigos.
contingenteRaro :: Contingentes -> Bool
contingenteRaro =  all (even . cantAmigos) 


{-- ===============================
Punto 2: Planes turísticos
Queremos modelar los siguientes planes turísticos:
● Villa Gesell: depende del mes, si es enero o febrero (mes 1 ó 2) aumenta el nivel de stress en 10,
en el resto del año disminuye el stress en la mitad

● Las Toninas: si el plan es con plata disminuye el nivel de stress de la persona a la mitad, si se va
sin plata aumenta el stress 10 * la cantidad de amigues que tiene.

● Puerto Madryn: hace que tengas un nuevo amigue.

● La Adela: no produce cambios en la persona que se va. 

Se pide que evite especialmente la repetición de código.--}

type PlanTuristico = Persona -> Persona
-- ● Villa Gesell: depende del mes, si es enero o febrero (mes 1 ó 2) aumenta el nivel de stress en 10,
-- en el resto del año disminuye el stress en la mitad
villaGessel :: String -> PlanTuristico
villaGessel mes    
    | (== "enero") mes || (== "febrero") mes = modificarEstres (+10)  
    | otherwise = modificarEstres (/2) 

-- ● Las Toninas: si el plan es con plata disminuye el nivel de stress de la persona a la mitad, si se va
-- sin plata aumenta el stress 10 * la cantidad de amigues que tiene.
lasToninas :: Bool -> PlanTuristico
lasToninas conPlata persona    
    | conPlata = modificarEstres (/2) persona
    | otherwise = modificarEstres (incrementoSinPlata persona) persona

incrementoSinPlata :: Persona -> (Number -> Number)
incrementoSinPlata  persona = (+) . (*10) $ cantAmigos persona

modificarEstres :: (Number -> Number) -> PlanTuristico
modificarEstres ajusteEstress persona = 
    persona {nivelDeEstress = ajusteEstress $ nivelDeEstress persona}   

-- ● Puerto Madryn: hace que tengas un nuevo amigue.
puertoMadryn :: PlanTuristico 
puertoMadryn persona = persona { cantAmigos = (+1) . cantAmigos $ persona }  

-- ● La Adela: no produce cambios en la persona que se va. 
laAdela :: PlanTuristico
laAdela = id

{--
a) Queremos saber si en un conjunto de planes turísticos hay alguno que sea piola para una persona,
esto implica que su nivel de stress bajaría en caso que vaya.

b) Muestre el ejemplo de cómo invocaría a la función utilizando un ejemplo con planes variados (Villa
Gesell en enero, Las Toninas con plata, Puerto Madryn y La Adela).

c) Mostrar otro ejemplo de invocación con otros datos, que permita obtener un resultado diferente.
--}

-- a) Queremos saber si en un conjunto de planes turísticos hay alguno que sea piola para una persona,
-- esto implica que su nivel de stress bajaría en caso que vaya.
hayAlgunPlanPiola :: Persona -> [PlanTuristico] -> Bool
hayAlgunPlanPiola persona  = 
    any(bajaNivelEstressA persona) 

bajaNivelEstressA :: Persona -> PlanTuristico -> Bool
bajaNivelEstressA persona =
    (< nivelDeEstress persona) . nivelDeEstress . ($ persona)

-- b) Muestre el ejemplo de cómo invocaría a la función utilizando un ejemplo con planes variados (Villa
-- Gesell en enero, Las Toninas con plata, Puerto Madryn y La Adela).
planesVariados :: [PlanTuristico]
planesVariados = [villaGessel "enero", lasToninas True, puertoMadryn, laAdela]

pedro = Persona {
    nombre = "Pedro",
    nivelDeEstress = 10,
    preferencias = ["playa"],
    cantAmigos = 1000
} 

-- Ejemplo de invocacion:
-- hayAlgunPlanPiola :: Persona -> PlanesTuristicos -> Bool
-- hayAlgunPlanPiola pedro planesVariados


-- c) Mostrar otro ejemplo de invocación con otros datos, que permita obtener un resultado diferente.

juana =  Persona {
    nombre = "Juana",
    nivelDeEstress = 1000,
    preferencias = ["gastronomia"],
    cantAmigos = 5
}

planesEstresantes :: [PlanTuristico]
planesEstresantes = [villaGessel "febrero", lasToninas False]

-- Ejemplo de invocacion:
-- hayAlgunPlanPiola :: Persona -> PlanesTuristicos -> Bool
-- hayAlgunPlanPiola juana planesEstresantes