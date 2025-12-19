module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{--Hamburguejas al vapor
Queremos hacer un sistema para una cadena multinacional de comida rápida. Principalmente nos interesa modelar los
combos que venden para poder armar los pedidos, pero tenemos que hacerlo lo suficientemente flexible para que se
ajuste a los gustos de los clientes. Los combos que están a la venta incluyen una hamburguesa con ciertos ingredientes
y además vienen con papas fritas y una gaseosa mediana; luego el cliente puede a partir de dicho combo prearmado
pedir agrandar su bebida, cambiar el acompañamiento por otro o quitar ingredientes a la hamburguesa.

Los datos del programa están modelados de la siguiente forma:
type Ingrediente = String
data Hamburguesa = Hamburguesa {nombreHamburguesa :: String, ingredientes :: [Ingrediente]}
data Bebida = Bebida {nombreBebida :: String, tamanioBebida :: Number, light :: Bool}
type Acompaniamiento = String
type Combo = (Hamburguesa, Bebida, Acompaniamiento)--}

type Ingrediente = String
data Hamburguesa = Hamburguesa {nombreHamburguesa :: String, ingredientes :: [Ingrediente]}
data Bebida = Bebida {nombreBebida :: String, tamanioBebida :: Number, light :: Bool}
type Acompaniamiento = String
type Combo = (Hamburguesa, Bebida, Acompaniamiento)

{-- Se cuenta con las siguientes funciones para simplificar el desarrollo:
hamburguesa (h,_,_) = h
bebida (_,b,_) = b
acompaniamiento (_,_,a) = a --}

hamburguesa (h,_,_) = h
bebida (_,b,_) = b
acompaniamiento (_,_,a) = a

{-- Además disponemos de la siguiente constante que nos indica cuántas calorías tienen los distintos ingredientes usados
para hacer hamburguesas que no sean condimentos:

informacionNutricional = [("Carne", 250), ("Queso", 50), ("Pan", 20), ("Panceta", 541), ("Lechuga", 5), ("Tomate", 6)] --}

informacionNutricional = [("Carne", 250), ("Queso", 50), ("Pan", 20), ("Panceta", 541), ("Lechuga", 5), ("Tomate", 6)]

-- Y también sabemos cuáles son los condimentos:
-- condimentos = ["Barbacoa", "Mostaza", "Mayonesa", "Salsa big mac","Ketchup"]
condimentos = ["Barbacoa", "Mostaza", "Mayonesa", "Salsa big mac","Ketchup"]

{--Finalmente tenemos los siguientes datos de ejemplo para usar para probar el sistema:
comboQyB = (qyb, cocaCola, "Papas")
cocaCola = Bebida "Coca Cola" 2 False
qyb = Hamburguesa "QyB" ["Pan", "Carne" , "Queso", "Panceta", "Mayonesa", "Ketchup", "Pan"] --}

comboQyB = (qyb, cocaCola, "Papas")
cocaCola = Bebida "Coca Cola" 2 False
qyb = Hamburguesa "QyB" ["Pan", "Carne" , "Queso", "Panceta", "Mayonesa", "Ketchup", "Pan"]

{--Se pide desarrollar las siguientes funciones poniendo en práctica orden superior, aplicación parcial y composición
cuando sea posible.

1. Queremos saber cuántas calorías tiene un ingrediente, esto puede obtenerse a partir de la información
nutricional, a menos que sea un condimento, en cuyo caso la cantidad de calorías es 10.

> calorias "Panceta"
541
> calorias "Mostaza"
10
--}
cuantasCaloriasTiene :: Ingrediente -> Number
cuantasCaloriasTiene ingrediente 
    | esUnCondimento ingrediente = 10
    | otherwise =  snd . head . filter ((== ingrediente) . fst) $ informacionNutricional

esCondimento :: Ingrediente -> Bool
esCondimento ingrediente = elem ingrediente condimentos

{-- 2. Se quiere saber si un combo esMortal. Esto se cumple cuando 

la bebida no es dietética y el acompañamiento no es ensalada, 
o si la hamburguesa es una bomba (si tiene entre sus ingredientes al menos uno que tenga más de 300 calorías, 

o si en total la hamburguesa supera las 1000 calorías).--}

esMortal :: Combo -> Bool
esMortal combo = 
    (not . esDietetico $ combo)  
    || hamburEsUnaBomba (hamburguesa combo) 
    || hamburMuyCalorica (hamburguesa combo)

esDietetico :: Combo -> Bool
esDietetico combo =
    (light $ bebida combo) && 
    (== "ensalada") (acompaniamiento combo)

hamburEsUnaBomba :: Hamburguesa -> Bool
hamburEsUnaBomba = --any (>300) . map cuantasCaloriasTiene . ingredientes  
    any (tieneMasCaloriasQue 300) . ingredientes

tieneMasCaloriasQue :: Number -> Ingrediente -> Bool
tieneMasCaloriasQue valor  = (> valor) . cuantasCaloriasTiene

hamburMuyCalorica :: Hamburguesa -> Bool
hamburMuyCalorica  = 
    (>1000) . sum . map cuantasCaloriasTiene . ingredientes

{--hamburMuyCalorica :: Hamburguesa -> Bool
hamburMuyCalorica hamburguesa = 
    (>1000) . sum . map cuantasCaloriasTiene $ ingredientes hamburguesa--}


{-- 3. Definir las siguientes funciones para alterar un combo y declarar el tipo de las mismas:

a. agrandarBebida: el combo alterado debería tener el mismo tipo de bebida pero incrementando en 1 su
tamaño.

b. cambiarAcompañamientoPor: el combo alterado debería tener el acompañamiento elegido por el cliente.

c. peroSin: la hamburguesa del combo debería excluir ingredientes que cumplan con una determinada condición. En principio nos interesa contemplar las siguientes condiciones sobre los ingredientes, pero debería admitir otras condiciones del mismo tipo:

i. esCondimento: un ingrediente cumple esta condición si es igual a alguno de los condimentos
conocidos.

ii. masCaloricoQue: se cumple esta condición si las calorías del ingrediente superan un valor
dado. --}

-- a. agrandarBebida: el combo alterado debería tener el mismo tipo de bebida pero incrementando en 1 su tamaño.

-- VERSION 1, capaz la 2 mejor, pero no se ...
-- muy rebuscada esta?
agrandarBebida :: Combo -> Combo
agrandarBebida = modificarBebida agrandarTamanio 1

modificarBebida :: (Number -> Bebida -> Bebida) -> Number -> Combo -> Combo
modificarBebida ajusteBebida cantidad (hamburguesa, bebida, condimentos) =
    (hamburguesa, ajusteBebida 1 bebida , condimentos)

agrandarTamanio :: Number -> Bebida -> Bebida
agrandarTamanio cantidad bebida = 
    bebida {tamanioBebida = (+ cantidad) . tamanioBebida $ bebida}     

-- VERSION 2
agrandarBebida2 :: Combo -> Combo
agrandarBebida2 (hamburguesa, bebida, condimentos) =
    (hamburguesa, modificarBebida2 1 bebida , condimentos)

modificarBebida2 :: Number -> Bebida -> Bebida
modificarBebida2 cantidad bebida = 
    bebida {tamanioBebida = (+ cantidad) . tamanioBebida $ bebida}     


-- b. cambiarAcompañamientoPor: el combo alterado debería tener el acompañamiento elegido por el cliente.
cambiarAcompaniamientoPor :: Acompaniamiento -> Alteracion
cambiarAcompaniamientoPor nuevoAcompaniamiento (hamburguesa, bebida, acompaniamiento) =
    (hamburguesa, bebida, nuevoAcompaniamiento)

{-- c. peroSin: la hamburguesa del combo debería excluir ingredientes que cumplan con una determinada condición. En principio nos interesa contemplar las siguientes condiciones sobre los ingredientes, pero debería admitir otras condiciones del mismo tipo:

i. esCondimento: un ingrediente cumple esta condición si es igual a alguno de los condimentos
conocidos.

ii. masCaloricoQue: se cumple esta condición si las calorías del ingrediente superan un valor
dado. --}

peroSin :: ( Ingrediente -> Bool) -> Combo -> Combo 
peroSin condicionEliminacion = 
    modificarHamburguesa (quitarIngredientes condicionEliminacion)
       
modificarHamburguesa :: (Hamburguesa -> Hamburguesa) -> Combo -> Combo
modificarHamburguesa ajusteHambur (hamburguesa, bebida, acompaniamiento) = 
    (ajusteHambur hamburguesa, bebida, acompaniamiento)

quitarIngredientes :: (Ingrediente -> Bool) -> Hamburguesa -> Hamburguesa
quitarIngredientes condicionEliminacion hamburguesa =
    hamburguesa {ingredientes = filter (not . condicionEliminacion) . ingredientes $ hamburguesa}

-- i. esCondimento: un ingrediente cumple esta condición si es igual a alguno de los condimentos conocidos.
-- Hecha en el punto 1 como funcion auxiliar

--ii. masCaloricoQue: se cumple esta condición si las calorías del ingrediente superan un valor dado.
masCaloricoQue :: Number -> Ingrediente -> Bool
masCaloricoQue calorias  = (> calorias) . cuantasCaloriasTiene 

{-- 4. Realizar una consulta usando lo desarrollado hasta ahora que permita obtener a partir del comboQyB y una lista de alteraciones, aquellas alteraciones tras las cuales el combo en cuestión no es mortal.

Las alteraciones a incluir en la lista deben ser las siguientes: agrandar la bebida, cambiar el acompañamiento por ensalada, que venga sin condimento, que venga sin ingredientes con más de 400 calorías y que venga sin
queso.
--}

-- 4. Realizar una consulta usando lo desarrollado hasta ahora que permita obtener a partir del comboQyB y una lista de alteraciones, aquellas alteraciones tras las cuales el combo en cuestión no es mortal.

type Alteracion = Combo -> Combo

alteracionesNoMortales :: Combo -> [Alteracion] -> [Alteracion]
alteracionesNoMortales combo   =
    filter (not . esMortal . ($ combo)) 
-- alteracion -> not (esMortal (alteracion combo))

alteracionesEjemplo :: [Alteracion]
alteracionesEjemplo =
  [ agrandarBebida
  , cambiarAcompaniamientoPor "ensalada"
  , peroSin esCondimento
  , peroSin (masCaloricoQue 400)
  , peroSin (== "Queso")
  ]

-- Consulta del ejercicio:
-- alteracionesNoMortales comboQyB alteracionesEjemplo
-- devolveria [ agrandarBebida, cambiarAcompaniamientoPor "ensalada"]