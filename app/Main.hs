{-# LANGUAGE OverloadedStrings #-} --Esto es para que los string o char los pase a Query 
{-# LANGUAGE DeriveGeneric #-}
-- Esto nos ayudo como para los archivos CSV https://forkful.ai/es/haskell/data-formats-and-serialization/working-with-csv/

import Database.MySQL.Simple
    ( close,
      connect,
      defaultConnectInfo,
      execute,
      withTransaction,
      query,
      query_,
      ConnectInfo(connectDatabase, connectHost, connectPort, connectUser,
                  connectPassword),
      Connection,
      Only(..) )
import Database.MySQL.Simple.Types (Only(..))
import System.IO (hFlush, stdout)
import Data.String (fromString)
import Control.Monad.Reader
import Data.List (nub, groupBy, sortOn)
import Data.Time ( Day, parseTimeM, defaultTimeLocale, formatTime )
import Control.Monad (void)
import Data.List (nub)
import Control.Monad.ST (ST)
import Data.Time.Calendar (addDays)
import Data.Function (on)
import GHC.Generics (Generic) -- Libreria para el csv
import qualified Data.Csv as Csv -- Libreria para el csv
import qualified Data.Vector as V -- Libreria para el csv
import qualified Data.ByteString.Lazy as BL -- Libreria para el csv
-- Definición de datos de la aplicación
data Trabajador = Trabajador {
    nombre :: String,
    cedula :: String,
    rol :: String
} deriving (Show)

data Herramienta = Herramienta {
    codigoHA :: String,
    nombreHA :: String,
    descripcionHA :: String,
    tipoHA :: String
} deriving (Show, Eq, Generic)


instance Csv.ToRecord Herramienta

--Usamos un vector por cada fila agarramos el codigo, nombre, descripcion y tipo,
--Luego este dato lo devolvemos como uno de tipo Herramienta
instance Csv.FromRecord Herramienta where
    parseRecord v = do
        codigo <- v Csv..! 0   
        nombre <- v Csv..! 1   
        descripcion <- v Csv..! 2   
        tipo <- v Csv..! 3   
        return $ Herramienta codigo nombre descripcion tipo



data Parcela = Parcela {
    idParcela :: Int, 
    nombreP :: String,
    zonaP :: String,
    areaP :: Float, 
    vegetalesP :: [(String, Float)],  -- una tupla para la parte de vegetales y precio
    herramientasP :: [Herramienta]
} deriving (Show)

data Vegetal = Vegetal { 
    idParcelaVegetal :: Int,
    nombreVegetal :: String,
    precio :: Float
} deriving (Show)

data Cosecha = Cosecha {
    idCosecha    :: Int,
    idParcelac   :: Int,            
    fechaInicio  :: Day,
    fechaFin     :: Day,
    cedulaTrabajador :: String,   
    vegetal      :: String,
    cantidadKg   :: Int,
    estadoCosecha :: String,
    kilosRecogidos :: Int
} deriving (Show)



--Aqui generamos un tipo App para que este sea el que cuando se utilice sea más fácil llamar a la conexión
-- Es algo tipo global
type App = ReaderT Connection IO

runApp :: Connection -> App a -> IO a
runApp conn app = runReaderT app conn


main :: IO ()
main = do
    putStrLn "Intentando conectar a la base de datos..."
    --Datos para conectarme a la base
    let connectInfo = defaultConnectInfo {
            connectHost = "172.27.133.169",
            connectPort = 3307,
            connectUser = "root",
            connectPassword = "root",
            connectDatabase = "fincaAgricola"
        }

    conn <- connect connectInfo
    putStrLn "¡Conexión exitosa!"

    runApp conn $ do
        conn' <- ask
        results <- liftIO $ query_ conn' "SELECT 1+1" :: App [Only Int]
        let [Only result] = results
        liftIO $ putStrLn $ "Resultado de prueba: " ++ show result

        -- Llamamos al menu principal 
        menu

        -- Cierre de conexión para la base
    liftIO $ do
        putStrLn "Cerrando conexión..."
        close conn
        putStrLn "Conexión cerrada."

--Este es el menu para las opciones, operativas, generales y de salir
menu :: App ()
menu = do
    liftIO $ do
        putStrLn "1. Opciones operativas"
        putStrLn "2. Opciones generales"
        putStrLn "3. Salir"
        putStr "Opción: "
        hFlush stdout --Esto es para que la consola muestre el mensaje desde antes
    
    option <- liftIO getLine
    subMenu option

--Este es el submenu para la parte de opciones operativas, donde pedimos la cedula, si es correcta lo pasamos
--al otro menú sino le denegamos el acceso y lo devolvemos 
subMenu :: String -> App ()
subMenu "1" = do
    liftIO $ putStrLn "Antes de ingresar, por favor escriba su cédula"
    liftIO $ putStr "Por favor, ingrese su cédula: "
    liftIO $ hFlush stdout
    cedula <- liftIO getLine
    
    conn <- ask
    trabajadores <- liftIO $ obtenerTrabajadores conn
    
    if buscaTrabajador cedula trabajadores 
        then opcionesOperativas
        else liftIO (putStrLn "Acceso denegado") >> menu

subMenu "2" = do
    opcionesGenerales
    menu

subMenu "3" = liftIO $ putStrLn "Hasta la próxima"

subMenu _ = do
    liftIO $ putStrLn "Opción no válida"
    menu

--Esta parte es el sub menu de opciones generales - falta darle funcionalidad jsjsjs
opcionesGenerales :: App ()
opcionesGenerales = do
    liftIO $ putStrLn "\n1. Gestion de cosechas"
    liftIO $ putStrLn "2. Cierre de cosechas."
    liftIO $ putStrLn "3. Consultar cosecha por su ID."
    liftIO $ putStrLn "4. Cancelacion de cosecha."
    liftIO $ putStrLn "5. Modificar datos de una cosecha."
    liftIO $ putStrLn "6. Consulta disponibilidad de parcela"
    liftIO $ putStrLn "7. Volver"
    liftIO $ putStr "Opción: "
    liftIO $ hFlush stdout
    opcionSubG <- liftIO getLine
    case opcionSubG of
        "1" -> subMenuCosecha
        "2" -> cerrarCosecha
        "3" -> consultarCosechaPorID
        "4" -> cancelarCosecha
        "5" -> modificarCosecha
        "6" -> consultaDisponibilidadParcelas
        "7" -> menu -- Eso se debe de cambiar en todas las lugares que redireccionen a otra funcion.
        _   -> do
            liftIO $ putStrLn "Opción inválida"
            opcionesGenerales 


--Submenu para cuando se entra a las opciones operativas
opcionesOperativas :: App ()
opcionesOperativas = do
    liftIO $ do 
        putStrLn "\n1. Cargar y mostrar herramientas de campo"
        putStrLn "2. Registrar y mostrar parcelas de cultivo"
        putStrLn "3. Informe de cosechas"
        putStrLn "4. Volver"
        putStr "Opción: "
        hFlush stdout
    
    opcionOP <- liftIO getLine
    case opcionOP of
        "1" -> menuHerramientasOP
        "2" -> menuParcela
        "3" -> menuEstadisticas
        "4" -> return ()   
        _   -> do
            liftIO $ putStrLn "Opción inválida"
            opcionesOperativas 
menuEstadisticas :: App ()
menuEstadisticas = do
    conn <- ask
    estadistica conn
    liftIO $ do
        putStrLn "\n === Apartado de estadisticas ==="
        putStrLn "Estadistica#1: Parcela con mayor volumen de cosecha"
        putStrLn "Estadistica#2: Top 3 de parcelas con mayor venta"
        putStrLn "Estadistica#3: Trabajador con más cosechas realizadas"
        putStrLn "Estadistica#4: Mes-Año con mayor recolección acumulada"
        putStrLn "Estadistica#5: Cosechas con subproducción y sobreproducción"
        putStrLn "#6.Volver"
        putStrLn "Seleccione una opción por favor"
        hFlush stdout

    opcionEstadistica <- liftIO getLine
    
    case opcionEstadistica of
        "1" -> do
            estadistica1 conn
            menuEstadisticas
        "2" -> do
            estadistica2 conn
            menuEstadisticas
        "3" -> do
            estadistica3 conn
            menuEstadisticas
        "4" -> do
            estadistica4 conn
            menuEstadisticas
        "5" -> do
            estadistica5 conn
            menuEstadisticas
        "6" -> do
            liftIO $ putStrLn "Volviendo al menu operativo\n"
            opcionesOperativas
        _   -> do
            liftIO $ putStrLn "Opción inválida\n"
            menuEstadisticas



--Este es el apartado para agregar herramientas, ya sea si queremos solo agregar o solo ver, falta darle una opcion de volver
menuHerramientasOP :: App ()
menuHerramientasOP = do
    liftIO $ do
        putStrLn "Estamos en el apartado de herramientas"
        putStrLn "\n1. Agregar herramientas"
        putStrLn "2. Ver todas las herramientas"
        putStrLn "3. Volver"
        hFlush stdout
    
    opcionHOP <- liftIO getLine
    case opcionHOP of
        "1" -> agregarHerramientas
        "2" -> do
            conn <- ask
            herramientas <- liftIO $ obtenerHerramientas conn
            liftIO $ mapM_ imprimirHerramienta herramientas
            menuHerramientasOP
        "3" -> do
            liftIO $ putStrLn "\nVas a volver al menú de opciones operativas"
            opcionesOperativas
        _ -> do
            liftIO $ putStrLn "Opción invalida"
            menuHerramientasOP

--Le damos una ruta, le pasamos una Herramienta y lo que hacemos es que agregamos al final del csv
-- lo que hacemos es la herramienta se pasa a bytestring, se separa por comas y se agrega al archivo
guardarHerramientaCSV :: FilePath -> Herramienta -> IO ()
guardarHerramientaCSV ruta herramienta = do
    
    BL.appendFile ruta (Csv.encode [herramienta])


--Usamos un either porque si sale mal queremos un mensaje en cambio si sale bien tendremos un vector con herramientas
-- Para este caso con la ruta leeemos el archivo linea por linea, de esta forma pasa de binario a un tipo csv o sea
-- separado por comas algo asi  [Herramienta "H001" "Azadón" "Para remover tierra" "Manual",])
leerHerramientasCSV :: FilePath -> IO (Either String (V.Vector Herramienta))
leerHerramientasCSV ruta = do
    archivo <- BL.readFile ruta  -- Leemos el archivo CSV
    return $ Csv.decode Csv.HasHeader archivo  

--Esto es para cuando queremos agregar una herramienta, así le pedimos al usuario que la registre en el sistema
--Luego de esto la pasamos directo a la base de datos para insertarla.
agregarHerramientas :: App ()
agregarHerramientas = do
    liftIO $ do
        putStrLn "Estas agregando herramientas"
        putStrLn "Por favor primero indique la ruta del archivo .csv que quiere utilizar"
        putStrLn "Ejemplo data/Herramientas.csv"
        hFlush stdout
    ruta <- liftIO getLine

    liftIO $ do
        putStrLn "Ingrese el código de la herramienta ejemplo(HR001)"
        hFlush stdout
    codigoH <- liftIO getLine

    liftIO $ do
        putStrLn "Ingrese el nombre de la herramienta"
        hFlush stdout
    nombreH <- liftIO getLine

    liftIO $ do
        putStrLn "Ingrese una descripción para la herramienta"
        hFlush stdout
    descripcionH <- liftIO getLine

    liftIO $ do
        putStrLn "Ingrese un tipo de herramienta(manual, automatica o motorizada)"
        hFlush stdout
    tipoH <- liftIO getLine

    -- Creamos una herramienta
    let herramienta = Herramienta codigoH nombreH descripcionH tipoH

    -- Guardamos esa herramienta en el csv
    liftIO $ do
        putStrLn "Guardando herramienta en el archivo CSV..."
        guardarHerramientaCSV ruta herramienta

    -- Leemos todas las herramientas si el caso es correcto sino mostramos un mensaje de error
    -- Con el map recorremos todas las herramientas y se la pasamos a la base por cada herramienta que haya leido
    herramientas <- liftIO $ leerHerramientasCSV ruta
    case herramientas of
        Left errorH -> liftIO $ putStrLn $ "Error al leer el archivo CSV: " ++ errorH
        Right herramientas -> do
            -- Insertar cada herramienta en la base de datos
            mapM_ (\h -> agregarHerramientasBase (codigoHA h) (nombreHA h) (descripcionHA h) (tipoHA h)) herramientas
            liftIO $ putStrLn "¡Herramientas agregadas exitosamente a la base de datos!"
    menuHerramientasOP




--Aqui le pasamos el codigo,nombre,descripcion y tipo de la herramienta para que esta se pueda
--Registrar en la base de datos
agregarHerramientasBase :: String -> String -> String -> String -> App ()
agregarHerramientasBase codigoH nombreH descripcionH tipoH = do
    conn <- ask --Pedimos la conexion y la ejecutamos por medio del execute conn el cual envia
    --ese string como un query a mysql los valores , en este caso tienen signo de pregunta
    --para que la base le asigne el tipo de valor
    liftIO $ execute conn "INSERT IGNORE INTO herramientas VALUES (?,?,?,?)" 
                          (codigoH, nombreH, descripcionH, tipoH)
    return ()

-- Lo que hacemos es llamar a la base y en resultados lo que hacemos es registrarlo como un tipo de Herramienta
-- Entonces creamos el objeto con el map para asi ir fila por fila de la tupla de los resultados, asi es como
-- se le registra a herramienta
obtenerHerramientas :: Connection -> IO [Herramienta]
obtenerHerramientas conn = do
    resultados <- query_ conn "SELECT codigo, nombre, descripcion, tipo FROM herramientas"
    return $ map (\(c, n, d, t) -> Herramienta c n d t) resultados --registramos herramientas

--Se le pasa un objeto herramienta y este lo recorre hasta quedar totalmente vacio, este lo imprime
-- por codigo, nombre, descripcion y tipo, por eso esta funcion es IO porque solo son elementos de entrada o salida
imprimirHerramienta :: Herramienta -> IO ()
imprimirHerramienta h = do
    putStrLn $ "\nCódigo: " ++ codigoHA h
    putStrLn $ "Nombre: " ++ nombreHA h
    putStrLn $ "Descripción: " ++ descripcionHA h
    putStrLn $ "Tipo: " ++ tipoHA h


--Esto es para ir imprimiendo una herramienta con su indice 
imprimirHerramientaExtra :: Int -> Herramienta -> IO ()
imprimirHerramientaExtra idx h = do
    putStrLn $ show idx ++ ". " ++ nombreHA h


--Con esta parte lo que hacemos es ir mostrando las herramientas de 1.. hasta la cantidad que hayan o sea
-- con su indice correspondiente, la diferencia es que aqui llamammos a obtenerHerramientas para que nos de
-- Las herramientas que tenemos hasta el momento luego usamos uncurry que si tenemos dos valores (A,B) nos los
-- da separados, lo cual lo necesitamos para que vaya haciendo indice, herramienta pues así lo imprime 
-- la funcion de imprimirHerramientaExtra
mostrarTodasLasHerramientas :: App ()
mostrarTodasLasHerramientas = do
    conn <- ask
    herramientas <- liftIO $ obtenerHerramientas conn
    liftIO $ do
        putStrLn "\n=== Herramientas disponibles ==="
        mapM_ (uncurry imprimirHerramientaExtra) (zip [1..] herramientas)


-- Este el menu para poder ir agregando parcelas ya después de haber ingresado la cédula
menuParcela :: App ()
menuParcela = do
    liftIO $ do
        putStrLn "\n=== Menú de Parcelas ==="
        putStrLn "1. Agregar parcela"
        putStrLn "2. Ver parcela especifica"
        putStrLn "3. Volver"
        putStr "Opción: "
        hFlush stdout
    opcion <- liftIO getLine
    case opcion of
        "1" -> agregarParcelas >> menuParcela --Aqui llamo a agregar parcelas, registro, me devuelvo sin resultado y llamo al menú
        "2" -> do
            liftIO $ putStr "Ingrese el ID de la parcela que desea ver: "
            liftIO $ hFlush stdout
            idStr <- liftIO getLine
            let idParc = read idStr :: Int
            conn <- ask
            parcela <- construirParcelaCompleta conn idParc
            liftIO $ do
                putStrLn "\n=== Detalles de la Parcela ==="
                imprimirParcela parcela
            menuParcela

        "3" -> opcionesOperativas --return ()
        _   -> liftIO (putStrLn "Opción inválida") >> menuParcela
-- Fin de la funcion.

        


--Esta parte es solo para agregar parcelas, una vez ya se hayan terminado de agregar los datos, envio la parcela
-- a la base de datos para que se cargue, importante las herramientas y vegetales se muestran aparte porque generalmente
-- estas pueden repetirse entonces para no afectar el flujo se hacen aparte    
agregarParcelas :: App ()
agregarParcelas = do 
    liftIO $ putStrLn "\nEstas agregando una nueva parcela"

    -- liftIO $ putStr "Ingrese el nombre de la parcela: "
    -- liftIO $ hFlush stdout
    nombrePAR <- liftIO $ leerEntradaTexto "Ingrese el nombre de la parcela: "

    -- liftIO $ putStr "Ingrese la zona de la parcela: "
    -- liftIO $ hFlush stdout
    zonaPAR <- liftIO $ leerEntradaTexto "Ingrese el la zona de la parcela: "

    -- liftIO $ putStr "Ingrese el área en metros cuadrados: "
    -- liftIO $ hFlush stdout
    areaPAR <- liftIO $ leerEntradaNumero "Ingrese el area en metros cuadrados: "
    -- let areaPAR = read areaStr :: Float

    conn <- ask
    -- Esta parte es para mostrar la herramientas disponibles.
    herramientasDisponibles <- liftIO $ obtenerHerramientas conn
    liftIO $ do
        putStrLn "\n=== Herramientas disponibles ==="
        mapM_ (uncurry imprimirHerramientaExtra) (zip [1..] herramientasDisponibles)

    -- Esta optiene la lista de herramientas que el usuario añadira a esta parcela.
    herramientasSeleccionadas <- extraParcelasHerramientas []
    liftIO $ print herramientasSeleccionadas
    -- Esta parte es para registrar los vegetales que perteneceran a esta parcela.
    vegetales <- extraParcelas []
    liftIO $ print vegetales
    -- Esta parte de aqui registra los datos generales de la parcela.
    idParcela <- crearParcelaDB nombrePAR zonaPAR areaPAR
    
    -- Esta parte, guarda los datos de la verduras y herraminestas asociada a esta parcela.
    _ <- liftIO $ crearParcelaExtraDB conn vegetales herramientasSeleccionadas idParcela

    -- Muestra los datos de la parcela recien creada.
    liftIO $ do
        putStrLn "\nParcela registrada con éxito (simulado)"
        putStrLn $ "ID: " ++ show idParcela  -- Mostramos el ID aquí
        putStrLn $ "Nombre: " ++ nombrePAR
        putStrLn $ "Zona: " ++ zonaPAR
        putStrLn $ "Área: " ++ show areaPAR
        putStrLn $ "Vegetales: " ++ show vegetales
        putStrLn $ "Herramientas: " ++ show (herramientasSeleccionadas)

    liftIO $ putStrLn "Parcela registrada correctamente"
-- Fin de la funcion.




--Esto se puede ver como un menu o sub menu para que a las parcelas se les pueda agregar herramientas de forma
-- consecutiva pero mostrandole al usuario las herramientas registradas hasta el momento
-- extraParcelasHerramientas :: [Herramienta] -> App [Herramienta]
-- extraParcelasHerramientas acumuladas = do
--     conn <- ask
--     herramientas <- liftIO $ obtenerHerramientas conn

--     liftIO $ do
--         putStrLn "\n¿Desea agregar una herramienta (Debes de agregar al menos 1)?"
--         putStrLn "1. Sí"
--         putStrLn "2. No (continuar)"
--         putStr "Opción: "
--         hFlush stdout

--     opcion <- liftIO getLine
--     case opcion of
--         "1" -> do
--             liftIO $ do
--                 putStrLn "\n=== Herramientas disponibles ==="
--                 mapM_ (uncurry imprimirHerramientaExtra) (zip [1..] herramientas)
--                 putStr "Indique el número de la herramienta que desea agregar: "
--                 hFlush stdout
--             seleccionStr <- liftIO getLine
--             let seleccion = read seleccionStr :: Int

--             -- Primero revisamos que el número sea válido 
--             if seleccion >= 1 && seleccion <= length herramientas
--                 then do
--                     --Creamos una variable y le damos la selección -1 para que quede bien pero también
--                     -- es importante que esto !! lo que hace es acceder a la lista de objetos de las herramientas
--                     -- en el indice que le di, es como hacer esto en python Lista[1]
                    
                    
--                     -- Aqui hay que agregar una  validacion para que no se pueda registrar una herramienta preciamente registrada.
--                     let herramientaSeleccionada = herramientas !! (seleccion - 1)
--                     liftIO $ putStrLn $ "Has seleccionado: " ++ nombreHA herramientaSeleccionada
--                     --Llamamos recursivamente y agregamos la heeramienta al inicio de acumuladas
--                     extraParcelasHerramientas (herramientaSeleccionada : acumuladas)
--                 else do
--                     liftIO $ putStrLn "Número inválido"
--                     extraParcelasHerramientas acumuladas
--         "2" -> -- Para salir hay que validar que la lista de herramientas tengo al menos 1.
--             -- if length acumuladas <= 0
--             --     then do
--             --         liftIO $ putStrLn "Debes de seleccionar al menos una herramienta."
--             --         extraParcelasHerramientas acumuladas
--             -- else do
--                 return (reverse acumuladas)
--         _ -> do
--             liftIO $ putStrLn "Opción inválida"
--             extraParcelasHerramientas acumuladas



extraParcelasHerramientas :: [Herramienta] -> App [Herramienta]
extraParcelasHerramientas acumuladas = do
    conn <- ask
    herramientas <- liftIO $ obtenerHerramientas conn  -- Solo se obtiene la lista una vez

    liftIO $ do
        putStrLn "\n¿Desea agregar una herramienta (Debes de agregar al menos 1)?"
        putStrLn "1. Sí"
        putStrLn "2. No (continuar)"
        putStr "Opción: "
        hFlush stdout

    opcion <- liftIO getLine
    case opcion of
        "1" -> do
            liftIO $ do
                putStrLn "\n=== Herramientas disponibles ==="
                mapM_ (uncurry imprimirHerramientaExtra) (zip [1..] herramientas)
                putStr "Indique el número de la herramienta que desea agregar: "
                hFlush stdout
            seleccionStr <- liftIO getLine
            let seleccion = read seleccionStr :: Int

            -- Validamos que la selección sea correcta
            if seleccion >= 1 && seleccion <= length herramientas
                then do
                    let herramientaSeleccionada = herramientas !! (seleccion - 1)
                    
                    -- Nos aseguramos de que la herramienta no esté ya seleccionada
                    if herramientaSeleccionada `elem` acumuladas
                        then do
                            liftIO $ putStrLn "Error: La herramienta ya fue seleccionada."
                            extraParcelasHerramientas acumuladas
                        else do
                            liftIO $ putStrLn $ "Has seleccionado: " ++ nombreHA herramientaSeleccionada
                            extraParcelasHerramientas (herramientaSeleccionada : acumuladas)
                else do
                    liftIO $ putStrLn "Número invalido"
                    extraParcelasHerramientas acumuladas

        "2" -> 
            if null acumuladas
                then do
                    liftIO $ putStrLn "Debes de seleccionar al menos una herramienta."
                    extraParcelasHerramientas acumuladas
                else return (reverse acumuladas)  -- Retorna solo las seleccionadas

        _ -> do
            liftIO $ putStrLn "Opción inválida"
            extraParcelasHerramientas acumuladas
-- Fin de la funcion.


extraParcelas :: [(String, Float)] -> App [(String, Float)]
extraParcelas acumulados = do
    liftIO $ putStrLn "\n¿Desea agregar un vegetal?"
    liftIO $ putStrLn "1. Sí"
    liftIO $ putStrLn "2. No (continuar)"
    liftIO $ putStr "Opción: "
    liftIO $ hFlush stdout
    opcion <- liftIO getLine
    case opcion of
        "1" -> do
            liftIO $ putStr "Nombre del vegetal: "
            liftIO $ hFlush stdout
            nombre <- liftIO getLine

            liftIO $ putStr "Precio por kilo: "
            liftIO $ hFlush stdout
            precioStr <- liftIO getLine
            let precio = read precioStr :: Float

            extraParcelas ((nombre, precio):acumulados)

        "2" -> return (reverse acumulados)
        _   -> do
            liftIO $ putStrLn "Opción inválida"
            extraParcelas acumulados
-- Fin de la funcion.




crearParcelaDB :: String -> String -> Int -> App Int
crearParcelaDB nombrePAR zonaPAR areaPAR = do
    conn <- ask
    -- Primero insertamos la parcela
    liftIO $ insertarParcela conn nombrePAR zonaPAR areaPAR
    -- Luego obtenemos el ID
    liftIO $ obtenerUltimoID conn
-- Fin de la funcion.


insertarParcela :: Connection -> String -> String -> Int -> IO ()
insertarParcela conn nombre zona area = do
    execute conn 
        "INSERT INTO Parcela (nombre, zona, area) VALUES (?,?,?)" 
        (nombre, zona, area)
    return ()
-- Fin de la funcion.

-- Funcion para optener el ultimo id que se genero en la sesion actual despues de un insert a la base de datos.
obtenerUltimoID :: Connection -> IO Int
obtenerUltimoID conn = do
    result <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int]
    case result of
        [Only id] -> return id
        _ -> error "No se pudo obtener el ID insertado"
-- Fin de la funcion.

    
crearParcelaExtraDB :: Connection -> [(String, Float)] -> [Herramienta] -> Int -> IO ()
crearParcelaExtraDB conn  vegetalesP herramientasP idParcela = do
 -- Insertar vegetales en la base de datos
    mapM_ (\(nombreV, precioV) -> registrarVegetalesParcela conn nombreV precioV idParcela) vegetalesP

    -- Insertar herramientas en la base de datos
    mapM_ (\herramienta -> registrarHerramientasParcela conn (codigoHA herramienta) idParcela) herramientasP
-- Fin de la funcion.

registrarVegetalesParcela :: Connection -> String -> Float -> Int -> IO ()
registrarVegetalesParcela conn nombreVegetal precioVegetal idParcela_vegetal = do
    _ <- execute conn
        "INSERT INTO VegetalesPorParcela (IdParcela, NombreVegetal, Precio) VALUES (?,?,?)"
        (idParcela_vegetal, nombreVegetal, precioVegetal)
    return ()

registrarHerramientasParcela :: Connection -> String -> Int -> IO ()
registrarHerramientasParcela conn p_codigo_herramienta  idParcela_herramienta = do
    _ <- execute conn
        "INSERT INTO HerramientasPorParcela (IdParcela, CodigoHerramienta) VALUES (?,?)"
        (idParcela_herramienta, p_codigo_herramienta)
    return ()

obtenerTrabajadores :: Connection -> IO [Trabajador]
obtenerTrabajadores conn = do
    trabajadores <- query_ conn "SELECT nombreCompleto, cedula, rol FROM Trabajadores" 
    return (map (\(n,c,r) -> Trabajador n c r) trabajadores) -- registramos trabajadores en la app
-- Fin de la funcion.


buscaTrabajador :: String -> [Trabajador] -> Bool
buscaTrabajador cedulaBuscada = any (\t -> cedulaBuscada == cedula t)
-- Fin de la funcion.



obtenerTodasLasParcelas :: App [Parcela]
obtenerTodasLasParcelas = do
    conn <- ask
    idsResult <- lift $ query_ conn "SELECT idParcela FROM Parcela" :: App [Only Int]
    let ids = map fromOnly idsResult
    mapM (construirParcelaCompleta conn) ids

estadistica :: Connection -> App ()
estadistica conn = do
    resultados <- liftIO (query_ conn "SELECT * FROM MuestroEstadisticaIni" :: IO [(Int, Int, String, String, Day, Int, Int)])
    liftIO $ do
        putStrLn "\n=== Cosechas registradas ==="
        mapM_ (\(idCosecha, idParcela, nombreParcela, nombreVege, fechaFin, kilosPlanificados, kilosRecogidos) ->
            putStrLn $
                "ID Cosecha: " ++ show idCosecha ++
                ", Parcela: " ++ nombreParcela ++
                ", Vegetal: " ++ nombreVege ++
                ", Fecha: " ++ show fechaFin ++
                ", Planificado: " ++ show kilosPlanificados ++ " kg" ++
                ", Recogido: " ++ show kilosRecogidos ++ " kg"
            ) resultados





estadistica1 :: Connection -> App ()
estadistica1 conn = do
    resultados <- liftIO (query_ conn "SELECT * FROM ParcelaMayorVolumen" :: IO [(String, Double)])
    liftIO $ do
        putStrLn "\n=== Parcela con Mayor Volumen de Cosecha ==="
        mapM_ (\(nombre, volumen) -> 
            putStrLn $ "Parcela: " ++ nombre ++ ", Volumen total: " ++ show volumen ++ " kg"
            ) resultados

estadistica2 :: Connection -> App ()
estadistica2 conn = do
    resultados <- liftIO (query_ conn "SELECT * FROM ParcelasMayorVenta" :: IO [(Int, String, Double)])
    liftIO $ do
        putStrLn "\n=== Top 3 parcelas con mayores ventas ==="
        mapM_ (\(idParcelaE, nombreParcela, ventasP) -> 
            putStrLn $ "ID Parcela: " ++ show idParcelaE ++ ", Nombre de la parcela: " ++ nombreParcela ++ ", Cantidad de ventas: " ++ show ventasP
            ) resultados



estadistica3 :: Connection -> App ()
estadistica3 conn = do
    resultados <- liftIO (query_ conn "SELECT * FROM TrabajadorMasCosechas" :: IO [(String, Int)])
    liftIO $ do
        putStrLn "\n=== Trabajador con más cosechas realizadas ==="
        mapM_ (\(nombre, cosechas) -> 
            putStrLn $ "Nombre del trabajador: " ++ nombre ++ ", Cantidad de cosechas: " ++ show cosechas 
            ) resultados


estadistica4 :: Connection -> App ()
estadistica4 conn = do
    resultados <- liftIO (query_ conn "SELECT * FROM RecoleccionPorMes" :: IO [(Int, Int, Double)])
    liftIO $ do
        putStrLn "\n=== Recolección por mes y año ==="
        mapM_ (\(mes, anio, recoleccion) -> 
            putStrLn $ "Mes: " ++ show mes ++ ", Año: " ++ show anio ++ ", Recolección: " ++ show recoleccion ++ " kg"
            ) resultados


estadistica5 :: Connection -> App ()
estadistica5 conn = do
    resultados <- liftIO (query_ conn "SELECT * FROM VistaCosechasEstado" :: IO [(Int, Int, Int, Int, String)])
    liftIO $ do
        putStrLn "\n=== Cosechas con subproducción y sobreproducción ==="
        mapM_ (\(idcos, idpf, kilosPlani, kilosReco, estadoReco) -> 
            putStrLn $ "Id cosecha: " ++ show idcos ++ 
                       ", Id Parcela: " ++ show idpf ++ 
                       ", Kilos planificados: " ++ show kilosPlani ++ 
                       ", Kilos recogidos: " ++ show kilosReco ++ 
                       ", Estado de recolección: " ++ estadoReco
            ) resultados



construirParcelaCompleta :: Connection -> Int -> App Parcela
construirParcelaCompleta conn idParc = do
    [parcelaBase] <- lift $ query conn 
        "SELECT nombre, zona, area FROM Parcela WHERE idParcela = ?" 
        (Only idParc) :: App [(String, String, Int)]
    let (nombre, zona, area) = parcelaBase

    vegetales <- lift $ query conn 
        "SELECT NombreVegetal, Precio FROM VegetalesPorParcela WHERE IdParcela = ?" 
        (Only idParc) :: App [(String, Float)]

    codigosHerramientas <- lift $ query conn 
        "SELECT CodigoHerramienta FROM HerramientasPorParcela WHERE IdParcela = ?" 
        (Only idParc) :: App [Only String]

    herramientas <- mapM (obtenerHerramienta conn) (map fromOnly codigosHerramientas)

    -- Armar la parcela final
    return Parcela {
        idParcela = idParc,
        nombreP = nombre,
        zonaP = zona,
        areaP = fromIntegral area,
        vegetalesP = vegetales,
        herramientasP = herramientas
    }




obtenerHerramienta :: Connection -> String -> App Herramienta
obtenerHerramienta conn codigo = do
    [herramienta] <- lift $ query conn 
        "SELECT codigo, nombre, descripcion, tipo FROM Herramientas WHERE codigo = ?" 
        (Only codigo) :: App [(String, String, String, String)]
    return $ uncurry4 Herramienta herramienta
  where
    uncurry4 f (a,b,c,d) = f a b c d
-- Fin de la funcion.


imprimirParcela :: Parcela -> IO ()
imprimirParcela p = do
    putStrLn $ "\nID: " ++ show (idParcela p)
    putStrLn $ "Nombre: " ++ nombreP p
    putStrLn $ "Zona: " ++ zonaP p
    putStrLn $ "Área: " ++ show (areaP p) ++ " m²"
    putStrLn $ "Vegetales:"
    mapM_ (\(v, precio) -> putStrLn $ "  - " ++ v ++ ": ₡" ++ show precio ++ " por kilo") (vegetalesP p)
    putStrLn "Herramientas:"
    mapM_ (\h -> putStrLn $ "  - " ++ nombreHA h) (herramientasP p)
-- Fin de la funcion.


mostrarParcela :: Parcela -> IO ()
mostrarParcela p = do
    putStrLn  "\n=== Detalles de Parcela ==="
    putStrLn $ "ID: " ++ show (idParcela p)
    putStrLn $ "Nombre: " ++ nombreP p
    putStrLn $ "Zona: " ++ zonaP p
    putStrLn $ "Área: " ++ show (areaP p) ++ " m²"
    putStrLn "Vegetales:"
    mapM_ (\(v, precio) -> putStrLn $ "  - " ++ v ++ ": ₡" ++ show precio) (vegetalesP p)
    putStrLn "Herramientas:"
    mapM_ (\h -> putStrLn $ "  - " ++ nombreHA h ++ " (" ++ codigoHA h ++ ")") (herramientasP p)
-- Fin de la funcion.


subMenuCosecha :: App ()
subMenuCosecha = do
    liftIO $ putStrLn ">> Apartado para el cierre de la cosecha."

    -- Cedula del trabajador
    cedula <- liftIO $ leerEntradaTexto "Ingrese el identificador del trabajador (cedula): "

    -- id de la parcela
    idParcela_cosechar <- liftIO $ leerEntradaNumero "Ingrese el ID de la parcela a cosechar: "

    -- Fecha de inicio
    fechaInicioStr <- liftIO $ leerEntradaTexto "Fecha de inicio (ej: 30/04/2025): "

    let formato = "%d/%m/%Y"
    let fechaInicio = parseTimeM True defaultTimeLocale formato fechaInicioStr :: Maybe Day

    -- Fecha final
    fechaFinStr <- liftIO $ leerEntradaTexto "Fecha de fin (ej: 30/04/2026): "
    let fechaFin = parseTimeM True defaultTimeLocale formato fechaFinStr :: Maybe Day

    -- Tipo de vegetal (Aqui tambien podriamos mostrar los tipos de vegetales que tiene la parcela ingresada, pero como esta antes de la validacion, da problemas.)
    vegetal  <- liftIO $ leerEntradaTexto "Tipo de vegetal a cosechar: "

    -- Cantidad en kg
    cantidadEsperada <-  liftIO $ leerEntradaNumero "Ingrese los kilogramos que se esperan recoger de este vegetal: "


    let estado = "Abierto" -- Tipo de estado: Abierto, Cerrado, Cancelado.

    -- Kilos recogidos en esta cosecha, por defecto es 0.
    let k_recogidos = 0

    -- Iniciar conexion
    conn <- ask
    validacionParaRegistrarCosecha <- liftIO $ validarDatosCosechaEnRegistro conn cedula idParcela_cosechar fechaInicio fechaFin vegetal -- Comporbar si los datos ingresados son correctos.

    -- Comprobar el estado de la validacion para ver si podermos hacer el registro o no.
    if validacionParaRegistrarCosecha
        then do
            liftIO $ insertCosecha conn idParcela_cosechar cedula fechaInicio fechaFin vegetal cantidadEsperada estado k_recogidos
            liftIO $ putStrLn "\nLa cosecha se ha registrado exitosamente."
            id_cosecha <- liftIO $ obtenerUltimoID conn

            liftIO $ putStrLn $ "\n>> ID de la cosecha agregada: " ++ show id_cosecha

            opcionesGenerales
        else do
            liftIO $ putStrLn "Error: No se pudo registrar la cosecha debido a una validación fallida."
            subMenuCosecha

    -- Volver al menu de la opciones generales.
    opcionesGenerales
-- Fin de la funcion.

-- Funcion para registrar los datos de una cosecha en la base de datos.
insertCosecha :: Connection -> Int -> String -> Maybe Day -> Maybe Day -> String -> Int -> String -> Int-> IO ()
insertCosecha conn idParcela cedula fechaInicioF fechaFinF vegetal cantidad estado kilos_recogidos = do
    let fechaInicio = case fechaInicioF of
            Just fecha -> formatTime defaultTimeLocale "%Y-%m-%d" fecha
            Nothing -> "NULL" 
    
    let fechaFin = case fechaFinF of
            Just fecha -> formatTime defaultTimeLocale "%Y-%m-%d" fecha
            Nothing -> "NULL" 
    _ <- execute conn
        "INSERT INTO Cosechas (idParcela, fechainicio, fechafin, cedula, nombrevege, estadoCosecha, KilosPlanificados, KilosRecogidos) VALUES (?,?,?,?,?,?,?,?)"
        (idParcela, fechaInicio, fechaFin, cedula, vegetal, estado, cantidad, kilos_recogidos)
    return ()

-- Funcion para validar si un trabajador existe.
trabajadorExiste :: Connection -> String -> IO Bool
trabajadorExiste conn cedula = do
    resultado <- query conn "SELECT COUNT(*) FROM Trabajadores WHERE cedula = ?" (Only cedula) :: IO [Only Int]
    return $ case resultado of
        [Only count] -> count > 0
        _            -> False
-- Fin de la funcion.


-- Funcion par validar si una parcela existe.
parcelaExiste :: Connection -> Int -> IO Bool
parcelaExiste conn idParcela = do
    resultado <- query conn "SELECT COUNT(*) FROM Parcela WHERE idParcela = ?" (Only idParcela) :: IO [Only Int]
    return $ case resultado of
        [Only count] -> count > 0
        _            -> False
-- Fin de la funcion.


-- Esto seria para verificar si hay otra cosecha en el ranfo de fecha indicado para la parcela.
parcelaDisponible :: Connection -> Int -> Day -> Day -> IO Bool
parcelaDisponible conn idParcela fechaInicio fechaFin = do
    resultado <- query conn 
        "SELECT COUNT(*) FROM Cosechas WHERE idParcela = ? AND estadoCosecha = 'Abierto' \
        \AND ((fechainicio BETWEEN ? AND ?) OR (fechafin BETWEEN ? AND ?))"
        (idParcela, fechaInicio, fechaFin, fechaInicio, fechaFin) :: IO [Only Int]
    
    return $ case resultado of
        [Only count] -> count == 0
        _            -> False
-- Fin de la funcion.


-- Funcion para validar si el vegetal esta asociado a la lista de vegetales de la parcela que se va a cosechar.
vegetalPerteneceAParcela :: Connection -> Int -> String -> IO Bool
vegetalPerteneceAParcela conn idParcela vegetal = do
    resultado <- query conn 
        "SELECT COUNT(*) FROM VegetalesPorParcela WHERE IdParcela = ? AND NombreVegetal = ?" 
        (idParcela, vegetal) :: IO [Only Int]
    
    return $ case resultado of
        [Only count] -> count > 0
        _            -> False
-- Fin de la funcion.

-- Funcion para validar las fehcas que son validas para el registro de una cosecha.
fechasValidas :: Maybe Day -> Maybe Day -> Bool
fechasValidas (Just fechaInicio) (Just fechaFin) = fechaFin > fechaInicio
fechasValidas _ _ = False
-- Fin de la funcion.


-- Funcion para hacer de forma separada todas las validaciones de los datos de la cosecha que se va a registrar. (En esta podrias imprimir mesanjes de personalizados por cada false que se tenga.)
validarDatosCosechaEnRegistro :: Connection -> String -> Int -> Maybe Day -> Maybe Day -> String -> IO Bool
validarDatosCosechaEnRegistro conn cedula idParcela fechaInicio fechaFin vegetal = do
    trabajadorValido <- trabajadorExiste conn cedula
    parcelaValida <- parcelaExiste conn idParcela
    let fechasOk = fechasValidas fechaInicio fechaFin
    parcelaDisponibleValida <- case (fechaInicio, fechaFin) of
        (Just fi, Just ff) -> parcelaDisponible conn idParcela fi ff
        _ -> return False
    vegetalValido <- vegetalPerteneceAParcela conn idParcela vegetal

    return $ trabajadorValido && parcelaValida && fechasOk && parcelaDisponibleValida && vegetalValido
-- Fin de la funcion.

obtenerCosecha :: Connection -> IO [Cosecha]
obtenerCosecha conn = do
    resultados <- query_ conn "SELECT idCosecha, idParcela, fechainicio, fechafin, cedula, nombrevege, estadoCosecha, KilosPlanificados, KilosRecogidos FROM Cosechas"
    return $ map (\(idC, idP, fi, ff, ce, nv, kp, ec, kr) -> 
        Cosecha idC idP fi ff ce nv kp ec kr) resultados
-- Fin de la funcion.

-- Funcion para crear una entrada de teclado para texto, se valida que lo que se ingrese no seva vacio..
leerEntradaTexto :: String -> IO String
leerEntradaTexto mensaje = do
    putStrLn mensaje
    hFlush stdout
    entrada <- getLine
    if null entrada
        then do
            putStrLn "\nLa entrada no puede estar vacía. Intentelo de nuevo."
            leerEntradaTexto mensaje
        else return entrada
-- Fin de la funcion.

-- Funcion para crear una entrada de teclado para numeros.
leerEntradaNumero :: String -> IO Int
leerEntradaNumero mensaje = do
    putStrLn mensaje
    hFlush stdout
    entrada <- getLine
    let parsed = reads entrada :: [(Int, String)]
    case parsed of
        [(num, "")] | num >= 0 -> return num
        _ -> do
            putStrLn "\nEntrada invalida. Debe ser un numero mayor a 0. Intentelo de nuevo."
            leerEntradaNumero mensaje
-- Fin de la funcion.


-- Validar la posibilidad de optener una cosecha por su id, -1: No se encontro la cosecha, -2: No esta en estado Abierto, 1: Se puede optener la cosecha por su ID.
validarPosibilidadOptenerCosechaPorID :: Connection -> Int -> IO Int
validarPosibilidadOptenerCosechaPorID conn idCosecha = do
    resultado <- query conn
        "SELECT CASE WHEN NOT EXISTS (SELECT 1 FROM Cosechas WHERE idCosecha = ?) THEN -1 \
        \WHEN EXISTS (SELECT 1 FROM Cosechas WHERE idCosecha = ? AND estadoCosecha != 'Abierto') THEN -2 \
        \ELSE 1 END AS Resultado"
        (idCosecha, idCosecha) :: IO [Only Int]
    case resultado of
        [Only valor] -> return valor
        _            -> return (-99)
-- Fin de la funcion.


-- Optener los datos de una cosecha por su id 
optenerCosechaPorID :: Connection -> Int -> IO (Maybe Cosecha)
optenerCosechaPorID conn idCosecha = do
    resultados <- query conn
        "SELECT idCosecha, idParcela, fechainicio, fechafin, cedula, nombrevege, KilosPlanificados, estadoCosecha, KilosRecogidos \
        \FROM Cosechas WHERE idCosecha = ?"
        (Only idCosecha) :: IO [(Int, Int, Day, Day, String, String, Int, String, Int)]
    
    return $ case resultados of
        [(idC, idP, fi, ff, ce, nv, kp, ec, kr)] -> Just (Cosecha idC idP fi ff ce nv kp ec kr)
        _         -> Nothing -- Este nunca deberia de pasar, ya que hago una validacion por aparte para saber si puedo hacer la consulta o no.
-- Fin de la funcion.



-- Funcion para modificar el estado de una cosecha mediente su id.
actualizarEstadoCosecha :: Connection -> Int -> String -> Int -> IO ()
actualizarEstadoCosecha conn idCosecha nuevoEstado kilosRecogidos = do
    _ <- execute conn
        "UPDATE Cosechas SET estadoCosecha = ?, kilosRecogidos = ? WHERE idCosecha = ?"
        (nuevoEstado, kilosRecogidos, idCosecha)
    return ()
-- Fin de la funcion.





-- Funcion para cerrar una cosecha, el usuario ingresara el id de la cosecha que desea cerrar y la cantidad de kilos recogidos.
cerrarCosecha ::  App ()
cerrarCosecha = do
    conn <- ask  -- Este seria para para inicar la conexcion con la base de datos.
    liftIO $ putStrLn  ">> Apartado para el cierre de la cosecha."
    idCosechaIngreado <- liftIO $ leerEntradaNumero "Ingresa el id de la cosecha que desea cerrar: "

    resultado <- liftIO $ validarPosibilidadOptenerCosechaPorID conn idCosechaIngreado
    -- liftIO $ print resultado
    -- liftIO $ hFlush stdout
    -- _ <- liftIO $ (query_ conn "SELECT 1" :: IO [Only Int]) 
    
    case resultado of
        -1 -> do 
            liftIO $ putStrLn  "Error: El id de cosecha ingresado no coincide con ninguna cosecha registrada en el sistema.\n"
            liftIO $ hFlush stdout
            opcionesGenerales
        -2 -> do 
            liftIO $ putStrLn  "Error: La cosecha ingresada ya ha sido cerrada.\n"
            liftIO $ hFlush stdout
            opcionesGenerales
        _ -> do
            
            liftIO $ hFlush stdout
            kilosRecogidos_Input <- liftIO $ leerEntradaNumero "Ingresa la cantidad de kilogramos recogidos en esta cosecha: "
            _ <- liftIO $ actualizarEstadoCosecha conn idCosechaIngreado "Cerrado" kilosRecogidos_Input
            -- _ <- liftIO $ actualizarEstadoCosecha conn idCosechaIngreado "Abierta" kilosRecogidos_Input

            -- liftIO $ hFlush stdout
            -- _ <- liftIO $ (query_ conn "SELECT 1" :: IO [Only Int])
            liftIO $ putStrLn "La cosecha se ha cerrado correctamente. \n"
            liftIO $ hFlush stdout
            opcionesGenerales
-- Fin de la funcion.

mostrarDatosCosecha :: Cosecha -> IO ()
mostrarDatosCosecha p_datosCosechaMostrar = do
    putStrLn  "\n>> ==== Detalles de la cosecha ===="
    putStrLn $ "ID de la cosecha: " ++ show (idCosecha p_datosCosechaMostrar)
    putStrLn $ "ID de la parcela: " ++ show (idParcelac p_datosCosechaMostrar)
    putStrLn $ "Fecha de inicio: " ++ show (fechaInicio p_datosCosechaMostrar)
    putStrLn $ "Fecha de fin: " ++ show (fechaFin p_datosCosechaMostrar)
    putStrLn $ "Trabajador (cedula): " ++ cedulaTrabajador p_datosCosechaMostrar
    putStrLn $ "Vegetal: " ++ vegetal p_datosCosechaMostrar
    putStrLn $ "Cantidad (Kg): " ++ show (cantidadKg p_datosCosechaMostrar)
    putStrLn $ "Estado: " ++ estadoCosecha p_datosCosechaMostrar
    putStrLn $ "Kilos recogidos: " ++ show (kilosRecogidos p_datosCosechaMostrar)
    putStrLn "> == Fin de los datos de la cosecha. =="
    hFlush stdout
-- Fin de la funcion.


consultarCosechaPorID :: App ()
consultarCosechaPorID = do
    conn <- ask -- Este seria para para inicar la conexcion con la base de datos.
    liftIO $ putStrLn  ">> Apartado para la consultas de cosechas por su ID.\n"
    idCosechaIngreado <- liftIO $ leerEntradaNumero "Ingresa el id de la cosecha que desea ver: "

    resultado <- liftIO $ validarPosibilidadOptenerCosechaPorID conn idCosechaIngreado
    -- liftIO $ print resultado
    case resultado of
        -1 -> do 
            liftIO $ putStrLn  "Error: El id de cosecha ingresado no coincide con ninguna cosecha registrada en el sistema.\n"
            liftIO $ hFlush stdout
            opcionesGenerales

        _ -> do
            -- liftIO $ print "Pass 1."
            datosCosecha <- liftIO $ optenerCosechaPorID conn idCosechaIngreado
            case datosCosecha of
                Just cosecha -> liftIO $ mostrarDatosCosecha cosecha
                Nothing      -> liftIO $ putStrLn "Error: No se encontraron datos de la cosecha."
            opcionesGenerales
-- Fin de la funcion.

-- Funcion para eliminar las una cosecha por su id de la bases de datos.
eliminarCosechaDB :: Connection -> Int -> IO ()
eliminarCosechaDB conn idCosecha = do
    _ <- execute conn "DELETE FROM Cosechas WHERE idCosecha = ?" (Only idCosecha)
    return ()
-- Fin de la funcion.


-- Funcion para eliminar una cosecha que todavia se encuentre abierta, mediante su id.
cancelarCosecha ::  App ()
cancelarCosecha = do
    conn <- ask  -- Este seria para para inicar la conexcion con la base de datos.
    liftIO $ putStrLn  ">> Apartado para el cancele una cosecha.\n"
    idCosechaIngreado <- liftIO $ leerEntradaNumero "Ingresa el id de la cosecha que desea cerrar: "

    resultado <- liftIO $ validarPosibilidadOptenerCosechaPorID conn idCosechaIngreado
    case resultado of
        -1 -> do 
            liftIO $ putStrLn  "Error: El id de cosecha ingresado no coincide con ninguna cosecha registrada en el sistema.\n"
            liftIO $ hFlush stdout
            opcionesGenerales
        -2 -> do 
            liftIO $ putStrLn  "Error: La cosecha ingresada ya ha sido cerrada, no puede ser cancelada.\n"
            liftIO $ hFlush stdout
            opcionesGenerales
        _ -> do
            _ <- liftIO $ eliminarCosechaDB conn idCosechaIngreado 
            liftIO $ putStrLn "La cosecha se ha cancelado correctamente.\n"
            liftIO $ hFlush stdout
            opcionesGenerales
-- Fin de la funcion.


-- Funcion para modificar los datos de una cosecha.
modificarCosecha :: App ()
modificarCosecha = do
    conn <- ask
    liftIO $ putStrLn ">> Apartado para modificar los datos de una cosecha."

    -- id de la cosecha a modificar.
    idCosecha_modificar <- liftIO $ leerEntradaNumero "Ingrese el ID de la cosecha que desea modificar: "

    resultado <- liftIO $ validarPosibilidadOptenerCosechaPorID conn idCosecha_modificar

    case resultado of
        -1 -> do 
            liftIO $ putStrLn  "Error: El id de cosecha ingresado no coincide con ninguna cosecha registrada en el sistema.\n"
            liftIO $ hFlush stdout
            opcionesGenerales

        -2 -> do 
            liftIO $ putStrLn  "Error: La cosecha ingresada ya ha sido cerrada por lo tanto no puede ser modificada.\n"
            liftIO $ hFlush stdout
            opcionesGenerales

        _ -> do
            -- liftIO $ print "Pass 1."
            datosCosecha_modificar <- liftIO $ optenerCosechaPorID conn idCosecha_modificar

            -- Extraer los valores previos de `datosCosecha_modificar`
            case datosCosecha_modificar of
                Just cosecha -> do
                    -- Cedula del trabajador
                    cedula_nueva <- liftIO $ auxModificarCedulaCosecha conn (cedulaTrabajador cosecha)

                    -- ID de la parcela
                    idParcela_modificar <- liftIO $ auxModificarParcelaCosecha conn (idParcelac cosecha)

                    -- Fecha de inicio
                    fechaInicio_modificar <- liftIO $ auxModificarFechaInicioCosecha conn (fechaInicio cosecha)

                    -- Fecha de finalización
                    fechaFin_modificar <- liftIO $ auxModificarFechaFinalizacionCosecha conn (fechaFin cosecha)

                    -- Tipo de vegetal
                    vegetal_modificar <- liftIO $ auxModificarVegetalCosecha conn (vegetal cosecha)

                    -- Cantidad en kg
                    cantidadEsperada_modificar <- liftIO $ auxModificarCantidaCosecha conn (cantidadKg cosecha) 

                    validacionParaRegistrarCosecha <- liftIO $ validarDatosCosechaEnModificacion conn cedula_nueva (idParcelac cosecha) idParcela_modificar fechaInicio_modificar fechaFin_modificar vegetal_modificar idCosecha_modificar-- Comporbar si los datos ingresados son correctos.

                    -- Comprobar el estado de la validacion para ver si podermos hacer el registro o no.
                    if validacionParaRegistrarCosecha
                        then do
                            liftIO $ modificarCosechaDB conn idCosecha_modificar idParcela_modificar cedula_nueva  fechaInicio_modificar fechaFin_modificar vegetal_modificar cantidadEsperada_modificar
                            liftIO $ putStrLn "La cosecha se ha registrado exitosamente."
                            opcionesGenerales
                        else do
                            liftIO $ putStrLn "Error: No se pudo registrar la cosecha debido a una validación fallida."
                            opcionesGenerales

                    -- Volver al menu de la opciones generales.
                    opcionesGenerales
                
                Nothing -> do
                    liftIO $ putStrLn "Error crítico: No se encontraron datos de la cosecha, aunque la validación fue exitosa."
                    opcionesGenerales
-- Fin de la funcion.

-- Funcion para que el usuario decida si quiere cambiar la cedula del trabajador asignado a una cosecha, en caso de que el usuario no desee cambiarlo, se devolvera el dato anterior, si quiere cambiarlo se devolvera el nuevo dato ingresado.
auxModificarCedulaCosecha :: Connection -> String -> IO String
auxModificarCedulaCosecha conn cedula_anterior = do 
    liftIO $ do
        putStrLn $ "\n>>Cedula actual del trabajador asignado a esta cosecha: " ++ cedula_anterior
        putStrLn "\n¿Desea modificar la cedula del trabajador asignado?"
        putStrLn "1. Si"
        putStrLn "2. No (Mantener actual)"
        putStr "Opcion: "
        hFlush stdout

    opcion <- liftIO getLine

    case opcion of
        "1" -> liftIO $ leerEntradaTexto "Ingrese la nueva cedula del trabajador: "
        "2" -> return cedula_anterior
        _   -> do
            liftIO $ putStrLn "Opcion invalida. Inténtelo de nuevo."
            auxModificarCedulaCosecha conn cedula_anterior
-- Fin de la funcion.


auxModificarParcelaCosecha :: Connection -> Int -> IO Int
auxModificarParcelaCosecha conn p_idParcela_anterior = do 
    liftIO $ do
        putStrLn $ "\n>> ID de la parcela actual: " ++ show p_idParcela_anterior
        putStrLn "\n¿Desea modificar la parcela?"
        putStrLn "1. Si"
        putStrLn "2. No (Mantener actual)"
        putStr "Opcion: "
        hFlush stdout

    opcion <- liftIO getLine

    case opcion of
        "1" -> liftIO $ leerEntradaNumero "Ingrese el nuevo ID de la parcela: "
        "2" -> return p_idParcela_anterior
        _   -> do
            liftIO $ putStrLn "Opcion invalida. Intentelo de nuevo."
            auxModificarParcelaCosecha conn p_idParcela_anterior
-- Fin de la funcion.



auxModificarFechaInicioCosecha :: Connection -> Day -> IO Day
auxModificarFechaInicioCosecha conn p_fechaInicio_anterior = do 
    liftIO $ do
        putStrLn $ "\n>> Fecha de inicio actual: " ++ show p_fechaInicio_anterior
        putStrLn "\n¿Desea modificar la fecha de inicio?"
        putStrLn "1. Si"
        putStrLn "2. No (Mantener actual)"
        putStr "Opcion: "
        hFlush stdout

    opcion <- liftIO getLine
    case opcion of
        "1" -> do
            nuevaFechaInicioSTR <- liftIO $ leerEntradaTexto "Ingrese la nueva fecha de inicio (ej: 30/04/2025): "
            let formato = "%d/%m/%Y"
            case parseTimeM True defaultTimeLocale formato nuevaFechaInicioSTR :: Maybe Day of
                Just nuevaFecha -> return nuevaFecha
                Nothing -> do
                    liftIO $ putStrLn "Error: Fecha invalida. Intentelo de nuevo."
                    auxModificarFechaInicioCosecha conn p_fechaInicio_anterior
        "2" -> return p_fechaInicio_anterior
        _   -> do
            liftIO $ putStrLn "Opcion invalida. Intentelo de nuevo."
            auxModificarFechaInicioCosecha conn p_fechaInicio_anterior
-- Fin de la funcion.


auxModificarFechaFinalizacionCosecha :: Connection -> Day -> IO Day
auxModificarFechaFinalizacionCosecha conn p_fechaFinalizacion_anterior = do 
    liftIO $ do
        putStrLn $ "\n>> Fecha de finalizacion actual: " ++ show p_fechaFinalizacion_anterior
        putStrLn "\n¿Desea modificar la fecha de finalizacion?"
        putStrLn "1. Si"
        putStrLn "2. No (Mantener actual)"
        putStr "Opcion: "
        hFlush stdout

    opcion <- liftIO getLine
    case opcion of
        "1" -> do
            nuevaFechaFinalizacionSTR <- liftIO $ leerEntradaTexto "Ingrese la nueva fecha de finalizacion (ej: 30/04/2026): "
            let formato = "%d/%m/%Y"
            case parseTimeM True defaultTimeLocale formato nuevaFechaFinalizacionSTR :: Maybe Day of
                Just nuevaFecha -> return nuevaFecha
                Nothing -> do
                    liftIO $ putStrLn "Error: Fecha invalida. Intentelo de nuevo."
                    auxModificarFechaFinalizacionCosecha conn p_fechaFinalizacion_anterior
        "2" -> return p_fechaFinalizacion_anterior
        _   -> do
            liftIO $ putStrLn "Opcion invalida. Intentelo de nuevo."
            auxModificarFechaFinalizacionCosecha conn p_fechaFinalizacion_anterior
-- Fin de la funcion.


auxModificarVegetalCosecha :: Connection -> String -> IO String
auxModificarVegetalCosecha conn p_vegetal_anterior = do 
    liftIO $ do
        putStrLn $ "\n>> Vegetal actual de la cosecha: " ++ p_vegetal_anterior
        putStrLn "\n¿Desea modificar el vegetal asignado?"
        putStrLn "1. Si"
        putStrLn "2. No (mantener actual)"
        putStr "Opcion: "
        hFlush stdout

    opcion <- liftIO getLine
    case opcion of
        "1" -> liftIO $ leerEntradaTexto "Ingrese el nuevo vegetal asignado a la cosecha: "
        "2" -> return p_vegetal_anterior
        _   -> do
            liftIO $ putStrLn "Opcion invalida. Intentelo de nuevo."
            auxModificarVegetalCosecha conn p_vegetal_anterior

-- Fin de la funcion.

auxModificarCantidaCosecha :: Connection -> Int -> IO Int
auxModificarCantidaCosecha conn p_cantidad_anterior = do 
    liftIO $ do
        putStrLn $ "\n>> Cantidad del vegetal que se espera recoger actual: " ++ show p_cantidad_anterior
        putStrLn "\n¿Desea modificar la cantidad esperada?"
        putStrLn "1. Si"
        putStrLn "2. No (Mantener actual)"
        putStr "Opcion: "
        hFlush stdout

    opcion <- liftIO getLine

    case opcion of
        "1" -> liftIO $ leerEntradaNumero "Ingrese la nueva cantidad que espera recoger: "
        "2" -> return p_cantidad_anterior
        _   -> do
            liftIO $ putStrLn "Opcion invalida. Intentelo de nuevo."
            auxModificarCantidaCosecha conn p_cantidad_anterior
-- Fin de la funcion.



-- Funcion para hacer de forma separada todas las validaciones de los datos de la cosecha que se va a registrar. (En esta podrias imprimir mesanjes de personalizados por cada false que se tenga.)
validarDatosCosechaEnModificacion :: Connection -> String -> Int -> Int -> Day -> Day -> String -> Int -> IO Bool
validarDatosCosechaEnModificacion conn cedula idParcelaAnterior idParcelaNueva fechaInicio fechaFin vegetal p_codigo_cosecha = do
    liftIO $ putStrLn "Datos que llegaron."

    print (idParcelaAnterior, idParcelaNueva)
    
    trabajadorValido <- trabajadorExiste conn cedula
    print trabajadorValido

    parcelaValida <- parcelaExiste conn idParcelaNueva
    print parcelaValida

    let fechasOk = fechasValidas (Just fechaInicio) (Just fechaFin)  
    parcelaDisponibleValida <- 
        if idParcelaAnterior == idParcelaNueva
            then parcelaDisponibleExcluyendoActual conn idParcelaNueva p_codigo_cosecha fechaInicio fechaFin
            else parcelaDisponible conn idParcelaNueva fechaInicio fechaFin
    print fechasOk
    print parcelaDisponibleValida

    vegetalValido <- vegetalPerteneceAParcela conn idParcelaNueva vegetal
    print vegetalValido

    return $ trabajadorValido && parcelaValida && fechasOk && parcelaDisponibleValida && vegetalValido
-- Fin de la funcion.

parcelaDisponibleExcluyendoActual :: Connection -> Int -> Int -> Day -> Day -> IO Bool
parcelaDisponibleExcluyendoActual conn idParcela idCosechaActual fechaInicio fechaFin = do
    resultado <- query conn 
        "SELECT COUNT(*) FROM Cosechas WHERE idParcela = ? AND estadoCosecha = 'Abierto' \
        \AND idCosecha != ? \
        \AND ((fechainicio BETWEEN ? AND ?) OR (fechafin BETWEEN ? AND ?))"
        (idParcela, idCosechaActual, fechaInicio, fechaFin, fechaInicio, fechaFin) :: IO [Only Int]
    print resultado
    return $ case resultado of
        [Only count] -> count == 0
        _            -> False
-- Fin de la funcion.


-- Funcion para registrar los datos de una cosecha en la base de datos.
modificarCosechaDB :: Connection -> Int -> Int -> String -> Day -> Day -> String -> Int -> IO ()
modificarCosechaDB conn idCosecha idParcela cedula fechaInicio fechaFin vegetal cantidad = do
    let fechaInicioSQL = formatTime defaultTimeLocale "%Y-%m-%d" fechaInicio
    let fechaFinSQL = formatTime defaultTimeLocale "%Y-%m-%d" fechaFin

    _ <- execute conn
        "UPDATE Cosechas SET idParcela = ?, fechainicio = ?, fechafin = ?, cedula = ?, nombrevege = ?, KilosPlanificados = ? \
        \WHERE idCosecha = ?"
        (idParcela, fechaInicioSQL, fechaFinSQL, cedula, vegetal, cantidad, idCosecha)
    
    return ()

--  Funcion para el menu de despliegue del menu para la consulta de la deisponibilidad de parcelas en un rango de fechas especifico.
consultaDisponibilidadParcelas :: App ()
consultaDisponibilidadParcelas = do
    conn <- ask 
    liftIO $ do
        putStrLn "\n>> == Apartado para la consulta de la disponibilida de parcelas por rango de fechas."
        putStrLn "1. Ver las parcelas disponibles en un rango de fechas."
        putStrLn "2. Ver estado de las parcelas por dia en rango de fechas."
        putStrLn "3. Volver"
        putStr "Opcion: "
        hFlush stdout

    opcion <- liftIO getLine
    case opcion of
        "1" -> do
            liftIO $ putStrLn "\nIngrese el rango de fechas:"
            fechaInicio <- liftIO solicitarFechas
            fechaFin <- liftIO solicitarFechas
            parcelas <- liftIO $ obtenerParcelasDisponibles conn fechaInicio fechaFin
            liftIO $ do
                putStrLn "\n>> Parcelas disponibles:"
                if null parcelas
                    then putStrLn "   > No hay parcelas disponibles en el rango de fechas indicado."
                    else mapM_ (\idParcela -> putStrLn $ "   > ID: " ++ show idParcela) parcelas
            consultaDisponibilidadParcelas 

        "2" -> do
            liftIO $ putStrLn "\nIngrese el rango de fechas:"
            fechaInicio <- liftIO solicitarFechas
            fechaFin <- liftIO solicitarFechas
            estados <- liftIO $ obtenerEstadoParcelasPorDia conn fechaInicio fechaFin
            liftIO $ mostrarEstadoParcelasAgrupadas estados  
            consultaDisponibilidadParcelas

        "3" -> opcionesOperativas 

        _   -> liftIO (putStrLn "Opción inválida") >> consultaDisponibilidadParcelas
-- Fin de la funcion.

-- Funcion para permitir el ingreso de fechas de forma mas facil y con validaciones a los usuario.
solicitarFechas :: IO Day
solicitarFechas = do
    liftIO $ do
        nuevaFechaFinalizacionSTR <- liftIO $ leerEntradaTexto "Ingrese la nueva fecha de finalizacion (ej: 30/04/2026): "
        let formato = "%d/%m/%Y"
        case parseTimeM True defaultTimeLocale formato nuevaFechaFinalizacionSTR :: Maybe Day of
            Just nuevaFecha -> return nuevaFecha
            Nothing -> do
                liftIO $ putStrLn "Error: Fecha invalida. Intentelo de nuevo."
                solicitarFechas 
-- Fin de la funcion.

-- Funcion para hacer la consulta a la base de datos para optener todas las parcelas que esten disponibles en el rango de fechas indicado.
obtenerParcelasDisponibles :: Connection -> Day -> Day -> IO [Int]
obtenerParcelasDisponibles conn fechaInicio fechaFin = do
    resultados <- query conn
        "SELECT DISTINCT idParcela FROM Parcela \
        \WHERE idParcela NOT IN \
        \(SELECT idParcela FROM Cosechas WHERE estadoCosecha = 'Abierto' \
        \AND ((fechainicio BETWEEN ? AND ?) OR (fechafin BETWEEN ? AND ?)))"
        (fechaInicio, fechaFin, fechaInicio, fechaFin) :: IO [Only Int]

    return $ map fromOnly resultados
-- Fin de la funcion.


-- Consulta a la base de datos para optener por dia el estado de las parcelas en un rango de fechas indicado.
obtenerEstadoParcelasPorDia :: Connection -> Day -> Day -> IO [(Int, String, String)]
obtenerEstadoParcelasPorDia conn fechaInicio fechaFin = do
    let diasEnRango = [fechaInicio .. fechaFin]  -- Genera la lista de fechas

    resultados <- forM diasEnRango $ \fecha -> do
        let fechaStr = formatTime defaultTimeLocale "%Y-%m-%d" fecha  
        
        query conn
            "SELECT idParcela, ? AS fecha, \
            \(CASE WHEN EXISTS (SELECT 1 FROM Cosechas WHERE idParcela = Parcela.idParcela AND estadoCosecha = 'Abierto' \
            \AND fechainicio <= ? AND fechafin >= ?) \
            \THEN 'Utilizada' ELSE 'Disponible' END) AS estado \
            \FROM Parcela"
            (fechaStr, fechaStr, fechaStr) :: IO [(Int, String, String)]

    return $ concat resultados
-- Fin de la funcion.

-- Esta seria la funcion que se encarga de agrupar y ordenar los datos de las parcelas que retorno la consulta a la base de datos.
mostrarEstadoParcelasAgrupadas :: [(Int, String, String)] -> IO ()
mostrarEstadoParcelasAgrupadas resultados = do
    -- Ordenamos primero por idParcela para agrupar correctamente
    let resultadosOrdenados = sortOn (\(idParcela, _, _) -> idParcela) resultados

    -- Agrupamos por idParcela comparando solo el primer elemento de la tupla
    let agrupados = groupBy (\(id1, _, _) (id2, _, _) -> id1 == id2) resultadosOrdenados

    -- Mostramos cada grupo
    mapM_ mostrarParcelaEnRangoFecha agrupados
-- Fin de la funcion.

-- Funcion para mostrar los datos de las parcelas procesados, para lo de consulta de parcelas por rango de fechas
mostrarParcelaEnRangoFecha :: [(Int, String, String)] -> IO ()
mostrarParcelaEnRangoFecha parcelaDatos = do
    case parcelaDatos of
        ((idParcela, _, _):_) -> do
            putStrLn $ "\n>> Parcela: " ++ show idParcela
            mapM_ (\(_, fecha, estado) -> putStrLn $ "   > Fecha: " ++ fecha ++ " -> " ++ estado) parcelaDatos
-- Fin de la funcion.


