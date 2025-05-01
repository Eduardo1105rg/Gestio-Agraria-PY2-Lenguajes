{-# LANGUAGE OverloadedStrings #-} --Esto es para que los string o char los pase a Query 


import Database.MySQL.Simple
import Database.MySQL.Simple.Types (Only(..))
import System.IO (hFlush, stdout)
import Data.String (fromString)
import Control.Monad.Reader
import Data.List (nub)
import Data.Time



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
} deriving (Show)

data Parcela = Parcela {
    idParcela :: Int, 
    nombreP :: String,
    zonaP :: String,
    areaP :: Float, 
    vegetalesP :: [(String, Float)],  -- una tupla para la parte de vegetales y precio
    herramientasP :: [Herramienta]
} deriving (Show)



data Cosecha = Cosecha {
    idCosecha    :: Int,
    idParcelac    :: Int,            
    trabajadores :: [Trabajador],   
    fechaInicio  :: String,
    fechaFin     :: String,
    vegetal      :: String,         -- tipo de vegetal recolectado
    cantidadKg   :: Float           -- cuántos kilos se recolectaron
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
            connectHost = "192.168.50.136",
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
    liftIO $ putStrLn "\n1. Gestión de cosechas"
    liftIO $ putStrLn "2. Cierre de cosechas"
    liftIO $ putStrLn "3. Consultar cosecha"
    liftIO $ putStrLn "4. Cancelación o modificación de cosecha"
    liftIO $ putStrLn "5. Consulta disponibilidad de parcela"
    liftIO $ putStrLn "6. Volver"
    liftIO $ putStr "Opción: "
    liftIO $ hFlush stdout
    opcionSubG <- liftIO getLine
    case opcionSubG of
        "1" -> subMenuCosecha
        "2" -> liftIO $ putStrLn "Entrando a Cierre de cosechas"
        "3" -> liftIO $ putStrLn "Entrando a Consultar cosecha"
        "4" -> liftIO $ putStrLn "Entrando a Cancelación o modificación de cosecha"
        "5" -> liftIO $ putStrLn "Entrando a Consulta disponibilidad de parcela"
        "6" -> return ()  
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
        "3" -> liftIO (putStrLn "cccc") >> opcionesOperativas
        "4" -> return ()   
        _   -> do
            liftIO $ putStrLn "Opción inválida"
            opcionesOperativas 


--Este es el apartado para agregar herramientas, ya sea si queremos solo agregar o solo ver, falta darle una opcion de volver
menuHerramientasOP :: App ()
menuHerramientasOP = do
    liftIO $ do
        putStrLn "Estamos en el apartado de herramientas"
        putStrLn "\n1. Agregar herramientas"
        putStrLn "2. Ver todas las herramientas"
        hFlush stdout
    
    opcionHOP <- liftIO getLine
    case opcionHOP of
        "1" -> agregarHerramientas
        "2" -> do
            conn <- ask
            herramientas <- liftIO $ obtenerHerramientas conn
            liftIO $ mapM_ imprimirHerramienta herramientas
            menuHerramientasOP
        _ -> do
            liftIO $ putStrLn "Opción invalida"
            menuHerramientasOP

--Esto es para cuando queremos agregar una herramienta, así le pedimos al usuario que la registre en el sistema
--Luego de esto la pasamos directo a la base de datos para insertarla.
agregarHerramientas :: App ()
agregarHerramientas = do 
    liftIO $ do
        putStrLn "Estas agregando herramientas"
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
    
    liftIO $ putStrLn "Vamos a revisar si es posible agregarla"
    agregarHerramientasBase codigoH nombreH descripcionH tipoH

--Aqui le pasamos el codigo,nombre,descripcion y tipo de la herramienta para que esta se pueda
--Registrar en la base de datos
agregarHerramientasBase :: String -> String -> String -> String -> App ()
agregarHerramientasBase codigoH nombreH descripcionH tipoH = do
    conn <- ask --Pedimos la conexion y la ejecutamos por medio del execute conn el cual envia
    --ese string como un query a mysql los valores , en este caso tienen signo de pregunta
    --para que la base le asigne el tipo de valor
    liftIO $ execute conn "INSERT INTO herramientas VALUES (?,?,?,?)" 
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
        putStrLn "2. Ver todas las parcelas"
        putStrLn "3. Volver"
        putStr "Opción: "
        hFlush stdout
    opcion <- liftIO getLine
    case opcion of
        "1" -> agregarParcelas >> menuParcela --Aqui llamo a agregar parcelas, registro, me devuelvo sin resultado y llamo al menú
        "2" -> do
            parcelas <- obtenerTodasLasParcelas --Creo el objeto parcelas con la funcion y uso <- para que le de su resultado
            liftIO $ do
                putStrLn "\n=== Lista de Parcelas ==="
                mapM_ imprimirParcela parcelas
        "3" -> return ()
        _   -> liftIO (putStrLn "Opción inválida") >> menuParcela

        


--Esta parte es solo para agregar parcelas, una vez ya se hayan terminado de agregar los datos, envio la parcela
-- a la base de datos para que se cargue, importante las herramientas y vegetales se muestran aparte porque generalmente
-- estas pueden repetirse entonces para no afectar el flujo se hacen aparte    
agregarParcelas :: App ()
agregarParcelas = do 
    liftIO $ putStrLn "\nEstas agregando una nueva parcela"

    liftIO $ putStr "Ingrese el nombre de la parcela: "
    liftIO $ hFlush stdout
    nombrePAR <- liftIO getLine

    liftIO $ putStr "Ingrese la zona de la parcela: "
    liftIO $ hFlush stdout
    zonaPAR <- liftIO getLine

    liftIO $ putStr "Ingrese el área en metros cuadrados: "
    liftIO $ hFlush stdout
    areaStr <- liftIO getLine
    let areaPAR = read areaStr :: Float

    conn <- ask
    herramientasDisponibles <- liftIO $ obtenerHerramientas conn
    liftIO $ do
        putStrLn "\n=== Herramientas disponibles ==="
        mapM_ (uncurry imprimirHerramientaExtra) (zip [1..] herramientasDisponibles)

    herramientasSeleccionadas <- extraParcelasHerramientas herramientasDisponibles
    vegetales <- extraParcelas []
    idParcela <- crearParcelaDB nombrePAR zonaPAR areaPAR
    crearParcelaExtraDB vegetales herramientasSeleccionadas idParcela
    liftIO $ do
        putStrLn "\nParcela registrada con éxito (simulado)"
        putStrLn $ "ID: " ++ show idParcela  -- Mostramos el ID aquí
        putStrLn $ "Nombre: " ++ nombrePAR
        putStrLn $ "Zona: " ++ zonaPAR
        putStrLn $ "Área: " ++ show areaPAR
        putStrLn $ "Vegetales: " ++ show vegetales
        putStrLn $ "Herramientas: " ++ show (herramientasSeleccionadas)


    


    
    liftIO $ putStrLn "Parcela registrada correctamente"



