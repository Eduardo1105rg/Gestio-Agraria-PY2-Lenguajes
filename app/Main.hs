{-# LANGUAGE OverloadedStrings #-} --Esto es para que los string o char los pase a Query 


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
import Data.List (nub)
import Data.Time
import Control.Monad (void)


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
            connectHost = "172.22.112.1",
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
        "2" -> cerrarCosecha
        "3" -> consultarCosechaPorID
        "4" -> cancelarCosecha
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




--Esto se puede ver como un menu o sub menu para que a las parcelas se les pueda agregar herramientas de forma
-- consecutiva pero mostrandole al usuario las herramientas registradas hasta el momento
extraParcelasHerramientas :: [Herramienta] -> App [Herramienta]
extraParcelasHerramientas acumuladas = do
    conn <- ask
    herramientas <- liftIO $ obtenerHerramientas conn

    liftIO $ do
        putStrLn "\n¿Desea agregar una herramienta?"
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

            -- Primero revisamos que el número sea válido 
            if seleccion >= 1 && seleccion <= length herramientas
                then do
                    --Creamos una variable y le damos la selección -1 para que quede bien pero también
                    -- es importante que esto !! lo que hace es acceder a la lista de objetos de las herramientas
                    -- en el indice que le di, es como hacer esto en python Lista[1]
                    let herramientaSeleccionada = herramientas !! (seleccion - 1)
                    liftIO $ putStrLn $ "Has seleccionado: " ++ nombreHA herramientaSeleccionada
                    --Llamamos recursivamente y agregamos la heeramienta al inicio de acumuladas
                    extraParcelasHerramientas (herramientaSeleccionada : acumuladas)
                else do
                    liftIO $ putStrLn "Número inválido"
                    extraParcelasHerramientas acumuladas
        "2" -> return (reverse acumuladas)
        _ -> do
            liftIO $ putStrLn "Opción inválida"
            extraParcelasHerramientas acumuladas


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




crearParcelaDB :: String -> String -> Float -> App Int
crearParcelaDB nombrePAR zonaPAR areaPAR = do
    conn <- ask
    -- Primero insertamos la parcela
    liftIO $ insertarParcela conn nombrePAR zonaPAR areaPAR
    -- Luego obtenemos el ID
    liftIO $ obtenerUltimoID conn

insertarParcela :: Connection -> String -> String -> Float -> IO ()
insertarParcela conn nombre zona area = do
    execute conn 
        "INSERT INTO Parcela (nombre, zona, area) VALUES (?,?,?)" 
        (nombre, zona, area)
    return ()


obtenerUltimoID :: Connection -> IO Int
obtenerUltimoID conn = do
    result <- query_ conn "SELECT LAST_INSERT_ID()" :: IO [Only Int]
    case result of
        [Only id] -> return id
        _ -> error "No se pudo obtener el ID insertado"

    
crearParcelaExtraDB :: [(String, Float)] -> [Herramienta] -> Int -> App ()
crearParcelaExtraDB vegetalesP herramientasP idParcela = do
    conn <- ask
    liftIO $ forM_ vegetalesP $ \(nombreVege, precio) -> 
        forM_ herramientasP $ \herramienta -> do
            execute conn 
                "INSERT IGNORE INTO ParcelasFin(nombreVege, codigoHerramienta, idParcela, precio) VALUES (?,?,?,?)" 
                (nombreVege, codigoHA herramienta, idParcela, precio)




obtenerTrabajadores :: Connection -> IO [Trabajador]
obtenerTrabajadores conn = do
    trabajadores <- query_ conn "SELECT nombreCompleto, cedula, rol FROM Trabajadores" 
    return (map (\(n,c,r) -> Trabajador n c r) trabajadores) -- registramos trabajadores en la app

buscaTrabajador :: String -> [Trabajador] -> Bool
buscaTrabajador cedulaBuscada = any (\t -> cedulaBuscada == cedula t)



obtenerTodasLasParcelas :: App [Parcela]
obtenerTodasLasParcelas = do
    conn <- ask
    idsResult <- lift $ query_ conn "SELECT idParcela FROM Parcela" :: App [Only Int]
    let ids = map fromOnly idsResult
    mapM (construirParcelaCompleta conn) ids



construirParcelaCompleta :: Connection -> Int -> App Parcela
construirParcelaCompleta conn idParc = do
    [parcelaBase] <- lift $ query conn 
        "SELECT nombre, zona, area FROM Parcela WHERE idParcela = ?" 
        (Only idParc) :: App [(String, String, Int)]
    let (nombre, zona, area) = parcelaBase

    vegetales <- lift $ query conn 
        "SELECT DISTINCT nombreVege, precio FROM ParcelasFin WHERE idParcela = ?" 
        (Only idParc) :: App [(String, Float)]

    codigosHerramientas <- lift $ query conn 
        "SELECT DISTINCT codigoHerramienta FROM ParcelasFin WHERE idParcela = ?" 
        (Only idParc) :: App [Only String]

    herramientas <- mapM (obtenerHerramienta conn) (nub $ map fromOnly codigosHerramientas)

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


subMenuCosecha :: App ()
subMenuCosecha = do
    liftIO $ putStrLn "Entraste al apartado de gestión de cosecha"

    -- Cedula del trabajador
    liftIO $ putStr "Ingrese el identificador del trabajador (cédula): "
    liftIO $ hFlush stdout
    cedula <- liftIO getLine

    -- id de la parcela
    liftIO $ putStr "Ingrese el ID de la parcela: "
    liftIO $ hFlush stdout
    idParcelaStr <- liftIO getLine
    let idParcela = read idParcelaStr :: Int

    -- Fecha de inicio
    liftIO $ putStr "Fecha de inicio (ej: 30/04/2025): "
    liftIO $ hFlush stdout
    fechaInicioStr <- liftIO getLine
    let formato = "%d/%m/%Y"
    let fechaInicio = parseTimeM True defaultTimeLocale formato fechaInicioStr :: Maybe Day

    case fechaInicio of
        Just fecha -> do
            
            let fechaInicioF = formatTime defaultTimeLocale "%Y-%m-%d" fecha
            liftIO $ putStrLn $ "Fecha en formato MySQL: " ++ fechaInicioF
        Nothing -> do
            liftIO $ putStrLn "Fecha en formato incorrecto. Intenta de nuevo."
            return ()

   
    liftIO $ putStr "Fecha de fin (ej: 30/04/2026): "
    liftIO $ hFlush stdout
    fechaFinStr <- liftIO getLine
    let fechaFin = parseTimeM True defaultTimeLocale formato fechaFinStr :: Maybe Day

    case fechaFin of
        Just fecha -> do
            let fechaFinF = formatTime defaultTimeLocale "%Y-%m-%d" fecha
            liftIO $ putStrLn $ "Fecha en formato MySQL: " ++ fechaFinF
        Nothing -> do
            liftIO $ putStrLn "Fecha en formato incorrecto. Intenta de nuevo."
            return ()

    -- Tipo de vegetal
    liftIO $ putStr "Tipo de vegetal: "
    liftIO $ hFlush stdout
    vegetal <- liftIO getLine
    liftIO $ putStrLn $ "El vegetal ingresado es: " ++ vegetal


    -- Cantidad en kg
    cantidadEsperada <-  liftIO $ leerEntradaNumero "Ingrese los kilogramos que se esperan recoger de este vegetal: "
    -- liftIO $ putStr "Cantidad en KG: "
    -- liftIO $ hFlush stdout
    -- cantidadStr <- liftIO getLine
    -- let cantidad = read cantidadStr :: Int

    let estado = "Abierto" -- Tipo de estado: Abierto, Cerrado, Cancelado.

    let k_recogidos = 0

    conn <- ask
    liftIO $ insertCosecha conn idParcela cedula fechaInicio fechaFin vegetal cantidadEsperada estado k_recogidos

    liftIO $ putStrLn "Cosecha registrada correctamente."

    cosecha <- liftIO $ obtenerCosecha conn
    liftIO $ print cosecha 

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


obtenerCosecha :: Connection -> IO [Cosecha]
obtenerCosecha conn = do
    resultados <- query_ conn "SELECT idCosecha, idParcela, fechainicio, fechafin, cedula, nombrevege, estadoCosecha, KilosPlanificados, KilosRecogidos FROM Cosechas"
    return $ map (\(idC, idP, fi, ff, ce, nv, kp, ec, kr) -> 
        Cosecha idC idP fi ff ce nv kp ec kr) resultados

-- Funcion para crear una entrada de teclado para texto, se valida que lo que se ingrese no seva vacio..
leerEntradaTexto :: String -> IO String
leerEntradaTexto mensaje = do
    putStrLn mensaje
    hFlush stdout
    entrada <- getLine
    if null entrada
        then do
            putStrLn "La entrada no puede estar vacía. Inténtelo de nuevo."
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
            putStrLn "Entrada invalida. Debe ser un numero mayor a 0. Intentelo de nuevo."
            leerEntradaNumero mensaje
-- Fin de la funcion.


-- Validar la posibilidad de optener una cosecha por su id, -1: No se encontro la cosecha, -2: No esta en estado Abierto, 1: Se puede optener la cosecha por su ID.
validarPosibilidadOptenerCosechaPorID :: Connection -> Int -> IO Int
validarPosibilidadOptenerCosechaPorID conn idCosecha_Validar = do
    resultado <- query conn "CALL sp_ValidarPosibilidaOptenerCosechaID(?)" (Only idCosecha_Validar) :: IO [Only Int] -- El only es por que se espera que enviemos una tupla, pero en este caso podria dar error.
    -- El IO [[SQLInteger]], se indica el dato que se espera.
    case resultado of 
        [Only valor] -> return $ fromIntegral valor
        _ -> return (-99) -- Codigo de error desconocido.       
    -- return 1
-- Fin de la funcion.

-- Funcion para validar si el id de una cosecha existe.
validarExistenciaCosechaID :: Connection -> Int -> IO Int
validarExistenciaCosechaID conn idCosecha = do
    resultado <- query conn "CALL sp_ExisteCosechaPorID(?)" (Only idCosecha) :: IO [Only Int]
    case resultado of
        [Only valor] -> return valor
        _            -> return (-99) -- Error inesperado   

validarEstadoCosechaID :: Connection -> Int -> IO Int
validarEstadoCosechaID conn idCosecha = do
    resultado <- query conn "CALL sp_EstadoCosechaEsAbierto(?)" (Only idCosecha) :: IO [Only Int]
    case resultado of
        [Only valor] -> return valor
        _            -> return (-99) -- Error inesperado 

validarTodoEnUna :: Connection -> Int -> IO Int
validarTodoEnUna conn idCosecha = do
    resultado <- query conn
        "SELECT CASE WHEN NOT EXISTS (SELECT 1 FROM Cosechas WHERE idCosecha = ?) THEN -1 \
        \WHEN EXISTS (SELECT 1 FROM Cosechas WHERE idCosecha = ? AND estadoCosecha != 'Abierto') THEN -2 \
        \ELSE 1 END AS Resultado"
        (idCosecha, idCosecha) :: IO [Only Int]
    case resultado of
        [Only valor] -> return valor
        _            -> return (-99)


-- Optener los datos de una cosecha por su id 
-- optenerCosechaPorID :: Connection -> Int -> IO [Cosecha]
-- optenerCosechaPorID conn idCosecha_Optener = do
--     resultados <- query conn "CALL sp_OptenerCosechaID(?)" (Only idCosecha_Optener) :: IO [(Int, Int, Day, Day, String, String, String, Int, Int)]
--     return $ map (\(idC, idP, fi, ff, ce, nv, ec, kp, kr) -> 
--         Cosecha idC idP fi ff ce nv kp ec kr) resultados
optenerCosechaPorID :: Connection -> Int -> IO Cosecha
optenerCosechaPorID conn idCosecha_Optener = do
    resultados <- query conn "CALL sp_OptenerCosechaID(?)" (Only idCosecha_Optener) :: IO [(Int, Int, Day, Day, String, String, String, Int, Int)]
    case resultados of
        [(idC, idP, fi, ff, ce, nv, ec, kp, kr)] -> return $ Cosecha idC idP fi ff ce nv kp ec kr
        _ -> error "Error crítico: No se encontraron datos de la cosecha, aunque la validación fue exitosa."
-- Fin de la funcion.


-- Funcion para modificar el estado de una cosecha mediente su id.
actualizarEstadoCosecha :: Connection -> Int ->  String -> Int -> IO ()
actualizarEstadoCosecha conn idCosecha_estado nuevoEstado p_kilosRecogidos = do
    _ <- execute conn "CALL sp_ModificarEstadoCosecha(?, ?, ?)" (idCosecha_estado, nuevoEstado, p_kilosRecogidos)
    -- _ <- query_ conn (fromString $ "CALL sp_ModificarEstadoCosecha(" ++ show idCosecha_estado ++ ", " ++ nuevoEstado ++ ", " ++ show p_kilosRecogidos ++ ")") :: IO [Only Int]

    -- void $ (query_ conn "SELECT 1" :: IO [Only Int]) 
    return ()
-- Fin de la funcion.


-- Funcion para cerrar una cosecha, el usuario ingresara el id de la cosecha que desea cerrar y la cantidad de kilos recogidos.
cerrarCosecha ::  App ()
cerrarCosecha = do
    conn <- ask  -- Este seria para para inicar la conexcion con la base de datos.
    liftIO $ putStrLn  ">> Apartado para el cierre de la cosecha."
    idCosechaIngreado <- liftIO $ leerEntradaNumero "Ingresa el id de la cosecha que desea cerrar: "

    resultado <- liftIO $ validarTodoEnUna conn idCosechaIngreado
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
-- cerrarCosecha :: App ()
-- cerrarCosecha = do
--     conn <- ask  -- Conexión a la base de datos
--     liftIO $ putStrLn ">> Apartado para el cierre de la cosecha."
    
--     idCosechaIngresado <- liftIO $ leerEntradaNumero "Ingresa el id de la cosecha que desea cerrar: "

--     -- Validar existencia de la cosecha
--     existencia <- liftIO $ validarExistenciaCosechaID conn idCosechaIngresado
--     case existencia of
--         -1 -> do
--             liftIO $ putStrLn "Error: El id de cosecha ingresado no coincide con ninguna cosecha registrada en el sistema.\n"
--             liftIO $ hFlush stdout
--             opcionesGenerales
--         _ -> do
--             -- Validar que esté en estado Abierto
--             estado <- liftIO $ validarEstadoCosechaID conn idCosechaIngresado
--             case estado of
--                 -2 -> do
--                     liftIO $ putStrLn "Error: La cosecha ingresada ya ha sido cerrada.\n"
--                     liftIO $ hFlush stdout
--                     opcionesGenerales
--                 _ -> do
--                     -- Continuar con el cierre
--                     kilosRecogidos_Input <- liftIO $ leerEntradaNumero "Ingresa la cantidad de kilogramos recogidos en esta cosecha: "
--                     _ <- liftIO $ actualizarEstadoCosecha conn idCosechaIngresado "Cerrado" kilosRecogidos_Input
--                     liftIO $ putStrLn "La cosecha se ha cerrado correctamente. \n"
--                     liftIO $ hFlush stdout
--                     opcionesGenerales


mostrarDatosCosecha :: Cosecha -> IO ()
mostrarDatosCosecha p_datosCosechaMostrar = do
    putStrLn  "\n>> ==== Detalles de la cosecha ===="
    putStrLn $ "ID: " ++ show (idCosecha p_datosCosechaMostrar)
    putStrLn $ "Parcela ID: " ++ show (idParcelac p_datosCosechaMostrar)
    putStrLn $ "Fecha de inicio: " ++ show (fechaInicio p_datosCosechaMostrar)
    putStrLn $ "Fecha de fin: " ++ show (fechaFin p_datosCosechaMostrar)
    putStrLn $ "Trabajador (cédula): " ++ cedulaTrabajador p_datosCosechaMostrar
    putStrLn $ "Vegetal: " ++ vegetal p_datosCosechaMostrar
    putStrLn $ "Cantidad (Kg): " ++ show (cantidadKg p_datosCosechaMostrar)
    putStrLn $ "Estado: " ++ estadoCosecha p_datosCosechaMostrar
    putStrLn $ "Kilos recogidos: " ++ show (kilosRecogidos p_datosCosechaMostrar)
    putStrLn "> == Fin de los datos de la cosecha. =="
    hFlush stdout


consultarCosechaPorID :: App ()
consultarCosechaPorID = do
    conn <- ask -- Este seria para para inicar la conexcion con la base de datos.
    liftIO $ putStrLn  ">> Apartado para la consultas de cosechas por su ID.\n"
    idCosechaIngreado <- liftIO $ leerEntradaNumero "Ingresa el id de la cosecha que desea ver: "

    resultado <- liftIO $ validarPosibilidadOptenerCosechaPorID conn idCosechaIngreado

    case resultado of
        -1 -> do 
            liftIO $ putStrLn  "Error: El id de cosecha ingresado no coincide con ninguna cosecha registrada en el sistema.\n"
            liftIO $ hFlush stdout
            opcionesGenerales

        _ -> do
            datosCosecha <- liftIO $ optenerCosechaPorID conn idCosechaIngreado
            _ <- liftIO $ mostrarDatosCosecha datosCosecha
            opcionesGenerales


eliminarCosechaDB :: Connection -> Int -> IO ()
eliminarCosechaDB conn idCosecha_eliminar = do
    _ <- execute conn "CALL sp_EliminarCosecha(?)" (Only idCosecha_eliminar)
    return ()

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


-- Ok, voy a estar trabajando en esta funcion para el registro de una cosecha, de momento ya tenia esto programado, pero me esta dando algunos problemas y faltaban validaciones: