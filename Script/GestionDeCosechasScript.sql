create database fincaAgricola;
use fincaAgricola;

create table Trabajadores(
	cedula varchar(9) primary key,
    nombreCompleto varchar(40),
    rol varchar(20)
);
insert into Trabajadores(cedula, nombreCompleto, rol) 
values 
(703070931, 'Bryan Londoño Marchena', 'Supervisor'),
(703050943, 'Loki Dosantos Aveiro', 'Peon'),
(403212345, 'Jack Mora Brenes', 'Operario'),
(102345675, 'Sergio Mendoza Soto', 'Agrónomo'),
(235735583, 'Jaime Serrano Mendez', 'Peon');

create table Herramientas(
	codigo varchar(12) primary key,
    nombre varchar(25),
    descripcion varchar(70),
    tipo varchar(15)
);

create table Parcela (
    idParcela int auto_increment primary key,
    nombre varchar(30),
    zona varchar(30),
    area int
);

CREATE TABLE HerramientasPorParcela (
    IdParcela int NOT NULL,
    CodigoHerramienta varchar(12) NOT NULL,
    primary key(CodigoHerramienta, idParcela),

    constraint fk_Herramienta_Parcela foreign key (CodigoHerramienta) references Herramientas(codigo),
    constraint fk_Parcela_Herramienta foreign key (idParcela) references Parcela(idParcela)
);

CREATE TABLE VegetalesPorParcela (
    IdParcela INT NOT NULL,
    NombreVegetal VARCHAR(30) NOT NULL,
    Precio FLOAT NOT NULL,
    PRIMARY KEY (IdParcela, NombreVegetal),

    CONSTRAINT FK_Vegetal_Parcela FOREIGN KEY (IdParcela) references Parcela(idParcela)
);
 
create table Cosechas (
    idCosecha int auto_increment primary key,
    idParcela int,
    fechainicio date,
    fechafin date,
    cedula varchar(9),
    nombrevege varchar(20),  
    estadoCosecha varchar(15), -- Este seria para indicar si esta Abierto, Cerrada.
    KilosPlanificados int null,
    KilosRecogidos int null,
    constraint fk_ParcelaC foreign key (idParcela) references Parcela(idParcela),
    constraint fk_CedulaC foreign key (cedula) references Trabajadores(cedula)
);

use fincaAgricola; 
CREATE VIEW ParcelaMayorVolumen AS
SELECT 
    p.nombre AS nombreParcela,
    SUM(c.KilosRecogidos) AS Volumen
FROM 
    Cosechas c
JOIN 
    Parcela p ON c.idParcela = p.idParcela
GROUP BY 
    c.idParcela
ORDER BY 
    Volumen DESC
LIMIT 1;


-- select * from TrabajadorMasCosechas
use fincaAgricola;
CREATE VIEW TrabajadorMasCosechas AS
SELECT 
    t.nombreCompleto AS nombreTrabajador,
    COUNT(*) AS CosechasTrabajadas
FROM 
    Cosechas c
JOIN 
    Trabajadores t ON c.cedula = t.cedula
GROUP BY 
    c.cedula
ORDER BY 
    CosechasTrabajadas DESC
LIMIT 1;



DROP VIEW IF EXISTS RecoleccionPorMes;
CREATE VIEW RecoleccionPorMes AS
SELECT 
    MONTH(fechafin) AS mes,
    YEAR(fechafin) AS anio,
    SUM(KilosRecogidos) AS Recoleccion
FROM 
    Cosechas
GROUP BY 
    YEAR(fechafin),
    MONTH(fechafin)
ORDER BY 
    anio DESC, mes DESC;
SELECT * FROM RecoleccionPorMes;

-- drop VIEW ParcelasMayorVenta 
USE fincaAgricola;

CREATE VIEW ParcelasMayorVenta AS
SELECT 
    c.idParcela,
    p.nombre,
    SUM(c.KilosRecogidos * vpp.Precio) AS ventaTotal
FROM 
    Cosechas c
JOIN 
    VegetalesPorParcela vpp ON c.idParcela = vpp.idParcela 
        AND c.nombreVege = vpp.NombreVegetal
JOIN 
    Parcela p ON c.idParcela = p.idParcela
WHERE 
    c.estadoCosecha = 'Cerrado'
GROUP BY 
    c.idParcela
ORDER BY 
    ventaTotal DESC
LIMIT 3;

-- select * from ParcelasMayorVenta


use fincaAgricola;
CREATE VIEW VistaCosechasEstado AS
SELECT 
    idCosecha, 
    idParcela,
    KilosPlanificados,
    KilosRecogidos,
    CASE
        WHEN KilosRecogidos < KilosPlanificados THEN 'Subproducción'
        WHEN KilosRecogidos > KilosPlanificados THEN 'Sobreproducción'
        ELSE 'Producción correcta'
    END AS EstadoCosecha
FROM 
    Cosechas;

-- SELECT * FROM VistaCosechasEstado;



USE fincaAgricola;
CREATE VIEW MuestroEstadisticaIni AS 
SELECT
    c.idCosecha,
    c.idParcela,
    p.nombre AS nombreParcela,
    vpp.NombreVegetal,
    c.fechaFin,
    c.kilosPlanificados,
    c.kilosRecogidos
FROM 
    Cosechas c
JOIN
    Parcela p ON c.idParcela = p.idParcela
JOIN
    VegetalesPorParcela vpp 
    ON c.idParcela = vpp.idParcela 
    AND c.nombreVege = vpp.NombreVegetal;
-- select * from MuestroEstadisticaIni





-- delimiter $$
-- use fincaAgricola$$
-- create procedure sp_ValidarPosibilidaOptenerCosechaID (in p_idCosecha int) 
-- begin 
-- 	-- Variables para para los datos que se retornaran en caso de error.
-- 	declare existe int default 0;
-- 	declare estado varchar(15) default '';
--     declare resultado int default 0;
--     select count(*) into existe from Cosechas where idCosecha = p_idCosecha;
    
--     -- Validar que la cosecha exista.
--     if existe = 0 then
-- 		-- select -1 as Resultado;
--         set resultado := -1;
-- 	else
-- 		-- Ahora validar que el estado sea "Abierto".
-- 		select estadoCosecha into estado from Cosechas where idCosecha = p_idCosecha;
-- 		if estado != 'Abierto' then
-- 			-- select -2 as Resultado;
--             set resultado := -2;
-- 		else
-- 			-- En caso de que no haya error entonces devolvemos los datos de la tabla.
--             -- select 1 as Resultado;
--             set resultado := 1;
-- 		end if;
-- 	end if;
--     SELECT resultado AS Resultado; -- Devolver el resultado en un solo elemento, esto evita que se se espere que se devulvan todos los posibles valores a haskell.
-- end $$
-- DELIMITER ;
-- -- CALL sp_ValidarPosibilidaOptenerCosechaID(2);

-- DELIMITER $$
-- USE fincaAgricola$$

-- CREATE PROCEDURE sp_ExisteCosechaPorID (IN p_idCosecha INT)
-- BEGIN
--     DECLARE existe INT DEFAULT 0;

--     SELECT COUNT(*) INTO existe FROM Cosechas WHERE idCosecha = p_idCosecha;

--     IF existe = 0 THEN
--         SELECT -1 AS Resultado; -- No existe
--     ELSE
--         SELECT 1 AS Resultado;  -- Existe
--     END IF;
-- END$$

-- DELIMITER ;

-- DELIMITER $$
-- USE fincaAgricola$$

-- CREATE PROCEDURE sp_EstadoCosechaEsAbierto (IN p_idCosecha INT)
-- BEGIN
--     DECLARE estado VARCHAR(15) DEFAULT '';

--     SELECT estadoCosecha INTO estado FROM Cosechas WHERE idCosecha = p_idCosecha;

--     IF estado != 'Abierto' THEN
--         SELECT -2 AS Resultado; -- No está en estado Abierto
--     ELSE
--         SELECT 1 AS Resultado;  -- Sí está en estado Abierto
--     END IF;
-- END$$

-- DELIMITER ;





-- -- Procedure para optener una cosecha especifica por su id, en caso de no encontrar la cosecha devolveremos un codigo de error.
-- delimiter $$
-- use fincaAgricola$$
-- create procedure sp_OptenerCosechaID (in p_idCosecha int) 
-- begin 

--     select * from Cosechas where idCosecha = p_idCosecha;
    
-- end $$
-- DELIMITER ;    
-- -- call sp_OptenerCosechaID();
-- -- drop procedure sp_OptenerCosechaID;

-- delimiter $$
-- use fincaAgricola$$
-- create procedure sp_ModificarEstadoCosecha (in p_idCosecha int, in p_Estado varchar(15), in p_kilosRecolectados int) 
-- begin 

--     update Cosechas
--     set 
--     estadoCosecha = p_Estado,
--     KilosRecogidos = p_kilosRecolectados
--     where idCosecha = p_idCosecha;
    
-- end $$
-- DELIMITER ; 
-- -- call sp_ModificarEstadoCosecha(1, 'Cerrado', 3);
-- -- drop procedure sp_ModificarEstadoCosecha;

-- -- Procedure para eliminar una cosecha.
-- delimiter $$
-- use fincaAgricola$$
-- create procedure sp_EliminarCosecha (in p_idCosecha int) 
-- begin 
    
--     delete from Cosechas
--     where idCosecha = p_idCosecha;
    
-- end $$
-- DELIMITER ; 

