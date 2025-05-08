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
(235735583, 'Jaime Serrano Mendez', 'Peon'),
(803456789, 'Camila Rodríguez Solano', 'Supervisor'),
(609876542, 'Fernando Méndez Aguilar', 'Operario'),
(501234678, 'Valeria Castillo Muñoz', 'Agrónomo'),
(304567812, 'Ricardo López Fernández', 'Peon'),
(207654321, 'Esteban Jiménez Vargas', 'Supervisor');

create table Herramientas(
	codigo varchar(12) primary key,
    nombre varchar(25) not null,
    descripcion varchar(70) not null,
    tipo varchar(15) not null CHECK (tipo IN ('manual', 'motorizada', 'automatizada'))
);

create table Parcela (
    idParcela int auto_increment primary key,
    nombre varchar(30) not null,
    zona varchar(30) not null,
    area int not null
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
WHERE 
    c.estadoCosecha = 'Cerrado'
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
WHERE 
    c.estadoCosecha = 'Cerrado'
GROUP BY 
    c.cedula
ORDER BY 
    CosechasTrabajadas DESC
LIMIT 1;



-- DROP VIEW IF EXISTS RecoleccionPorMes;
CREATE VIEW RecoleccionPorMes AS
SELECT 
    MONTH(fechafin) AS mes,
    YEAR(fechafin) AS anio,
    SUM(KilosRecogidos) AS Recoleccion
FROM 
    Cosechas
WHERE
    estadoCosecha = 'Cerrado'
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
    Cosechas
WHERE 
    estadoCosecha = 'Cerrado';


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


