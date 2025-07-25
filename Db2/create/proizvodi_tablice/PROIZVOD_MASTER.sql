-- PROIZVOD MASTER - SAMO OSNOVNI PODACI
SET CURRENT SCHEMA = 'BANKING_TEST';
CREATE TABLESPACE TSPROIZV IN "BANKTEST" USING STOGROUP SYSDEFLT;
CREATE TABLE PROIZVOD_MASTER (
    PROIZVOD_KOD        CHAR(4)         NOT NULL,
    NAZIV_PROIZVODA     VARCHAR(50)     NOT NULL,
    OPIS_PROIZVODA      VARCHAR(200),
    TIP_RACUNA          CHAR(3)         NOT NULL,
    OSNOVNA_KAMATA      DECIMAL(5,4)    DEFAULT 0.0000,
    MINIMALNI_SALDO     DECIMAL(10,2)   DEFAULT 0.00,
    MJESECNA_NAKNADA    DECIMAL(7,2)    DEFAULT 0.00,
    MINUS_NAKNADA       DECIMAL(7,2)    DEFAULT 0.00,
    LIMIT_TRANSAKCIJA   INTEGER         DEFAULT 999,
    STATUS              CHAR(1)         DEFAULT 'A',
    DATUM_VAZENJA       DATE            NOT NULL,
    DATUM_ISTEKA        DATE,
    
    CONSTRAINT PK_PROIZVOD_MASTER PRIMARY KEY (PROIZVOD_KOD),
    CONSTRAINT FK_PROIZVOD_TIP FOREIGN KEY (TIP_RACUNA) 
        REFERENCES TIP_RACUNA_REF (TIP_KOD),
    CONSTRAINT CHK_PROIZVOD_STATUS CHECK 
     (STATUS IN ('A', 'N', 'B')),
    CONSTRAINT CHK_PROIZVOD_DATUMI CHECK 
     (DATUM_ISTEKA IS NULL OR DATUM_ISTEKA > DATUM_VAZENJA)
) IN BANKTEST.TSPROIZV;