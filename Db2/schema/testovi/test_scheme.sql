-- ==================================================================================
-- BANKING SYSTEM - VALIDACIJA DEPLOYMENT-A
-- Environment: TEST
-- ==================================================================================

SET CURRENT SCHEMA = 'BANKING_TEST';

-- ==================================================================================
-- PROVJERA 1: POSTOJANJE TABLICA
-- ==================================================================================

SELECT 'REFERENCE TABLICE:' AS KATEGORIJA, COUNT(*) AS BROJ_TABLICA
FROM SYSIBM.SYSTABLES 
WHERE CREATOR = 'BANKING_TEST' 
AND NAME IN ('POSLOVNICE', 'RIZIK_OCJENA_REF', 'STATUS_REF', 
             'TIP_RACUNA_REF', 'TRANSAKCIJA_TIP_REF', 'IZVORNI_SUSTAV_REF')

UNION ALL

SELECT 'MASTER ENTITETI:', COUNT(*)
FROM SYSIBM.SYSTABLES 
WHERE CREATOR = 'BANKING_TEST' 
AND NAME IN ('KLIJENTI', 'PROIZVOD_MASTER', 'RACUNI')

UNION ALL

SELECT 'NORMALIZIRANE TABLICE:', COUNT(*)
FROM SYSIBM.SYSTABLES 
WHERE CREATOR = 'BANKING_TEST' 
AND NAME IN ('KLIJENT_ADRESE', 'KLIJENT_KONTAKTI', 'KLIJENT_DOKUMENTI', 
             'KAMATNE_STOPE', 'RACUN_PROIZVODI')

UNION ALL

SELECT 'TRANSAKCIJSKE TABLICE:', COUNT(*)
FROM SYSIBM.SYSTABLES 
WHERE CREATOR = 'BANKING_TEST' 
AND NAME IN ('TRANSAKCIJE', 'BLOKADE', 'RED_TRANSAKCIJA', 'GLAVNA_KNJIGA')

UNION ALL

SELECT 'UKUPNO TABLICA:', COUNT(*)
FROM SYSIBM.SYSTABLES 
WHERE CREATOR = 'BANKING_TEST';

-- ==================================================================================
-- PROVJERA 2: BROJ ZAPISA U REFERENCE TABLICAMA
-- ==================================================================================

SELECT 'POSLOVNICE' AS TABLICA, COUNT(*) AS BROJ_ZAPISA FROM POSLOVNICE
UNION ALL
SELECT 'RIZIK_OCJENA_REF', COUNT(*) FROM RIZIK_OCJENA_REF
UNION ALL
SELECT 'STATUS_REF', COUNT(*) FROM STATUS_REF
UNION ALL
SELECT 'TIP_RACUNA_REF', COUNT(*) FROM TIP_RACUNA_REF
UNION ALL
SELECT 'TRANSAKCIJA_TIP_REF', COUNT(*) FROM TRANSAKCIJA_TIP_REF
UNION ALL
SELECT 'IZVORNI_SUSTAV_REF', COUNT(*) FROM IZVORNI_SUSTAV_REF
UNION ALL
SELECT 'PROIZVOD_MASTER', COUNT(*) FROM PROIZVOD_MASTER
UNION ALL
SELECT 'KAMATNE_STOPE', COUNT(*) FROM KAMATNE_STOPE
ORDER BY TABLICA;

-- ==================================================================================
-- PROVJERA 3: POSTOJANJE INDEKSA
-- ==================================================================================

SELECT 'UKUPNO INDEKSA:' AS TIP, COUNT(*) AS BROJ
FROM SYSIBM.SYSINDEXES 
WHERE CREATOR = 'BANKING_TEST'

UNION ALL

SELECT 'UNIQUE INDEKSI:', COUNT(*)
FROM SYSIBM.SYSINDEXES 
WHERE CREATOR = 'BANKING_TEST' 
AND UNIQUERULE IN ('U', 'P')

UNION ALL

SELECT 'OBIČNI INDEKSI:', COUNT(*)
FROM SYSIBM.SYSINDEXES 
WHERE CREATOR = 'BANKING_TEST' 
AND UNIQUERULE NOT IN ('U', 'P');

-- ==================================================================================
-- PROVJERA 4: POSTOJANJE SEKVENCI
-- ==================================================================================

SELECT 'UKUPNO SEKVENCI:' AS TIP, COUNT(*) AS BROJ
FROM SYSIBM.SYSSEQUENCES 
WHERE SCHEMA = 'BANKING_TEST';

-- Prikaz svih sekvenci s trenutnim vrijednostima
SELECT NAME AS SEKVENCA, 
       START AS POCETNA_VRIJEDNOST,
       INCREMENT AS KORAK
FROM SYSIBM.SYSSEQUENCES 
WHERE SCHEMA = 'BANKING_TEST'
ORDER BY NAME;

-- ==================================================================================
-- PROVJERA 5: FOREIGN KEY CONSTRAINTS
-- ==================================================================================

SELECT 'UKUPNO FK CONSTRAINTS:' AS TIP, COUNT(*) AS BROJ
FROM SYSIBM.SYSRELS
WHERE CREATOR = 'BANKING_TEST';

-- Prikaz svih foreign key veza
SELECT TBNAME AS CHILD_TABLICA, 
       REFTBNAME AS PARENT_TABLICA,
       RELNAME AS FK_NAME
FROM SYSIBM.SYSRELS 
WHERE CREATOR = 'BANKING_TEST'
ORDER BY TBNAME, REFTBNAME;

-- ==================================================================================
-- PROVJERA 6: CHECK CONSTRAINTS
-- ==================================================================================

SELECT 'UKUPNO CHECK CONSTRAINTS:' AS TIP, COUNT(*) AS BROJ
FROM SYSIBM.SYSCHECKS 
WHERE CREATOR = 'BANKING_TEST';

-- ==================================================================================
-- PROVJERA 7: TABLESPACE KORIŠTENJE
-- ==================================================================================

SELECT TSNAME AS TABLESPACE_NAME, COUNT(*) AS BROJ_TABLICA
FROM SYSIBM.SYSTABLES 
WHERE CREATOR = 'BANKING_TEST'
AND TSNAME IS NOT NULL
GROUP BY TSNAME
ORDER BY TSNAME;

-- ==================================================================================
-- PROVJERA 8: SAMPLE TESTOVI REFERENTNIH VEZA
-- ==================================================================================

INSERT INTO KLIJENTI (
    KUPAC_ID, OIB, IME, PREZIME, DATUM_RODJENJA, 
    RIZIK_OCJENA, POSLOVNICA_KREIRANJE, KREIRAO_USER
) VALUES (
    NEXT VALUE FOR SEQ_KUPAC_ID, 
    '12345678901', 
    'Test', 
    'Klijent', 
    '1990-01-01', 
    'S1', 
    '0001', 
    'IBMUSER'
);

-- Test 2: Provjeri mogu li kreirati račun za test klijenta
\echo 'Test 2: Kreiranje test računa...'

INSERT INTO RACUNI (
    RACUN_BROJ, KUPAC_ID, TIP_RACUNA, TRENUTNI_SALDO, 
    DOSTUPNI_SALDO, POSLOVNICA_OTVARANJA, DATUM_OTVARANJA, KREIRAO_USER
) VALUES (
    '100000000001', 
    (SELECT KUPAC_ID FROM KLIJENTI WHERE OIB = '12345678901'), 
    'TEK', 
    1000.00, 
    1000.00, 
    '0001', 
    CURRENT DATE, 
    'IBMUSER'
);

-- Test 3: Provjeri mogu li kreirati transakciju

INSERT INTO TRANSAKCIJE (
    TRANSAKCIJA_ID, RACUN_BROJ, KOD_TRANSAKCIJE, IZNOS,
    DATUM_TRANSAKCIJE, DATUM_KNJIZENJA, OPIS, TEKUCI_SALDO,
    IZVORNI_SUSTAV, POSLOVNICA_UNOS, OPERATER_UNOS
) VALUES (
    '20250723000000000001',
    '100000000001',
    'UPL',
    1000.00,
    CURRENT DATE,
    CURRENT DATE,
    'Test uplata',
    1000.00,
    'BAN',
    '0001',
    'IBMUSER'
);

COMMIT;

-- ==================================================================================
-- PROVJERA 9: FINALNA VALIDACIJA
-- ==================================================================================

-- Provjeri da su test podaci uspješno uneseni
SELECT 'TEST KLIJENTI:' AS TIP, COUNT(*) AS BROJ 
FROM KLIJENTI WHERE OIB = '12345678901'

UNION ALL

SELECT 'TEST RAČUNI:', COUNT(*) 
FROM RACUNI WHERE RACUN_BROJ = '100000000001'

UNION ALL

SELECT 'TEST TRANSAKCIJE:', COUNT(*) 
FROM TRANSAKCIJE WHERE RACUN_BROJ = '100000000001';

-- Provjeri join preko foreign key veza
SELECT 
    K.IME || ' ' || K.PREZIME AS KLIJENT,
    R.RACUN_BROJ,
    R.TRENUTNI_SALDO,
    T.IZNOS AS ZADNJA_TRANSAKCIJA,
    P.NAZIV_POSLOVNICE
FROM KLIJENTI K
JOIN RACUNI R ON K.KUPAC_ID = R.KUPAC_ID
JOIN TRANSAKCIJE T ON R.RACUN_BROJ = T.RACUN_BROJ
JOIN POSLOVNICE P ON R.POSLOVNICA_OTVARANJA = P.POSLOVNICA_KOD
WHERE K.OIB = '12345678901';