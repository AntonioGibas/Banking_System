-- ==================================================================================
-- BANKING SYSTEM - ROLLBACK SCRIPT
-- Environment: TEST
-- Platform: IBM DB2 z/OS v13
--
-- WARNING: THIS SCRIPT WILL DELETE ALL DATA AND STRUCTURE!
-- The destructive section is wrapped in a comment block for safety.
-- Uncomment the block below ONLY when you intentionally want to wipe everything.
-- ==================================================================================

/*

SET CURRENT SCHEMA = 'BANKING_TEST';

-- ==================================================================================
-- STEP 1: DELETE DATA (REVERSE FK DEPENDENCY ORDER)
-- ==================================================================================

DELETE FROM GLAVNA_KNJIGA;
DELETE FROM RED_TRANSAKCIJA;
DELETE FROM TRANSAKCIJE;
DELETE FROM BLOKADE;
DELETE FROM RACUN_PROIZVODI;
DELETE FROM KLIJENT_DOKUMENTI;
DELETE FROM KLIJENT_KONTAKTI;
DELETE FROM KLIJENT_ADRESE;
DELETE FROM KAMATNE_STOPE;
DELETE FROM RACUNI;
DELETE FROM KLIJENTI;
DELETE FROM PROIZVOD_MASTER;
DELETE FROM IZVORNI_SUSTAV_REF;
DELETE FROM TRANSAKCIJA_TIP_REF;
DELETE FROM TIP_RACUNA_REF;
DELETE FROM STATUS_REF;
DELETE FROM RIZIK_OCJENA_REF;
DELETE FROM POSLOVNICE;

-- ==================================================================================
-- STEP 2: DROP TABLES (REVERSE FK DEPENDENCY ORDER)
-- ==================================================================================

DROP TABLE GLAVNA_KNJIGA;
DROP TABLE RED_TRANSAKCIJA;
DROP TABLE TRANSAKCIJE;
DROP TABLE BLOKADE;
DROP TABLE RACUN_PROIZVODI;
DROP TABLE KLIJENT_DOKUMENTI;
DROP TABLE KLIJENT_KONTAKTI;
DROP TABLE KLIJENT_ADRESE;
DROP TABLE KAMATNE_STOPE;
DROP TABLE RACUNI;
DROP TABLE KLIJENTI;
DROP TABLE PROIZVOD_MASTER;
DROP TABLE IZVORNI_SUSTAV_REF;
DROP TABLE TRANSAKCIJA_TIP_REF;
DROP TABLE TIP_RACUNA_REF;
DROP TABLE STATUS_REF;
DROP TABLE RIZIK_OCJENA_REF;
DROP TABLE POSLOVNICE;

-- ==================================================================================
-- STEP 3: DROP SEQUENCES
-- ==================================================================================

DROP SEQUENCE SEQ_STOPA_ID;
DROP SEQUENCE SEQ_DOKUMENT_ID;
DROP SEQUENCE SEQ_KONTAKT_ID;
DROP SEQUENCE SEQ_ADRESA_ID;
DROP SEQUENCE SEQ_RED_ID;
DROP SEQUENCE SEQ_UNOS_GK_ID;
DROP SEQUENCE SEQ_BLOKADA_ID;
DROP SEQUENCE SEQ_TRANSAKCIJA_ID;
DROP SEQUENCE SEQ_KUPAC_ID;

-- ==================================================================================
-- STEP 4: DROP TABLESPACES
-- ==================================================================================

DROP TABLESPACE BANKTEST.TSGLAVNA;
DROP TABLESPACE BANKTEST.TSRED;
DROP TABLESPACE BANKTEST.TSTRANS;
DROP TABLESPACE BANKTEST.TSBLOK;
DROP TABLESPACE BANKTEST.TSRACPRO;
DROP TABLESPACE BANKTEST.TSDOKUM;
DROP TABLESPACE BANKTEST.TSKONTKT;
DROP TABLESPACE BANKTEST.TSADRESE;
DROP TABLESPACE BANKTEST.TSKAMATA;
DROP TABLESPACE BANKTEST.TSRACUNI;
DROP TABLESPACE BANKTEST.TSKLIENT;
DROP TABLESPACE BANKTEST.TSPROIZV;
DROP TABLESPACE BANKTEST.TSIZVOR;
DROP TABLESPACE BANKTEST.TSTIPTRN;
DROP TABLESPACE BANKTEST.TSTIPRAC;
DROP TABLESPACE BANKTEST.TSSTATUS;
DROP TABLESPACE BANKTEST.TSRIZIK;
DROP TABLESPACE BANKTEST.TSPOSLOV;

-- ==================================================================================
-- STEP 5: DROP DATABASE (OPTIONAL - removes everything else inside)
-- ==================================================================================

DROP DATABASE BANKTEST;

COMMIT;

*/

-- ==================================================================================
-- END OF ROLLBACK SCRIPT
--
-- To execute:
--   1. Remove the surrounding comment block.
--   2. Verify you are connected to the correct DB2 subsystem.
--   3. Run the script.
--
-- WARNING: NO RECOVERY AFTER DELETION!
-- ==================================================================================
