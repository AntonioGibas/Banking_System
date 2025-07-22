-- ==================================================================================
-- BANKING SYSTEM - ROLLBACK SCRIPT
-- Environment: TEST
-- PAŽNJA: OVAJ SCRIPT BRIŠE SVE PODATKE I STRUKTURU!
-- ==================================================================================

\echo ''
\echo '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
\echo '!! PAŽNJA: ROLLBACK BANKING_TEST SUSTAVA !!'
\echo '!! OVAJ SCRIPT ĆE OBRISATI SVE PODATKE !!'
\echo '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
\echo ''

-- Samo kao sigurnosna provjera
-- UKLONITE KOMENTAR ISPOD SAMO AKO STVARNO ŽELITE OBRISATI SVE!

/*

SET CURRENT SCHEMA = 'BANKING_TEST';

-- ==================================================================================
-- KORAK 1: BRISANJE PODATAKA IZ TABLICA (OBRNUT REDOSLIJED DEPENDENCIES)
-- ==================================================================================

\echo 'Brišem podatke iz tablica...'

-- Transakcijske tablice (nema child dependencija)
DELETE FROM GLAVNA_KNJIGA;
DELETE FROM RED_TRANSAKCIJA;
DELETE FROM TRANSAKCIJE;
DELETE FROM BLOKADE;

-- Many-to-many veze
DELETE FROM RACUN_PROIZVODI;

-- Normalizirane tablice (ovisne o master tablicama)
DELETE FROM KLIJENT_DOKUMENTI;
DELETE FROM KLIJENT_KONTAKTI;
DELETE FROM KLIJENT_ADRESE;
DELETE FROM KAMATNE_STOPE;

-- Master entiteti
DELETE FROM RACUNI;
DELETE FROM KLIJENTI;
DELETE FROM PROIZVOD_MASTER;

-- Reference tablice (zadnje jer su parent)
DELETE FROM IZVORNI_SUSTAV_REF;
DELETE FROM TRANSAKCIJA_TIP_REF;
DELETE FROM TIP_RACUNA_REF;
DELETE FROM STATUS_REF;
DELETE FROM RIZIK_OCJENA_REF;
DELETE FROM POSLOVNICE;

\echo 'Svi podaci obrisani.'

-- ==================================================================================
-- KORAK 2: BRISANJE TABLICA (OBRNUT REDOSLIJED FOREIGN KEY DEPENDENCIES)
-- ==================================================================================

\echo 'Brišem tablice...'

-- Transakcijske tablice
DROP TABLE IF EXISTS GLAVNA_KNJIGA;
DROP TABLE IF EXISTS RED_TRANSAKCIJA;
DROP TABLE IF EXISTS TRANSAKCIJE;
DROP TABLE IF EXISTS BLOKADE;

-- Many-to-many i normalizirane tablice
DROP TABLE IF EXISTS RACUN_PROIZVODI;
DROP TABLE IF EXISTS KLIJENT_DOKUMENTI;
DROP TABLE IF EXISTS KLIJENT_KONTAKTI;
DROP TABLE IF EXISTS KLIJENT_ADRESE;
DROP TABLE IF EXISTS KAMATNE_STOPE;

-- Master entiteti
DROP TABLE IF EXISTS RACUNI;
DROP TABLE IF EXISTS KLIJENTI;
DROP TABLE IF EXISTS PROIZVOD_MASTER;

-- Reference tablice
DROP TABLE IF EXISTS IZVORNI_SUSTAV_REF;
DROP TABLE IF EXISTS TRANSAKCIJA_TIP_REF;
DROP TABLE IF EXISTS TIP_RACUNA_REF;
DROP TABLE IF EXISTS STATUS_REF;
DROP TABLE IF EXISTS RIZIK_OCJENA_REF;
DROP TABLE IF EXISTS POSLOVNICE;

\echo 'Sve tablice obrisane.'

-- ==================================================================================
-- KORAK 3: BRISANJE SEKVENCI
-- ==================================================================================

\echo 'Brišem sekvence...'

DROP SEQUENCE IF EXISTS SEQ_STOPA_ID;
DROP SEQUENCE IF EXISTS SEQ_DOKUMENT_ID;
DROP SEQUENCE IF EXISTS SEQ_KONTAKT_ID;
DROP SEQUENCE IF EXISTS SEQ_ADRESA_ID;
DROP SEQUENCE IF EXISTS SEQ_RED_ID;
DROP SEQUENCE IF EXISTS SEQ_UNOS_GK_ID;
DROP SEQUENCE IF EXISTS SEQ_BLOKADA_ID;
DROP SEQUENCE IF EXISTS SEQ_TRANSAKCIJA_ID;
DROP SEQUENCE IF EXISTS SEQ_KUPAC_ID;

\echo 'Sve sekvence obrisane.'

-- ==================================================================================
-- KORAK 4: BRISANJE TABLESPACE-OVA (OPCIONALNO)
-- ==================================================================================

\echo 'Brišem custom tablespace-ove...'

-- PAŽNJA: Ovo može utjecati na druge sustave ako se koriste!
-- Uklonite komentar samo ako ste sigurni da su ovi tablespace-ovi 
-- ekskluzivno za BANKING_TEST

-- DROP TABLESPACE TS_BANK_IX_TEST;
-- DROP TABLESPACE TS_BANK_TRANS_TEST;
-- DROP TABLESPACE TS_BANK_MASTER_TEST;
-- DROP TABLESPACE TS_BANK_REF_TEST;

\echo 'Tablespace-ovi zadržani (sigurnost).'

-- ==================================================================================
-- KORAK 5: BRISANJE SCHEMA (OPCIONALNO)
-- ==================================================================================

\echo 'Schema BANKING_TEST zadržana za ponovnu upotrebu.'

-- Ako želite kompletno obrisati schema:
-- DROP SCHEMA BANKING_TEST RESTRICT;

COMMIT;

\echo ''
\echo '==============================================='
\echo 'BANKING_TEST SUSTAV USPJEŠNO UKLONJEN!'
\echo '==============================================='
\echo 'Obrisano:'
\echo '- Svi podaci'
\echo '- Sve tablice'  
\echo '- Sve sekvence'
\echo '- Svi indeksi (automatski s tablicama)'
\echo ''
\echo 'Zadržano:'
\echo '- Schema BANKING_TEST'
\echo '- Tablespace-ovi'
\echo ''
\echo 'Možete ponovno pokrenuti deployment skripte.'
\echo '==============================================='

*/

\echo ''
\echo 'ROLLBACK SCRIPT JE SPREMAN ALI KOMENTIRAN.'
\echo ''
\echo 'Za izvršavanje:'
\echo '1. Uklonite /* i */ komentare'
\echo '2. Provjerite da stvarno želite obrisati sve'
\echo '3. Pokrenite script'
\echo ''
\echo 'PAŽNJA: NEMA POVRATKA NAKON BRISANJA!'