# TEHNIČKA DOKUMENTACIJA BANKOVNOG SUSTAVA BAZE PODATAKA

## 1. PREGLED SUSTAVA

### 1.1 Osnovno
- **Platform**: IBM DB2 z/OS v13
- **Arhitektura**: Normalizirana relacijska baza podataka
- **Normalizacijska razina**: 3NF (Treća normalna forma)
- **Encoding**: UTF-8 (CCSID UNICODE)
- **Transakcijski model**: ACID compliant

### 1.2 Karakteristike sustava
- **Hijerarhijski pristup**: Organiziran prema bankovnim standardima
- **Referentni integritet**: Potpuno implementiran kroz foreign key constraints
- **Skalabilnost**: Dizajniran za velike volumene transakcija
- **Performanse**: Optimizirano kroz indekse i sekvence
- **Sigurnost**: Implementirane sigurnosne razine i audit trail

## 2. ARHITEKTURA BAZE PODATAKA

### 2.1 Database i tablespace organizacija

```sql
-- Database
CREATE DATABASE BANKTEST
    BUFFERPOOL BP1
    INDEXBP    BP2
    STOGROUP   SYSDEFLT
    CCSID      UNICODE;

-- Tablespace strategija (4K page size)
TS_REFERENCE  -> TSPOSLOV, TSRIZIK, TSSTATUS, TSTIPRAC, TSTIPTRN, TSIZVOR (8MB svaki)
TS_MASTER     -> TSKLIENT, TSPROIZV (32MB svaki)
TS_TRANSACT   -> TSTRANS, TSRED, TSGLAVNA, TSBLOK (128MB svaki)
TS_DEPENDENT  -> TSADRESE, TSKONTKT, TSDOKUM, TSKAMATA, TSRACPRO, TSRACUNI (16MB svaki)
```

### 2.2 Buffer pool strategija (DB2 z/OS)

```sql
BP0  -- DB2 catalog/directory (REZERVIRAN - ne koristiti za korisničke objekte)
BP1  -- Reference tablice (POSLOVNICE, *_REF)
BP2  -- Indeksi (svi user indexes)
BP3  -- Master entiteti (KLIJENTI, RACUNI, PROIZVOD_MASTER)
BP4  -- Transakcijske tablice (TRANSAKCIJE, BLOKADE, GLAVNA_KNJIGA)

-- Provjera buffer pool aktivacije:
-DISPLAY BUFFERPOOL(BP1)
-DISPLAY BUFFERPOOL(BP2)
```

### 2.3 Kategorije tablica

#### A) REFERENCE TABLICE (Lookup/Code Tables)
- `POSLOVNICE` - Organizacijska struktura
- `RIZIK_OCJENA_REF` - Rizični profili
- `STATUS_REF` - Standardizirani status kodovi
- `TIP_RACUNA_REF` - Tipovi računa
- `TRANSAKCIJA_TIP_REF` - Tipovi transakcija
- `IZVORNI_SUSTAV_REF` - Izvorni sustavi

#### B) MASTER ENTITETI
- `KLIJENTI` - Osnovni klijent podaci
- `PROIZVOD_MASTER` - Katalog bankovnih proizvoda
- `RACUNI` - Računi klijenata

#### C) NORMALIZIRANI PODACI
- `KLIJENT_ADRESE` - Adresni podaci (1:N)
- `KLIJENT_KONTAKTI` - Kontakt informacije (1:N)
- `KLIJENT_DOKUMENTI` - Dokumentacija (1:N)
- `KAMATNE_STOPE` - Kamatni razredi (1:N)
- `RACUN_PROIZVODI` - Račun-proizvod veze (M:N)

#### D) TRANSAKCIJSKI SUSTAV
- `TRANSAKCIJE` - Provedene transakcije
- `RED_TRANSAKCIJA` - Queue za obradu
- `BLOKADE` - Blokade računa
- `GLAVNA_KNJIGA` - Glavno knjižni zapisi

## 3. DETALJNI OPIS TABLICA

### 3.1 KLIJENTI (Master entitet)
```sql
Primarna tablica: KUPAC_ID (BIGINT, 12 digits)
Unique constraint: OIB (11 karaktera)
Foreign keys:
  - RIZIK_OCJENA → RIZIK_OCJENA_REF
  - POSLOVNICA_KREIRANJE → POSLOVNICE
```

**Kritična polja:**
- `KUPAC_ID`: Sekvencijski broj 100000000000-999999999999
- `OIB`: Osobni identifikacijski broj (validacija duljine - vidjeti sekciju 7.1)
- `RIZIK_OCJENA`: Poveznica na rizični profil
- `DATUM_KREIRANJE`/`ZADNJE_AZURIRANJE`: Audit trail

### 3.2 RACUNI (Core banking entity)
```sql
Primarna tablica: RACUN_BROJ (CHAR(12))
Foreign keys:
  - KUPAC_ID → KLIJENTI
  - TIP_RACUNA → TIP_RACUNA_REF
  - POSLOVNICA_OTVARANJA → POSLOVNICE
```

**Kritična polja:**
- `TRENUTNI_SALDO`/`DOSTUPNI_SALDO`: Preciznost DECIMAL(15,2)
- `MINUS_LIMIT`: Overdraft limit
- `ZADNJA_AKTIVNOST`: Performance optimization timestamp

**Business rules:**
```sql
CHK_SALDO_LOGIKA: DOSTUPNI_SALDO <= TRENUTNI_SALDO + MINUS_LIMIT
```

### 3.3 TRANSAKCIJE (Transakcijski core)
```sql
Volumeni: Visoki (milijuni zapisa)
Particioniranje: Po DATUM_TRANSAKCIJE (mjesečno) - vidjeti sekciju 5.3
Archiving: Stariji od 7 godina
```

**Ključni indeksi:**
- `IDX_TRANSAKCIJE_RACUN` - Transakcije po računu
- `IDX_TRANSAKCIJE_DATUM` - Datumski opsezi
- `IDX_TRANSAKCIJE_STATUS` - Status filtriranje
- `IDX_TRANSAKCIJE_RACUN_DATUM` - Composite za izvod računa

### 3.4 BLOKADE (Risk management)
```sql
Real-time impact: Utječe na DOSTUPNI_SALDO
Tipovi blokada: CEK, PRA, SIG, KRE, NAP
Automatsko istjecanje: Preko DATUM_ISTEKA
```

## 4. NORMALIZACIJA I VEZE

### 4.1 Normalizacijski principi
- **1NF**: Atomske vrijednosti, jedinstveni redovi
- **2NF**: Potpuna funkcionalna ovisnost o primarnom ključu
- **3NF**: Eliminacija tranzitivnih ovisnosti

### 4.2 Odnosi između entiteta
```
KLIJENTI (1) ←→ (N) KLIJENT_ADRESE
KLIJENTI (1) ←→ (N) KLIJENT_KONTAKTI
KLIJENTI (1) ←→ (N) KLIJENT_DOKUMENTI
KLIJENTI (1) ←→ (N) RACUNI
RACUNI (M) ←→ (N) PROIZVOD_MASTER (preko RACUN_PROIZVODI)
PROIZVOD_MASTER (1) ←→ (N) KAMATNE_STOPE
RACUNI (1) ←→ (N) TRANSAKCIJE
RACUNI (1) ←→ (N) BLOKADE
```

### 4.3 Referentni integritet
- **Cascade rules**: Nema CASCADE DELETE (sigurnost)
- **Orphan prevention**: Svi foreign key-ovi su NOT NULL ili imaju default
- **Constraint naming**: Standardizirano `FK_[CHILD]_[PARENT]` format

## 5. PERFORMANSE I OPTIMIZACIJA

### 5.1 Indeksi strategija (DB2 z/OS)

```sql
-- Composite indeksi za često korištene kombinacije
CREATE INDEX IDX_TRANSAKCIJE_RACUN_DATUM
    ON TRANSAKCIJE (RACUN_BROJ, DATUM_TRANSAKCIJE DESC)
    USING STOGROUP SYSDEFLT
    PRIQTY 8192 SECQTY 2048
    BUFFERPOOL BP2;

-- Unique indeks (kombinira PRIMARY KEY ili UNIQUE constraint)
CREATE UNIQUE INDEX UX_KLIJENTI_OIB
    ON KLIJENTI (OIB)
    USING STOGROUP SYSDEFLT
    PRIQTY 2048 SECQTY 512
    BUFFERPOOL BP2;
```

**Napomena:** DB2 z/OS **NE PODRŽAVA** partial/filtered indekse (WHERE klauzulu na CREATE INDEX). To je značajka PostgreSQL-a i SQL Servera.

Alternativa za "samo aktivni zapisi" upite:
- Composite indeks `(STATUS, drugi_kljuc)` + DB2 optimizer skip-scan
- Materialized Query Table (MQT) za teška filtriranja
- Particioniranje po STATUS-u (rijetko korisno)

### 5.2 Sekvence performanse
```sql
CACHE settings:
- SEQ_KUPAC_ID: CACHE 1000 (visoka frekvencija)
- SEQ_TRANSAKCIJA_ID: CACHE 500 (srednja frekvencija)
- SEQ_BLOKADA_ID: CACHE 50 (niska frekvencija)
```

**Trade-off:** Kod restarta DB2 subsustava može se izgubiti do CACHE vrijednosti ID-ova. Za 12-cifreni KUPAC_ID ovo nije problem; ako je ID gap kritičan, smanji CACHE ili koristi ORDER klauzulu.

### 5.3 Particioniranje strategija (DB2 z/OS Universal Tablespace - PBR)

```sql
-- Tablespace za particioniranu tablicu
CREATE TABLESPACE TSTRANS
    IN BANKTEST
    USING STOGROUP SYSDEFLT
        PRIQTY 4096 SECQTY 1024
    NUMPARTS 12
    SEGSIZE 32
    LOCKSIZE PAGE
    BUFFERPOOL BP4
    CCSID UNICODE;

-- Particionirana tablica (Partition By Range)
CREATE TABLE TRANSAKCIJE (
    -- column definitions ...
)
PARTITION BY RANGE (DATUM_TRANSAKCIJE)
    (PARTITION 1  ENDING AT ('2026-02-01'),
     PARTITION 2  ENDING AT ('2026-03-01'),
     PARTITION 3  ENDING AT ('2026-04-01'),
     -- ...
     PARTITION 12 ENDING AT ('2026-12-31'))
IN BANKTEST.TSTRANS;
```

**Ključne razlike u odnosu na druge platforme:**
- DB2 z/OS koristi `ENDING AT`, ne `VALUES LESS THAN` (PostgreSQL/Oracle).
- Particije su numerirane (1, 2, 3...), ne imenovane.
- Tablespace mora biti deklariran kao `NUMPARTS n` ili `MAXPARTITIONS n`.

**Operacijske prednosti particioniranja:**
- ROTATE PARTITION za jeftino arhiviranje starih particija
- Paralelizacija upita po particijama
- Granular REORG/RUNSTATS po particiji

## 6. SIGURNOST I AUDIT

### 6.1 Sigurnosne razine
```sql
IZVORNI_SUSTAV_REF.SIGURNOSNA_RAZINA:
  5 - Banker (maksimalna sigurnost)
  4 - Interna služba
  3 - Web/ACH sustavi
  2 - Telefonsko bankarstvo
  1 - Vanjski sustavi (minimalna)
```

### 6.2 Audit trail implementacija
- `DATUM_KREIRANJE` - Timestamp kreiranja
- `ZADNJE_AZURIRANJE` - Timestamp zadnje promjene
- `KREIRAO_USER` - User ID koji je kreirao zapis
- `VREMENSKI_ZAPIS` - Detaljni audit timestamp

### 6.3 Povjerljivost podataka
```sql
-- Sensitive podaci u KLIJENT_DOKUMENTI
PUTANJA_DATOTEKE - Enkriptirane lokacije (encryption-ready)
TIP_DOKUMENTA    - Kodirani tipovi (LK, PAS, VLZ)
```

Za production: razmotriti DB2 z/OS native funkcionalnosti:
- Column-level encryption (`ENCRYPT_TDES`)
- Row Permission / Column Mask (RACF integracija)
- Audit Policy (`-START TRACE(AUDIT)`)

## 7. BUSINESS RULES I CONSTRAINTS

### 7.1 Check constraints

```sql
-- OIB validacija (samo duljina)
CHK_OIB_FORMAT: LENGTH(TRIM(OIB)) = 11

-- Saldo logika
CHK_SALDO_LOGIKA: DOSTUPNI_SALDO <= TRENUTNI_SALDO + MINUS_LIMIT

-- Datum validacije
CHK_ADRESA_DATUMI: DATUM_DO IS NULL OR DATUM_DO > DATUM_OD
```

**VAŽNO - OIB checksum validacija:**

Hrvatski OIB ima checksum algoritam (mod 11). CHECK constraint **NE MOŽE** implementirati taj algoritam jer DB2 z/OS check constraints podržavaju samo deterministička izračunavanja na razini reda (bez kontrolnih petlji).

Za potpunu validaciju OIB-a koristiti jedan od sljedećih pristupa:

1. **BEFORE INSERT/UPDATE trigger** - validacija na DB sloju
2. **Stored procedure** pozvana iz aplikacije prije INSERT-a
3. **Validacija u COBOL programu** prije izvršavanja INSERT-a (preferirano za performanse)

### 7.2 Business logic kroz status kodove
```sql
STATUS_KATEGORIJA implementacije:
- 'GENERAL'     - Općeniti statusi (A, N, Z, S, B, M)
- 'TRANSAKCIJA' - Transakcijski statusi (K, P, O, G)
- 'QUEUE'       - Queue statusi (C, U, N, P)
- 'BLOKADA'     - Blokada statusi (A, O, I)
```

## 8. ODRŽAVANJE I MONITORING

### 8.1 Statistike tablica (DB2 z/OS RUNSTATS)

```sql
-- DB2 z/OS RUNSTATS utility (poziva se kroz DSNUTILB ili DSNUTILU)
RUNSTATS TABLESPACE BANKTEST.TSKLIENT
    TABLE(BANKING_TEST.KLIJENTI)
    INDEX(ALL)
    KEYCARD
    UPDATE ALL
    REPORT YES;

RUNSTATS TABLESPACE BANKTEST.TSTRANS
    TABLE(BANKING_TEST.TRANSAKCIJE)
    INDEX(ALL)
    KEYCARD
    UPDATE ALL
    REPORT YES;
```

**Učestalost:** Tjedno za master tablice, dnevno za transakcijske tablice nakon značajnog volumena INSERT/UPDATE/DELETE.

### 8.2 Reorg strategija (DB2 z/OS REORG)

```sql
-- REORG TABLESPACE utility
REORG TABLESPACE BANKTEST.TSTRANS
    SHRLEVEL CHANGE
    SORTDATA
    LOG NO;

-- REORG INDEX-a (ako se ne radi automatski uz REORG TABLESPACE)
REORG INDEX BANKING_TEST.IDX_TRANSAKCIJE_RACUN
    SHRLEVEL CHANGE;
```

**SHRLEVEL opcije:**
- `NONE` - read-only tijekom REORG-a (najbrže, ali invasive)
- `REFERENCE` - read-only za writes, čitanje OK
- `CHANGE` - online REORG, dopušta read+write (preferirano za production)

### 8.3 Backup strategija
- **Image Copy** (FULL): Tjedno
- **Incremental Image Copy**: Dnevno
- **Active Log archiving**: Kontinuirano
- **DSNJU004 (BSDS print)**: Tjedno za bookkeeping
- **Archive log retention**: 1 godina

## 9. MIGRACIJA I DEPLOYMENT

### 9.1 Deployment redoslijed

1. **Database**: `00_create_database.sql`
2. **Schema**: `create_schema.sql`
3. **Reference tablice** (neovisne)
4. **Master entiteti** (KLIJENTI, PROIZVOD_MASTER)
5. **Ovisni entiteti** (RACUNI, normalizirane tablice)
6. **Transakcijski sustav** (TRANSAKCIJE, BLOKADE, RED_TRANSAKCIJA, GLAVNA_KNJIGA)
7. **Sekvence** (`sekvence.sql`)
8. **Indeksi** (`indeksi_optimizacija.sql`)
9. **Reference podaci** (insert skripte)
10. **Privilegije** (`prava.sql`)
11. **Validacija** (`test_scheme.sql`)

### 9.2 Data loading redoslijed
```sql
-- 1. Učitavanje referentnih podataka
INSERT INTO POSLOVNICE ...
INSERT INTO RIZIK_OCJENA_REF ...
-- ostali insert_*.sql fajlovi

-- 2. Master podaci
INSERT INTO KLIJENTI ...
INSERT INTO PROIZVOD_MASTER ...

-- 3. Operativni podaci
INSERT INTO RACUNI ...
INSERT INTO TRANSAKCIJE ...
```

Za masovno učitavanje koristiti DB2 LOAD utility umjesto INSERT statement-a:
```
LOAD DATA INDDN(SYSREC) RESUME YES
    INTO TABLE BANKING_TEST.TRANSAKCIJE
    ...
```

## 10. TROUBLESHOOTING I DIJAGNOSTIKA

### 10.1 Česti problemi
- **Deadlock** na TRANSAKCIJE: koristi LOCKSIZE ROW i kratke transakcije
- **Lock timeout**: provjeri IRLMRWT vrijednost (`-DISPLAY GROUP`)
- **Constraint violations**: provjeri redoslijed insertanja prema FK ovisnostima
- **-805 SQLCODE (package not found)**: BIND PACKAGE nakon DDL promjena
- **-911 SQLCODE (deadlock)**: implementiraj retry logiku u COBOL aplikaciji

### 10.2 Upiti praćenja performansi (DB2 z/OS)

```sql
-- Najsporiji upiti iz Statement Cache
-- Prerequisite: EXPLAIN STMTCACHE ALL puni DSN_STATEMENT_CACHE_TABLE

SELECT
    STMT_ID,
    STMT_TOKEN,
    STAT_EXEC AS BROJ_IZVRSAVANJA,
    STAT_GPAG AS BROJ_GETPAGE,
    STAT_CPU  AS CPU_VRIJEME_MS,
    STAT_ELAP AS ELAPSED_MS,
    SUBSTR(STMT_TEXT, 1, 200) AS UPIT
FROM DSN_STATEMENT_CACHE_TABLE
WHERE STAT_EXEC > 0
ORDER BY STAT_ELAP DESC
FETCH FIRST 10 ROWS ONLY;
```

**Lock contention analiza:**

DB2 z/OS lock contention se prati kroz IFCID trace, ne kroz SQL view-ove (kao u DB2 LUW).

```
-START TRACE(PERFM) CLASS(6) IFCID(044,045)
```

- IFCID 044: Lock suspends
- IFCID 045: Lock resumes

Rezultati se analiziraju kroz vanjske alate: **OMEGAMON for DB2 PE**, **DB2 PA**, ili **Tivoli**. DB2 z/OS u v13 ima i `SYSIBMADM.MON_LOCKWAIT` view kao native pristup.

**Statement cache management:**
```
-DISPLAY DYNQUERY(STMTCACHE) STATS
```

### 10.3 Skripte održavanja

```sql
-- Čišćenje starih transakcija (DB2 z/OS sintaksa - CURRENT DATE bez podvlake)
DELETE FROM TRANSAKCIJE
WHERE DATUM_TRANSAKCIJE < CURRENT DATE - 7 YEARS;

-- Bolji pristup za velike volumene:
--  1. UNLOAD utility na arhivu
--  2. LOAD REPLACE praznom datotekom (ako briše čitavu particiju)
--  3. Brisanje po particijama (ALTER TABLE ... ROTATE PARTITION)
```

```sql
-- Rebalancing sekvenci (rijetko potrebno)
ALTER SEQUENCE SEQ_TRANSAKCIJA_ID RESTART WITH 1000000000;
```

```sql
-- Provjera prostora po tablespace-u
SELECT
    DBNAME,
    NAME AS TSNAME,
    SPACE       AS KORISTENI_KB,
    NACTIVE     AS AKTIVNE_STR,
    NTABLES     AS BROJ_TABLICA
FROM SYSIBM.SYSTABLESPACE
WHERE DBNAME = 'BANKTEST'
ORDER BY SPACE DESC;
```

---

## VERZIJA DOKUMENTACIJE

| Verzija | Datum | Promjene |
|---------|-------|----------|
| 1.0 | 2025-07-23 | Inicijalna verzija |
| 1.1 | 2026-05-03 | Ispravak DB2 LUW -> z/OS sintakse u sekcijama 5.1, 5.3, 7.1, 8.1, 8.2, 10.2, 10.3. Dodana sekcija 2.2 (Buffer Pool strategija). Dodana napomena o OIB checksum validaciji u 7.1. Database creation eksplicitno dokumentirano u 2.1. |

**Status**: PRODUCTION READY
