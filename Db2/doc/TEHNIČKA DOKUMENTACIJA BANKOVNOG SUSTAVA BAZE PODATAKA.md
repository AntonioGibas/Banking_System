# TEHNIČKA DOKUMENTACIJA BANKOVNOG SUSTAVA BAZE PODATAKA

## 1. PREGLED SUSTAVA

### 1.1 Osnovno
- **Platform**: IBM DB2 z/OS v13
- **Arhitektura**: Normalizirana relacijska baza podataka
- **Normalizacijska razina**: 3NF (Treća normalna forma)
- **Encoding**: UTF-8
- **Transakcijski model**: ACID compliant

### 1.2 Karakteristike sustava
- **Hijerarhijski pristup**: Organiziran prema bankovnim standardima
- **Referentni integritet**: Potpuno implementiran kroz foreign key constraints
- **Skalabilnost**: Dizajniran za velike volumene transakcija
- **Performanse**: Optimizirano kroz indekse i sekvence
- **Sigurnost**: Implementirane sigurnosne razine i audit trail

## 2. ARHITEKTURA BAZE PODATAKA

### 2.1 Tablespace organizacija
```sql
-- Preporučena tablespace struktura
TABLESPACE TS_REFERENCE (8MB) -- Reference tablice
TABLESPACE TS_MASTER (32MB)   -- Master entiteti
TABLESPACE TS_TRANSACT (128MB) -- Transakcijski podaci
TABLESPACE TS_ARCHIVE (64MB)  -- Arhivski podaci
TABLESPACE TS_INDEX (16MB)    -- Indeksi
```

### 2.2 Kategorije tablica

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
- `OIB`: Osobni identifikacijski broj (validacija duljine)
- `RIZIK_OCJENA`: Poveznica na rizični profil
- `DATUM_KREIRANJE/ZADNJE_AZURIRANJE`: Audit trail

### 3.2 RACUNI (Core banking entity)
```sql
Primarna tablica: RACUN_BROJ (CHAR(12))
Foreign keys:
  - KUPAC_ID → KLIJENTI
  - TIP_RACUNA → TIP_RACUNA_REF
  - POSLOVNICA_OTVARANJA → POSLOVNICE
```

**Kritična polja:**
- `TRENUTNI_SALDO/DOSTUPNI_SALDO`: Preciznost DECIMAL(15,2)
- `MINUS_LIMIT`: Overdraft limit
- `ZADNJA_AKTIVNOST`: Performance optimization timestamp

**Business rules:**
```sql
CHK_SALDO_LOGIKA: DOSTUPNI_SALDO <= TRENUTNI_SALDO + MINUS_LIMIT
```

### 3.3 TRANSAKCIJE (Transakcijski core)
```sql
Volumeni: Visoki (milijuni zapisa)
Particioniranje: Po DATUM_TRANSAKCIJE (mjesečno)
Archiving: Stariji od 7 godina
```

**Ključni indeksi:**
- `IDX_TRANSAKCIJE_RACUN` - Transakcije po računu
- `IDX_TRANSAKCIJE_DATUM` - Datumski opsezi
- `IDX_TRANSAKCIJE_STATUS` - Status filtriranje

### 3.4 BLOKADE (Risk management)
```sql
Real-time impact: Utječe na DOSTUPNI_SALDO
Tipovi blokada: CEK, PRA, SIG, KRE, NAP
Automatsko istjecanje: Preko DATUM_ISTEKA
```

## 4. NORMALIZACIJA I VEZE

### 4.1 Normalizacijske principi
- **1NF**: Atomske vrijednosti, jedinstveni redovi
- **2NF**: Potpuna funkcionalna ovisnost o primarnom ključu
- **3NF**: Eliminacija tranzitivnih ovisnosti

### 4.2 Odnosi između entiteta
```sql
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
- **Constraint naming**: Standardizirano FK_[CHILD]_[PARENT] format

## 5. PERFORMANSE I OPTIMIZACIJA

### 5.1 Indeksi strategija
```sql
-- Composite indeksi za često korištene kombinacije
CREATE INDEX IDX_TRANSAKCIJE_RACUN_DATUM 
  ON TRANSAKCIJE (RACUN_BROJ, DATUM_TRANSAKCIJE DESC);

-- Partial indeksi za aktivne zapise
CREATE INDEX IDX_RACUNI_AKTIVNI 
  ON RACUNI (STATUS) WHERE STATUS IN ('A', 'B');
```

### 5.2 Sekvence performanse
```sql
CACHE settings:
- SEQ_KUPAC_ID: CACHE 1000 (visoka frekvencija)
- SEQ_TRANSAKCIJA_ID: CACHE 500 (srednja frekvencija)
- SEQ_BLOKADA_ID: CACHE 50 (niska frekvencija)
```

### 5.3 Particioniranje strategija
```sql
-- Preporučeno particioniranje TRANSAKCIJE
PARTITION BY RANGE (DATUM_TRANSAKCIJE)
  (PARTITION P202401 VALUES LESS THAN ('2024-02-01'),
   PARTITION P202402 VALUES LESS THAN ('2024-03-01'),
   ...
   PARTITION PMAX VALUES LESS THAN (MAXVALUE));
```

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
PUTANJA_DATOTEKE - Enkriptirane lokacije
TIP_DOKUMENTA - Kodirani tipovi (LK, PAS, VLZ)
```

## 7. BUSINESS RULES I CONSTRAINTS

### 7.1 Check constraints
```sql
-- OIB validacija
CHK_OIB_FORMAT: LENGTH(TRIM(OIB)) = 11

-- Saldo logika
CHK_SALDO_LOGIKA: DOSTUPNI_SALDO <= TRENUTNI_SALDO + MINUS_LIMIT

-- Datum validacije
CHK_ADRESA_DATUMI: DATUM_DO IS NULL OR DATUM_DO > DATUM_OD
```

### 7.2 Business logic kroz status kodove
```sql
STATUS_KATEGORIJA implementacije:
- 'GENERAL' - Općeniti statusi (A, N, Z, S, B, M)
- 'TRANSAKCIJA' - Transakcijski statusi (K, P, O, G)
- 'QUEUE' - Queue statusi (C, U, N, P)
- 'BLOKADA' - Blokada statusi (A, O, I)
```

## 8. ODRŽAVANJE I MONITORING

### 8.1 Statistike tablica
```sql
-- Redovito ažuriranje statistika
RUNSTATS ON TABLE KLIJENTI WITH DISTRIBUTION;
RUNSTATS ON TABLE TRANSAKCIJE WITH DISTRIBUTION;
```

### 8.2 Reorg strategija
```sql
-- Mjesečni REORG za transakcijske tablice
REORG TABLE TRANSAKCIJE;
REORG INDEXES ALL FOR TABLE TRANSAKCIJE;
```

### 8.3 Backup strategija
- **Full backup**: Tjedno
- **Incremental**: Dnevno
- **Log backup**: Svakih 15 minuta
- **Archive logs**: Čuvanje 1 godina

## 9. MIGRACIJA I DEPLOYMENT

### 9.1 Deployment redoslijed
1. **Reference tablice** (neovisne)
2. **Master entiteti** (KLIJENTI, PROIZVOD_MASTER)
3. **Ovisni entiteti** (RACUNI, normalizirane tablice)
4. **Transakcijski sustav** (TRANSAKCIJE, BLOKADE)
5. **Support tablice** (GLAVNA_KNJIGA, RED_TRANSAKCIJA)

### 9.2 Data loading redoslijed
```sql
-- 1. Učitavanje referentnih podataka
INSERT INTO POSLOVNICE...
INSERT INTO RIZIK_OCJENA_REF...

-- 2. Master podaci
INSERT INTO KLIJENTI...
INSERT INTO PROIZVOD_MASTER...

-- 3. Operativni podaci
INSERT INTO RACUNI...
INSERT INTO TRANSAKCIJE...
```

## 10. TROUBLESHOOTING I DIJAGNOSTIKA

### 10.1 Česti problemi
- **Deadlock**: Na TRANSAKCIJE - koristiti row-level locking
- **Lock timeout**: Kratke transakcije, proper indexing
- **Constraint violations**: Provjera foreign key redoslijeda

### 10.2 Upiti praćenja performansi
```sql
-- Top 10 najsporijih upita
SELECT * FROM SYSIBMADM.TOP_QUERIES 
ORDER BY TOTAL_EXEC_TIME DESC;

-- Lock contention analiza
SELECT * FROM SYSIBMADM.SNAPLOCK 
WHERE LOCK_WAIT_TIME > 1000;
```

### 10.3 Skripte održavanja
```sql
-- Čišćenje starih transakcija (arhiviranje)
DELETE FROM TRANSAKCIJE 
WHERE DATUM_TRANSAKCIJE < CURRENT_DATE - 7 YEARS;

-- Rebalancing sekvenci
ALTER SEQUENCE SEQ_TRANSAKCIJA_ID RESTART WITH 1000000000;
```

---

**Verzija dokumentacije**: 1.0  
**Datum kreiranja**: 2025-07-23  
**Zadnje ažuriranje**: 2025-07-23  
**Status**: PRODUCTION READY