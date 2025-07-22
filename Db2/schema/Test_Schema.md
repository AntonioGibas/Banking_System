# BANKING_TEST - Dokumentacija Testne Sheme

## 1. PREGLED

### 1.1 Svrha
**BANKING_TEST** je sveobuhvatan testni okoliš za razvoj COBOL bankovnih aplikacija. Ova shema pruža kompletnu infrastrukturu bankovnog sustava za učenje, testiranje i razvoj.

### 1.2 Specifikacije Okoliša
- **Platforma**: IBM DB2 z/OS
- **Naziv Sheme**: `BANKING_TEST`
- **Cilj**: Razvojni i učni okoliš
- **COBOL Integracija**: Potpuno pripremljeno za CICS/IMS COBOL programe
- **Volumen Podataka**: Mali do srednji testni skupovi podataka

### 1.3 Ključni Dizajnerski Principi
- **Normalizirano**: 3NF usklađenost za integritet podataka
- **Skalabilno**: Dizajnirano za rast volumena transakcija
- **Performanse-usmjereno**: Strateški indeksi za česte upite
- **Sigurnosno-svjesno**: Spreman za kontrolu pristupa temeljenu na ulogama
- **COBOL-prijateljski**: Veličine polja i tipovi optimizirani za COBOL razvoj

---

## 2. ARHITEKTURA SHEME

### 2.1 Strategija Tablespace-ova

```sql
TS_BANK_REF_TEST     -- Reference tablice (8MB)
TS_BANK_MASTER_TEST  -- Glavni entiteti (32MB)  
TS_BANK_TRANS_TEST   -- Transakcijski podaci (128MB)
TS_BANK_IX_TEST      -- Indeksi (16MB)
```

**Obrazloženje**: Odvojeni tablespace-ovi omogućavaju:
- Nezavisne strategije backup/recovery
- Optimizirane dodjele buffer pool-a
- Granularno upravljanje prostorom
- Podešavanje performansi po tipu podataka

### 2.2 Kategorije Tablica

#### A) **REFERENCE TABLICE** (6 tablica)
*Statični konfiguracijski podaci - rijetko se mijenjaju*
- `POSLOVNICE` - Poslovni uredi
- `RIZIK_OCJENA_REF` - Ocjene rizika
- `STATUS_REF` - Statusni kodovi  
- `TIP_RACUNA_REF` - Tipovi računa
- `TRANSAKCIJA_TIP_REF` - Tipovi transakcija
- `IZVORNI_SUSTAV_REF` - Izvorni sustavi

#### B) **GLAVNI ENTITETI** (3 tablice)
*Temeljni poslovni objekti*
- `KLIJENTI` - Master klijenata
- `PROIZVOD_MASTER` - Katalog proizvoda
- `RACUNI` - Master računa

#### C) **NORMALIZIRANE TABLICE** (5 tablica)
*1:N odnosi za normalizaciju podataka*
- `KLIJENT_ADRESE` - Adrese klijenata
- `KLIJENT_KONTAKTI` - Kontakti klijenata
- `KLIJENT_DOKUMENTI` - Dokumenti klijenata
- `KAMATNE_STOPE` - Razredi kamata
- `RACUN_PROIZVODI` - Odnosi račun-proizvod (M:N)

#### D) **TRANSAKCIJSKE TABLICE** (4 tablice)
*Visokofrekventni operacijski podaci*
- `TRANSAKCIJE` - Knjižene transakcije
- `BLOKADE` - Blokade/zaštite računa
- `RED_TRANSAKCIJA` - Red transakcija
- `GLAVNA_KNJIGA` - Unosi u glavnu knjigu

---

## 3. DETALJNE SPECIFIKACIJE TABLICA

### 3.1 Domena Klijenata

#### KLIJENTI (Master Klijenata)
```sql
Primary Key: KUPAC_ID (BIGINT, 12 cifara)
Unique Key: OIB (11 znakova)
Volume: ~1,000 testnih klijenata
```

**Ključne Značajke:**
- 12-cifreni ID klijenta za skalabilnost
- Hrvatska OIB validacija
- Integracija ocjene rizika
- Audit trail (vremenske oznake kreiranja/modificiranja)

**COBOL Razmatranja:**
- `KUPAC_ID` mapira na `PIC 9(12)` 
- `OIB` mapira na `PIC X(11)`
- Datumska polja koriste `PIC X(10)` format

#### KLIJENT_ADRESE (Adrese Klijenata)
```sql
Relationship: 1:N s KLIJENTI
Types: ST(Kućna), PO(Poštanska), RA(Posao), PR(Ostala)
```

#### KLIJENT_KONTAKTI (Kontakti Klijenata)  
```sql
Relationship: 1:N s KLIJENTI
Types: TE(Telefon), MO(Mobitel), EM(Email), FA(Fax)
```

#### KLIJENT_DOKUMENTI (Dokumenti Klijenata)
```sql
Relationship: 1:N s KLIJENTI  
Types: LK(Osobna), PAS(Putovnica), VLZ(Vozačka), OST(Ostalo)
Security: Enkriptiranje putanje datoteke spremno
```

### 3.2 Domena Proizvoda

#### PROIZVOD_MASTER (Katalog Proizvoda)
```sql
Primary Key: PROIZVOD_KOD (CHAR(4))
Products: 5 testnih proizvoda (TEK, STD, ORO, KRE, KAR)
```

**Testni Proizvodi:**
- `P001` - Premium tekući račun
- `P002` - Standardni tekući račun  
- `P003` - Mladenačka štednja
- `P004` - 12M oročena štednja
- `P005` - Gotovinski kredit

#### KAMATNE_STOPE (Razredi Kamata)
```sql
Relationship: 1:N s PROIZVOD_MASTER
Tiers: Izračun kamata temeljen na saldu
Methods: DAN(Dnevno), MJE(Mjesečno), KVA(Kvartalno), GOD(Godišnje)
```

### 3.3 Domena Računa

#### RACUNI (Master Računa)
```sql
Primary Key: RACUN_BROJ (CHAR(12))
Format: Provjera kontrolne cifre spremna
Balance Fields: DECIMAL(15,2) preciznost
```

**Kritična Poslovna Logika:**
```sql
DOSTUPNI_SALDO <= TRENUTNI_SALDO + MINUS_LIMIT
```

**Statusni Kodovi:**
- `A` - Aktivan
- `Z` - Zatvoren  
- `B` - Blokiran
- `M` - Održavanje

### 3.4 Domena Transakcija

#### TRANSAKCIJE (Knjižene Transakcije)
```sql
Primary Key: TRANSAKCIJA_ID (CHAR(20))
Format: YYYYMMDDHHMISSNNNNNN (vremenska oznaka + sekvenca)
Volume: Visok (particioniranje preporučeno za produkciju)
```

**Tipovi Transakcija:**
- `UPL` - Uplata gotovine (+)
- `ISP` - Isplata gotovine (-)
- `PRE` - Prijenos (=)
- `NAK` - Naknada (-)
- `KAM` - Kamata (+)
- `PRI` - Vanjsko primanje (+)
- `STO` - Storno (=)

#### BLOKADE (Blokade Računa)
```sql
Types: CEK(Čekovna), PRA(Pravna), SIG(Sigurnosna), KRE(Kreditna), NAP(Ostala)
Auto-expiry: Rukovanje DATUM_ISTEKA
Impact: Smanjuje DOSTUPNI_SALDO
```

#### RED_TRANSAKCIJA (Red Transakcija)
```sql
Purpose: Red za obradu transakcija u stvarnom vremenu
Statuses: C(Čeka), U(U obradi), N(Završeno), P(Na čekanju)
Retry Logic: BROJ_PONOVNIH (maksimalno 5 pokušaja)
```

---

## 4. STRATEGIJA INDEKSIRANJA

### 4.1 Performanse-Kritični Indeksi

#### Indeksi Klijenata
```sql
UX_KLIJENTI_OIB          -- Unique constraint + pretraživanje
IDX_KLIJENTI_RIZIK       -- Upiti temeljeni na riziku
IDX_KLIJENTI_POSLOVNICA  -- Izvještavanje po poslovnicama
```

#### Indeksi Računa  
```sql
IDX_RACUNI_KUPAC         -- Popis računa klijenta
IDX_RACUNI_SALDO         -- Upiti temeljeni na saldu
IDX_RACUNI_AKTIVNOST     -- Čišćenje temeljeno na aktivnosti
```

#### Indeksi Transakcija (Visok Volumen)
```sql
IDX_TRANSAKCIJE_RACUN              -- Upiti izvoda računa
IDX_TRANSAKCIJE_DATUM              -- Izvještavanje po datumskim rasponima
IDX_TRANSAKCIJE_RACUN_DATUM        -- Kompozitni za performanse
```

### 4.2 Veličina Indeksa (DB2 z/OS)
```sql
PRIQTY/SECQTY alokacija:
- Indeksi klijenata: 1-2MB primary
- Indeksi transakcija: 4-8MB primary (visok volumen)
- Reference indeksi: 256K-1MB primary
```

---

## 5. UPRAVLJANJE SEKVENCAMA

### 5.1 Strategija Generiranja ID-ova
```sql
SEQ_KUPAC_ID       -- 100,000,000,000 do 999,999,999,999
SEQ_TRANSAKCIJA_ID -- 1,000,000,000+ (visok volumen)
SEQ_BLOKADA_ID     -- 1,000,000+ (srednji volumen)
```

### 5.2 Cache Postavke
```sql
Visoka frekvencija: CACHE 1000 (KUPAC_ID, GK_ID)
Srednja frekvencija: CACHE 500 (TRANSAKCIJA_ID) 
Niska frekvencija: CACHE 50-100 (ostali)
```

**COBOL Integracija:**
```cobol
EXEC SQL
    SELECT NEXT VALUE FOR SEQ_KUPAC_ID
    INTO :WS-NEW-CUSTOMER-ID
    FROM SYSIBM.SYSDUMMY1
END-EXEC
```

---

## 6. INTEGRITET PODATAKA I OGRANIČENJA

### 6.1 Foreign Key Odnosi
```sql
KLIJENTI --> POSLOVNICE (validacija poslovnice)
KLIJENTI --> RIZIK_OCJENA_REF (validacija rizika)
RACUNI --> KLIJENTI (vlasništvo klijenta)
RACUNI --> TIP_RACUNA_REF (validacija tipa računa)
TRANSAKCIJE --> RACUNI (postojanje računa)
TRANSAKCIJE --> TRANSAKCIJA_TIP_REF (validacija tipa transakcije)
```

### 6.2 Poslovna Pravila (CHECK Constraints)
```sql
-- OIB validacija
CHK_OIB_FORMAT: LENGTH(TRIM(OIB)) = 11

-- Logika salda
CHK_SALDO_LOGIKA: DOSTUPNI_SALDO <= TRENUTNI_SALDO + MINUS_LIMIT

-- Validacija datuma  
CHK_ADRESA_DATUMI: DATUM_DO IS NULL OR DATUM_DO > DATUM_OD

-- Validacija iznosa
CHK_IZNOS_BLOKADE: IZNOS_BLOKADE >= 0
```

### 6.3 Standardizacija Statusnih Kodova
```sql
GENERAL: A(Aktivan), N(Neaktivan), Z(Zatvoren), S(Suspendiran), B(Blokiran), M(Održavanje)
TRANSAKCIJA: K(Knjižen), P(Na čekanju), O(Otkazan), G(Greška)
QUEUE: C(Čeka), U(U obradi), N(Završen), P(Na čekanju)  
BLOKADA: A(Aktivna), O(Oslobođena), I(Istekla)
```

---

## 7. RAZMATRANJA ZA COBOL RAZVOJ

### 7.1 COPYBOOK Struktura
```cobol
01  CUSTOMER-RECORD.
    05  KUPAC-ID            PIC 9(12).
    05  OIB                 PIC X(11).
    05  IME                 PIC X(30).
    05  PREZIME             PIC X(30).
    05  DATUM-RODJENJA      PIC X(10).
    05  RIZIK-OCJENA        PIC X(2).
    05  STATUS              PIC X(1).
```

### 7.2 Uobičajeni SQL Uzorci
```sql
-- Pretraživanje klijenta po OIB-u
SELECT KUPAC_ID, IME, PREZIME, STATUS
FROM BANKING_TEST.KLIJENTI 
WHERE OIB = :WS-OIB
AND STATUS = 'A';

-- Upit salda računa  
SELECT TRENUTNI_SALDO, DOSTUPNI_SALDO, STATUS
FROM BANKING_TEST.RACUNI
WHERE RACUN_BROJ = :WS-ACCOUNT-NUMBER
AND STATUS IN ('A', 'B');

-- Povijest transakcija
SELECT DATUM_TRANSAKCIJE, KOD_TRANSAKCIJE, IZNOS, OPIS
FROM BANKING_TEST.TRANSAKCIJE  
WHERE RACUN_BROJ = :WS-ACCOUNT-NUMBER
AND DATUM_TRANSAKCIJE >= :WS-FROM-DATE
ORDER BY DATUM_TRANSAKCIJE DESC, VREMENSKI_ZAPIS DESC;
```

### 7.3 Rukovanje Greškama
```cobol
EVALUATE SQLCODE
    WHEN 0
        CONTINUE
    WHEN 100  
        MOVE 'ZAPIS NIJE PRONAĐEN' TO WS-ERROR-MESSAGE
    WHEN -803
        MOVE 'DUPLIKAT KLJUČA' TO WS-ERROR-MESSAGE  
    WHEN OTHER
        MOVE 'SQL GREŠKA' TO WS-ERROR-MESSAGE
END-EVALUATE
```

---

## 8. STRATEGIJA TESTNIH PODATAKA

### 8.1 Referentni Podaci (Produkcijski-slični)
```sql
Poslovnice: 3 poslovnice (Zagreb, Split, Rijeka)
Ocjene Rizika: 6 razina (N1, N2, S1, S2, V1, V2)  
Tipovi Računa: 5 tipova (TEK, STD, ORO, KRE, KAR)
Tipovi Transakcija: 7 tipova (UPL, ISP, PRE, NAK, KAM, PRI, STO)
```

### 8.2 Master Podaci (Sintetički)
```sql
Klijenti: 100-1000 testnih klijenata
Računi: 200-2000 testnih računa  
Proizvodi: 5 bankovnih proizvoda s realističnim parametrima
Kamatne Stope: Razredovane stope po proizvodu
```

### 8.3 Transakcijski Podaci (Realistični Volumen)
```sql
Transakcije: 1000-10000 testnih transakcija
Blokade: 50-100 testnih blokada računa
Queue: Simulacija obrade u stvarnom vremenu
GL Unosi: Simulacija računovodstva
```

---

## 9. SIGURNOST I KONTROLA PRISTUPA

### 9.1 Sigurnost na Razini Sheme
```sql
Vlasnik Sheme: IBMUSER (potpuni administrativni pristup)
Aplikacijska Uloga: BANKING_APP_ROLE (CRUD operacije)
Read-Only Uloga: BANKING_READ_ROLE (samo SELECT)
```

### 9.2 Privilegije na Razini Tablica
```sql
Reference Tablice: SELECT za aplikacije
Master Tablice: SELECT, INSERT, UPDATE za aplikacije  
Transakcijske Tablice: Potpuni CRUD za aplikacije
Sequences: USAGE za generiranje ID-ova
```

### 9.3 Privatnost Podataka
```sql
Dokumenti Klijenata: Enkriptiranje putanje spremno
Kontaktne Informacije: Prijavljanje pristupa spremno
Financijski Podaci: Audit trail implementiran
```

---

## 10. NADZOR I ODRŽAVANJE

### 10.1 Nadzor Performansi
```sql
Ključne Metrike:
- Volumen transakcija po satu
- Prosječno vrijeme odgovora za upite salda  
- Statistike korištenja indeksa
- Iskorištenje tablespace-a
```

### 10.2 Zadaci Održavanja
```sql
Dnevno:
- Čišćenje reda transakcija
- Uklanjanje isteklih blokada
- Nadzor sekvenci

Tjedno:  
- Ažuriranje statistika indeksa (RUNSTATS)
- Pregled iskorištenja prostora

Mjesečno:
- Arhiviranje starih transakcija (ako je volumen visok)
- Razmatranje reorg-a
```

### 10.3 Strategija Backup-a
```sql
Reference Tablice: Tjedno potpuni backup
Master Tablice: Dnevni inkrementalni  
Transakcijske Tablice: Kontinuirani log backup
Recovery Point: Maksimalno 15 minuta
```

---

## 11. KONTROLNA LISTA DEPLOYMENT-A

### 11.1 Pre-Deployment
- [ ] Pristup DB2 subsustavu potvrđen
- [ ] IBMUSER privilegije provjerene  
- [ ] Dostupnost tablespace prostora provjerena
- [ ] Konfiguracija buffer pool-a pregledana

### 11.2 Koraci Deployment-a  
- [ ] Kreiranje sheme i tablespace-ova
- [ ] Kreiranje referentnih tablica
- [ ] Kreiranje master entitet tablica
- [ ] Kreiranje normaliziranih tablica
- [ ] Kreiranje transakcijskih tablica
- [ ] Kreiranje svih indeksa
- [ ] Kreiranje sekvenci
- [ ] Dodjela privilegija
- [ ] Učitavanje referentnih podataka
- [ ] Pokretanje validacijskih testova

### 11.3 Post-Deployment
- [ ] Uspostavljena baseline performansi
- [ ] COBOL copybook-ovi generirani
- [ ] Testiranje konekcije završeno
- [ ] Korisnički pristup provjeren
- [ ] Dokumentacija isporučena

---

## 12. BUDUĆA RAZMATRANJA

### 12.1 Skalabilnost
- **Particioniranje**: Razmotriti mjesečno particioniranje za TRANSAKCIJE tablicu
- **Arhiviranje**: Implementirati automatizirano arhiviranje za transakcije > 7 godina
- **Indeksi**: Nadzirati i dodavati indekse prema stvarnim uzorcima upita

### 12.2 Mogućnosti Poboljšanja  
- **Audit Tablice**: Potpuni audit trail za sve promjene
- **Workflow**: Workflow-ovi za odobravanje transakcija
- **Real-time**: Obradu vođenu događajima
- **Analitika**: Integracija s data warehouse-om

### 12.3 Migracija u Produkciju
- **Sizing Volumena**: Preračunavanje veličina tablespace-a za produkcijske volumene
- **Performanse**: Sveobuhvatno testiranje performansi
- **Sigurnost**: Poboljšana enkripcija i kontrole pristupa
- **Compliance**: Validacija regulatorne usklađenosti

---

**Verzija Dokumenta**: 1.0  
**Datum**: 23. srpnja 2025.  