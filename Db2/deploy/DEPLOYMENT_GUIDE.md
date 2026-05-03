# BANKING SYSTEM - DEPLOYMENT GUIDE

**Platform:** IBM DB2 z/OS v13
**Schema:** `BANKING_TEST`
**Database:** `BANKTEST`
**Mode:** Manual step-by-step execution (run each file in order, verify, then proceed)

---

## PRE-REQUISITES

Before starting deployment, confirm the following:

| Item | How to check | Expected |
|------|--------------|----------|
| DB2 version | `SELECT GETVARIABLE('SYSIBM.VERSION') FROM SYSIBM.SYSDUMMY1;` | DSN13xxx (v13) |
| User authority | `SELECT * FROM SYSIBM.SYSUSERAUTH WHERE GRANTEE = USER;` | Has DBADM, SYSADM, or DBCTRL |
| STOGROUP exists | `SELECT NAME FROM SYSIBM.SYSSTOGROUP WHERE NAME = 'SYSDEFLT';` | One row |
| Buffer pools active | `-DISPLAY BUFFERPOOL(BP1)` and `-DISPLAY BUFFERPOOL(BP2)` | ACTIVE status |
| Old BANKTEST cleanup | `SELECT NAME FROM SYSIBM.SYSDATABASE WHERE NAME = 'BANKTEST';` | No rows (else drop first) |

If old `BANKTEST` exists, run `Db2/schema/rollback/rollback.sql` first.

---

## STEP 1: CREATE DATABASE

**File:** `Db2/schema/00_create_database.sql`

```sql
CREATE DATABASE BANKTEST
    BUFFERPOOL BP1
    INDEXBP    BP2
    STOGROUP   SYSDEFLT
    CCSID      UNICODE;
COMMIT;
```

**Verify:**
```sql
SELECT NAME, BPOOL, IBMREQD
FROM SYSIBM.SYSDATABASE
WHERE NAME = 'BANKTEST';
```
Expected: 1 row showing BANKTEST with BP1.

---

## STEP 2: SET SCHEMA CONTEXT

**File:** `Db2/schema/create_schema.sql`

```sql
SET CURRENT SCHEMA = 'BANKING_TEST';
```

Note: `CREATE SCHEMA BANKING_TEST AUTHORIZATION IBMUSER, AGIBAS;` is commented out
in your file because schema creation on z/OS typically happens via DBA or is implicit.
If your shop requires explicit schema, uncomment and run.

---

## STEP 3: REFERENCE TABLES

These have no foreign keys to other tables and must be created first.

| Order | File | Tablespace |
|-------|------|------------|
| 3.1 | `Db2/create/referentne_tablice/POSLOVNICE.sql` | TSPOSLOV |
| 3.2 | `Db2/create/referentne_tablice/RIZIK_OCIJENA_REF.sql` | TSRIZIK |
| 3.3 | `Db2/create/referentne_tablice/STATUS_REF.sql` | TSSTATUS |
| 3.4 | `Db2/create/referentne_tablice/TIP_RACUNA_REF.sql` | TSTIPRAC |
| 3.5 | `Db2/create/referentne_tablice/TRANSAKCIJA_TIP_REF.sql` | TSTIPTRN |
| 3.6 | `Db2/create/referentne_tablice/IZVORNI_SUSTAVI.sql` | TSIZVOR |

**Verify after each file:**
```sql
SELECT NAME, TSNAME FROM SYSIBM.SYSTABLES
WHERE CREATOR = 'BANKING_TEST'
AND NAME = '<TABLE_NAME>';
```

**Verify after all reference tables created:**
```sql
SELECT COUNT(*) AS REF_TABLE_COUNT
FROM SYSIBM.SYSTABLES
WHERE CREATOR = 'BANKING_TEST'
AND NAME IN ('POSLOVNICE', 'RIZIK_OCJENA_REF', 'STATUS_REF',
             'TIP_RACUNA_REF', 'TRANSAKCIJA_TIP_REF', 'IZVORNI_SUSTAV_REF')
AND TYPE = 'T';
```
Expected: 6.

---

## STEP 4: MASTER ENTITIES

| Order | File | Tablespace | FK dependencies |
|-------|------|------------|-----------------|
| 4.1 | `Db2/create/master_tablice/KLIJENTI.sql` | TSKLIENT | POSLOVNICE, RIZIK_OCJENA_REF |
| 4.2 | `Db2/create/proizvodi_tablice/PROIZVOD_MASTER.sql` | TSPROIZV | TIP_RACUNA_REF |

**Verify:**
```sql
SELECT NAME, COLCOUNT, NPAGES
FROM SYSIBM.SYSTABLES
WHERE CREATOR = 'BANKING_TEST'
AND NAME IN ('KLIJENTI', 'PROIZVOD_MASTER');
```

---

## STEP 5: DEPENDENT ENTITIES

These reference master entities or reference tables.

| Order | File | Tablespace | Key FK dependencies |
|-------|------|------------|---------------------|
| 5.1 | `Db2/create/racuni_tablice/RACUNI.sql` | TSRACUNI | KLIJENTI, TIP_RACUNA_REF, POSLOVNICE |
| 5.2 | `Db2/create/master_tablice/KLIJENT_ADRESE.sql` | TSADRESE | KLIJENTI |
| 5.3 | `Db2/create/master_tablice/KLIJENT_KONTAKTI.sql` | TSKONTKT | KLIJENTI |
| 5.4 | `Db2/create/master_tablice/KLIJENT_DOKUMENTI.sql` | TSDOKUM | KLIJENTI |
| 5.5 | `Db2/create/proizvodi_tablice/KAMATNE_STOPE.sql` | TSKAMATA | PROIZVOD_MASTER |
| 5.6 | `Db2/create/racuni_tablice/RACUN_PROIZVODI.sql` | TSRACPRO | RACUNI, PROIZVOD_MASTER |

**Verify:**
```sql
SELECT NAME FROM SYSIBM.SYSTABLES
WHERE CREATOR = 'BANKING_TEST'
AND TYPE = 'T'
ORDER BY NAME;
```
Should show 11 tables so far (6 reference + 2 master + 6 dependent = wait, 5 dependent here, so 6+2+6 = 14 total expected after this step, but RACUNI is here too so 6+2+6 = 14 actually 6 reference + 2 master + 6 dependent = 14 -- let me recount: POSLOVNICE, RIZIK, STATUS, TIPRAC, TIPTRN, IZVOR (6) + KLIJENTI, PROIZVOD_MASTER (2) + RACUNI, KLIJENT_ADRESE, KLIJENT_KONTAKTI, KLIJENT_DOKUMENTI, KAMATNE_STOPE, RACUN_PROIZVODI (6) = 14).

Expected after step 5: 14 tables.

---

## STEP 6: TRANSACTION TABLES

| Order | File | Tablespace | Key FK dependencies |
|-------|------|------------|---------------------|
| 6.1 | `Db2/create/transakcije_tablice/TRANSAKCIJE.sql` | TSTRANS | RACUNI, TRANSAKCIJA_TIP_REF, IZVORNI_SUSTAV_REF, POSLOVNICE |
| 6.2 | `Db2/create/blokade_tablice/BLOKADE.sql` | TSBLOK | RACUNI, POSLOVNICE |
| 6.3 | `Db2/create/red_transakcija_tablice/RED_TRANSAKCIJA.sql` | TSRED | RACUNI, TRANSAKCIJA_TIP_REF, IZVORNI_SUSTAV_REF, POSLOVNICE |
| 6.4 | `Db2/create/glavna_knjiga_tablice/GLAVNA_KNJIGA.sql` | TSGLAVNA | POSLOVNICE |

**Verify:**
```sql
SELECT COUNT(*) AS TOTAL_TABLES
FROM SYSIBM.SYSTABLES
WHERE CREATOR = 'BANKING_TEST'
AND TYPE = 'T';
```
Expected: 18.

---

## STEP 7: SEQUENCES

**File:** `Db2/create/sekvence/sekvence.sql`

Creates 9 sequences for ID generation.

**Verify:**
```sql
SELECT NAME, START, INCREMENT, CACHE
FROM SYSIBM.SYSSEQUENCES
WHERE SCHEMA = 'BANKING_TEST'
ORDER BY NAME;
```
Expected: 9 rows (SEQ_KUPAC_ID, SEQ_TRANSAKCIJA_ID, SEQ_BLOKADA_ID, SEQ_UNOS_GK_ID, SEQ_RED_ID, SEQ_ADRESA_ID, SEQ_KONTAKT_ID, SEQ_DOKUMENT_ID, SEQ_STOPA_ID).

---

## STEP 8: INDEXES

**File:** `Db2/create/indeksi/indeksi_optimizacija.sql` (use this one - production-grade)

Note: there are two index files in the repo. Use `indeksi_optimizacija.sql`
(with PRIQTY/SECQTY/BUFFERPOOL clauses) NOT `indeksi_baze.sql` (minimal version,
useful only for very early prototyping).

**Verify:**
```sql
SELECT COUNT(*) AS TOTAL_INDEXES
FROM SYSIBM.SYSINDEXES
WHERE CREATOR = 'BANKING_TEST';
```
Expected: ~24+ indexes (4 KLIJENTI + 5 RACUNI + 7 TRANSAKCIJE + 3 BLOKADE + 2 RED + 2 GK + auto-created PK indexes).

---

## STEP 9: REFERENCE DATA INSERTS

Order matters - parent tables before child tables.

| Order | File | Inserts into | FK dependency |
|-------|------|--------------|---------------|
| 9.1 | `Db2/insert/insert_poslovnice.sql` | POSLOVNICE | none |
| 9.2 | `Db2/insert/insert_rizik_ocijena_ref.sql` | RIZIK_OCJENA_REF | none |
| 9.3 | `Db2/insert/insert_status_ref.sql` | STATUS_REF | none |
| 9.4 | `Db2/insert/insert_tip_racuna.sql` | TIP_RACUNA_REF | none |
| 9.5 | `Db2/insert/insert_tip_transakcija.sql` | TRANSAKCIJA_TIP_REF | none |
| 9.6 | `Db2/insert/insert_izvorni_sustav.sql` | IZVORNI_SUSTAV_REF | none |
| 9.7 | `Db2/insert/insert_proizvod_master.sql` | PROIZVOD_MASTER | TIP_RACUNA_REF (9.4) |
| 9.8 | `Db2/insert/insert_kamatne_stope.sql` | KAMATNE_STOPE | PROIZVOD_MASTER (9.7) |

**Verify:**
```sql
SELECT 'POSLOVNICE' AS TABLICA, COUNT(*) AS BROJ FROM POSLOVNICE
UNION ALL SELECT 'RIZIK_OCJENA_REF', COUNT(*) FROM RIZIK_OCJENA_REF
UNION ALL SELECT 'STATUS_REF', COUNT(*) FROM STATUS_REF
UNION ALL SELECT 'TIP_RACUNA_REF', COUNT(*) FROM TIP_RACUNA_REF
UNION ALL SELECT 'TRANSAKCIJA_TIP_REF', COUNT(*) FROM TRANSAKCIJA_TIP_REF
UNION ALL SELECT 'IZVORNI_SUSTAV_REF', COUNT(*) FROM IZVORNI_SUSTAV_REF
UNION ALL SELECT 'PROIZVOD_MASTER', COUNT(*) FROM PROIZVOD_MASTER
UNION ALL SELECT 'KAMATNE_STOPE', COUNT(*) FROM KAMATNE_STOPE;
```

Expected counts:
| Table | Rows |
|-------|------|
| POSLOVNICE | 3 |
| RIZIK_OCJENA_REF | 6 |
| STATUS_REF | 13 |
| TIP_RACUNA_REF | 5 |
| TRANSAKCIJA_TIP_REF | 7 |
| IZVORNI_SUSTAV_REF | 6 |
| PROIZVOD_MASTER | 5 |
| KAMATNE_STOPE | 6 |

---

## STEP 10: TABLE COMMENTS (optional - documentation only)

**File:** `Db2/komentari/komentari.sql`

Adds COMMENT ON TABLE entries. Does not affect functionality - skip if pressed for time.

---

## STEP 11: GRANTS

**File:** `Db2/schema/prava/prava.sql`

Grants SELECT/INSERT/UPDATE/DELETE on user tables and USAGE on sequences to IBMUSER and AGIBAS.

**Verify:**
```sql
SELECT GRANTEE, TTNAME, AUTHHOWGOT
FROM SYSIBM.SYSTABAUTH
WHERE TCREATOR = 'BANKING_TEST'
AND GRANTEE IN ('IBMUSER', 'AGIBAS')
ORDER BY GRANTEE, TTNAME;
```

---

## STEP 12: VALIDATION

**File:** `Db2/schema/testovi/test_scheme.sql`

Runs full system validation:
- Checks all tables exist (counts by category)
- Verifies index and sequence counts
- Lists FK constraints
- Inserts a test KLIJENT, RACUN, TRANSAKCIJA
- Tests cross-table JOIN

If all queries return expected counts and the test JOIN returns one row, deployment is successful.

---

## ROLLBACK

If deployment fails partway, you have two options:

**Option A: Targeted cleanup**
Drop only what was created up to the failure point. Use `Db2/drop/brisanje.sql` for tables only (assumes tablespaces and database remain).

**Option B: Full reset**
Use `Db2/schema/rollback/rollback.sql` (uncomment the destructive block first). Drops everything including tablespaces and the database. After this, restart from STEP 1.

---

## TROUBLESHOOTING QUICK REFERENCE

| Error | Likely cause | Fix |
|-------|-------------|-----|
| -204 Object not found | Parent table not created yet | Check FK dependency order |
| -601 Object already exists | Re-running without rollback | Run brisanje.sql or rollback.sql |
| -551 Not authorized | Missing privilege | Check Pre-Requisites; contact DBA |
| -612 Duplicate column | Typo in DDL | Compare to repo file |
| -668 Cannot reference table | FK column type mismatch | Verify CHAR lengths match parent |
| -679 Naming conflict tablespace | Tablespace created previously | Drop tablespace or skip CREATE TABLESPACE |

---

## NOTES

- Each `.sql` file in the repo starts with `SET CURRENT SCHEMA = 'BANKING_TEST';` so schema context is set per file.
- After every COMMIT, changes are durable. If running batch, COMMIT between major steps is recommended.
- For SPUFI: copy contents of each file into the SPUFI input dataset, run, check SPUFI output for SQLCODE = 0 before proceeding to next file.
- For batch (DSNTEP2/DSNTIAD): you can concatenate files into one input member, but keep COMMIT statements between major sections.

---

**Deployment guide version:** 1.0
**Last updated:** 2026-05-03
