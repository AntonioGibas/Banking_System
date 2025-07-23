# Banking System Database

Enterprise-grade banking database system for IBM DB2 z/OS with COBOL integration support.

## 🏗️ Architecture

- **Platform**: IBM DB2 z/OS v13
- **Normalization**: 3NF compliant
- **Schema**: `BANKING_TEST` 
- **Tables**: 18 tables organized in 4 categories
- **Encoding**: UTF-8, ACID compliant

## 📊 Database Structure

### Reference Tables (6)
Static configuration data
- `POSLOVNICE` - Bank branches
- `RIZIK_OCJENA_REF` - Risk assessments  
- `STATUS_REF` - Status codes
- `TIP_RACUNA_REF` - Account types
- `TRANSAKCIJA_TIP_REF` - Transaction types
- `IZVORNI_SUSTAV_REF` - Source systems

### Master Entities (3)
Core business objects
- `KLIJENTI` - Customer master
- `PROIZVOD_MASTER` - Product catalog
- `RACUNI` - Account master

### Normalized Tables (5)
1:N relationships for data normalization
- `KLIJENT_ADRESE` - Customer addresses
- `KLIJENT_KONTAKTI` - Customer contacts
- `KLIJENT_DOKUMENTI` - Customer documents
- `KAMATNE_STOPE` - Interest rate tiers
- `RACUN_PROIZVODI` - Account-product links (M:N)

### Transaction Tables (4)
High-frequency operational data
- `TRANSAKCIJE` - Posted transactions
- `BLOKADE` - Account blocks/holds
- `RED_TRANSAKCIJA` - Transaction queue
- `GLAVNA_KNJIGA` - General ledger entries

## 🚀 Quick Start

### Prerequisites
- IBM DB2 z/OS access
- IBMUSER or equivalent privileges
- Minimum 200MB tablespace allocation

### Deployment
```bash
# 1. Create schema and tablespaces
db2 -f create_schema.sql
db2 -f TS_BANK_*.sql

# 2. Deploy reference tables first
db2 -f POSLOVNICE.sql
db2 -f RIZIK_OCIJENA_REF.sql
# ... continue with dependency order

# 3. Create sequences and indexes
db2 -f sekvence.sql
db2 -f indeksi_optimizacija.sql

# 4. Load reference data
db2 -f insert_*.sql

# 5. Set permissions
db2 -f prava.sql

# 6. Validate deployment
db2 -f test_scheme.sql
```

### Cleanup
```bash
# Complete rollback (CAUTION: Deletes all data!)
db2 -f rollback.sql
```

## 💳 Key Features

- **Croatian Banking Standards**: OIB validation, local formats
- **Risk Management**: 6-tier risk assessment (N1→V2)
- **Multi-Product Support**: 5 banking products with rate tiers  
- **Real-time Processing**: Transaction queue with retry logic
- **Audit Trail**: Complete timestamp tracking
- **COBOL Ready**: Field sizes optimized for COBOL development

## 🔧 Technical Specs

### Performance
- **Sequences**: Optimized cache settings (50-1000)
- **Indexes**: Strategic composite indexes for common queries
- **Partitioning**: Monthly partitioning ready for TRANSAKCIJE
- **Buffer Pools**: BP0 optimized allocation

### Security
- **Access Control**: Role-based permissions (IBMUSER, AGIBAS)
- **Data Privacy**: Encryption-ready document paths
- **Audit Levels**: 5-tier security classification
- **Constraint Validation**: Business rule enforcement

### COBOL Integration
```cobol
01  CUSTOMER-RECORD.
    05  KUPAC-ID            PIC 9(12).
    05  OIB                 PIC X(11).
    05  IME                 PIC X(30).
    05  PREZIME             PIC X(30).
```

## 📋 Sample Operations

### Customer Lookup
```sql
SELECT KUPAC_ID, IME, PREZIME, STATUS
FROM BANKING_TEST.KLIJENTI 
WHERE OIB = ? AND STATUS = 'A';
```

### Account Balance
```sql
SELECT TRENUTNI_SALDO, DOSTUPNI_SALDO
FROM BANKING_TEST.RACUNI
WHERE RACUN_BROJ = ? AND STATUS IN ('A', 'B');
```

### Transaction History  
```sql
SELECT DATUM_TRANSAKCIJE, KOD_TRANSAKCIJE, IZNOS, OPIS
FROM BANKING_TEST.TRANSAKCIJE  
WHERE RACUN_BROJ = ?
ORDER BY DATUM_TRANSAKCIJE DESC;
```

## 📈 Project Timeline

- **Phase 1**: COPYBOOK & Basic Programs (24.7-24.8)
- **Phase 2**: Core Business Logic (25.8-19.10)  
- **Phase 3**: Integration & Workflow (20.10-23.11)
- **Phase 4**: Finalization (24.11-14.12)

## 📚 Documentation

- `TEHNIČKA DOKUMENTACIJA.md` - Complete technical specs
- `Test_Schema.md` - Detailed schema documentation  
- `Project roadmap.txt` - Development timeline
- `komentari.sql` - Table documentation comments

## ⚠️ Important Notes

- **Test Environment**: This is `BANKING_TEST` schema for development
- **Production Sizing**: Adjust tablespace sizes for production volumes
- **Backup Strategy**: Implement appropriate backup/recovery procedures
- **Monitoring**: Set up performance monitoring for transaction volumes

## 🤝 Contributing

1. Follow Croatian banking standards
2. Maintain 3NF normalization
3. Update documentation for schema changes
4. Test all constraints and foreign keys
5. Optimize indexes for query patterns

---

**Version**: 1.0  
**Created**: July 23, 2025  
**Environment**: TEST  
**Status**: Production Ready