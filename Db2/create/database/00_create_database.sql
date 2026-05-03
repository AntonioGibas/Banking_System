-- ==================================================================================
-- BANKING SYSTEM - DATABASE CREATION
-- Platform: IBM DB2 z/OS v13
-- ==================================================================================
-- This script must be executed FIRST, before any tablespace or table DDL.
-- All tablespaces in the banking system reference IN BANKTEST.
-- ==================================================================================

CREATE DATABASE BANKTEST
    BUFFERPOOL BP1
    INDEXBP    BP2
    STOGROUP   SYSDEFLT
    CCSID      UNICODE;

COMMIT;