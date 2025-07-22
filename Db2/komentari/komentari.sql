-- ==================================================================================
-- KOMENTARI TABLICA ZA DOKUMENTACIJU
-- ==================================================================================

COMMENT ON TABLE POSLOVNICE IS 'Organizacijska struktura - poslovnice banke';
COMMENT ON TABLE KLIJENTI IS 'Glavni podaci o klijentima - normalizirano';
COMMENT ON TABLE KLIJENT_ADRESE IS 'Adrese klijenta - vise adresa po klijentu';
COMMENT ON TABLE KLIJENT_KONTAKTI IS 'Kontakt podaci klijenta - telefoni, emailovi';
COMMENT ON TABLE KLIJENT_DOKUMENTI IS 'Dokumenti klijenta - sigurnosni podaci';
COMMENT ON TABLE PROIZVOD_MASTER IS 'Master katalog bankovnih proizvoda';
COMMENT ON TABLE KAMATNE_STOPE IS 'Kamatni razredi vezani za proizvode';
COMMENT ON TABLE RACUNI IS 'Racuni klijenata - ispravljena hijerarhija';
COMMENT ON TABLE RACUN_PROIZVODI IS 'Veza racun-proizvod (M:N relationship)';
COMMENT ON TABLE TRANSAKCIJE IS 'Transakcijska povijest - pojednostavljeno';
COMMENT ON TABLE BLOKADE IS 'Blokade racuna - bez redundantnih podataka';
COMMENT ON TABLE GLAVNA_KNJIGA IS 'Glavno knjizni unosi - bez klijent podataka';
COMMENT ON TABLE RED_TRANSAKCIJA IS 'Queue transakcija - pojednostavljeno';
COMMENT ON TABLE RIZIK_OCJENA_REF IS 'Reference tabela - rizik kategorije';
COMMENT ON TABLE STATUS_REF IS 'Reference tabela - statusni kodovi';
COMMENT ON TABLE TIP_RACUNA_REF IS 'Reference tabela - tipovi racuna';
COMMENT ON TABLE TRANSAKCIJA_TIP_REF IS 'Reference tabela - tipovi transakcija';
COMMENT ON TABLE IZVORNI_SUSTAV_REF IS 'Reference tabela - izvorni sustavi';