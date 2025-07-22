-- TRANSAKCIJA TIPOVI

INSERT INTO TRANSAKCIJA_TIP_REF VALUES 
    ('UPL', 'Uplata gotovine', 'Uplata gotovine na racun', '+', 'N', 'A');
INSERT INTO TRANSAKCIJA_TIP_REF VALUES 
    ('ISP', 'Isplata gotovine', 'Isplata gotovine s racuna', '-', 'D', 'A');
INSERT INTO TRANSAKCIJA_TIP_REF VALUES 
    ('PRE', 'Prijenos sredstava', 'Prijenos izmedu racuna', '=', 'D', 'A');
INSERT INTO TRANSAKCIJA_TIP_REF VALUES 
    ('NAK', 'Naknada', 'Banka naplacuje naknadu', '-', 'N', 'A');
INSERT INTO TRANSAKCIJA_TIP_REF VALUES 
    ('KAM', 'Kamata', 'Kamata na racun', '+', 'N', 'A');
INSERT INTO TRANSAKCIJA_TIP_REF VALUES 
    ('PRI', 'Primanje', 'Primanje iz vanjskih izvora', '+', 'N', 'A');
INSERT INTO TRANSAKCIJA_TIP_REF VALUES 
    ('STO', 'Storno', 'Storno transakcije', '=', 'N', 'A');