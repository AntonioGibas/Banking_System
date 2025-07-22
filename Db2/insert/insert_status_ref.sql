-- STATUS REFERENCE

INSERT INTO STATUS_REF VALUES
    ('A', 'Aktivan', 'Entitet je aktivan i operativan', 'GENERAL');
INSERT INTO STATUS_REF VALUES
    ('N', 'Neaktivan', 'Entitet je neaktivan', 'GENERAL');
INSERT INTO STATUS_REF VALUES
    ('Z', 'Zatvoren', 'Entitet je zatvoren', 'GENERAL');
INSERT INTO STATUS_REF VALUES
    ('S', 'Suspendiran', 'Entitet je privremeno suspendiran', 'GENERAL');
INSERT INTO STATUS_REF VALUES
    ('B', 'Blokiran', 'Entitet je blokiran', 'GENERAL');
INSERT INTO STATUS_REF VALUES
    ('M', 'Odrzavanje', 'Entitet je u procesu odrzavanja', 'GENERAL');
INSERT INTO STATUS_REF VALUES
    ('K', 'Knjizen', 'Transakcija je uspjesno knjizena', 'TRANSAKCIJA');
INSERT INTO STATUS_REF VALUES
    ('P', 'Pending', 'Transakcija je na cekanju', 'TRANSAKCIJA');
INSERT INTO STATUS_REF VALUES
    ('O', 'Otkazana', 'Transakcija je otkazana', 'TRANSAKCIJA');
INSERT INTO STATUS_REF VALUES
    ('G', 'Greska', 'Greska u obradi transakcije', 'TRANSAKCIJA');
INSERT INTO STATUS_REF VALUES
    ('C', 'Ceka', 'U redu za obradu', 'QUEUE');
INSERT INTO STATUS_REF VALUES
    ('U', 'U obradi', 'Trenutno se obradjuje', 'QUEUE');
INSERT INTO STATUS_REF VALUES
    ('I', 'Istekla', 'Blokada je istekla', 'BLOKADA');