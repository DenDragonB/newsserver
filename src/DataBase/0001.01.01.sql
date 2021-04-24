INSERT INTO
MigrationHistory ( MajorVersion, MinorVersion, FileNumber, Comment,    DateApplied )
VALUES           ( '01',         '01',         '0001',     '0001',     NOW() )

CREATE TABLE Comments
    ( Id INT(11) PRIMARY KEY NOT NULL 
    , Comment TEXT ARRAY
    );