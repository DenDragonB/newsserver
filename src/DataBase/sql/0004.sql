INSERT INTO
MigrationHistory ( MajorVersion, MinorVersion, FileNumber, Comment,    DateApplied )
VALUES           ( '01',         '01',         '0004',     '0004',     NOW() );

INSERT INTO
Users  ( UserName, Pass,   Adm,  Token,   RegDate)
VALUES ( 'admin', 'admin', True, 'admin', NOW() );
