INSERT INTO
MigrationHistory ( MajorVersion, MinorVersion, FileNumber, Comment,    DateApplied )
VALUES           ( '01',         '01',         '0002',     '0002',     NOW() );

ALTER TABLE Drafts RENAME COLUMN Autor TO Author;