CREATE TABLE MigrationHistory
    ( Id INT PRIMARY KEY NOT NULL
    , MajorVersion VARCHAR(2),
    , MinorVersion VARCHAR(2),
    , FileNumber VARCHAR(4),
    , Comment VARCHAR(255),
    , DateApplied DATETIME
    );
INSERT INTO
MigrationHistory ( MajorVersion, MinorVersion, FileNumber, Comment,    DateApplied )
VALUES           ( '01',         '01',         '0000',     'Baseline', NOW() )

CREATE TABLE Users 
    ( Id INT(11) PRIMARY KEY NOT NULL 
    , FirstName TEXT(30)
    , LastName TEXT(30)
    , Avatar TEXT
    , UserName TEXT(30) NOT NULL
    , Pass TEXT(30) NOT NULL
    , RegDate DATE NOT NULL
    , Adm BOOLEAN NOT NULL DEFAULT FALSE
    , Token TEXT
    );
CREATE TABLE Authors 
    ( Id INT(11) PRIMARY KEY NOT NULL 
    , UserID INT(11) NOT NULL
    , About TEXT
    );
CREATE TABLE Categories 
    ( Id INT(11) PRIMARY KEY NOT NULL 
    , CatName INT(11) NOT NULL
    , Parent INT(11)
    );
CREATE TABLE Tags
    ( Tag TEXT PRIMARY KEY NOT NULL
    ); 