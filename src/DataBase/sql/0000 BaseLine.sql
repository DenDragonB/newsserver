CREATE TABLE MigrationHistory
    ( Id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY
    , MajorVersion VARCHAR(2)
    , MinorVersion VARCHAR(2)
    , FileNumber VARCHAR(4)
    , Comment VARCHAR(255)
    , DateApplied DATE
    );
INSERT INTO
MigrationHistory ( MajorVersion, MinorVersion, FileNumber, Comment,    DateApplied )
VALUES           ( '01',         '01',         '0000',     'Baseline', NOW() );

CREATE TABLE Users 
    ( Id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY 
    , FirstName VARCHAR(30)
    , LastName VARCHAR(30)
    , Avatar TEXT
    , UserName VARCHAR(30) NOT NULL
    , Pass VARCHAR NOT NULL
    , RegDate DATE NOT NULL
    , Adm BOOLEAN NOT NULL DEFAULT FALSE
    , Token TEXT
    );
CREATE TABLE Authors 
    ( Id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY 
    , UserID INT NOT NULL
    , About TEXT
    );
CREATE TABLE Categories 
    ( Id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY 
    , CatName TEXT NOT NULL
    , Parent INT NOT NULL
    );
CREATE TABLE Tags
    ( Id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY 
    , Tag TEXT
    );
CREATE TABLE News
    ( Id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY 
    , Header TEXT NOT NULL
    , RegDate DATE NOT NULL
    , Author INT NOT NULL
    , Category INT NOT NULL
    , Tags INT ARRAY
    , Content TEXT
    , MainPhoto TEXT
    , Photos TEXT ARRAY 
    );
CREATE TABLE Drafts
    ( Id INT GENERATED BY DEFAULT AS IDENTITY PRIMARY KEY
    , News INT
    , Header TEXT NOT NULL
    , RegDate DATE NOT NULL
    , Autor INT NOT NULL
    , Category INT NOT NULL
    , Tags INT ARRAY
    , Content TEXT
    , MainPhoto TEXT
    , Photos TEXT ARRAY 
    );   