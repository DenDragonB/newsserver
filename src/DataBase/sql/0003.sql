INSERT INTO
MigrationHistory ( MajorVersion, MinorVersion, FileNumber, Comment,    DateApplied )
VALUES           ( '01',         '01',         '0003',     '0003',     NOW() );

ALTER TABLE Authors ADD FOREIGN KEY (UserID) REFERENCES Users (Id);

ALTER TABLE News ADD FOREIGN KEY (Author) REFERENCES Authors (Id);
ALTER TABLE News ADD FOREIGN KEY (Category) REFERENCES Categories (Id);

ALTER TABLE Drafts ADD FOREIGN KEY (Author) REFERENCES Authors (Id);
ALTER TABLE Drafts ADD FOREIGN KEY (Category) REFERENCES Categories (Id);