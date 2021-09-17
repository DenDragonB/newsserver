INSERT INTO
MigrationHistory ( MajorVersion, MinorVersion, FileNumber, Comment,    DateApplied )
VALUES           ( '01',         '01',         '0006',     '0006',     NOW() );

INSERT INTO draftstotags (draftid,tagid)
SELECT d.id,UNNEST(d.tags) FROM drafts d
ON CONFLICT DO NOTHING;

ALTER TABLE drafts DROP COLUMN tags;
ALTER TABLE news DROP COLUMN tags;
