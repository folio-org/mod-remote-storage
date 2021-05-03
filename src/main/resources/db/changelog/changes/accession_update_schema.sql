ALTER TABLE accession_queue
 ADD COLUMN IF NOT EXISTS instance_contributors  TEXT,
 ADD COLUMN IF NOT EXISTS publisher              TEXT,
 ADD COLUMN IF NOT EXISTS publish_year           VARCHAR(100),
 ADD COLUMN IF NOT EXISTS publish_place          TEXT,
 ADD COLUMN IF NOT EXISTS volume                 VARCHAR(100),
 ADD COLUMN IF NOT EXISTS enumeration            VARCHAR(100),
 ADD COLUMN IF NOT EXISTS chronology             VARCHAR(100),
 ADD COLUMN IF NOT EXISTS issn                   TEXT,
 ADD COLUMN IF NOT EXISTS isbn                   TEXT,
 ADD COLUMN IF NOT EXISTS oclc                   TEXT,
 ADD COLUMN IF NOT EXISTS physical_description   TEXT,
 ADD COLUMN IF NOT EXISTS material_type          TEXT,
 ADD COLUMN IF NOT EXISTS copy_number            TEXT,
 ADD COLUMN IF NOT EXISTS permanent_location_id  UUID

