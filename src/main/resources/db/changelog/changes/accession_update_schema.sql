ALTER TABLE accession_queue
 ADD instance_contributors  TEXT,
 ADD publisher              TEXT,
 ADD publish_year           VARCHAR(100),
 ADD publish_place          TEXT,
 ADD volume                 VARCHAR(100),
 ADD enumeration            VARCHAR(100),
 ADD chronology             VARCHAR(100),
 ADD issn                   TEXT,
 ADD isbn                   TEXT,
 ADD oclc                   TEXT,
 ADD physical_description   TEXT,
 ADD material_type          TEXT,
 ADD copy_number            TEXT,
 ADD permanent_location_id  UUID

