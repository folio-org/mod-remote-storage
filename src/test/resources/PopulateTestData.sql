CREATE EXTENSION IF NOT EXISTS pgcrypto;
INSERT INTO remote_storage_configurations (id, name, provider_name, url, accession_delay, accession_time_unit, created_date)
  VALUES (gen_random_uuid(), 'RS2', 'Dematic', 'http://rs2.dematic.com', 1, 'hours', now());

INSERT INTO location_mappings (folio_location_id, configuration_id)
VALUES ('53cf956f-c1df-410b-8bea-27f712cca7c0', 'de17bad7-2a30-4f1c-bee5-f653ded15629');
