CREATE EXTENSION IF NOT EXISTS pgcrypto;
INSERT INTO remote_storage_configurations (id, name, provider_name, url, accession_delay, accession_time_unit, created_date)
  VALUES (gen_random_uuid(), 'RS2', 'Dematic', 'http://rs2.dematic.com', 1, 'hours', now());
