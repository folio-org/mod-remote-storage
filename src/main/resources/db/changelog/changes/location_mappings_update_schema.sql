CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
ALTER TABLE location_mappings
 RENAME COLUMN folio_location_id TO final_location_id;
ALTER TABLE location_mappings
 RENAME COLUMN configuration_id  TO remote_configuration_id;
ALTER TABLE location_mappings
 ADD COLUMN IF NOT EXISTS original_location_id UUID default uuid_generate_v1();
ALTER TABLE location_mappings
 ALTER COLUMN             original_location_id SET NOT NULL;
UPDATE location_mappings SET original_location_id = COALESCE(final_location_id);
