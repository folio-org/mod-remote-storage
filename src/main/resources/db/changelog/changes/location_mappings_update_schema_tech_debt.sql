CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

ALTER TABLE original_locations
  DROP CONSTRAINT original_locations_pkey;
ALTER TABLE original_locations
  ADD COLUMN id UUID DEFAULT uuid_generate_v4() PRIMARY KEY;
ALTER TABLE original_locations
  ALTER COLUMN final_location_id DROP NOT NULL;

