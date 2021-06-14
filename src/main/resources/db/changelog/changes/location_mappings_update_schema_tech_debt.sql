ALTER TABLE original_locations
  DROP CONSTRAINT original_locations_pkey;
ALTER TABLE original_locations
  ALTER COLUMN final_location_id DROP NOT NULL;
