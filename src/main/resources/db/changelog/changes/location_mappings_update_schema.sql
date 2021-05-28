ALTER TABLE location_mappings
    RENAME COLUMN folio_location_id TO final_location_id;
ALTER TABLE location_mappings
    RENAME COLUMN configuration_id  TO remote_configuration_id;

DROP TABLE IF EXISTS original_locations;

CREATE TABLE original_locations (
  final_location_id UUID NOT NULL,
  original_location_id UUID NOT NULL,
  FOREIGN KEY(final_location_id) REFERENCES location_mappings(final_location_id)
    ON DELETE CASCADE ON UPDATE CASCADE,
  PRIMARY KEY (final_location_id, original_location_id)
);
