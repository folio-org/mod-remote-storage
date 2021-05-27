DROP TABLE IF EXISTS original_locations;

CREATE TABLE original_locations (
  folio_location_id UUID NOT NULL,
  original_location_id UUID NOT NULL,
  FOREIGN KEY(folio_location_id) REFERENCES location_mappings(folio_location_id)
    ON DELETE CASCADE ON UPDATE CASCADE,
  PRIMARY KEY (folio_location_id, original_location_id)
);
