ALTER TABLE remote_storage_configurations
 ADD COLUMN IF NOT EXISTS accession_workflow_details VARCHAR(256);
ALTER TABLE remote_storage_configurations
 ADD COLUMN IF NOT EXISTS returning_workflow_details VARCHAR(256);
