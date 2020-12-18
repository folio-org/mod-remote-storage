CREATE TABLE remote_storage_configurations (
    id                      UUID PRIMARY KEY,
    name                    VARCHAR(256) CONSTRAINT unq_RemoteStorageConfigurations_Name UNIQUE NOT NULL,
    provider_name           VARCHAR(256),
    url                     VARCHAR(256),
    accession_delay         INT,
    accession_time_unit     VARCHAR(50),
    created_date            TIMESTAMP        default now(),
    created_by_user_id      UUID,
    created_by_username     VARCHAR(100),
    updated_date            TIMESTAMP,
    updated_by_user_id      UUID,
    updated_by_username     VARCHAR(100)
);
