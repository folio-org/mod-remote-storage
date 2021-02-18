CREATE TABLE IF NOT EXISTS remote_storage_configurations
(
    id                  UUID PRIMARY KEY,
    name                VARCHAR(256)
        CONSTRAINT unq_RemoteStorageConfigurations_Name UNIQUE NOT NULL,
    api_key             VARCHAR(256),
    provider_name       VARCHAR(256),
    url                 VARCHAR(256),
    status_url          VARCHAR(256),
    accession_delay     INT,
    accession_time_unit VARCHAR(50),
    created_date        TIMESTAMP default now(),
    created_by_user_id  UUID,
    created_by_username VARCHAR(100),
    updated_date        TIMESTAMP,
    updated_by_user_id  UUID,
    updated_by_username VARCHAR(100)
);

CREATE TABLE IF NOT EXISTS location_mappings
(
    folio_location_id UUID PRIMARY KEY,
    configuration_id  UUID NOT NULL
);

CREATE TABLE IF NOT EXISTS accession_queue
(
    id                  UUID PRIMARY KEY,
    item_barcode        VARCHAR(256),
    created_date_time   TIMESTAMP default now(),
    accessioned_date_time TIMESTAMP,
    remote_storage_id   UUID,
    call_number         TEXT,
    instance_title      TEXT,
    instance_author     TEXT
);

CREATE TABLE IF NOT EXISTS retrieval_queue
(
    id                  UUID PRIMARY KEY,
    hold_id             VARCHAR(256),
    item_barcode        VARCHAR(256),
    created_date_time   TIMESTAMP default now(),
    retrieved_date_time TIMESTAMP,
    remote_storage_id   UUID,
    instance_title      TEXT,
    instance_author     TEXT,
    call_number         TEXT,
    patron_barcode      TEXT,
    patron_name         TEXT,
    pickup_location     TEXT,
    request_status      TEXT,
    request_note        TEXT
);

CREATE TABLE IF NOT EXISTS system_user_parameters
(
    id          UUID PRIMARY KEY,
    username    VARCHAR(50) NOT NULL,
    password    VARCHAR(50) NOT NULL,
    okapi_token VARCHAR(8000),
    okapi_url   VARCHAR(100),
    tenant_id   VARCHAR(100)
);
