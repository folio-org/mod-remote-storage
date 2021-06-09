# mod-remote-storage

Copyright (C) 2020 The Open Library Foundation

This software is distributed under the terms of the Apache License,
Version 2.0. See the file "[LICENSE](LICENSE)" for more information.

## Introduction

Backend module for remote storages

The mod-remote-storage module provides API for:
  * storing and retrieving all remote-storages settings
  * setting the remote-storage for the location from UI
  * adapter modules

## Additional information

API provides the following URLs for working with remote storage configurations:

| Method | URL| Permissions | Description | 
|---|---|---|---|
| POST | /remote-storage/configurations  | remote-storage.configurations.item.post | Creates a remote storage configuration |
| GET | /remote-storage/configurations | remote-storage.configurations.collection.get   | Retrieves all remote storage configurations |
| GET | /remote-storage/configurations/{configurationId} | remote-storage.configurations.item.get | Retrieves a remote storage configuration by id |
| PUT | /remote-storage/configurations/{configurationId} | remote-storage.configurations.item.put | Updates a remote storage configuration |
| DELETE | /remote-storage/configurations/{configurationId} | remote-storage.configurations.item.delete | Deletes a remote storage configuration by id |

API provides the following URLs for working with mappings between Folio locations and remote storage configurations (one-to-one):

|  Method | URL| Permissions  | Description  | 
|---|---|---|---|
| POST | /remote-storage/mappings | remote-storage.mappings.item.post | Creates new or updates an existing location mapping |
| DELETE | /remote-storage/mappings/{finalLocationId} | remote-storage.mappings.item.delete | Deletes location mapping by Folio (final) location id |
| GET | /remote-storage/mappings | remote-storage.configurations.mappings.get   | Retrieves all location mappings |
| GET | /remote-storage/mappings/{finalLocationId} | remote-storage.mappings.item.get | Retrieves a location mapping by Folio (final) location id |

API provides the following URLs for working with extended mappings (remote (final) locations, remote storage configurations and original locations):

|  Method | URL| Query parameters| Permissions  | Description  | 
|---|---|---|---|---|
| POST | /remote-storage/extended-mappings | - | remote-storage.extended-mappings.item.post | Creates new or updates an existing extended location mapping |
| DELETE | /remote-storage/extended-mappings | Required either `originalLocationId` or `finalLocationId` | remote-storage.extended-mappings.item.delete | Deletes extended location mapping (with all associated original locations) by `finalLocationId` or original location only by `originalLocationId`|
| GET | /remote-storage/extended-mappings | Optional: `finalLocationId`, `remoteConfigurationId` or `originalLocationId` | remote-storage.configurations.extended-mappings.get   | Retrieves all extended location mappings (if no query parameters specified), or by query parameter |
| GET | /remote-storage/extended-mappings/{finalLocationId} | - | remote-storage.extended-mappings.item.get | Retrieves an extended location mapping by Folio (final) location id |
| GET | /remote-storage/extended-mappings/locations | - | remote-storage.extended-mappings-locations.collection.get   | Retrieves all original locations with corresponding final (remote) locations, if they exist |

API provides the following URL to retrieve by check-in and return item using barcode:

|  Method | URL| Permissions  | Description  | 
|---|---|---|---|
| POST | /remote-storage/retrieve/{remoteStorageConfigurationId}/checkInItem | remote-storage.check-in.item.post | Check-in item by barcode |
| POST | /remote-storage/return/{remoteStorageConfigurationId} | remote-storage.return.item.post | Return item by barcode |

API provides the following URL to retrieve providers:

|  Method | URL| Permissions  | Description  | 
|---|---|---|---|
| GET |  /remote-storage/providers | remote-storage.providers.collection.get | Get list of providers |

### Required Permissions
Institutional users should be granted the following permissions in order to use this remote storage API:
- `remote-storage.all`

### Issue tracker
See project [MODRS](https://issues.folio.org/browse/MODRS)
at the [FOLIO issue tracker](https://dev.folio.org/guidelines/issue-tracker).

### Other documentation
Other [modules](https://dev.folio.org/source-code/#server-side) are described,
with further FOLIO Developer documentation at
[dev.folio.org](https://dev.folio.org/)
