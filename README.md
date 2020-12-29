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

API provides the following URLs for working with mappings between Folio locations and remote storage configurations:

|  Method | URL| Permissions  | Description  | 
|---|---|---|---|
| POST | /remote-storage/mappings | remote-storage.mappings.item.post | Creates new or updates an existing location mapping |
| DELETE | /remote-storage/mappings/{folioLocationId} | remote-storage.mappings.item.delete | Deletes location mapping by Folio location id |
| GET | /remote-storage/mappings | remote-storage.configurations.mappings.get   | Retrieves all location mappings |
| GET | /remote-storage/mappings/{folioLocationId} | remote-storage.mappings.item.get | Retrieves a location mapping by Folio location id |

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
