## 1.6.0 Unreleased

## 1.5.0 Released
This release was focused on minor improvements and bug fixes

[Full Changelog](https://github.com/folio-org/mod-remote-storage/compare/v1.4.2...v1.5.0)

### Technical tasks
* [MODRS-116](https://issues.folio.org/browse/MODRS-116) - mod-remote-storage: folio-spring-base v3 update

### Stories
* [MODRS-102](https://issues.folio.org/browse/MODRS-102) - Add Notes field to remote storage API output
* [MODRS-115](https://issues.folio.org/browse/MODRS-115) - Support circulation interface v13
* [MODRS-101](https://issues.folio.org/browse/MODRS-101) - Update sample/testing data

### Bug fixes
* [MODRS-104](https://issues.folio.org/browse/MODRS-104) - Accessioning holdings & items to remote storage fails when no order record is present
* [MODRS-103](https://issues.folio.org/browse/MODRS-103) - Kiwi R3 2021 - Log4j vulnerability verification and correction
* [MODRS-94](https://issues.folio.org/browse/MODRS-94) - Improve remote storage accessioning process

## 1.4.0 Released
The primary focus of this release was to fix bugs, provide sample data for caiaisoft module

[Full Changelog](https://github.com/folio-org/mod-remote-storage/compare/v1.3.2...v1.4.0)

### Technical tasks
* [MODRS-85](https://issues.folio.org/browse/MODRS-85) - mod-remote-storage: folio-spring-base v2 update
* [MODRS-79](https://issues.folio.org/browse/MODRS-79) - mod-remote-storage unavailable in snapshot environments

### Stories
* [MODRS-82](https://issues.folio.org/browse/MODRS-82) - Make system user updatable
* [MODRS-74](https://issues.folio.org/browse/MODRS-74) - Implement endpoint to mark item as missing by barcode
* [MODRS-71](https://issues.folio.org/browse/MODRS-71) - Kafka Topics should use ENV variable and tenant id in their naming convention
* [MODRS-64](https://issues.folio.org/browse/MODRS-64) - align dependency versions affected by Inventory's Optimistic Lockings
* [MODRS-63](https://issues.folio.org/browse/MODRS-63) - Create sample data to test caiasoft retrieving jobs
* [MODRS-48](https://issues.folio.org/browse/MODRS-48) - Configuration fields enhancement

### Bug fixes
* [MODRS-93](https://issues.folio.org/browse/MODRS-93) - Issues related to optimistic locking while PUT executed for inventory/items/ endpoint
* [MODRS-91](https://issues.folio.org/browse/MODRS-91) - Kiwi - Item status becomes ‘In Transit’ when item returns to the remote storage
* [MODRS-84](https://issues.folio.org/browse/MODRS-84) - Cannot mark item retrieved if there are more than 1 in retrieval queue
* [MODRS-81](https://issues.folio.org/browse/MODRS-81) - Tenant deletion API does not purge database schema
* [MODRS-72](https://issues.folio.org/browse/MODRS-72) - Improve logging: Accession flow for Dematic doesn't work

## 1.2.0 Released
The primary focus of this release was to fix bugs

[Full Changelog](https://github.com/folio-org/mod-remote-storage/compare/v1.1.0...v1.2.0)

### Bug fixes
* [MODRS-68](https://issues.folio.org/browse/MODRS-68) - Accession queue records created as already accessioned

## 1.1.0 Released
The primary focus of this release was to implement business logic for CaiaSoft

[Full Changelog](https://github.com/folio-org/mod-remote-storage/compare/v1.0.3...v1.1.0)

### Technical tasks
* [MODRS-41](https://issues.folio.org/browse/MODRS-41) - Update postgresql to 42.2.18 or spring-boot-starter-parent to 2.3.5.RELEASE

### Stories
* [MODRS-67](https://issues.folio.org/browse/MODRS-67) - Improvements for querying mechanism for MappingsController and ExtendedMappingsController
* [MODRS-66](https://issues.folio.org/browse/MODRS-66) - Implement querying mechanism for MappingsController and ExtendedMappingsController
* [MODRS-58](https://issues.folio.org/browse/MODRS-58) - Update circulation interface dependency to support v11
* [MODRS-54](https://issues.folio.org/browse/MODRS-54) - Extend RS configuration to support accession workflow for CaiaSoft
* [MODRS-52](https://issues.folio.org/browse/MODRS-52) - Accession Flow (Cornell)
* [MODRS-51](https://issues.folio.org/browse/MODRS-51) - Accession Flow (Duke)
* [MODRS-50](https://issues.folio.org/browse/MODRS-50) - CaiaSoft Accession Table
* [MODRS-47](https://issues.folio.org/browse/MODRS-47) - Accession Queue Record updating for CaiaSoft Integration
* [MODRS-45](https://issues.folio.org/browse/MODRS-45) - Remove accession workflow preference from Configuration
* [MODRS-43](https://issues.folio.org/browse/MODRS-43) - Caiasoft: Returning an Item to remote storage (Cornell flow)
* [MODRS-39](https://issues.folio.org/browse/MODRS-39) - Caiasoft: Returning an Item to remote storage (Duke flow)
* [MODRS-38](https://issues.folio.org/browse/MODRS-38) - Caiasoft: Requesting a Remote Storage Item for Circulation
* [MODRS-37](https://issues.folio.org/browse/MODRS-37) - CaiaSoft: Accession Flow
* [MODRS-36](https://issues.folio.org/browse/MODRS-36) - Mark fields as required
* [MODRS-31](https://issues.folio.org/browse/MODRS-31) - Standard health check endpoint
* [MODRS-28](https://issues.folio.org/browse/MODRS-28) - Make sample data consistent
* [MODRS-24](https://issues.folio.org/browse/MODRS-24) - Extend RS configuration to support accession/return workflows for CaiaSoft
* [MODRS-23](https://issues.folio.org/browse/MODRS-23) - Add CaiaSoft Provider to providers list

### Bug fixes
* [MODRS-56](https://issues.folio.org/browse/MODRS-56) - Accession flow (Dematic) doesn't work
* [MODRS-32](https://issues.folio.org/browse/MODRS-32) - Update circulation interface version to 10.0

## 1.0.3 Released
The focus of this release was to fix issues related to the cache

[Full Changelog](https://github.com/folio-org/mod-remote-storage/compare/v1.0.2...v1.0.3)

### Bug Fixes
* [MODRS-42](https://issues.folio.org/browse/MODRS-42) - Unable to view configuration edits in third pane

## 1.0.2 Released
The focus of this release was to create health-check and update circulation interface version

[Full Changelog](https://github.com/folio-org/mod-remote-storage/compare/v1.0.1...v1.0.2)

### Bug Fixes
* [MODRS-31](https://issues.folio.org/browse/MODRS-31) - Standard health check endpoint

## 1.0.1 Released
The focus of this release was to fix deployment issue

[Full Changelog](https://github.com/folio-org/mod-remote-storage/compare/v1.0.0...v1.0.1)

### Bug Fixes
* [MODRS-30](https://issues.folio.org/browse/MODRS-30) - mod-remote-storage building failure due to unit test

## 1.0.0 Released
The primary focus of this release was to implement Remote Storage logic

### Stories
* [MODRS-18](https://issues.folio.org/browse/MODRS-18) Tech Debt: caching for remote storage services
* [MODRS-17](https://issues.folio.org/browse/MODRS-17) Configurations: add credential properties supporting
* [MODRS-15](https://issues.folio.org/browse/MODRS-15) API for list of providers
* [MODRS-11](https://issues.folio.org/browse/MODRS-11) Method for check-in item in primary Service Point
* [MODRS-10](https://issues.folio.org/browse/MODRS-10) Handling item return notification
* [MODRS-6](https://issues.folio.org/browse/MODRS-6) Requesting (Retrieval) a Remote Storage Item
* [MODRS-5](https://issues.folio.org/browse/MODRS-5) API for edge modules
* [MODRS-4](https://issues.folio.org/browse/MODRS-4) FOLIO-initiated sending items to the accession queue
* [MODRS-3](https://issues.folio.org/browse/MODRS-3) Remote storages configuration in FOLIO
* [MODRS-2](https://issues.folio.org/browse/MODRS-2) Work with remote storage locations
* [MODRS-1](https://issues.folio.org/browse/MODRS-1) Project Setup: mod-remote-storage

### Bug Fixes
* [MODRS-21](https://issues.folio.org/browse/MODRS-21) - mod-remote-storage tenant initialization fails with 500 error
* [MODRS-20](https://issues.folio.org/browse/MODRS-20) - Accessions API doesn't work with compound query
* [MODRS-16](https://issues.folio.org/browse/MODRS-16) - Remote storage backend requests do not work
