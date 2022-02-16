# neocache (development version)

- Due to a backwards incompatible change to the data schema (type-safety is now better), users will need to delete existing caches on upgrade so that they can be repopulated using the new schema. You can accomplish this via `nc_delete_cache("cache-name")`.

# neocache 0.0.0.9003

- Added a `NEWS.md` file to track changes to the package.
