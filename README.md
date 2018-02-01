# Fragile Families r-package

This package allows one to retrieve metadata for the Fragile Families challenge via the PHP Web API.  Users can
select, filter, and search metadata for relevant variables.


Some notes:

These files are currently not formatted for proper package documentation etc. due to some recent revisions. Documentation for older files is still present in the "man" directory.

In the validNames list in constants.R, the "value1-68" and "label1-68" variables still need to be added in full.

call_api in helpers.R currently has a temporary workaround to reading JSON objects due to some formatting errors at the PHP endpoint. This involves writing and reading JSON objects from "json_temp.txt"

filterMetadata in endpoints.R has a potentially temporary way of handling many parameters using ...





