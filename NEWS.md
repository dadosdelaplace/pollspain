<!-- 

# pollspain 0.5.0

- dataviz

# pollspain 0.4.0

- reparto de escaños

# pollspain 0.3.0

- surveys

# pollspain 0.2.1

- vignettes
- pending: Fixed documentation of get_elections_data(), aggregate_election_data(), summary_election_data()
- menu 2019 en los import

-->

# pollspain 0.2.2

- Lazy duckDB system of files was implemented: similar speed as before but between 6 and 10 times lower RAM consumption.

# pollspain 0.2.1

- Fixed `aggregate_election_data()` function (now ccaa-prov-mun names are included as well as a properly `id_INE_...`).
- `aggregate_election_data()` function now includes a list of id_candidacies at province level when we aggregate 
- Fixed `summary_election_data()`
- Included a CERA flag in `summary_election_data()` to allow remove CERA ballots.
- Included first unit tests for `utils.R`
- Included global dictionary parties.


# pollspain 0.2.0

- Created pollspaindata repository.
- Fixed 2019 elections (user has to choose between two dates).
- Fixed errors elections files (MIR files are corrupt).
- Allowed the user to input only the year of the elections in `import_elections_data.R` and `get_elections_data.R`.
- Allowed the user to provide both year and dates for different elections.
- Checked examples and documentation.
- Fixed `by_parties = TRUE` by default.
- Fix discrepancies in files. Some poll stations does not match the individual ballots with summaries provided by Ministry of the Interior. The discrepancies were resolved by using the individual votes by party. 

# pollspain 0.1.0

- Fix errors in the accompanying `pollspain-data` repository.
- Recode the INE codes of municipalities and provinces according to their latest updated version.
- Added `recod_mun()`.
- Create pkgdown website.
- Fix errors in examples and documentation.
- Allow the user to input both a year and a date (or a vector of both) at the same time in `import_elections_data.R` and `get_elections_data.R` functions.
- Fix `short_version = TRUE` by default.


# pollspain 0.0.9

- Initial release.


