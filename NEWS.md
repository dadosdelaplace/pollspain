<!-- 

# pollspain 0.4.0

- escaños

# pollspain 0.4.0

- reparto de escaños

# pollspain 0.3.0

- surveys

# pollspain 0.2.1

- vignettes

# pollspain 0.2.0

- Created pollspaindata repository.
- Fixed 2019 elections (user has to choose between two dates).
- Fixed errors elections files (MIR files are corrupt).
- Allowed the user to input only the year of the elections in `import_elections_data.R` and `get_elections_data.R`.
- Allowed the user to provide both year and dates for different elections.
- Checked examples and documentation.
- Fixed `by_parties = TRUE` by default.
- Fixed aggregate_election_data() function (now ccaa-prov-mun names are included as well as a properly id_INE_...).
- aggregate_election_data() function now includes a list of id_candidacies at province level when we aggregate 
- Fixed summary_election_data()
- Included a CERA flag in summary_election_data() to allow remove CERA ballots.

- pending: Fixed documentation of get_elections_data(), aggregate_election_data(), summary_election_data()
- pending: add more examples in summary()
- pending: Added `recod_parties()`
- pending: añadir nombre partidos en el aggregte

- Fix discrepancies in files. Some poll stations does not match the individual ballots with summaries provided by Ministry of the Interior. The discrepancies were resolved by using the individual votes by party. Discrepancies were found in the following poll stations 
  - `2015` a lot of them with a small (1-2 ballots) discrepancies.
  - `2000`: 02-50-001-01-001-U / 01-21-001-01-001-U / 02-22-001-01-001-U / 05-35-001-01-001-A / 09-17-001-01-001-U / 09-43-001-01-001-A / 16-20-001-01-001-U / 14-30-001-01-001-A
  - `1996`: 04-07-001-01-001-A / 09-17-001-01-001-U / 09-25-001-01-001-U / 09-43-001-01-001-A / 11-06-001-01-001-U / 12-27-001-01-001-U / 12-36-001-01-001-U / 16-20-001-01-001-U / 16-48-001-01-001-U / 14-30-001-01-001-A / 10-46-001-01-001-A / 10-12-001-01-001-A / 02-22-001-01-001-U / 05-35-001-01-001-A / 12-15-001-01-001-U / 15-31-001-01-001-U
  - `1993`: 05-35-001-01-0001-A / 05-38-001-01-0001-U / 08-19-001-01-0001-A / 07-47-001-01-0001-A / 07-42-001-01-0001-U / 07-49-002-01-0001-A / 15-31-001-01-0001-U / 10-03-001-01-0001-U / 02-44-001-01-0001-A / 08-16-001-01-0001-A / 08-45-001-01-0001-U / 07-34-001-01-0001-U / 10-12-001-01-0001-A / 07-47-999-09-0000-U / 07-37-999-09-0000-U / 17-26-999-09-0000-U
  - `1989`: 02-44-001-01-0001-A / 05-38-001-01-0001-A / 08-16-001-01-0001-A / 07-34-001-01-0001-A / 07-42-001-01-0001-A / 07-49-002-01-0001-A / 15-31-001-01-0001-A / 16-01-001-01-0001-A / 10-03-001-01-0001-U / 10-12-001-01-0001-A / 05-35-001-01-0001-A / 08-19-001-01-0001-A / 08-45-001-01-0001-A / 07-47-001-01-0001-A / 12-27-999-09-0000-U
  - `1986`: 01-18-001-01-0001-A / 05-35-001-01-0001-A / 05-38-001-01-0001-A / 12-32-001-01-0001-A / 17-26-001-01-0001-A / 10-03-001-01-0001-A / 18-51-001-01-0001-A
  - `1982`: 05-35-001-01-0001-A / 13-28-079-01-0001-L / 13-28-079-01-0001-K / 13-28-079-01-0001-G / 13-28-079-01-0001-M / 13-28-079-06-0001-C / 13-28-079-06-0001-D / 13-28-079-06-0001-G / 17-26-001-01-0001-A / 01-14-001-01-0001-A / 01-23-001-01-0001-A / 08-13-001-01-0001-A / 08-45-001-01-0001-A / 07-05-001-01-0001-A / 07-49-002-01-0001-A / 09-08-001-01-0001-A / 11-06-001-01-0001-A / 12-15-001-01-0001-A / 13-28-079-01-0001-E / 13-28-079-01-0001-C / 13-28-079-01-0001-J / 13-28-079-01-0001-I / 13-28-079-06-0001-F / 13-28-079-06-0001-H / 13-28-079-01-0001-A / 13-28-079-01-0001-D / 13-28-079-01-0001-F / 13-28-079-06-0001-A / 13-28-079-06-0001-B / 13-28-079-06-0001-I

-->

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


