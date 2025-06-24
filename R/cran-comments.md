## Data
* To comply with CRAN package size restrictions, the data used by the
package has been moved to a companion data package named
`pollspaindata`, which is hosted on GitHub. This package has been
included as a dependency to ensure that all required data is availabl
locally once installed, so the user does not need internet access to
use the main package functionality. The `pollspaindata` package is
installed via `Remotes:` in the `DESCRIPTION` file using GitHub,
ensuring reproducibility and offline access to the data.

## Test environments
* local macOS, R 4.2.3 (aarch64-apple-darwin20, 64-bit, macOS 14.6.1)
* GitHub Actions: macOS, Ubuntu (release, devel), Windows

## R CMD check results
There were no ERRORs or WARNINGs

## Comments
One test is conditionally skipped on Linux using `skip_on_os("linux")`
due to a known transactional issue in `duckdb` under Linux-only
environments (only in release). This does not affect functionality or
user experience. All other tests pass.
