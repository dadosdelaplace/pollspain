## Test environments
* local macOS, R 4.2.3 (aarch64-apple-darwin20, 64-bit, macOS 14.6.1)
* GitHub Actions: macOS, Ubuntu (release, devel), Windows

## R CMD check results
There were no ERRORs or WARNINGs.
One NOTE due to a test skipped on Linux.

## Comments
One test is conditionally skipped on Linux using `skip_on_os("linux")`
due to a known transactional issue in `duckdb` under Linux-only environments.
This does not affect functionality or user experience. All other tests pass.
