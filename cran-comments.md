## Test environments
- Local Linux: Ubuntu 20.04.6 LTS, R 4.5.2 (x86_64)
- Local Windows: Windows 11 Home 25H2 64 bits
- win-builder R-release (x86_64-w64-mingw32): R 4.6.1 (2026-06-24 ucrt)
- win-builder R-devel (x86_64-w64-mingw32): R Under development (unstable) (2026-09-29 r90598 ucrt)

## R CMD check results
- R CMD check --as-cran was run via devtools::check(cran = TRUE) on Linux
- 0 errors
- 0 warnings
- 0 notes

## win-builder results
- R-release (x86_64-w64-mingw32): 0 errors, 0 warnings, 1 note
- R-devel (x86_64-w64-mingw32): 0 errors, 0 warnings, 1 note

### Note explanation
- **CRAN incoming feasibility: New submission**: This is the first submission of the package to CRAN. The note is expected and will not appear in future submissions.

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Additional notes
- All long-running examples (e.g., the full clinprog pipeline) are wrapped in \dontrun{} to comply with CRAN's example runtime limits. Users can still access these examples via the package documentation.

## Comments
- This is the first submission of the package.
- The package implements the clinprog pipeline for transcriptomic survival analysis and molecular signature discovery.
- All exported functions are documented.
- The package includes a vignette tutorial and example toy datasets.
- A Shiny app is also provided via `run_app()`.
- No external data is required to use the package.
- All tests pass on Linux, Windows (release), and Windows (devel).
