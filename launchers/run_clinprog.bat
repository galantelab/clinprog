@echo off
setlocal enabledelayedexpansion

:: ============================================================
:: clinprog Interactive Suite Launcher for Windows
:: ============================================================

echo ======================================
echo    clinprog Interactive Suite
echo ======================================
echo.

:: Step 1: Check R
echo [1/4] Checking R installation...
where R >nul 2>nul
if %errorlevel% neq 0 (
    echo.
    echo [ERROR] R is not installed or not in PATH.
    echo Please install R from: https://cran.r-project.org/bin/windows/base/
    echo.
    pause
    exit /b 1
)
echo        R found successfully.
echo.

:: Step 2: Check Rscript
echo [2/4] Checking Rscript installation...
where Rscript >nul 2>nul
if %errorlevel% neq 0 (
    echo.
    echo [ERROR] Rscript not found.
    echo Please reinstall R and check "Add R to the system PATH"
    echo.
    pause
    exit /b 1
)
echo        Rscript found successfully.
echo.

:: Step 3: Check if clinprog package is installed and loadable
echo [3/4] Checking clinprog package...

Rscript -e "library(clinprog, quietly = TRUE)" >nul 2>nul
if %errorlevel% neq 0 (
    echo        clinprog package not found or not loadable.
    echo        Attempting to install...
    Rscript -e "if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes', repos='https://cloud.r-project.org/')"
    Rscript -e "remotes::install_github('galantelab/clinprog')"
    if %errorlevel% neq 0 (
        echo.
        echo [ERROR] Failed to install clinprog package.
        echo Please try manually in R:
        echo   remotes::install_github("galantelab/clinprog")
        echo.
        pause
        exit /b 1
    )
    echo        clinprog package installed successfully.
) else (
    echo        clinprog package found and loaded successfully.
)
echo.

:: Step 4: Launch the app
echo [4/4] Starting clinprog Interactive Suite...
echo       The application will open in your browser.
echo       Press Ctrl+C to stop.
echo.

Rscript -e "options(shiny.launch.browser = TRUE); clinprog::run_app()"

echo.
echo [ERROR] Application failed to start or was stopped.
pause
