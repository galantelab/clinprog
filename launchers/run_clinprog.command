#!/bin/bash

# ============================================================
# clinprog Interactive Suite Launcher for macOS
# ============================================================
# This script installs (if needed) and launches the clinprog
# Shiny application for regression and survival analysis.
# ============================================================

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "======================================"
echo "   clinprog Interactive Suite"
echo "======================================"
echo ""

# Function to check if R is installed
check_r() {
    if ! command -v R &> /dev/null; then
        echo "[ERROR] R is not installed or not in PATH."
        echo "Please install R first:"
        echo "  - Download from: https://cran.r-project.org/bin/macosx/"
        echo "  - Or using Homebrew: brew install r"
        read -p "Press Enter to exit..."
        exit 1
    fi
}

# Function to check if Rscript is installed
check_rscript() {
    if ! command -v Rscript &> /dev/null; then
        echo "[ERROR] Rscript not found."
        echo "Please install R (which includes Rscript):"
        echo "  - Download from: https://cran.r-project.org/bin/macosx/"
        echo "  - Or using Homebrew: brew install r"
        read -p "Press Enter to exit..."
        exit 1
    fi
}

# Function to check and install required R packages
check_packages() {
    echo "Checking R packages..."

    # Check if devtools is installed
    if ! Rscript -e "if (!requireNamespace('devtools', quietly = TRUE)) quit(status=1)" &> /dev/null; then
        echo "Installing devtools..."
        Rscript -e "install.packages('devtools', repos='https://cloud.r-project.org/')"
    fi

    # Check if clinprog package is installed - using your GitHub repository
    if ! Rscript -e "if (!requireNamespace('clinprog', quietly = TRUE)) quit(status=1)" &> /dev/null; then
        echo "Installing clinprog package from GitHub (galantelab/clinprog)..."
        Rscript -e "devtools::install_github('galantelab/clinprog')"
    else
        echo "clinprog package is already installed."
    fi
}

# Function to run the Shiny app
run_app() {
    echo ""
    echo "Starting clinprog Interactive Suite..."
    echo "The application will open in your default browser."
    echo "Press Ctrl+C to stop the application."
    echo ""

    # Run the Shiny app and open browser automatically
    Rscript -e "options(shiny.launch.browser = TRUE); clinprog::run_app()"
}

# Main execution
main() {
    check_r
    check_rscript
    check_packages
    run_app
}

# Run the main function
main
