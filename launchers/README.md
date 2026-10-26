# clinprog launchers

Helper scripts to start the **clinprog** Shiny app with a double-click, without typing
any R code. They are provided for convenience and are not part of the installed R package.

| System | Script |
|---|---|
| Linux | `run_clinprog.sh` |
| macOS | `run_clinprog.command` |
| Windows | `run_clinprog.bat` |

If you already work in R, you do not need them: run `clinprog::run_app()` instead.

## Requirements

1. **R** installed and available in the system `PATH`
   (open a terminal or the Windows Command Prompt and check that `Rscript --version` works).
2. The **clinprog** R package and all its dependencies installed.
3. Optional, for the *report* feature only:
   [Quarto](https://quarto.org/) (HTML and PDF reports) and a LaTeX distribution such as
   TinyTeX (PDF reports; install with `quarto install tinytex`).

## How to use

Download the script for your system from this folder (or clone the repository), then:

### Linux

The file needs execute permission. If it is missing, run this once:

```bash
chmod +x run_clinprog.sh
```

Then start it from a terminal:

```bash
./run_clinprog.sh
```

Some file managers also start it with a double-click (you may need to choose "Run" or
"Run in terminal").

### macOS

Double-click `run_clinprog.command`.

The first time, macOS may refuse to open a downloaded file. In that case, right-click
the file, choose **Open**, and confirm. If the file lost its execute permission, run
this once in the Terminal:

```bash
chmod +x run_clinprog.command
```

### Windows

Double-click `run_clinprog.bat`.

If Windows SmartScreen shows a warning, choose **More info** and then **Run anyway**.

## While the app is running

- A terminal window and your web browser both stay open. **Do not close either of
  them** while you use the app. Closing the terminal stops the app.
- To stop the app, close the browser tab and then close the terminal window
  (or press `Ctrl+C` in it).
- Result files are written to the folder you choose in the app. Check that you
  have permission to write there.

## Troubleshooting

| Problem | What to try |
|---|---|
| `Rscript` is not found | Install R and add it to the system `PATH`, then open a new terminal. |
| "there is no package called 'clinprog'" (or another package) | Install clinprog and its dependencies in the R installation the script uses. |
| The browser does not open | Open the address printed in the terminal (for example `http://127.0.0.1:XXXX`) manually. |
| `Permission denied` (Linux/macOS) | Run `chmod +x` on the script, as shown above. |
| PDF reports fail | Install Quarto and TinyTeX (see Requirements), or use the HTML report format. |

The launchers offer the same app as `clinprog::run_app()`. The app has a more limited set
of features than the R functions.

## Reporting problems

Please open an issue at <https://github.com/galantelab/clinprog/issues> and include your
operating system, your R version (`R.version.string`), and the text shown in the terminal.
