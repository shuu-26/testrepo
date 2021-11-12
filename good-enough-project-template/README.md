# Self-controlled risk interval (SCRI) meta-analysis

## Project description
This project meta-analysis SCRI output from several local data access providers (DAPs), creating tabular and figure output.
The code runs on dummy data, which is provided in data/temp.
The code is not yet final and thus contains many work in progress parts.

## Requirements
- R; project was run using version 4.0.3
- packages: `tidyverse`, `meta`, `docstring`
- package versions: `tidyverse 1.3.1`, `meta 5.0.1`, `docstring 1.0.0`
Note that the code does not (yet) install these packages automatically, so users have to manually install them if they haven't done so before. This can be done using `install.packages`

## Project organization
- PG = project-generated
- HW = human-writable
- RO = read only
```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)
