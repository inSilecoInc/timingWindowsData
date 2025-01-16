
<!-- README.md is generated from README.Rmd. Please edit that file -->

# National Timing Windows Dataset <a href=''><img src='man/figures/logo.png' align="right" height="175" /></a>

<!-- badges: start -->

[![License: GPL (&gt;=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
[![LifeCycle](https://img.shields.io/badge/lifecycle-experimental-orange)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Dependencies](https://img.shields.io/badge/dependencies-2/70-green?style=flat)](#)
![](https://img.shields.io/badge/status-preliminary-blue.svg)
<!-- badges: end -->

This repository contains the *research compendium* to harvest and
integrate the data necessary to create the National Timing Windows
Dataset for the project *Timing Windows”*. It contains all the code
required to import, format, and integrate the data needed for this
project, as well as the code used to perform the analyses, figures, and
the project report.

### How to cite

Please cite this research compendium as follows:

> **{{ PLEASE ADD A CITATION }}**

## Structure and Content

This research compendium is designed to facilitate reproducible research
by organizing data, scripts, and outputs within a structured framework.
By emulating the structure of an R package, the compendium combines the
rigour of package development with the flexibility required for complex
analytical workflows. This structure ensures transparency,
reproducibility, and ease of navigation for researchers and
collaborators. The compendium not only supports the organization and
documentation of workflows but also allows the seamless integration of R
tools and functions. Adopting an R package-like structure provides
several advantages:

-   **Reproducibility**: The standardized structure ensures that all
    components (data, code, outputs) are easily accessible and linked,
    reducing the likelihood of errors in replication.
-   **Portability**: The compendium can be shared and installed like an
    R package, enabling collaborators to reproduce the work on their
    systems.
-   **Documentation**: Built-in support for documentation (e.g., `man/`,
    `README.md`) enhances understanding and usability for current and
    future users.

While not including the data directly, the *research compendium*
contains all the resources making it possible to access and transform
the raw data and prepare the threat layers for this project. It also
contains the code creating figures, tables and this report. Only
sensitive data for which confidentiality agreements have been signed
remain inaccessible; still, these are stored on Google Cloud Storage in
a secure bucket that can be accessed programmatically with an access
key. This ensures that the whole project remains fully reproducible even
if access to some data is limited.

The research compendium is organized into the following components:

    myResearchCompendium/
    │
    ├── _targets/
    │
    ├── data/
    │
    ├── docs/
    │
    ├── figures/
    │
    ├── man/
    │
    ├── pubs/
    │
    ├── R/
    │
    ├── workspace/
    │   ├── bibliographies/
    │   ├── config/
    │   ├── credentials/
    │   ├── data/
    │   │   ├── harvested/
    │   │   └── analyzed/
    │   ├── pipelines/
    │   │   ├── harvesting/
    │   │   └── analytical/
    │   └── script/
    │
    ├── _targets.R
    ├── DESCRIPTION
    ├── LICENSE.md
    ├── NAMESPACE
    ├── README.md
    └── README.Rmd

Below is a description of each component of the research compendium.

### Root-Level Files

-   **`_targets.R`**: The central configuration file for the
    [`targets`](https://CRAN.R-project.org/package=targets) R package,
    which manages and tracks the execution of analytical workflows. This
    file defines the targets (steps) in the analysis and their
    dependencies.
-   **`DESCRIPTION`**: Provides metadata about the compendium, including
    its title, version, author information, and dependencies. This file
    mirrors the `DESCRIPTION` file in R packages, enabling compatibility
    with R’s package ecosystem.
-   **`LICENSE.md`**: Contains the licensing terms under which the
    compendium is distributed, ensuring clarity regarding usage and
    redistribution rights.
-   **`NAMESPACE`**: Specifies the exported functions and imports from
    other packages, similar to an R package, to manage the scope and
    dependencies of functions within the compendium.
-   **`README.md`** and **`README.Rmd`**: Provide an overview of the
    project, its goals, and instructions for setup and use. The R
    Markdown file (`README.Rmd`) can be rendered to create the Markdown
    file (`README.md`).

### Directories

#### **`_targets/`**

This directory contains internal files used by the `targets` package to
manage workflow execution. It tracks dependencies, outputs, and
progress, ensuring reproducibility and enabling efficient re-execution
of only the steps affected by changes. This folder is present once the
`_targets.R` file has been run once.

#### **`data/`**

This directory stores raw and cleaned data files that are essential to
the analyses but not directly produced by the workflows. This allows the
compendium to maintain a clear separation between input data and
processed outputs.

#### **`docs/`**

Documentation files for the project, such as user guides, vignettes, and
any additional explanatory materials that provide context for the
workflows and outputs.

#### **`figures/`**

A repository for plots, charts, and visualizations generated by the
analytical workflows. This directory helps centralize all visual outputs
for reporting and publication.

#### **`man/`**

Documentation for functions included in the compendium. This directory
mirrors the `man/` folder in R packages and contains `.Rd` files that
describe each function’s purpose, usage, and arguments.

#### **`pubs/`**

A location for storing draft manuscripts, reports, and other
publications derived from the project. This ensures that research
outputs are connected to their analytical source.

#### **`R/`**

Contains R scripts defining functions and utilities used across the
workflows. This is the primary location for reusable, well-documented R
functions that are central to the analyses.

#### **`workspace/`**

A comprehensive directory for project-specific resources and
configurations. It is further divided into:

-   **`bibliographies/`**: Bibliographic files, such as `.bib` files,
    used for citations in reports and publications.
-   **`config/`**: Configuration files (e.g., YAML or JSON) that specify
    pipeline parameters and global settings for the analyses.
-   **`credentials/`**: Secure storage for authentication keys and other
    sensitive information required for accessing data sources.
-   **`data/`**: Organized into two subdirectories:
    -   **`harvested/`**: Raw data files downloaded or collected through
        the harvesting pipelines.
    -   **`analyzed/`**: Processed data files generated by the
        analytical pipelines.
-   **`pipelines/`**: Divided into:
    -   **`harvesting/`**: YAML configurations and scripts for
        harvesting data from external sources.
    -   **`analytical/`**: YAML configurations and scripts for
        performing analyses on harvested data.
-   **`script/`**: Scripts and functions used by the targets master
    workflow.

## Navigating and Using the Compendium

1.  **Setting Up the Workspace**:
    -   The `README.md` provides instructions for setting up the
        compendium, including installing dependencies, configuring
        paths, and loading required libraries.
2.  **Credentials**:
    -   This project requires credentials to access data from two
        different sources:
        -   Secure google cloud storage managed by inSileco
            (`pof-stac-insileco.json`)
    -   These credentials have to be stored in `workspace/credentials/`
3.  **Running the Pipelines**:
    -   Running the master pipeline contained in `_targets.R` is
        achieved by executing the `targets::tar_make()` command in `R`
        from the root of the research compendium. Depending on your
        computer and internet connection, the first run will likely take
        a full day to complete.
4.  **Exploring Outputs**:
    -   Processed data is stored in `workspace/data/analyzed/`.
    -   Visualizations and figures are available in `figures/`, while
        reports and publications can be found in `pubs/`.
