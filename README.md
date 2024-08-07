
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Land Suitability Evaluation Module (LaSEM)

<!-- badges: start -->
<!-- badges: end -->


Land Suitability Evaluation Module (LaSEM) is a modular R-based Shiny app that serves as a decision-support system for conducting agricultural land suitability analysis. This early version focuses on suitability analysis for biophysical factors, guiding users in collecting and preparing data to visualize suitability maps of crops on an interactive map. Built upon the theory of land evaluation and suitability assessment (FAO, 1976; FAO, 2007), LaSEM's main feature is its spatially-explicit functionality and streamlined interface, which automatically evaluates land suitability based on biophysical factors using the FAO framework.

LaSEM aims to provide a user-friendly interface that broadens the potential user base for land suitability analysis. While other packages for evaluating land suitability may exist, LaSEM is one of the first known open-source apps to offer a streamlined and accessible interface for this purpose. However, some familiarity with GIS is still needed for preparing inputs. Please note that the package is currently under development and only available from the GitHub repository.

## Installation

To use the LaSEM package, you need to have R and RStudio installed on
your computer.

### Installing R

1.  Go to the R project website: <https://www.r-project.org/>
2.  Click on the “Download R” link.
3.  Choose your operating system (Windows, Mac, or Linux).
4.  Click on the appropriate download link for your system.
5.  Follow the installation instructions provided.

### Installing RStudio

1.  Go to the RStudio website:
    <https://www.rstudio.com/products/rstudio/download/>
2.  Click on the “Download RStudio” button.
3.  Choose the appropriate installer for your operating system.
4.  Follow the installation instructions provided.

### Installing LaSEM Package

Once you have R and RStudio installed, follow these steps to install the
LaSEM package:

1.  Open RStudio.
2.  In the R console, run the following commands:

``` r
   library(remotes)
   install_github("https://github.com/icraf-indonesia/LaSEM")
```

This will install the LaSEM package and its dependencies from the GitHub
repository.

After the installation is complete, load the LaSEM package:

``` r
library(LaSEM)
```

To launch the LaSEM Shiny app, run the following command:

``` r
LaSEM_app()
```

The user interface for the LaSEM app will appear in your default web
browser.
![Picture1](https://github.com/icraf-indonesia/LaSEM/assets/14798903/e41bb0bd-7a6d-48de-ac89-bb35e2a6ff68)



## Usage

1.  Upload the required data:
    - Soil and Climate Data: Click on the “Upload Lookup Table of Soil
      and Climate Data (CSV)” button and select the appropriate CSV
      file. Fill in the site location name in the provided text input
      field.

    - Crop Suitability Parameters: Click on the “Upload Crop Suitability
      Parameters (CSV)” button and select the appropriate CSV file. Fill
      in the crop name in the provided text input field.

    - Intervention Lookup: Click on the “Upload Intervention Lookup
      Table (CSV)” button and select the appropriate CSV file.
2.  Click on the “Submit” button for each data category to confirm the
    uploaded data.
3.  Navigate to the “Suitability Analysis” tab to view the results.
4.  Explore the suitability map, suitability by factors, and suitability
    polygon data.
5.  To download the analysis results, click on the “Download Results”
    button.To download the analysis results, click on the “Download
    Results” button.

## Contributing

We welcome contributions to the LaSEM package. If you encounter any
issues or have suggestions for improvements, please open an issue on the
GitHub repository.

## References

- FAO. (1976). *A framework for land evaluation (Vol. 32).* Food and
  Agriculture Organization of the United Nations.

- FAO. (2007). Land evaluation: towards a revised framework. *Land and
  Water Discussion Paper 6*. Food and Agriculture Organization of the
  United Nations.

- Ritung, S., Nugroho, K., Mulyani, A., & Suryani, E. (2011). *Petunjuk
  Teknis Evaluasi Lahan Untuk Komoditas Pertanian (Edisi Revisi)*. Balai
  Besar Penelitian dan Pengembangan Sumberdaya Lahan Pertanian, Badan
  Penelitian dan Pengembangan Pertanian, Bogor.

- Wahyunto, *et al*. (2016). *Petunjuk Teknis Pedoman Penilaian
  Kesesuaian Lahan untuk Komoditas Pertanian Strategis Tingkat Semi
  Detail Skala 1:50.000.* Balai Besar Penelitian dan Pengembangan
  Sumberdaya Lahan Pertanian, Badan Penelitian dan Pengembangan
  Pertanian, Bogor.
