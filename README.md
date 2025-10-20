# 🌍 OpenUrbanAirandMeteorological  
### *Citizen Science and STEM Education with R: Teaching Innovation through Open Urban Climate Data*  

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
[![Project DOI](https://img.shields.io/badge/DOI-coming_soon-blue.svg)](https://doi.org/)
[![Made with R](https://img.shields.io/badge/Made%20with-R-blue.svg)](https://www.r-project.org/)
[![Quarto Document](https://img.shields.io/badge/Rendered_with-Quarto-4A90E2.svg)](https://quarto.org/)
[![GitHub Pages](https://img.shields.io/badge/Website-online-brightgreen.svg)](https://jcaceres-academico.github.io/OpenUrbanAirandMeteorological/)

---

## 🧠 Overview

This repository hosts the open, reproducible materials developed for the article submitted to *Applied Sciences (MDPI, 2025)*:

> **Cáceres-Tello, J.** & **Galán-Hernández, J.J.** (2025).  
> *Citizen Science and STEM Education with R: Teaching Innovation through Open Urban Climate Data.*  
> *Applied Sciences* (MDPI).  

The project demonstrates how **R, Quarto, and open environmental datasets** can be integrated to support **STEM education, citizen science, and methodological transparency**.

---

## 🧩 Reproducible Workflow

The workflow follows a complete educational and research pipeline:

1. **Open data acquisition** → Madrid air quality & meteorological datasets (2020–2024)  
2. **Preprocessing and harmonisation** → validation, Parquet conversion, and reproducibility scripts  
3. **Exploratory analysis** → pollutant variability and inter-station correlations  
4. **Forecasting models** → Prophet–LSTM hybrid approach  
5. **Educational visualisation** → Quarto Notebook for classroom integration  

Rendered notebook:  
👉 [**OpenUrbanAir_and_Meteorological_Workflow.html**](https://jcaceres-academico.github.io/OpenUrbanAirandMeteorological/OpenUrbanAir_and_Meteorological_Workflow.html)

---

## 🗂️ Repository Structure

```text
OpenUrbanAirandMeteorological/
├── docs/                      # Quarto notebook and rendered HTML
│   ├── OpenUrbanAir_and_Meteorological_Workflow.qmd
│   ├── OpenUrbanAir_and_Meteorological_Workflow.html
│   └── images/                # PNG figures used in the notebook
├── figures_tiff/              # High-resolution TIFF figures for publication
├── scripts/                   # R scripts for preprocessing and analysis
├── .gitignore
├── LICENSE
├── datos.Rproj
└── README.md
``` 

## ⚙️ Technologies Used

| Category        | Tools / Packages |
|-----------------|------------------|
| **Programming** | [R](https://www.r-project.org/) 4.4 +  · [Quarto](https://quarto.org/) |
| **Data Handling** | `tidyverse` · `arrow` · `lubridate` |
| **Visualisation** | `ggplot2` · `patchwork` · `leaflet` · `cowplot` |
| **Forecasting** | `prophet` · `keras` · `tensorflow` · `torch` |
| **Reproducibility** | `renv` · `knitr` · `rmarkdown` |

## 🧭 Educational Use

This repository supports STEM education by providing:

Hands-on examples for environmental informatics and sustainability courses

Open-source reproducible R-Quarto workflow

Didactic visualisations for citizen science and air quality literacy

## 📚 Citation

If you reuse or adapt this resource, please cite as:

Cáceres-Tello, J.; Galán-Hernández, J.J. (2025).
Citizen Science and STEM Education with R: Teaching Innovation through Open Urban Climate Data.
Applied Sciences (MDPI).
https://jcaceres-academico.github.io/OpenUrbanAirandMeteorological

## ⚖️ License

Code and notebooks: Creative Commons Attribution 4.0 (CC BY 4.0)

Data (if reused): CC0 1.0 Public Domain Dedication

## 📬 Contact

Jesús Cáceres Tello
Department of Computer Systems and Computing
Universidad Complutense de Madrid
📧 jcaceres.academico@gmail.com / jescacer@ucm.es

This repository supports open, transparent, and reproducible research in environmental data science and STEM education.
