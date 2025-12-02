# WormTagDB

A comprehensive database and interactive RShiny application for exploring endogenously tagged proteins in *C. elegans*.

WormTagDB provides a searchable, user-friendly interface to access information about protein tagging experiments, including exsiting tagged alleles, guide sequences, and primer designs for the entire of *C. elegans* genome.

Visit the live application at: **https://wormtagdb.rc.duke.edu**

## Repository Contents

```
WormTagDB/
├── app.R              # Main Shiny application
├── data/              # Database files
├── www/               # Static assets (CSS, images)
└── README.md
```

## Related Resources

The CRISPR guide and primer designs were generated using a computational pipeline:
- **Pipeline Repository**: [CRISPR Guide and Primer Design Pipeline for C. elegans](https://github.com/jakeleyhr/CRISPR-Guide-and-Primer-Design-Pipeline-for-C.-elegans)
- **Manuscript**: Leyhr, J. et al. (2025) WormTagDB: Systematic Survey of Endogenously Tagged Proteins in C. elegans and a Roadmap Towards the Tagged Proteome. *Submitted*.

## Installation & Usage

To run the application locally:

```bash
git clone https://github.com/jakeleyhr/WormTagDB.git
```
In R:
```r
# Set WormTagDB folder as WD

# Install dependencies
install.packages(c("shiny", "shinydashboard", "DT", "dplyr", "readr", "stringr"))
BiocManager::install("igvShiny")

# Run the app
shiny::runApp()
```
