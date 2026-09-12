# genetic_diversity_analysis

R scripts for the population-genetic analysis of *Nymphaea* (water lily) germplasm
collected across India, based on SSR (microsatellite) marker data.

The workflow covers the four things a diversity study usually needs: summary
statistics, population differentiation (AMOVA / F<sub>ST</sub>), clustering
(neighbour-joining and PCoA), and distribution maps of the sampled sites.

## Contents

| Script | What it does |
| --- | --- |
| `basic_stats.R` | Observed/expected heterozygosity (`Ho`, `Hs`), per-population summaries via `poppr()` and `basic.stats()`, and `find.clusters()` for a first look at structure. |
| `amova.R` | AMOVA over 3- and 4-population groupings, with a hierarchical `~Pop/Subpop` model, significance by 999 permutations, and pairwise F<sub>ST</sub> (Weir–Cockerham and Nei). |
| `nj.R` | Neighbour-joining tree with 1000 bootstrap replicates, plotted unrooted with coloured tips per cluster. Wrapped as `nj_tree(file, working_dir)`. |
| `pcoa.R` | Principal coordinates analysis (`dudi.pco`) plotted with `ggplot2`, points coloured and labelled by individual. |
| `read_genelax_file_create_genelax_class.R` | Reads a GenAlEx-formatted Excel sheet with `readGenalex` and writes it out as a Genepop file. |
| `india_map.R` | All-India map of sampling locations built from GADM shapefiles with `sf` + `ggplot2`. |
| `map_assam_meghalaya.R` | Regional inset for Assam and Meghalaya, at state and district level. |
| `final_map.R` | The composite publication figure, with `scatterpie` pies overlaid on the map. |

`map_population_3.xlsx` holds the sampling coordinates used by the map scripts.

## Input format

Genotype data is read with `poppr::read.genalex()`, so it must be a **GenAlEx-formatted
CSV**: the first three header rows give the number of loci, number of samples and
population sizes, followed by one row per individual with two columns per locus.

Map scripts additionally need:

- GADM shapefiles for India — `India_States_ADM1_GADM-shp` and `India_Districts_ADM2_GADM-shp`
- an Excel workbook of coordinates with one worksheet per species/region, columns `long`, `lat`

## Requirements

R ≥ 4.0 and the following packages:

```r
install.packages(c(
  "poppr", "hierfstat", "pegas", "adegenet", "ape",
  "ggplot2", "sf", "readxl", "scatterpie", "dplyr", "remotes"
))
remotes::install_github("douglasgscofield/readGenalex")
```

The mapping scripts were written against `rgdal`/`maptools`, both of which were
retired from CRAN in 2023. `sf` is the current replacement and is what
`india_map.R` and `map_assam_meghalaya.R` already use.

## Running

Each script is currently standalone and starts by pointing R at a local working
directory. Set that path to your own copy of the data before sourcing:

```r
setwd("path/to/your/data")
source("basic_stats.R")
```

## Note on paths

Several scripts contain absolute paths from the machine they were written on
(`C:/Users/RIZWAN/...`, `/Users/Seema/Documents/...`). Change these to your own
locations — or better, run from the project root and use relative paths — before
running anything.

## License

MIT — see [LICENSE](LICENSE).
