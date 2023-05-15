# SCONE

This repository contains the code developed and used for the analysis of the SCONE study in the Netherlands, as described in ["Contact patterns of frail and non-frail elderly persons in the Netherlands during the COVID-19 pandemic"](https://www.medrxiv.org/content/10.1101/2023.05.09.23289550v1), by [Jantien A Backer](mailto:jantien.backer@rivm.nl), Jan van de Kassteele, Fatima El Fakiri, Niel Hens, and Jacco Wallinga, consisting of:

* `scripts`: code for two scripts (data cleaning and analysis) and helper functions

* `data`: additional data files that are needed for analysis

* `article`: Rmd and helper files of submitted version(s) of the manuscript and supplementary material

* `results`: intermediate results produced by analysis script, used in Rmd files

* `figures`: figures produced by analysis script, used in Rmd files

All code has been written in the programming language [R](https://www.r-project.org/about.html); see [Requirements](#requirements) for detailed specification.

## Data

The cleaned data is publicly available on [Zenodo](https://dx.doi.org/10.5281/zenodo.7649376) in the [socialcontactdata.org](https://www.socialcontactdata.org) format. You can either:

* Download the data files from the repository and place them in the `data` folder, or

* Run the analysis script which will download the data files from the repository without storing them

The raw data is not publicly available due to GDPR constraints.


## Usage

`scripts` contains the scripts for data cleaning and analysis, and some helper functions. Each script consists of several numbered R files that reproduce the results when executed in order. Note that the data cleaning script can't be run for lack of raw data.


## <a name = "requirements"></a> Requirements

The code has been developed and runs under the RIVM R-Studio servers.

```
R version 4.2.3 (2023-03-15) Shortstop Beagle
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: Red Hat Cloud Infrastructure
```

Next to the R base packages, the following packages need to be installed

```
readxl
tidyverse
lubridate
mgcv
mgcv.helper
emmeans
ggsignif
viridis
cowplot
```

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.

## Funding

The SCONE study has been financed by the Netherlands Organisation for Health Research and Development (ZonMw; grant number 10150511910020).

## Feedback

If you encounter a clear bug, please file an issue with a minimal reproducible example on GitHub.

