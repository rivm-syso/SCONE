################################################################################
#
# Copyright 2022 Rijksinstituut voor Volksgezondheid en Milieu (RIVM).
#
# This program is free software: you can redistribute it and/or modify it under 
# the terms of the GNU Affero General Public License as published by the Free 
# Software Foundation, either version 3 of the License, or any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR 
# A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with 
# this program.  If not, see <https://www.gnu.org/licenses/>.”
#
################################################################################
#
# Masterscript for SCONE data pipeline
# NOTE: raw data is not publicly accessible due to privacy issues
# 
################################################################################


source("./scripts/data/01_Load_packages.r")
source("./scripts/data/02_Load_raw_data.r")
source("./scripts/data/03_Reorganize_data.r")
source("./scripts/data/04_Check_and_clean_data.r")
source("./scripts/data/05_Save_data.r")

