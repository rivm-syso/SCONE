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
# Masterscript for SCONE analysis
# 
################################################################################


source("./scripts/analysis/01_Load_packages.r")
source("./scripts/analysis/02_Load_data.r")
source("./scripts/analysis/03_Prepare_data.r")
source("./scripts/analysis/04_Examine_response_rate.r")
source("./scripts/analysis/05_Analyze_studypopulation.r")
source("./scripts/analysis/06_Analyze_frailty.r")
source("./scripts/analysis/07_Analyze_fatigue.r")
source("./scripts/analysis/08_Analyze_contacts.r")
source("./scripts/analysis/09_Analyze_contact_characteristics.r")
source("./scripts/analysis/10_Compare_contacts.r")


