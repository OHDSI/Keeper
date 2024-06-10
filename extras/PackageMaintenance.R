# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of Keeper
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Format and check code --------------------------------------------------------
styler::style_pkg()
OhdsiRTools::checkUsagePackage("Keeper")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Create manual, vignetes, and website -----------------------------------------
unlink("extras/Keeper.pdf")
system("R CMD Rd2pdf ./ --output=extras/Keeper.pdf")

rmarkdown::render("vignettes/UsingKeeperWithLlms.Rmd",
                  output_file = "../inst/doc/UsingKeeperWithLlms.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("vignettes/SettingKeeperParamethers.Rmd",
                  output_file = "../inst/doc/SettingKeeperParamethers.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

pkgdown::build_site()
OhdsiRTools::fixHadesLogo()
