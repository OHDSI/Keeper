# Copyright 2026 Observational Health Data Sciences and Informatics
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

withCache <- function(expression, cacheFolder, fileName) {
  if (!is.null(cacheFolder)) {
    ext <- tolower(tools::file_ext(fileName))
    
    if (file.exists(file.path(cacheFolder, fileName))) {
      result <- switch(ext,
                       "rds" = readRDS(file.path(cacheFolder, fileName)),
                       "csv" = readr::read_csv(file.path(cacheFolder, fileName), show_col_types = FALSE),
                       "txt" = readLines(file.path(cacheFolder, fileName)),
                       "json" = readLines(file.path(cacheFolder, fileName)))
      return(result)
    }
  }
  result <- expression
  
  if (!is.null(cacheFolder)) {
    switch(ext,
           "rds" = saveRDS(result, file = file.path(cacheFolder, fileName)),
           "csv" = readr::write_csv(result, file.path(cacheFolder, fileName)),
           "txt" = writeLines(as.character(result), con = file.path(cacheFolder, fileName)),
           "json" = writeLines(as.character(result), con = file.path(cacheFolder, fileName)))
  }
  return(result)
}
