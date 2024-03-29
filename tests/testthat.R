# Part of the rstanarm package for estimating model parameters
# Copyright (C) 2015 Trustees of Columbia University
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

library(testthat)
suppressPackageStartupMessages(library(rstanarm))
Sys.unsetenv("R_TESTS")
o <- utils::capture.output(example(example_model, echo = FALSE))
if (.Platform$OS.type != "windows") { # || .Platform$r_arch != "i386"
  test_check("rstanarm", invert = FALSE,
             filter = if (Sys.getenv("NOT_CRAN") != "true") "stan_functions")
}

