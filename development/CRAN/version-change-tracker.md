# r3PG version change tracker

This file is a concise running summary of changes made during the revision of the package.  
More items will be added as development progresses.

## Current updates

- Adjusted variable descriptions in `src/i_decl_var.h` to match the descriptions used in `3PG_report_draft3.docx`.
- Noted that the description of `mR` and `mS` still needs to be corrected in `3PG_report_draft3.docx`.
- Cleaned up formatting in `src/i_decl_var.h` to match the aligned style used in the other Fortran include files.
- Adjusted `testthat` tests to match the new package structure.
- Adjusted `R/prepare_species.R` to comply with the revised input structure.
- Adjusted `R/prepare_thinning.R` to comply with the revised input structure.
- Aligned formatting in the Fortran interface/include files to improve readability and consistency.
- Moved temporary variable declarations from `src/md_3PG.f95` into `src/i_decl_var.h`, placing them in their logical sections (modifiers, stand, mortality, defoliation, Weibull) with descriptive comments. Standardized `kind` types to `kind(0.0d0)`.
- Completed Phase 1 synchronization of internal `i_parameters` with the new 88-row parameter naming used by `d_input$parameters`.
- Standardized naming in `tests/testthat/test-run_3PG.R` to use `result` consistently for model output objects.
- Replaced non-portable Fortran `kind=8` usage in the main model declarations/computations with portable `kind(0.0d0)` equivalents.
- Standardized long-term modifier declarations so `lt_fT`, `lt_fPhys`, and `hist_ptr` are dimensioned by `n_sp` in `i_decl_var.h`.
- Removed multiple unused declarations from `i_decl_var.h` and simplified `s_height_crown_allometry` by removing an unused dummy argument (`age`).
- Updated data documentation in `R/data.R`: corrected `i_parameters` row count (86), verified and updated `i_output` row count (220), and expanded dataset field descriptions for `d_mixture`, `d_regeneration`, and `d_defoliation`.
- Regenerated Rd files with `devtools::document()`.
- Added targeted tests for mortality model site-parameter behavior (`mort_model = 2` vs `mort_model = 3`), defoliation validation edge cases, and backward compatibility warnings for legacy site inputs.
- Started step-by-step `md_3PG.f95` refactoring by extracting the monthly long-term modifier update logic into a dedicated subroutine (`s_update_long_term_modifiers`).
- Continued step-by-step `md_3PG.f95` refactoring by modularizing duplicated DBH Weibull distribution calculations into `s_update_weibull_distribution`.
- Added scalar wrapper functions `f_exp_s` and `f_exp_foliage_s` to eliminate the `tmp_vec` intermediate for single-value modifier calls; removed `tmp_vec` from `i_decl_var.h`.
- Aligned and cleaned up formatting throughout `md_3PG.f95`: re-indented `mort_model` blocks, simplified redundant weight calculations, cleaned comment markers, trimmed excessive blank lines (~2600 → 2512 lines).
- Removed 5 unused variables from `i_decl_var.h` and `i_init_var.h`: `n_sp_max`, `mort_stress`, `mort_thinn` (array), `growing_season_length`, `bias_scale`.
- Migrated default-data references in tests/examples/documentation from old objects (`d_input`, `d_site`, `d_species`, `d_climate`, `d_thinning`, `d_parameters`, `d_sizeDist`) to `d_mixture$...` where applicable.

## Technical log

### `src/i_decl_var.h`

- Reviewed the variable descriptions against the terminology used in `3PG_report_draft3.docx` and the internal parameter reference table.
- Updated comment text for the long-term modifiers, litterfall parameters, mortality parameters, canopy process parameters, and defoliation-related variables so that the wording is consistent with the current package documentation.
- Standardized several comments for mortality model settings, including the labels for `mort_model = 1`, `mort_model = 2`, and `mort_model = 3` related variables.
- Corrected small wording and spelling issues in comments to make the header file easier to audit later.
- Standardized spacing and visual alignment of declarations and comments so that the file now follows the same general layout as the other include files.
- Kept this cleanup at the formatting level only, without changing the declared variables or their role in the model.
- Identified a remaining follow-up outside the code: the `mR` and `mS` descriptions should still be corrected in `3PG_report_draft3.docx` so the document stays aligned with the code.
- Absorbed temporary variable declarations previously in `md_3PG.f95` into their logical sections:
    - **Modifiers**: `f_sw_tmp`, `f_vpd_tmp`, `f_phys_tmp`, `vpd_mean` (long-term modifier scratch variables).
    - **Stand variables**: `height_wtav_LAI` (LAI-weighted average height for `height_rel_wt`).
    - **Mortality**: `thinIntercept_eff`, `N_max`, `dbh_safe`, `expo`, `logN`, `weight`, `weight_sum`, `loss_sum`, `scale` (mort_model 2); `pp`, `dbh_prev_safe`, `dbh_ratio`, `modifiers`, `delta_term`, `inner`, `betaN_eff`, `inv_exp` (mort_model 3).
    - **Defoliation**: `NPP_eff` (recovery), `tmp_vec`, `jj` (coppice events).
    - **Weibull**: `dlocation`, `DWeibullShape_gamma` (dbh distribution temporaries).
- Converted all moved variables from `c_double` / `kind=8` to the standard `kind(0.0d0)` used throughout the file.

### `src/md_3PG.f95`

- Removed the 20+ temporary variable declarations that were between the subroutine arguments and the `include 'i_decl_var.h'` line. These are now declared centrally in `i_decl_var.h`.
- The subroutine argument list and the `output` array declaration remain unchanged.
- Replaced non-portable `real(..., kind=8)` conversions used in long-term modifier averaging with `real(..., kind=kind(0.0d0))`.
- Removed now-obsolete allocation calls for `lt_fT` and `lt_fPhys` after converting them to fixed `dimension(n_sp)` declarations.
- Removed allocation for `hist_ptr` after converting it to fixed `dimension(n_sp)`.
- Removed unused dummy argument `age` from `s_height_crown_allometry` and updated all call sites accordingly.
- Confirmed cleanup of legacy bias-correction helper `p_min_max` in `md_3PG.f95`; this function is no longer used in the current main model path.
- Extracted the inline monthly long-term modifier update loop into `s_update_long_term_modifiers(...)` to improve structure and isolate logic.
- Kept `vpd_mean` as a scalar in initialization and moved its computation outside the species loop (it is stand-level, not species-specific).
- Verified no compile diagnostics for `src/md_3PG.f95` after this extraction.
- Replaced two duplicated inline DBH distribution blocks (initialization + monthly loop) with calls to `s_update_weibull_distribution(...)`.
- Added new internal subroutine `s_update_weibull_distribution(...)` encapsulating Weibull scale/shape/location updates and fallback location logic.
- Added scalar wrapper functions `f_exp_s(x, g0, gx, tg, ng)` and `f_exp_foliage_s(x, f1, f0, tg)` that call their array counterparts with `n_m = 1` and return a scalar result. Replaced 5 call sites that previously used `tmp_vec` as a throwaway single-element array.
- Removed `tmp_vec` declaration from `i_decl_var.h` after it became unused.
- Re-indented `mort_model = 3` block from column 0 to the standard 23-space indent level, matching `mort_model = 1` and `mort_model = 2`.
- Simplified weight calculation in both `mort_model = 2` and `mort_model = 3` blocks: both if/else branches were identical, so each was collapsed to a single unconditional assignment and all dead commented-out alternative code was removed.
- Fixed coppice height-update indentation to match the surrounding 12-space level.
- Replaced noisy `!!!!` comment block with a concise `! TODO:` note about dbh update after self-thinning.
- Corrected `s_update_long_term_modifiers` call indentation from 13 to 12 spaces.
- Added missing body indent under the `if (ii == 1) then` guard for `age_m`.
- Collapsed 3+ consecutive blank lines throughout the file to a maximum of 2, reducing total line count from ~2600 to 2512.

### `src/i_decl_var.h`

- Replaced remaining `real(kind=8)` declarations (`lt_fT`, `lt_fPhys`, `fT_hist`, `fPhys_hist`) with `kind(0.0d0)`-based declarations for portability.
- Changed `lt_fT`, `lt_fPhys`, and `hist_ptr` to explicit `dimension(n_sp)` arrays to match their use and avoid unnecessary dynamic allocation.
- Removed unused declarations flagged during Fortran diagnostics cleanup (`b_cor`, `b_n`, `n`, `NPP_def`, `biom_stem_pre`, `biom_foliage_pre`, `biom_root_pre`, `tmp`, `denom`, `denom_stems`, `def_test_var`).
- Removed 5 additional unused variables identified via `-Wunused-variable` audit: `n_sp_max` (unused parameter), `mort_stress` and `mort_thinn` (superseded by `stems_loss_stress`/`mort_thinn_total`), `growing_season_length` (never assigned/read), `bias_scale` (never assigned/read). Corresponding initializations removed from `i_init_var.h`.

### `src/i_read_input.h`

- Made conversion explicit for `lt_mod_mths` (`lt_mod_mths = int(siteInputs(9))`) to reduce implicit REAL→INTEGER conversion risk in the Fortran input mapping.

### `R/data.R` and generated `man/*.Rd`

- Corrected `i_parameters` documentation from "82 rows" to "86 rows" and fixed variable count to 4.
- Verified `i_output` and updated documentation from "150 rows" to "220 rows".
- Replaced generic dataset component descriptions with explicit field-level documentation for:
    - `d_mixture` (site/species/climate/thinning + parameter/sizeDist component role)
    - `d_regeneration` (same structure as `d_mixture`)
    - `d_defoliation` (including explicit `defoliation` table fields)
- Regenerated `man/i_parameters.Rd`, `man/i_output.Rd`, `man/d_mixture.Rd`, `man/d_regeneration.Rd`, and `man/d_defoliation.Rd` using `devtools::document()`.

### `tests/testthat`

- Adjusted the `testthat` suite to reflect the revised package structure and the updated input objects.
- Updated tests so they check the new input layout and revised interfaces rather than the older package organization.
- This was done to keep regression testing aligned with the current branch and to make it easier to spot output changes introduced by the new mortality, management, and defoliation work.
- Updated expected values for the regeneration mortality and biomass-management tests to match the current model outputs after recent mortality/management updates.
- Standardized test object naming by replacing mixed `out`/`result` usage with `result` throughout `test-run_3PG.R` (including the commented defoliation block) for consistency and readability.
- Added two targeted mortality tests in `test-run_3PG.R`:
    - `mort_model = 2` is unaffected by changes in `beta*` site parameters.
    - `mort_model = 3` is unaffected by changes in `st_*` site parameters.
- Added new `test-prepare_defoliation.R` covering defoliation validation edge cases:
    - valid coppice input path,
    - invalid epicormic constraints,
    - invalid recovery time (`def_recover_t < 2`).
- Added backward compatibility warning test in `test-prepare_site.R` to confirm legacy `altitude` is warned and converted to `elevation`.

### `R/prepare_site.R`

- Implemented backward-compatible handling for legacy site input column `altitude`:
    - emits a deprecation warning,
    - converts `altitude` to `elevation` when `elevation` is not provided.
- Updated `test-prepare_input.R` to use `d_mixture$site`, `d_mixture$species`, `d_mixture$climate`, and `d_mixture$thinning` instead of `d_input$...`.

### `inst/examples`, `README.md`, `vignettes_build/r3PG-ReferenceManual.Rmd`

- Updated default-data example calls to use `d_mixture$...` consistently:
    - `README.md` main `run_3PG()` example (`site/species/climate/thinning/parameters/size_dist`).
    - `inst/examples/prepare_input-help.R` (`prepare_input(...)` call).
    - `inst/examples/prepare_site-help.R` and `prepare_species-help.R` default-data calls.
    - `inst/examples/prepare_climate-help.R`, `prepare_parameters-help.R`, `prepare_sizeDist-help.R`, and `prepare_thinning-help.R` default-data calls.
    - `vignettes_build/r3PG-ReferenceManual.Rmd` text and model-run code chunk.
- Kept standalone toy `data.frame` examples (e.g., local `d_site`, `d_species`) unchanged where they are intentionally user-defined example objects rather than package internal data.

### `data-raw/data.default.xlsx`, `data-raw/0_create_input_data.R`, `R/sysdata.rda`

- Completed synchronization of internal `i_parameters` with the current parameter naming scheme used by `d_input$parameters`.
- The internal parameter reference now reflects the expanded/new naming set (e.g., `beta0`, `betaB`, `nHB`, `nHC`) and resolves the previous old-name mismatch (`gammaAPAR`, `Hd`, `nH1`-`nH4`, `nK1`-`nK4`, `nHL1`-`nHL4`).
- This milestone addresses the Phase 1 blocker associated with the majority of failing tests.

### `R/prepare_species.R`

- Revised the species input preparation code to comply with the updated input structure used in the current branch.
- Kept the species table validation aligned with the current expected columns and species-level input handling.
- This change supports the broader restructuring of package inputs and reduces mismatch risk when running `prepare_input()` and `run_3PG()`.

### `R/prepare_thinning.R`

- Revised the thinning input preparation code to comply with the updated structure for management events.
- Updated the handling of thinning inputs so it is consistent with the revised package input organization.
- This supports the newer management workflow and helps keep the R-side input validation consistent with the current model interface.

### `src/i_init_var.h`, `src/i_read_input.h`, `src/i_read_param_sizeDist.h`, `src/i_read_param_sub.h`, `src/i_read_param.h`, `src/i_write_out.h`

- Standardized spacing and visual alignment of assignments across the main Fortran include files used for initialization, input reading, parameter reading, and output writing.
- Reworked these files only at the formatting level, with the aim of making the mapping between indices, variables, and outputs easier to inspect during further development.
- Kept the content and ordering unchanged while making the blocks more uniform, especially in the longer parameter and output mapping sections.
- Checked the edited files after the cleanup and no file-level errors were reported.

## Notes

- `Current updates` is intended to stay brief.
- `Technical log` can contain a more informative record of what was changed, why it was changed, and what still needs follow-up.
- New changes can be appended here as the revision continues.
