# r3PG version change tracker

This file is a concise running summary of changes made during the revision of the package.  
More items will be added as development progresses.

## Current updates

### Fortran source (`src/`)

- Replaced all non-portable `kind=8` usage with `kind(0.0d0)` equivalents across `md_3PG.f95` and `i_decl_var.h`.
- Moved 20+ temporary variable declarations from `md_3PG.f95` into `i_decl_var.h`, grouped by subsystem (modifiers, stand, mortality, defoliation, Weibull).
- Extracted reusable subroutines: `s_update_long_term_modifiers`, `s_update_weibull_distribution`.
- Added scalar wrapper functions `f_exp_s` and `f_exp_foliage_s`; removed `tmp_vec`.
- Standardized long-term modifier arrays (`lt_fT`, `lt_fPhys`, `hist_ptr`) to fixed `dimension(n_sp)`.
- Removed unused dummy argument `age` from `s_height_crown_allometry`.
- Removed 16+ unused declarations (including `n_sp_max`, `mort_stress`, `mort_thinn`, `growing_season_length`, `bias_scale`).
- Re-indented `mort_model` blocks, simplified redundant weight calculations, collapsed excessive blank lines (~2600 → 2512 lines).
- Aligned formatting across all include files (`i_decl_var.h`, `i_init_var.h`, `i_read_input.h`, `i_read_param*.h`, `i_write_out.h`).
- Made `lt_mod_mths` conversion explicit (`int(siteInputs(9))`) to avoid implicit REAL→INTEGER risk.
- Updated variable descriptions in `i_decl_var.h` to match `3PG_report_draft3.docx` terminology.

### R code (`R/`)

- `prepare_species.R`, `prepare_thinning.R`: revised to comply with the updated input structure.
- `prepare_site.R`: added backward-compatible handling for legacy `altitude` column (warns and converts to `elevation`).
- `prepare_defoliation.R`: replaced unconditional age sort with conditional logic — preserves user-provided row order for coppice species (`def_type == 2`), sorts by age for all others.
- `prepare_thinning.R`: replaced `order(species, age)` with `order(species)` to always preserve user-provided row order within species (required for post-coppice events).
- `run_3PG.R`: rewrote dead-cohort output masking to use a one-month lag of `stems_n`, preserving the management-event month when a cohort is thinned to zero.
- `data.R`: corrected `i_parameters` (86 rows), `i_output` (220 rows), expanded field-level documentation for `d_mixture`, `d_regeneration`, `d_defoliation`.

### Data & internal objects

- Synchronised internal `i_parameters` with the 86-row parameter naming scheme (`beta0`, `betaB`, `nHB`, `nHC`, etc.).
- Migrated all default-data references from old flat objects (`d_input`, `d_site`, etc.) to list-based `d_mixture$...`.

### Tests (115 total, 0 fail, 0 warn, 0 skip)

- Restructured `test-run_3PG.R` into 8 numbered sections with comment headers.
- Added mortality model isolation tests (`mort_model = 2` vs `3` parameter independence).
- Added `test-prepare_defoliation.R`: coppice validation, epicormic constraints, recovery time, coppice row-order preservation, non-coppice age sorting.
- Added `test-prepare_thinning.R`: row-order preservation.
- Added `test-prepare_site.R`: backward compatibility warning for `altitude`.
- Added 4 defoliation scenario tests (coppice, epicormic, pruning ± physiological assistance) using `.rds` fixtures in `tests/testthat/fixtures/`.
- Migrated fixtures from `tests/Test_examples/*.xlsx` to `.rds`; removed `readxl` test dependency.

### Documentation & vignettes

- Updated examples in `inst/examples/`, `README.md`, and `vignettes_build/r3PG-ReferenceManual.Rmd` to use `d_mixture$...`.
- Created new vignette `vignettes_build/r3PG-NewFeatures.Rmd` covering all v0.2.0 additions: mortality models, biomass-based thinning, defoliation events (pruning, coppice, epicormic, stand-replacing), multi-cohort regeneration. Uses only internal datasets; no calibration/Bayesian content.
- Pre-built HTML and `.html.asis` stub added to `vignettes/` for `R.rsp::asis` packaging.
- Regenerated all Rd files with `devtools::document()`.

### Bug fixes

- **mort_model=3 non-determinism**: Fixed out-of-bounds array reads when initialization period `lt_mod_mths` exceeds climate array size `n_m`. Applied clamping at initialization (`lt_init_len = min(lt_mod_mths, n_m)`) to prevent reading garbage memory. Also reformulated mort_model=3 equation using relative-perturbation + Taylor expansion to avoid catastrophic cancellation. Verified deterministic output: 20/20 identical runs across repeated simulations.
- **Regenerating cohort R-side masking**: Fixed masking logic inadvertently removing newly-activated cohorts when the previous month had zero stems. Added `age_prev_mat >= 0` guard to ensure "dead for 2+ consecutive months" rule only applies within a cohort's lifetime, not across activation boundaries. Affected regenerating species with temporal gaps (e.g., species 2 planted >1 month after species 1 dies).
- **Regenerating cohort Fortran mortality spike**: Fixed excessive mortality for newly-activated cohorts caused by stale `dbh_total_prev` from previous species. Added code to reset `dbh_total_prev = dbh_total` immediately after cohort activation to prevent misleading dbh_ratio calculations in mort_model=3. Species 2 now activates with expected stem count (~1000) instead of zero.
- Fixed post-simulation output masking hiding management-event month.
- Fixed event ordering breaking post-coppice events when age resets to 0.
- Fixed Fortran division-by-zero producing NaN in outputs at thinning months (`biom_tree / stems_n` when `stems_n = 0`) and at planting months (`Log(dbh)` / `Log(age)` when `dbh = 0` or `age = 0`). Affected variables: `basal_area`, `dbh`, `biom_tree`, `DWeibullScale`, `DWeibullShape`, `DWeibullLocation`.
- Fixed uninitialized Fortran variables written to the output array at month 1, causing platform-dependent garbage values on Windows/Linux (macOS ARM zeros stack memory, masking the bug). Added 9 missing initializations in `i_init_var.h` and proper `basal_area_prop` computation before the first output write.
- Added R-side `NaN → NA` sanitization in `run_3PG.R` as a safety net.
- Fixed out-of-bounds array reads in long-term modifier initialization when `lt_mod_mths > n_m`, causing non-deterministic density-dependent mortality (`mort_model = 3`) results across repeated runs; also reformulated the mort_model = 3 equation using relative-perturbation to avoid catastrophic cancellation.
- Fixed masking logic for dead/not-yet-recruited cohorts in `run_3PG.R`: when a new cohort activates within 1–2 months after another cohort dies (gap > 1 month), the activation month was incorrectly masked as NA because the previous month's `stems_n` (from the dead cohort) was ≤ 0. New logic checks `age_prev >= 0` to only apply the "dead for 2+ months" rule within a cohort's lifetime, not across activation boundaries.

### Tests (115 total, 0 fail, 0 warn, 0 skip)

- Increased test precision from 3 to 5 significant digits (`tolerance = 5e-6`) now that run-to-run determinism is guaranteed.
- Masking logic verified to correctly handle regenerating cohorts with gaps after previous cohort death.

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
- Fixed division-by-zero bug in post-management stand structure update (~line 1112): added `where(stems_n > 0) ... elsewhere` guard to `biom_tree`, `dbh`, and `basal_area` computations, preventing NaN when a cohort is thinned to 0 stems.
- Fixed same division-by-zero in end-of-month `biom_tree` recalculation (~line 1320): added `where(stems_n > 0) ... elsewhere` guard.
- Fixed `s_update_weibull_distribution` subroutine: replaced array-wide `Log(dbh(:))` / `Log(age_row(:))` calls with per-species loop that checks `dbh > 0`, `age > 0`, and `competition_total > 0` before computing; sets outputs to 0 (or 1 for shape) for dead/just-planted cohorts, preventing NaN from `Log(0)`.

### `src/i_decl_var.h`

- Replaced remaining `real(kind=8)` declarations (`lt_fT`, `lt_fPhys`, `fT_hist`, `fPhys_hist`) with `kind(0.0d0)`-based declarations for portability.
- Changed `lt_fT`, `lt_fPhys`, and `hist_ptr` to explicit `dimension(n_sp)` arrays to match their use and avoid unnecessary dynamic allocation.
- Removed unused declarations flagged during Fortran diagnostics cleanup (`b_cor`, `b_n`, `n`, `NPP_def`, `biom_stem_pre`, `biom_foliage_pre`, `biom_root_pre`, `tmp`, `denom`, `denom_stems`, `def_test_var`).
- Removed 5 additional unused variables identified via `-Wunused-variable` audit: `n_sp_max` (unused parameter), `mort_stress` and `mort_thinn` (superseded by `stems_loss_stress`/`mort_thinn_total`), `growing_season_length` (never assigned/read), `bias_scale` (never assigned/read). Corresponding initializations removed from `i_init_var.h`.

### `src/i_init_var.h` (uninitialized-variable fix)

- Added 9 missing variable initializations that were written to the output array (via `i_write_out.h`) before being computed:
    - `basal_area_prop(:) = 0.d0` — main culprit; on Windows/Linux showed values like 400 instead of 0.
    - `m_apar(:) = 1.d0` — second culprit; neutral multiplier, showed garbage on non-ARM platforms.
    - `height_rel(:) = 0.d0`, `crown_length(:) = 0.d0`, `crown_width(:) = 0.d0` — stand geometry.
    - `stems_n_ha(:) = 0.d0` — per-cohort monoculture-equivalent density.
    - `dbh_prev(:) = 0.d0`, `dbh_total_prev = 0.d0` — previous-step DBH for mortality calculations.
    - `water_runoff_polled = 0.d0` — pooled runoff accumulator.
- Root cause: On macOS ARM, the OS zeroes stack-allocated memory, so the uninitialised values happened to be 0.0 and tests passed. On Windows and Linux x86-64, stack memory contains arbitrary bit patterns, producing garbage values (400, 4.65e-310, NaN) that failed `expect_equal()` in the mortality isolation tests.

### `src/md_3PG.f95` (init-section `basal_area_prop`)

- Added `basal_area_prop` computation (with `1.0d-6` floor) immediately after `basal_area_total` is calculated in the initialisation block, before the first `include 'i_write_out.h'` call at month 1.
- Previously, `basal_area_prop` was only computed inside the density-dependent mortality section (which runs from month 2 onwards), so the month-1 output row contained either zero or uninitialised garbage depending on platform.

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

### `tests/testthat/test-run_3PG.R`

- Restructured the file into 8 numbered sections with `# ===` comment headers:
    1. Basic model runs (array and data.frame output)
    2. Single-species: Evergreen (3-PGpjs and 3-PGmix)
    3. Single-species: Broadleaf (3-PGpjs and 3-PGmix)
    4. Mixed-species (3-PGmix)
    5. Mortality models (mort_model 2 biomass, beta* isolation, st_* isolation)
    6. Management (biomass-based thinning)
    7. Defoliation — package built-in data (`d_defoliation`)
    8. Defoliation — Excel-based scenarios (`tests/Test_examples`)
- Added file-level helper functions `read_defoliation_scenario(xlsx_path)` and `run_defoliation_scenario(xlsx_path, mort_model)` to standardize Excel scenario loading and `run_3PG()` calls.
- Added 4 new tests (section 8) that read from `tests/Test_examples/*.xlsx` files:
    - **Coppice** (`Input_coppice.xlsx`): Verifies height resets from ~18.2 m to ~1.5 m at month 181 (age 20 coppice event), stem and foliage biomass zeroed, `def_type` flag written to output.
    - **Epicormic** (`Input_epicormic.xlsx`): Verifies `stems_n` halves from 200 to 100 at month 121 (age 40, `stem_retained = 0.5`), defoliation stem loss of 100 recorded, height preserved across event.
    - **Pruning with physiological assistance** (`Input_pruning.xlsx` vs `Input_pruning_no_phys_assistance.xlsx`): Asserts that `prop_carbs > 0` / `prop_npp > 0` yields higher final stem biomass than the no-assistance counterpart.
    - **Pruning without physiological assistance** (`Input_pruning_no_phys_assistance.xlsx`): Checks final height and biomass regression values.
- All 4 Excel-based tests use `skip_if_not_installed("readxl")` and `skip_if_not(file.exists(...))` guards so they are skipped gracefully when the external dependencies or data files are unavailable.
- Migrated all 4 defoliation scenario tests from reading `tests/Test_examples/*.xlsx` via `readxl` to loading pre-processed `.rds` fixture lists from `tests/testthat/fixtures/`. Removed the `readxl` skip guards (`skip_if_not_installed`, `skip_if_not(file.exists(...))`) and the `read_defoliation_scenario()` xlsx helper, replaced with a single `run_defoliation_scenario(rds_path)` helper that calls `readRDS()`. Deleted the `tests/Test_examples/` directory (4 xlsx files + 4 interactive R scripts).
- Each `.rds` fixture is a named list with components: `site`, `species`, `climate` (pre-processed via `prepare_climate()`), `defoliation`, `parameters`, `sizeDist`.
- Reference values were generated by running each scenario through `run_3PG()` and rounding outputs at the assertion precision (3 decimal places for biomass/height, 0 for stem counts).
- Total test count increased from 88 to 107 (19 new assertions across 4 test cases).

### `R/run_3PG.R`

- Rewrote the dead-cohort / not-yet-recruited output masking logic (lines ~149–160).
- Previous behaviour: masked all outputs where `stems_n < 0` or `age < 0`, which meant the management-event month (where `stems_n` first drops to 0) was also masked, hiding loss outputs.
- New behaviour: uses a one-month lag of `stems_n` so the first month with `stems_n ≤ 0` is preserved (management event visible); masking starts only from the second consecutive month with `stems_n ≤ 0`. The `age < 0` condition (not-yet-recruited) is unchanged.
- Implementation extracts `stems_n` and `age` as explicit `[n_m, n_sp]` matrices via `matrix()`, computes `stems_n_prev` with `rbind(NA_real_, ...)`, and builds a 2-D logical mask that is broadcast to the full 4-D array.
- Added `r3PG_out[is.nan(r3PG_out)] <- NA_real_` after the masking step as a safety net against any remaining Fortran NaN values (e.g. from edge-case 0/0 or Log(0) computations).

### `R/prepare_defoliation.R`

- Replaced the unconditional `order(defoliation$species, defoliation$age)` sort with conditional logic:
    - Species that contain any `def_type == 2` (coppice) event: preserve the user-provided row order within each species, since coppice resets age to 0 and post-coppice events would otherwise be mis-sorted.
    - All other species: continue sorting by `(species, age)` as before.
- The Fortran event-matching loop uses sequential pointers (`d_n(i)`) that increment after each event, so the array layout must match the intended chronological order — not necessarily ascending age order.

### `R/prepare_thinning.R`

- Replaced `order(thinning$species, thinning$age)` with `order(thinning$species)` to preserve user-provided row order within each species while still grouping rows by species.
- This is required for post-coppice management events where age resets to 0, and is harmless for non-coppice scenarios where users naturally supply events in ascending age.
- The subsequent `merge()`→`order(thin_n)` pipeline is unaffected; `thin_n` sequence numbers are now assigned based on row order rather than age order.

### `tests/testthat/test-prepare_defoliation.R`

- Added 2 new ordering tests:
    - `prepare_defoliation preserves row order for coppice species`: supplies coppice (age 20) + pruning (age 5) in that order and verifies the prepared array retains coppice first, pruning second.
    - `prepare_defoliation sorts by age for non-coppice species`: supplies two pruning events in reverse age order and verifies they are sorted ascending.

### `tests/testthat/test-prepare_thinning.R`

- Added 1 new ordering test:
    - `prepare_thinning preserves user-provided row order`: supplies two events in descending age order and verifies the prepared array retains that order.

### `R/prepare_site.R`

- Implemented backward-compatible handling for legacy site input column `altitude`:
    - emits a deprecation warning,
    - converts `altitude` to `elevation` when `elevation` is not provided.
- Updated `test-prepare_input.R` to use `d_mixture$site`, `d_mixture$species`, `d_mixture$climate`, and `d_mixture$thinning` instead of `d_input$...`.

### `vignettes_build/r3PG-NewFeatures.Rmd` and `vignettes/r3PG-NewFeatures.html.asis`

- Created a new beginner-oriented vignette documenting all v0.2.0 features.
- Structure: Introduction → Mortality models (table + comparison plot) → Thinning (standard + biomass-based) → Multi-cohort regeneration (`d_regeneration`, 25 cohorts) → Defoliation events (input structure, constraint table, 4 types with code examples, epicormic worked example with `d_defoliation`, defoliated vs. undisturbed comparison) → Model settings reference table → Output variable reference.
- All examples use only internal datasets (`d_mixture`, `d_regeneration`, `d_defoliation`); no calibration, Bayesian, or Morris content.
- Pre-built HTML rendered via `rmarkdown::render()` and placed in `vignettes/` alongside an `R.rsp::asis` stub, matching the existing `r3PG-ReferenceManual` packaging pattern.

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

### `src/md_3PG.f95` (out-of-bounds array read fix — non-determinism root cause)

- **Root cause of mort_model = 3 non-determinism**: `lt_mod_mths` is typically 120 (10 years × 12 months) but the simulation length `n_m` can be shorter (e.g. 92 months). The initialization code computed `sum(vpd_day(2:lt_mod_mths))` and `sum(f_tmp(1:lt_mod_mths, i))`, reading 28 elements past the end of the arrays (dimensioned to `n_m`). These out-of-bounds reads returned whatever happened to be in memory, which varied between calls — producing different long-term modifier seeds and therefore different mortality trajectories on every run.
- Fix: introduced `lt_init_len = min(lt_mod_mths, n_m)` and clamped all initialization indexing (`vpd_day`, `f_tmp`, `fT_hist`, `fPhys_hist`) to this safe upper bound.
- Changed `fT_hist` and `fPhys_hist` allocation from `if (.not. allocated(...)) allocate(...)` to unconditional `deallocate` + `allocate`. The previous pattern could retain stale values from a prior call in the same R session, contributing to run-to-run variation.
- Verified fix: 20 consecutive identical-input runs now produce exactly the same `stems_n` (1171.9335732413) — previously 7 different values were observed across 20 runs.

### `src/md_3PG.f95` (mort_model = 3 reformulation)

- Reformulated the mort_model = 3 stem-loss equation to avoid catastrophic cancellation.
- Original: `mort_thinn_total = stems_n_total - Exp(inv_exp * Log(inner))` where `inner = stems_n_total**expo + delta_stuff`. When `delta_stuff` is small relative to `stems_n_total**expo`, the subtraction loses precision.
- New formulation uses relative perturbation: computes `frac_nk = delta_stuff / base_nk` and `log_term = inv_exp * log(1 + frac_nk)`, with a Taylor expansion (`expm1` equivalent) when `|log_term| < 1e-4`. This is mathematically equivalent but numerically superior.
- Added variable declarations `base_nk`, `frac_nk`, `log_term` in `i_decl_var.h`.

### `src/i_decl_var.h` (OOB fix variables)

- Added `lt_init_len` (integer): safe upper bound for indexing climate arrays during initialization, defined as `min(lt_mod_mths, n_m)`.

### `tests/testthat/test-run_3PG.R` (test precision increase)

- Updated all 13 `expect_equal` assertions from `round(..., 3)` comparisons to `tolerance = 5e-6` with 5-digit reference values.
- This tighter precision is now possible because the OOB fix eliminated run-to-run variation; previously the 3-digit rounding was necessary to absorb platform-dependent jitter.
- Total test count remains 115, all passing.

## Notes

- `Current updates` is intended to stay brief.
- `Technical log` can contain a more informative record of what was changed, why it was changed, and what still needs follow-up.
- New changes can be appended here as the revision continues.
