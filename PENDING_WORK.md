# Pending Work and Roadmap for ximage

This document catalogs all open issues and potential PRs that were identified but not yet completed.

## Recent Completed Work

- **PR #19** (Merged 2026-02-15): Clean up codebase for CRAN submission
  - Removed empty `R/meshplot.R` file
  - Cleaned up commented-out code in `R/ximage.R`

- **PR #18** (Merged): Fix roxygen documentation typos in ximage.R
- **PR #17** (Merged): Update GitHub Actions to latest stable versions
- **PR #16** (Merged): Fix DESCRIPTION language field from es to en
- **PR #12** (Merged): Support gdalraster output as vector

## Open Issues Requiring PRs

### High Priority Issues

#### Issue #15: Need constant alpha for normal ximage() rgb use
**Status**: Open (created 2025-08-26)  
**Description**: Requires handling alpha channel consistently for RGB images  
**Example Code**: Available in issue description showing hillshade overlay use case  
**Estimated Effort**: Medium

#### Issue #14: Array orientation - quick explainer
**Status**: Open (created 2025-04-10)  
**Type**: Documentation  
**Description**: Documentation issue explaining raster order vs matrix order  
**Action**: Could be enhanced with additional examples or moved to vignette  
**Estimated Effort**: Low

#### Issue #13: Missing from fastpng cases
**Status**: Open (created 2024-02-15)  
**Description**: Grey alpha support missing  
**Estimated Effort**: Medium

#### Issue #11: Support gdalraster attributed output
**Status**: Open (created 2024-02-12)  
**Description**: Support for attributed output from gdalraster  
**Reference**: https://github.com/USDAForestService/gdalraster/pull/198  
**Comments**: 5 comments, active discussion  
**Estimated Effort**: Medium-High

#### Issue #10: Not passing down breaks
**Status**: Open (created 2024-01-25)  
**Description**: Breaks parameter not being passed correctly  
**Comments**: 1 comment  
**Estimated Effort**: Low-Medium

#### Issue #9: alpha() for nativeRaster
**Status**: Open (created 2024-01-17)  
**Description**: Create a scales::alpha() for arrays, hex, rgb, nativeRaster that doesn't drop dimension  
**Comments**: 1 comment  
**Estimated Effort**: Medium

#### Issue #8: Extent for rasterImage angle
**Status**: Open (created 2024-01-17)  
**Description**: Handle angle parameter with extent calculations  
**Example Code**: Full implementation provided in issue  
**Comments**: 2 comments  
**Estimated Effort**: Medium-High

#### Issue #7: Deal with missing values (NaN) in 3 band list
**Status**: Open (created 2023-10-26)  
**Description**: Error when handling NaN in RGB data from Sentinel imagery  
**Error**: `color intensity nan, not in [0,1]`  
**Estimated Effort**: Medium

#### Issue #6: Check the sf RasterIO stuff
**Status**: Open (created 2023-06-29)  
**Description**: Possible zero/one problem in ximage_sf_data  
**Estimated Effort**: Low-Medium

### Feature Requests

#### Issue #5: Add mesh_plot mode from anglr
**Status**: Open (created 2022-12-20)  
**Description**: Add mesh plotting capability from anglr package  
**Comments**: 3 comments  
**Checklist**:
- [ ] Unpack raster/nativeraster to colours
- [ ] Handle extent as 2D arrays for xy coords
- [ ] Handle corner cases (for ROMS etc)  
**Estimated Effort**: High

#### Issue #4: Colours not ok
**Status**: Open (created 2022-09-12)  
**Description**: Color handling issues with single values and NA/NaN  
**Examples**: Multiple test cases showing broken behavior  
**Estimated Effort**: Medium

#### Issue #3: Placeholder for base R vector/raster decoupling issue
**Status**: Open (created 2022-08-30)  
**Reference**: https://github.com/hypertidy/quadmesh/issues/15  
**Estimated Effort**: Unknown

#### Issue #2: See {nara} for nativeRaster
**Status**: Open (created 2022-07-26)  
**Reference**: https://github.com/coolbutuseless/nara  
**Description**: Investigation/integration opportunity  
**Estimated Effort**: Research needed

#### Issue #1: Support list-xyz
**Status**: Open (created 2022-07-04)  
**Description**: Support list-xyz format, reconcile matrix orientation  
**Also**: Support spatstat, stars, raster, terra via Suggests  
**Estimated Effort**: High

## Suggested PR Priorities

Based on the issues above, here's a suggested order for creating PRs:

### Phase 1: Bug Fixes & Critical Issues
1. **PR for Issue #7**: Fix NaN handling in 3-band RGB data
2. **PR for Issue #10**: Fix breaks parameter passing
3. **PR for Issue #4**: Fix color handling edge cases

### Phase 2: Alpha Channel Improvements
4. **PR for Issue #15**: Implement constant alpha for RGB use
5. **PR for Issue #9**: Create alpha() function for multiple types
6. **PR for Issue #13**: Add grey alpha support for fastpng

### Phase 3: Feature Enhancements
7. **PR for Issue #11**: Support gdalraster attributed output
8. **PR for Issue #8**: Implement extent calculations for angle parameter
9. **PR for Issue #6**: Fix sf RasterIO issues

### Phase 4: Major Features (Separate Planning)
10. **Issue #5**: mesh_plot mode (requires detailed design)
11. **Issue #1**: list-xyz support (requires detailed design)
12. **Issues #2, #3**: Research and investigation

## Notes

- The previous Copilot session mentioned "more PRs to do" but specific details were not found in commit history
- This document serves as a comprehensive roadmap based on all open issues
- Each issue can be converted into a focused PR following the priority order above
- Consider grouping related issues (e.g., alpha-related issues) into single PRs if appropriate

## Next Steps

To continue from the lost session:
1. Review this roadmap with the team
2. Select the next issue to address based on priority
3. Create a focused PR for that issue
4. Update this document as issues are resolved

---

*Last updated: 2026-02-15*  
*Issues as of: 14 open issues tracked*
