# Key Differences Between previouswork.r and analysis_country.r

## Results Show Different Coefficients

The table shown has different results suggesting bugs in the new script. Here are the main issues:

### 1. **Deficit Date Format Bug** ✓ FIXED
- **previouswork.r**: Uses `format = "%d/%m/%Y"` for both debt and deficit
- **analysis_country.r**: Corrected deficit to `format = "%Y-%m-%d"` (actual format)
- **Note**: This difference could cause misaligned dates if deficit baseline_date is actually %d/%m/%Y

### 2. **Variable Names - Control Variables**
- **previouswork.r** uses: `tbill3m` (from TB3MS.csv), `ebp`, `recession` (USREC)
- **analysis_country.r** uses: `st_rate` (from ST_rate.csv), `risk_ind`, `recession` (REC.csv)
- **Note**: Check if ST_rate.csv = TB3MS.csv and REC.csv = USREC.csv

### 3. **Foreign Holdings - CRITICAL DIFFERENCE**
- **previouswork.r**: Calculates `foreign_holdings = (FDHBFIN / (GFDEBTN / 1000)) * 100`
  - Uses two FRED series to compute a percentage
- **analysis_country.r**: Sets `foreign_holdings = NULL/NA` (file doesn't exist)
- **Action needed**: Add FDHBFIN.csv and GFDEBTN.csv to compute foreign holdings ratio

### 4. **Dependent Variables - Rate Terms**
- **previouswork.r** uses 5 rate variables: `fwd_5_10y`, `fwd_10_15y`, `t10`, `tprem5`, `tprem10`
- **analysis_country.r** uses: `rate_long`, `fwd_5_10y`, `fwd_10_15y`, `tprem`
- **Note**: Need to extract both `tprem5` and `tprem10` from ACMTermPremium

### 5. **Term Premium Special Handling**
- **previouswork.r**: Removes `tbill3m` when regressing term premium variables (makes economic sense)
- **analysis_country.r**: No special handling for tprem

### 6. **Regression Model Specifications**
- **previouswork.r M3**: `t + tbill3m` (no gdp_growth)
- **analysis_country.r M3**: `t + st_rate` (should be same if ST_rate=TB3MS)

### 7. **Missing Files**
Need to check data structure:
- Is `data/independent_variables/` or `data/us/independent_variables/`?
- Is `FDHBFIN.csv` and `GFDEBTN.csv` available for foreign holdings?
- Is `TB3MS` same as `ST_rate`?
