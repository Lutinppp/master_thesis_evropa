"""
ACM term premium estimation pipeline (rewrite)
=============================================
This script implements the full pipeline requested by the user:
1) Bootstrap knot zero-coupon curves from par yields
2) Interpolate to quarterly fine grid via natural cubic spline
3) Estimate ACM model and term premia

Input:
  ACM/EGBs.csv

Outputs:
  ACM/ZC_UK.csv, ACM/ZC_GER.csv, ACM/ZC_FRA.csv
  ACM/ZC_FINE_UK.csv, ACM/ZC_FINE_GER.csv, ACM/ZC_FINE_FRA.csv
  data/uk/control_variables/ACMTermPremium.csv
  data/germany/control_variables/ACMTermPremium.csv
  data/france/control_variables/ACMTermPremium.csv
"""

import os
import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from scipy.interpolate import CubicSpline


# ============================================================================
# CONFIGURATION
# ============================================================================

INPUT_FILE = "ACM/EGBs.csv"
COUNTRY_PREFIXES = {
    "UK": "TRUK",
    "GERMANY": "TRBD",
    "FRANCE": "TRFR",
}

SUFFIX_TO_MATURITY = {
    "3MT": 0.25,
    "6MT": 0.5,
    "1YT": 1.0,
    "2YT": 2.0,
    "3YT": 3.0,
    "4YT": 4.0,
    "5YT": 5.0,
    "7YT": 7.0,
    "10T": 10.0,
    "15T": 15.0,
    "20T": 20.0,
    "25T": 25.0,
}

EXPECTED_KNOT_MATURITIES = np.array(sorted(SUFFIX_TO_MATURITY.values()), dtype=float)
H = 0.25
FINE_GRID = np.arange(0.25, 25.0 + 1e-12, 0.25)
TP_TARGET_YEARS = [5.0, 10.0, 15.0, 25.0]


# ============================================================================
# UTILITIES
# ============================================================================


def ensure_dirs():
    os.makedirs("ACM", exist_ok=True)
    os.makedirs("data/uk/control_variables", exist_ok=True)
    os.makedirs("data/germany/control_variables", exist_ok=True)
    os.makedirs("data/france/control_variables", exist_ok=True)



def parse_country_columns(col_codes, country_prefix):
    """Return sorted list of (column_name, maturity) for one country."""
    pairs = []
    for c in col_codes:
        if not isinstance(c, str):
            continue
        if c.startswith(country_prefix):
            suffix = c[len(country_prefix) :]
            if suffix in SUFFIX_TO_MATURITY:
                pairs.append((c, SUFFIX_TO_MATURITY[suffix]))
    pairs.sort(key=lambda x: x[1])
    return pairs



def to_date_str(series):
    return pd.to_datetime(series).dt.strftime("%Y-%m-%d")



def maturity_label_knot(m):
    if np.isclose(m, 0.25):
        return "ZC0.25"
    if np.isclose(m, 0.5):
        return "ZC0.5"
    return f"ZC{int(round(m))}"



def maturity_label_fine(m):
    return f"ZC{m:.2f}"



def nearest_two_known_points(known_mats, known_zc, target):
    """Pick two nearest known points by absolute distance for linear interpolation/extrapolation."""
    d = np.abs(known_mats - target)
    order = np.argsort(d)
    i1, i2 = order[0], order[1]
    t1, t2 = known_mats[i1], known_mats[i2]
    z1, z2 = known_zc[i1], known_zc[i2]
    if np.isclose(t1, t2):
        return t1, z1, t2 + 1e-12, z2
    return t1, z1, t2, z2



def linear_interp(x1, y1, x2, y2, x):
    return y1 + (y2 - y1) * (x - x1) / (x2 - x1)



def solve_ols(Y, X, add_const=True):
    """
    OLS helper.
    Y: (T,) or (T,k)
    X: (T,p)
    Returns beta where shape is (p+1,k) if add_const else (p,k)
    """
    if Y.ndim == 1:
        Y = Y.reshape(-1, 1)
    if add_const:
        Xd = np.column_stack([np.ones(X.shape[0]), X])
    else:
        Xd = X
    beta, _, _, _ = np.linalg.lstsq(Xd, Y, rcond=None)
    return beta


# ============================================================================
# STEP 0: LOAD INPUT
# ============================================================================


def load_input():
    raw = pd.read_csv(INPUT_FILE, sep=";", header=None)
    if raw.shape[0] < 7:
        raise ValueError("Input file does not contain expected metadata + data rows.")

    col_codes = raw.iloc[4].tolist()
    data = raw.iloc[6:].copy().reset_index(drop=True)
    data.columns = col_codes

    date_col = col_codes[0]
    data[date_col] = pd.to_datetime(data[date_col], format="%d/%m/%Y", errors="coerce")
    data = data.loc[data[date_col].notna()].reset_index(drop=True)

    return data, col_codes, date_col


# ============================================================================
# STEP 1: BOOTSTRAP KNOT ZC CURVES
# ============================================================================


def bootstrap_country_knot_curves(data, date_col, country_name, country_prefix):
    # -------------------- 1A. collect country columns --------------------
    pairs = parse_country_columns(list(data.columns), country_prefix)
    if len(pairs) != len(EXPECTED_KNOT_MATURITIES):
        raise ValueError(
            f"{country_name}: expected {len(EXPECTED_KNOT_MATURITIES)} maturity columns, found {len(pairs)}."
        )

    maturities = np.array([m for _, m in pairs], dtype=float)
    if not np.allclose(maturities, EXPECTED_KNOT_MATURITIES):
        raise ValueError(f"{country_name}: maturity grid mismatch in input columns.")

    cols = [c for c, _ in pairs]
    y_par = data[cols].apply(pd.to_numeric, errors="coerce") / 100.0  # decimals
    dates = data[date_col].copy()

    # -------------------- 1B. per-date bootstrap --------------------
    zc = np.full((len(y_par), len(maturities)), np.nan, dtype=float)
    p_nonpos_warnings = 0

    for t in range(len(y_par)):
        par_row = y_par.iloc[t].values.astype(float)
        zc_row = np.full(len(maturities), np.nan, dtype=float)

        # 0.25, 0.5, 1.0 direct assignment
        for i in range(len(maturities)):
            m = maturities[i]
            if np.isclose(m, 0.25) or np.isclose(m, 0.5) or np.isclose(m, 1.0):
                if np.isfinite(par_row[i]):
                    zc_row[i] = par_row[i]

        # Remaining maturities in sorted order (>1)
        for i in range(len(maturities)):
            n = maturities[i]
            if n <= 1.0:
                continue

            c = par_row[i]
            if not np.isfinite(c):
                continue

            discounted_coupon_sum = 0.0
            feasible = True

            # coupon dates t=1,2,...,n-1 (annual)
            for tau_int in range(1, int(round(n))):
                tau = float(tau_int)

                # known already-bootstrapped points for this date row
                known_mask = np.isfinite(zc_row)
                known_m = maturities[known_mask]
                known_z = zc_row[known_mask]
                if len(known_m) < 2:
                    feasible = False
                    break

                # exact known maturity
                exact = np.where(np.isclose(known_m, tau))[0]
                if len(exact) > 0:
                    z_tau = known_z[exact[0]]
                else:
                    # linear interpolation between two nearest already-bootstrapped yields
                    x1, y1, x2, y2 = nearest_two_known_points(known_m, known_z, tau)
                    z_tau = linear_interp(x1, y1, x2, y2, tau)

                if not np.isfinite(z_tau):
                    feasible = False
                    break

                df_tau = 1.0 / ((1.0 + z_tau) ** tau)
                discounted_coupon_sum += df_tau

            if not feasible:
                continue

            # P(n) = (1 - c * sum(df_tau)) / (1 + c)
            p_n = (1.0 - c * discounted_coupon_sum) / (1.0 + c)

            if p_n <= 0 or (not np.isfinite(p_n)):
                p_nonpos_warnings += 1
                # forward-fill from previous valid date for that maturity only
                if t > 0 and np.isfinite(zc[t - 1, i]):
                    zc_row[i] = zc[t - 1, i]
                continue

            zc_row[i] = (1.0 / p_n) ** (1.0 / n) - 1.0

        zc[t, :] = zc_row

    # Keep data-driven rows where at least one knot is finite
    keep = np.isfinite(zc).any(axis=1)
    dates = dates.loc[keep].reset_index(drop=True)
    zc = zc[keep]

    # Save knot file
    out = pd.DataFrame({"date": to_date_str(dates)})
    for i, m in enumerate(maturities):
        out[maturity_label_knot(m)] = zc[:, i]

    return {
        "country": country_name,
        "dates": dates.reset_index(drop=True),
        "maturities": maturities,
        "zc_knot": zc,
        "warnings_p_nonpos": p_nonpos_warnings,
        "knot_df": out,
    }


# ============================================================================
# STEP 2: CUBIC SPLINE TO QUARTERLY GRID
# ============================================================================


def interpolate_fine_grid(country_result):
    dates = country_result["dates"]
    kmat = country_result["maturities"]
    zc_knot = country_result["zc_knot"]

    fine = np.full((len(dates), len(FINE_GRID)), np.nan, dtype=float)

    for t in range(len(dates)):
        yk = zc_knot[t, :]

        # valid knots for this date
        valid = np.isfinite(yk)
        if valid.sum() < 2:
            continue

        xk = kmat[valid]
        yv = yk[valid]

        # spline where possible; fallback to linear on valid knots
        try:
            cs = CubicSpline(xk, yv, bc_type="natural", extrapolate=True)
            yi = cs(FINE_GRID)
        except Exception:
            yi = np.interp(FINE_GRID, xk, yv)

        # Replace negative or non-finite values with linear interpolation
        bad = (~np.isfinite(yi)) | (yi < 0)
        if np.any(bad):
            valid_knots = np.isfinite(yk) & (yk >= 0)
            if valid_knots.sum() >= 2:
                yi_lin = np.interp(FINE_GRID, kmat[valid_knots], yk[valid_knots])
                yi[bad] = yi_lin[bad]
            else:
                # ultimate fallback: nearest finite knot (or zero)
                fallback = np.nanmedian(yk[np.isfinite(yk)]) if np.isfinite(yk).any() else 0.0
                yi[bad] = max(0.0, float(fallback))

        fine[t, :] = yi

    out = pd.DataFrame({"date": to_date_str(dates)})
    for i, m in enumerate(FINE_GRID):
        out[maturity_label_fine(m)] = fine[:, i]

    return fine, out


# ============================================================================
# STEP 3: ACM ESTIMATION ON FINE GRID
# ============================================================================


def acm_country_from_fine(country_name, dates, y_fine, warnings_step1):
    # -------------------- 3.0 align to quarterly frequency --------------------
    # Step 3 formulas use h=0.25 as one-period horizon, so we estimate on
    # quarter-end observations.
    panel = pd.DataFrame(y_fine, index=pd.to_datetime(dates))
    panel = panel.resample("QE").last()
    panel = panel.dropna(how="all")

    dates = pd.Series(panel.index).reset_index(drop=True)
    y_fine = panel.values

    # -------------------- 3.1 clean panel --------------------
    # Drop rows where >20% non-finite
    non_finite_count = np.sum(~np.isfinite(y_fine), axis=1)
    keep = non_finite_count <= int(0.20 * y_fine.shape[1])

    dates = dates.loc[keep].reset_index(drop=True)
    y = y_fine[keep].copy()

    # Forward-fill isolated NaNs on remaining rows
    y_df = pd.DataFrame(y)
    y_df = y_df.ffill()
    y_df = y_df.dropna(axis=0)
    y = y_df.values
    dates = dates.iloc[: len(y)].reset_index(drop=True)

    # -------------------- 3.2 PCA on demeaned panel --------------------
    y_demeaned = y - np.nanmean(y, axis=0, keepdims=True)

    pca = PCA(n_components=5)
    X = pca.fit_transform(y_demeaned)  # (T,5)
    cum_var = np.cumsum(pca.explained_variance_ratio_)

    # -------------------- 3.3 VAR(1) --------------------
    X_lag = X[:-1]
    X_cur = X[1:]
    beta_var = solve_ols(X_cur, X_lag, add_const=True)  # (6,5)
    mu = beta_var[0, :]  # (5,)
    Phi = beta_var[1:, :]  # (5,5)
    v = X_cur - (np.column_stack([np.ones(len(X_lag)), X_lag]) @ beta_var)  # (T-1,5)
    Sigma = np.cov(v.T)

    # -------------------- 3.4 short-rate loadings --------------------
    short_idx = np.where(np.isclose(FINE_GRID, 0.25))[0]
    if len(short_idx) != 1:
        raise ValueError("Quarterly grid missing 0.25 maturity.")
    short_idx = short_idx[0]

    r = y[:, short_idx]
    beta_r = solve_ols(r, X, add_const=True).flatten()  # (6,)
    delta_0 = float(beta_r[0])
    delta_1 = beta_r[1:]  # (5,)

    # -------------------- 3.5 cross-sectional OLS loadings --------------------
    A = np.zeros(y.shape[1], dtype=float)
    B = np.zeros((y.shape[1], 5), dtype=float)

    r2_list = []
    X_with_const = np.column_stack([np.ones(len(X)), X])
    for j in range(y.shape[1]):
        yj = y[:, j]
        beta_j, _, _, _ = np.linalg.lstsq(X_with_const, yj, rcond=None)
        A[j] = beta_j[0]
        B[j, :] = beta_j[1:]

        yhat = X_with_const @ beta_j
        ss_res = np.sum((yj - yhat) ** 2)
        ss_tot = np.sum((yj - np.mean(yj)) ** 2)
        r2 = 1.0 - ss_res / ss_tot if ss_tot > 0 else np.nan
        r2_list.append(r2)

    # -------------------- 3.6 market prices of risk (requested simplified ACM) --------------------
    # Excess returns for maturities n > 0.25 i.e. indices 1..99
    T = len(y)
    RX = np.zeros((T - 1, y.shape[1] - 1), dtype=float)
    for j in range(1, y.shape[1]):
        n = FINE_GRID[j]
        y_n_t = y[:-1, j]
        y_n_minus_h_t1 = y[1:, j - 1]
        r_t = y[:-1, short_idx]
        RX[:, j - 1] = n * y_n_t - (n - H) * y_n_minus_h_t1 - H * r_t

    # Regress VAR residuals on RX via OLS: v = RX * Theta + const
    # Lambda_hat requested as (5,99); we estimate without intercept for direct mapping.
    # Solve RX @ Theta = v, Theta shape (99,5), then transpose.
    theta, _, _, _ = np.linalg.lstsq(RX, v, rcond=None)
    Lambda_hat = theta.T  # (5,99)

    lambda_0 = Lambda_hat @ np.mean(RX, axis=0)  # (5,)
    lambda_1 = Lambda_hat @ ((RX.T @ X_lag) / T)  # (5,5)

    # Normalization
    lambda_0 = np.linalg.solve(Sigma, lambda_0)
    lambda_1 = np.linalg.solve(Sigma, lambda_1)

    # -------------------- 3.7 risk-neutral affine recursion --------------------
    # n indexes quarters: 1..100.
    # We keep all yields in annual decimals and use h=0.25 in log-price recursion.
    Nq = len(FINE_GRID)
    A_q = np.zeros(Nq + 1, dtype=float)
    B_q = np.zeros((Nq + 1, 5), dtype=float)

    # One-quarter bond log price: p_{1,t} = -h * r_t
    A_q[1] = -H * delta_0
    B_q[1, :] = -H * delta_1

    for n in range(1, Nq):
        Bn = B_q[n, :]
        B_q[n + 1, :] = Bn @ Phi - Bn @ lambda_1 - H * delta_1
        A_q[n + 1] = A_q[n] + Bn @ mu - Bn @ lambda_0 + 0.5 * (Bn @ Sigma @ Bn.T) - H * delta_0

    # Bond log-price and annualized yield
    y_q = np.full((T, Nq), np.nan, dtype=float)
    for n in range(1, Nq + 1):
        p_n_t = A_q[n] + X @ B_q[n, :]
        y_q[:, n - 1] = -p_n_t / (n * H)

    # -------------------- 3.8 term premia --------------------
    TP = y - y_q

    tp_cols = []
    tp_data = {}
    for target in TP_TARGET_YEARS:
        idx = np.where(np.isclose(FINE_GRID, target))[0]
        if len(idx) != 1:
            raise ValueError(f"Target maturity {target}Y not found on quarterly grid.")
        idx = idx[0]
        col = f"TP{int(round(target))}Y"
        tp_cols.append(col)
        tp_data[col] = TP[:, idx] * 100.0  # basis points per user spec

    tp_df = pd.DataFrame({"date": to_date_str(dates)})
    for c in tp_cols:
        tp_df[c] = tp_data[c]

    # -------------------- diagnostics --------------------
    eigvals = np.linalg.eigvals(Phi)
    tp10 = tp_df["TP10Y"].values
    tp10_mean = float(np.nanmean(tp10))
    tp10_std = float(np.nanstd(tp10))

    print("\n" + "=" * 70)
    print(f"DIAGNOSTICS - {country_name}")
    print("=" * 70)
    print(f"Sample period: {tp_df['date'].iloc[0]} to {tp_df['date'].iloc[-1]}")
    print(f"N observations: {len(tp_df)}")
    print(f"Cumulative variance explained by 5 PCs: {cum_var[-1] * 100:.2f}%")
    print(f"Phi eigenvalues: {eigvals}")
    print(f"TP10Y mean (bp): {tp10_mean:.4f}")
    print(f"TP10Y std (bp): {tp10_std:.4f}")
    print(f"Bootstrap warnings (P(n)<=0): {warnings_step1}")
    if (tp10_mean < -200.0) or (tp10_mean > 500.0):
        print("!!! LOUD WARNING: TP10Y mean is outside [-200bp, +500bp] sanity range !!!")

    return tp_df


# ============================================================================
# PIPELINE ORCHESTRATION
# ============================================================================


def main():
    ensure_dirs()

    print("=" * 70)
    print("ACM TERM PREMIUM ESTIMATION - FULL REWRITE")
    print("=" * 70)

    data, col_codes, date_col = load_input()

    # STEP 1 outputs
    step1_results = {}

    for country_name, prefix in COUNTRY_PREFIXES.items():
        print("\n" + "=" * 70)
        print(f"STEP 1 - BOOTSTRAP KNOT CURVES: {country_name}")
        print("=" * 70)

        res = bootstrap_country_knot_curves(data, date_col, country_name, prefix)
        step1_results[country_name] = res

        knot_path = {
            "UK": "ACM/ZC_UK.csv",
            "GERMANY": "ACM/ZC_GER.csv",
            "FRANCE": "ACM/ZC_FRA.csv",
        }[country_name]
        res["knot_df"].to_csv(knot_path, index=False)
        print(f"Saved: {knot_path} ({len(res['knot_df'])} rows)")

    # STEP 2 outputs
    fine_results = {}
    for country_name in COUNTRY_PREFIXES.keys():
        print("\n" + "=" * 70)
        print(f"STEP 2 - QUARTERLY SPLINE GRID: {country_name}")
        print("=" * 70)

        fine_matrix, fine_df = interpolate_fine_grid(step1_results[country_name])
        fine_results[country_name] = {
            "dates": step1_results[country_name]["dates"],
            "fine": fine_matrix,
            "fine_df": fine_df,
        }

        fine_path = {
            "UK": "ACM/ZC_FINE_UK.csv",
            "GERMANY": "ACM/ZC_FINE_GER.csv",
            "FRANCE": "ACM/ZC_FINE_FRA.csv",
        }[country_name]
        fine_df.to_csv(fine_path, index=False)
        print(f"Saved: {fine_path} ({len(fine_df)} rows)")

    # STEP 3 outputs
    for country_name in COUNTRY_PREFIXES.keys():
        print("\n" + "=" * 70)
        print(f"STEP 3 - ACM ESTIMATION: {country_name}")
        print("=" * 70)

        tp_df = acm_country_from_fine(
            country_name=country_name,
            dates=fine_results[country_name]["dates"],
            y_fine=fine_results[country_name]["fine"],
            warnings_step1=step1_results[country_name]["warnings_p_nonpos"],
        )

        tp_path = {
            "UK": "data/uk/control_variables/ACMTermPremium.csv",
            "GERMANY": "data/germany/control_variables/ACMTermPremium.csv",
            "FRANCE": "data/france/control_variables/ACMTermPremium.csv",
        }[country_name]
        tp_df.to_csv(tp_path, index=False)
        print(f"Saved: {tp_path} ({len(tp_df)} rows)")

    print("\n" + "=" * 70)
    print("PIPELINE COMPLETE")
    print("=" * 70)


if __name__ == "__main__":
    main()
