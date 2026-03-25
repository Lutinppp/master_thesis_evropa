"""Build country forward-rate, long-rate, and short-rate control variables from EGB par yields.

Outputs (overwritten):
  - data/uk/dependent_variables/gsw_forward_rates.csv
  - data/uk/dependent_variables/long_rate.csv
  - data/france/dependent_variables/gsw_forward_rates.csv
  - data/france/dependent_variables/long_rate.csv
  - data/germany/dependent_variables/gsw_forward_rates.csv
  - data/germany/dependent_variables/long_rate.csv
  - data/uk/control_variables/ST_rate.csv       (3M benchmark yield, equiv. of US TB3MS)
  - data/france/control_variables/ST_rate.csv
  - data/germany/control_variables/ST_rate.csv
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd


INPUT_FILE = Path("data/EGBs.csv")

COUNTRIES = {
	"uk": "TRUK",
	"germany": "TRBD",
	"france": "TRFR",
}

SUFFIX_TO_MATURITY = {
	"3MT": 0.25,
	"6MT": 0.50,
	"1YT": 1.00,
	"2YT": 2.00,
	"3YT": 3.00,
	"4YT": 4.00,
	"5YT": 5.00,
	"7YT": 7.00,
	"10T": 10.00,
	"15T": 15.00,
	"20T": 20.00,
	"25T": 25.00,
}

BOOTSTRAP_MATS = [2.0, 3.0, 4.0, 5.0, 7.0, 10.0, 15.0, 20.0, 25.0]


@dataclass
class CountryPanel:
	dates: pd.Series
	par: pd.DataFrame


def load_egbs(path: Path) -> tuple[pd.Series, list[str], pd.DataFrame]:
	raw = pd.read_csv(path, sep=";", header=None, dtype=str)
	code_row = raw.iloc[4].fillna("").tolist()
	data_rows = raw.iloc[6:].reset_index(drop=True)

	date_series = pd.to_datetime(data_rows.iloc[:, 0], format="%d/%m/%Y", errors="coerce")
	values = data_rows.iloc[:, 1:].replace("", np.nan).apply(pd.to_numeric, errors="coerce")
	values.columns = code_row[1:]
	return date_series, code_row, values


def build_country_panel(dates: pd.Series, values: pd.DataFrame, prefix: str) -> CountryPanel:
	cols = []
	mats = []
	for col in values.columns:
		if not isinstance(col, str) or not col.startswith(prefix):
			continue
		suffix = col[len(prefix) :]
		if suffix in SUFFIX_TO_MATURITY:
			cols.append(col)
			mats.append(SUFFIX_TO_MATURITY[suffix])

	order = np.argsort(mats)
	sorted_cols = [cols[i] for i in order]
	sorted_mats = [mats[i] for i in order]

	panel = values[sorted_cols].copy() / 100.0  # percent -> decimal
	panel.columns = sorted_mats
	return CountryPanel(dates=dates, par=panel)


def linear_interp_with_anchor(k: float, known: dict[float, float], anchor_t: float, anchor_y: float) -> float:
	if k in known:
		return float(known[k])

	xs = sorted(known.keys())
	lower = None
	upper = None
	for x in xs:
		if x < k:
			lower = x
		elif x > k:
			upper = x
			break

	if lower is None and upper is None:
		return float(anchor_y)
	if lower is None:
		return float(known[upper])
	if upper is None:
		if anchor_t > lower and k < anchor_t:
			y0 = float(known[lower])
			return y0 + (anchor_y - y0) * ((k - lower) / (anchor_t - lower))
		return float(known[lower])

	y0 = float(known[lower])
	y1 = float(known[upper])
	return y0 + (y1 - y0) * ((k - lower) / (upper - lower))


def bootstrap_row(par_row: pd.Series) -> dict[float, float]:
	known: dict[float, float] = {}

	for t in (0.25, 0.5, 1.0):
		y = par_row.get(t, np.nan)
		if np.isfinite(y):
			known[t] = float(y)

	for n in BOOTSTRAP_MATS:
		c = par_row.get(n, np.nan)
		if not np.isfinite(c):
			continue

		c = float(c)
		df_sum = 0.0
		ok = True
		for k in range(1, int(n)):
			zk = linear_interp_with_anchor(float(k), known, n, c)
			if not np.isfinite(zk) or zk <= -0.999:
				ok = False
				break
			df_sum += 1.0 / ((1.0 + zk) ** k)

		if not ok:
			continue

		denom = 1.0 + c
		if denom <= 0.0:
			continue

		df_n = (1.0 - c * df_sum) / denom
		if not np.isfinite(df_n) or df_n <= 0.0:
			continue

		z_n = df_n ** (-1.0 / n) - 1.0
		if np.isfinite(z_n) and z_n > -0.999:
			known[n] = float(z_n)

	return known


def compute_forward(z_a: pd.Series, z_b: pd.Series, a: float, b: float) -> pd.Series:
	ratio = ((1.0 + z_b) ** b) / ((1.0 + z_a) ** a)
	fwd = ratio ** (1.0 / (b - a)) - 1.0
	fwd[(~np.isfinite(fwd)) | (ratio <= 0)] = np.nan
	return fwd


def build_outputs(country: str, panel: CountryPanel) -> tuple[pd.DataFrame, pd.DataFrame]:
	z5 = []
	z10 = []
	z15 = []

	for _, row in panel.par.iterrows():
		known = bootstrap_row(row)
		z5.append(known.get(5.0, np.nan))
		z10.append(known.get(10.0, np.nan))
		z15.append(known.get(15.0, np.nan))

	out = pd.DataFrame(
		{
			"observation_date": panel.dates,
			"z5": z5,
			"z10": z10,
			"z15": z15,
		}
	)
	out = out.dropna(subset=["observation_date"]).copy()
	out = out.sort_values("observation_date")

	out["fwd_5_10y"] = compute_forward(out["z5"], out["z10"], 5.0, 10.0) * 100.0
	out["fwd_10_15y"] = compute_forward(out["z10"], out["z15"], 10.0, 15.0) * 100.0
	out["value"] = out["z10"] * 100.0

	forward_df = out[["observation_date", "fwd_5_10y", "fwd_10_15y"]].dropna(
		subset=["fwd_5_10y", "fwd_10_15y"]
	)
	long_df = out[["observation_date", "value"]].dropna(subset=["value"])

	forward_df["observation_date"] = forward_df["observation_date"].dt.strftime("%Y-%m-%d")
	long_df["observation_date"] = long_df["observation_date"].dt.strftime("%Y-%m-%d")

	return forward_df, long_df


def write_outputs(country: str, forward_df: pd.DataFrame, long_df: pd.DataFrame) -> None:
	base = Path("data") / country / "dependent_variables"
	base.mkdir(parents=True, exist_ok=True)

	forward_path = base / "gsw_forward_rates.csv"
	long_path = base / "long_rate.csv"

	forward_df.to_csv(forward_path, index=False)
	long_df.to_csv(long_path, index=False)

	print(
		f"[{country}] wrote {forward_path} ({len(forward_df)} rows) and {long_path} ({len(long_df)} rows)"
	)


def write_st_rate(country: str, panel: CountryPanel) -> None:
	"""Write the 3-month par yield as control_variables/ST_rate.csv (percent)."""
	if 0.25 not in panel.par.columns:
		print(f"[{country}] WARNING: 3M column not found in panel; ST_rate.csv not written.")
		return

	out = pd.DataFrame(
		{
			"observation_date": panel.dates,
			"value": panel.par[0.25] * 100.0,  # decimal -> percent
		}
	)
	out = out.dropna(subset=["observation_date", "value"]).copy()
	out = out.sort_values("observation_date")
	out["observation_date"] = out["observation_date"].dt.strftime("%Y-%m-%d")

	out_dir = Path("data") / country / "control_variables"
	out_dir.mkdir(parents=True, exist_ok=True)
	out_path = out_dir / "ST_rate.csv"
	out.to_csv(out_path, index=False)
	print(f"[{country}] wrote {out_path} ({len(out)} rows)")


def main() -> None:
	dates, _, values = load_egbs(INPUT_FILE)
	for country, prefix in COUNTRIES.items():
		panel = build_country_panel(dates, values, prefix)
		forward_df, long_df = build_outputs(country, panel)
		write_outputs(country, forward_df, long_df)
		write_st_rate(country, panel)


if __name__ == "__main__":
	main()
