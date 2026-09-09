"""Local secured-loan losses and liquidation thresholds for washLending.tex.

These calculations hold debt fixed, take the collateral markdown as exogenous,
and assume no intervening liquidation. They do not model consolidated vault NAV.

Usage:
    python3 wash_tranche.py
    python3 wash_tranche.py --M 15 --lltv 0.945
"""
from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np


def attachment_point(M: float) -> float:
    """Equity-wipeout threshold; lender shortfall starts above it for M > 0."""
    if not np.isfinite(M) or M < 0:
        raise ValueError("Multiplier must be finite and nonnegative")
    return 1.0 / (1.0 + M)


def liquidation_threshold(M: float, lltv: float) -> float:
    """Trigger markdown for a positive-debt, initially healthy position."""
    if not np.isfinite(lltv) or not 0 < lltv < 1:
        raise ValueError("LLTV must lie strictly between zero and one")
    if not np.isfinite(M) or not 0 < M <= lltv / (1 - lltv):
        raise ValueError("Multiplier must be positive and initially feasible")
    return 1.0 - M / (lltv * (1.0 + M))


def attachment_point_heterogeneous(Ms: np.ndarray, es: np.ndarray) -> float:
    """First local lender-loss threshold; unused equity is not cross-pledged."""
    Ms, es = np.asarray(Ms, dtype=float), np.asarray(es, dtype=float)
    if Ms.ndim != 1 or Ms.shape != es.shape:
        raise ValueError("Multipliers and equities must be matching vectors")
    if (not np.all(np.isfinite(Ms)) or not np.all(np.isfinite(es))
            or np.any(Ms < 0) or np.any(es < 0)):
        raise ValueError("Multipliers and equities must be finite and nonnegative")
    active = (Ms > 0) & (es > 0)
    return float(np.min(1 / (1 + Ms[active]))) if np.any(active) else float("inf")


def loss_waterfall(delta: np.ndarray, M: float, equity: float
                   ) -> tuple[np.ndarray, np.ndarray]:
    """Borrower equity loss and lender shortfall, both in loan-asset units."""
    attachment_point(M)
    delta = np.asarray(delta, dtype=float)
    if np.any(~np.isfinite(delta)) or np.any((delta < 0) | (delta > 1)):
        raise ValueError("Collateral markdown must lie between zero and one")
    if not np.isfinite(equity) or equity <= 0:
        raise ValueError("Equity must be finite and positive")
    collateral_loss = delta * (1 + M) * equity
    return np.minimum(collateral_loss, equity), np.maximum(collateral_loss - equity, 0)


def plot_waterfall(M: float, lltv: float, equity: float = 100,
                   out: str | None = None) -> Path:
    trigger = liquidation_threshold(M, lltv)
    attachment = attachment_point(M)
    delta = np.linspace(0, 1, 601)
    junior, lender = loss_waterfall(delta, M, equity)
    fig = plt.figure(figsize=(7, 6.5), layout="constrained")
    grid = fig.add_gridspec(2, 2)
    axes = [fig.add_subplot(grid[0, 0]), fig.add_subplot(grid[0, 1]),
            fig.add_subplot(grid[1, :])]
    ax = axes[0]
    ax.stackplot(delta, junior, lender, labels=["Borrower equity loss", "Lender shortfall"],
                 colors=["#e7a44b", "#bd4f53"], alpha=0.8)
    ax.axvline(trigger, color="#35618f", linestyle=":", label=f"Liquidation: {trigger:.1%}")
    ax.axvline(attachment, color="black", linestyle="--", label=f"Equity wipeout: {attachment:.1%}")
    ax.set(xlabel=r"Collateral markdown $\delta$", ylabel="Loss (loan asset)",
           title=f"Local loss allocation (m={M:g})")
    ax.legend(fontsize=8, loc="upper left")

    leverage = np.linspace(0, 20, 401)
    ax = axes[1]
    ax.plot(leverage, 1 / (1 + leverage), color="black", label="Equity wipeout")
    for ll, color in [(0.83, "#35618f"), (0.915, "#6d8a4d"), (0.945, "#bd4f53")]:
        # Never draw a liquidation curve for initially infeasible leverage.
        ms = np.linspace(0.001, ll / (1 - ll), 201)
        ax.plot(ms, 1 - ms / (ll * (1 + ms)), color=color, linestyle="--",
                label=f"Liquidation, LLTV={ll:g}")
    ax.set(xlabel=r"Debt/equity multiplier $m$", ylabel="Collateral markdown",
           title="Thresholds versus leverage", ylim=(0, 1))
    ax.legend(fontsize=8)

    ax = axes[2]
    scenarios = [(4, 0.83), (6, 0.915), (15, 0.945)]
    x = np.arange(len(scenarios))
    triggers = [100 * liquidation_threshold(m, ll) for m, ll in scenarios]
    attachments = [100 * attachment_point(m) for m, _ in scenarios]
    for bars in [ax.bar(x - 0.18, triggers, 0.36, label="Liquidation", color="#35618f"),
                 ax.bar(x + 0.18, attachments, 0.36, label="Equity wipeout", color="#e7a44b")]:
        ax.bar_label(bars, fmt="%.2f", fontsize=8)
    ax.set(xticks=x, xticklabels=[f"m={m}\nLLTV={ll}" for m, ll in scenarios],
           ylabel="Collateral markdown (%)", title="Illustrative position buffers", ylim=(0, 25))
    ax.legend(fontsize=8, loc="upper right")
    fig.suptitle("Fixed debt, exogenous collateral markdown, no intervening liquidation", fontsize=9)
    path = Path(out) if out else Path(__file__).with_suffix(".png")
    fig.savefig(path, dpi=180)
    plt.close(fig)
    return path


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--M", type=float, default=4, help="Debt/equity multiplier")
    parser.add_argument("--lltv", type=float, default=0.83, help="Liquidation LTV")
    parser.add_argument("--equity", type=float, default=100, help="Borrower equity in loan asset")
    parser.add_argument("--out", type=str, default=None, help="Output image path")
    args = parser.parse_args()
    try:
        path = plot_waterfall(args.M, args.lltv, args.equity, args.out)
    except ValueError as exc:
        parser.error(str(exc))
    print(f"Liquidation trigger: {liquidation_threshold(args.M, args.lltv):.4%}")
    print(f"Equity wipeout: {attachment_point(args.M):.4%}")
    print(f"Saved to {path}")


if __name__ == "__main__":
    main()
