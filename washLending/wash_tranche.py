"""Tranche waterfall for the wash-lending equilibrium.

Plots attachment points, loss allocation, and loss probabilities
for the implicit CDO structure described in washLending.tex.

Usage:
    uv run wash_tranche.py                # default Spark-like params
    uv run wash_tranche.py --M 15 --lltv 0.945  # Morpho high-LLTV
"""

from __future__ import annotations

import argparse
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import norm


# ---------------------------------------------------------------------------
# Attachment point
# ---------------------------------------------------------------------------

def attachment_point(M: float, lltv: float) -> float:
    """Collateral-loss fraction that wipes out the junior tranche (homogeneous)."""
    return 1.0 - M * lltv / (1.0 + M)


def attachment_point_heterogeneous(
    Ms: np.ndarray, es: np.ndarray, lltv: float
) -> float:
    """Attachment point for heterogeneous borrowers.

    Finds the smallest delta such that sum of shortfalls = sum of equities.
    """
    E_J = es.sum()
    # Sort by liquidation threshold (ascending delta_i* = most leveraged first)
    delta_stars = 1.0 - Ms / (lltv * (1.0 + Ms))
    order = np.argsort(delta_stars)

    # Binary search for delta_A where cumulative_loss(delta) = E_J
    def cumulative_loss(delta: float) -> float:
        shortfalls = es * np.maximum(0.0, delta * (1.0 + Ms) - 1.0)
        return shortfalls.sum()

    lo, hi = 0.0, 1.0
    for _ in range(200):
        mid = (lo + hi) / 2.0
        if cumulative_loss(mid) < E_J:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2.0


# ---------------------------------------------------------------------------
# Loss waterfall
# ---------------------------------------------------------------------------

def loss_waterfall(
    delta: np.ndarray, M: float, lltv: float, E_J: float, E_S: float
) -> tuple[np.ndarray, np.ndarray]:
    """Compute junior and senior losses as a function of collateral shock delta."""
    total_loss = E_J * np.maximum(0.0, delta * (1.0 + M) - 1.0) / 1.0
    # Junior absorbs up to E_J
    junior_loss = np.minimum(total_loss, E_J)
    senior_loss = np.maximum(total_loss - E_J, 0.0)
    # Cap senior at E_S
    senior_loss = np.minimum(senior_loss, E_S)
    return junior_loss, senior_loss


# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------

def plot_waterfall(
    M: float,
    lltv: float,
    E_J: float = 100.0,
    E_S: float = 400.0,
    sigma: float = 0.80,
    tau_days: float = 7.0,
    out: str | None = None,
):
    delta = np.linspace(0, 1, 500)
    delta_A = attachment_point(M, lltv)

    junior_loss, senior_loss = loss_waterfall(delta, M, lltv, E_J, E_S)

    fig, axes = plt.subplots(1, 3, figsize=(15, 4.5))

    # Panel 1: Loss waterfall
    ax = axes[0]
    ax.fill_between(delta, 0, junior_loss, alpha=0.6, label="Junior loss", color="C1")
    ax.fill_between(delta, junior_loss, junior_loss + senior_loss, alpha=0.6,
                    label="Senior loss", color="C3")
    ax.axvline(delta_A, color="k", ls="--", lw=1, label=f"Attach. pt = {delta_A:.1%}")
    ax.set_xlabel(r"Collateral shock $\delta$")
    ax.set_ylabel("Loss (S)")
    ax.set_title("Loss waterfall")
    ax.legend(fontsize=8)

    # Panel 2: Attachment point vs leverage
    ax = axes[1]
    Ms = np.linspace(0.5, 20, 200)
    for ll, ls_, c in [(0.83, "-", "C0"), (0.86, "--", "C1"), (0.945, ":", "C3")]:
        ap = 1.0 - Ms * ll / (1.0 + Ms)
        ax.plot(Ms, ap, ls=ls_, color=c, label=f"LLTV = {ll}")
    ax.axhline(0, color="gray", lw=0.5)
    ax.set_xlabel(r"Multiplier $M$")
    ax.set_ylabel(r"Attachment point $\delta_A$")
    ax.set_title(r"$\delta_A$ vs leverage")
    ax.legend(fontsize=8)

    # Panel 3: Loss probability vs horizon
    ax = axes[2]
    taus = np.linspace(1, 30, 100)  # days
    for ll, ms, ls_, c in [
        (0.83, 4, "-", "C0"),
        (0.86, 8, "--", "C1"),
        (0.945, 15, ":", "C3"),
    ]:
        da = attachment_point(ms, ll)
        prob = norm.cdf(-da / (sigma * np.sqrt(taus / 365)))
        ax.semilogy(taus, prob, ls=ls_, color=c,
                    label=f"M={ms}, LLTV={ll}, $\\delta_A$={da:.1%}")
    ax.set_xlabel("Horizon (days)")
    ax.set_ylabel(r"$\Pr[\delta > \delta_A]$")
    ax.set_title(rf"Senior breach prob ($\sigma$={sigma:.0%})")
    ax.legend(fontsize=7)

    fig.suptitle(
        f"Wash-lending tranche structure  |  M={M}, LLTV={lltv}, "
        f"$\\delta_A$={delta_A:.1%}",
        fontsize=11,
    )
    plt.tight_layout()

    if out:
        fig.savefig(out, dpi=150, bbox_inches="tight")
        print(f"Saved to {out}")
    else:
        out_path = Path(__file__).with_suffix(".png")
        fig.savefig(out_path, dpi=150, bbox_inches="tight")
        print(f"Saved to {out_path}")
    plt.close(fig)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    p = argparse.ArgumentParser(description="Wash-lending tranche waterfall")
    p.add_argument("--M", type=float, default=4.0, help="Equilibrium multiplier")
    p.add_argument("--lltv", type=float, default=0.83, help="Liquidation LTV")
    p.add_argument("--E-junior", type=float, default=100.0, help="Junior equity")
    p.add_argument("--E-senior", type=float, default=400.0, help="Senior equity")
    p.add_argument("--sigma", type=float, default=0.80, help="Collateral vol (ann.)")
    p.add_argument("--tau", type=float, default=7.0, help="Horizon in days")
    p.add_argument("--out", type=str, default=None, help="Output file path")
    args = p.parse_args()

    print(f"M={args.M}, LLTV={args.lltv}")
    da = attachment_point(args.M, args.lltv)
    print(f"Attachment point: delta_A = {da:.4f} ({da:.1%} collateral drop)")
    print(f"At M=15, LLTV=0.945: delta_A = {attachment_point(15, 0.945):.4f}")

    plot_waterfall(
        M=args.M,
        lltv=args.lltv,
        E_J=args.E_junior,
        E_S=args.E_senior,
        sigma=args.sigma,
        tau_days=args.tau,
        out=args.out,
    )


if __name__ == "__main__":
    main()
