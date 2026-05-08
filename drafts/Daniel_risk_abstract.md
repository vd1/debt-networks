---
title: Abstract

---

# [RFC] A Morpho-compatible DeFi Collateral Asset Risk Model

## Abstract

### The Curationship Model and Its Promises

Morpho has redefined lending protocol design by separating infrastructure from risk management. Anyone can now spin up a vault in a few clicks, define collateral exposure, set supply caps, and offer curated lending strategies to depositors. The protocol provides the rails; the curator provides the judgment.

The closest analogy in traditional finance is an asset manager: it both sets strategy and manage risk, but vault curators operate non-custodially. Execution is automated through smart contracts and offchain agents, and users can deposit and withdraw at will without anyone being able to prevent it.

The model's appeal is real. But its rapid expansion has exposed a structural gap: **the rigor of collateral risk assessment varies enormously across curators, and depositors have consistently lacked the tools to evaluate it**.

### A Growing Pattern of Confidence Failure

#### When Curator Risk Became Visible

Three recent episodes made curator collateral risk impossible to ignore.
- January 10, 2025: USD0++ / Usual Protocol. Usual abruptly changed USD0++'s redemption mechanism, replacing unconditional 1:1 redemption with an $0.87 floor price without meaningful advance warning. Vault curators on Morpho had hardcoded USD0++: USDC prices at 1:1 parity rather than at market rate. The result was dramatic: interest rates spiked, massive liquidations swept through lending markets, and TVL in the affected vault halved to in hours. Some curators exited their positions ahead of the announcement, prompting public accusations of insider trading. The episode exposed, simultaneously, a protocol that changed its terms without warning, oracles that didn't reflect market reality, and depositors with no way of knowing any of it.
- November 2025: Stream Finance / xUSD. Curators had routed USDC deposits into leverage loops backed by the synthetic stablecoin xUSD, leaving an estimated $285M–$700M at risk across multiple lending protocols when its oracle refused to update. Curators who had publicly claimed zero exposure were later found to have significant positions when the protocol collapsed.
- March 2026: Resolv / USR Exploit. An attacker exploited a flaw in Resolv's USR minting contract, creating approximately 80 million unbacked tokens from roughly $200,000 in USDC. The oracle was hardcoded and never repriced: wstUSR was marked at $1.13 while trading at approximately $0.63 on secondary markets, allowing traders to post cheap collateral at inflated valuations and drain USDC from lending vaults. **Fifteen Morpho vaults were impacted**, and some curators' automated systems continued providing liquidity into compromised markets hours after the exploit began.

Collateral assumptions broke under stress, liquidity evaporated, and depositors were left holding losses that a rigorous upfront risk assessment might have anticipated.

## Motivation

These incidents aforementioned share a common thread: they were not fundamentally unpredictable. The risks were present onchain before each failure, in oracle configurations, minting mechanics, governance structures, liquidity depth, and backing ratios. What was missing was **a systematic framework to read those signals, quantify them, and translate them into actionable risk parameters for curators and depositors alike**.

Existing players proposed an interesting first framework to rate assets. It was a necessary starting point. But asset rating is not risk management in itself. Rating tells you a score at a point in time; risk management tells you how that score changes under stress, how it correlates with other positions in a vault, and what it implies for liquidation probability at a given LTV. **These are the questions a curator must answer and today, they answer them inconsistently, opaquely, and often only after something has already broken**.

This RFC proposes a dedicated, quantitative, risk framework for Morpho collateral assets. Its goals are concrete:
- **Continuity**: risk scores update in real time from onchain data, not from quarterly manual reviews.
- **Comparability**: a score of 0.8 on a stablecoin and 0.8 on a liquid staking token mean the same thing, e.g., current volatility is highly abnormal relative to that asset's own history.
- **Auditability**: every output is traceable to a specific onchain read, a calibrated parameter, and a documented methodology.
- **Actionability**: the framework produces not just scores but probability of default estimates conditioned on actual vault LTV structures, the number a curator needs to set caps and the number a depositor needs to assess exposure.

The Morpho ecosystem has demonstrated that permissionless curation can scale. This RFC proposes the risk infrastructure that scaling responsibly requires.

## Prior Art: Credora by RedStone

The most visible effort to date is Credora (acquired by RedStone in September 2025), which introduced consensus-based risk ratings on Morpho in March 2025. Credora's contribution was genuine and necessary: it established that DeFi vault ratings could be surfaced to depositors directly in the protocol interface and forced a long-overdue conversation about risk transparency. We acknowledge this pioneering work and build on the foundation it established.

As the ecosystem has matured and collateral complexity has grown, however, several structural limitations of the Credora framework have become apparent. These are not criticisms of execution quality but of architectural choices that constrain what the framework can express.

**Continuous model vs. binary loss threshold**: Credora's core metric, the Probability of Significant Loss (PSL), asks whether bad debt will exceed a fixed 1% of principal, a binary question applied uniformly regardless of asset type, LLTV, or market structure. Our model computes a continuous distance-to-default (DD): how far the asset's stress-adjusted value sits from the endogenous barrier implied by the market's own LTV and liquidation structure, scaled by time-varying effective volatility. The output moves continuously with every block, giving curators a leading indicator rather than a lagging binary. A DD of 1.2 is meaningfully worse than a DD of 2.5, there is no threshold below which the model goes silent.

**Qualitative inputs in the scoring loop**: Credora blends quantitative simulations with expert judgment through a Consensus Ratings Protocol where human reviewers contribute qualitative assessments. While this mirrors traditional credit agency practice, it introduces irreproducibility: two independent observers cannot arrive at the same rating from the same onchain state. Our model requires zero qualitative inputs. Every factor score is derived from a documented onchain read, and any observer with an archive node can reproduce any output at any block height.

**Independence assumption between risk dimensions**: Credora assesses price risk, oracle risk, and protocol risk through separate modifier layers applied sequentially, implicitly treating risk dimensions as independent. In practice they are highly correlated precisely when it matters most: oracle staleness spikes during high-volatility periods, liquidity evaporates alongside price crashes, and exploits trigger simultaneous depeg and liquidity drain. Our model captures these correlations explicitly through a Vasicek one-factor covariance structure calibrated against historical DeFi stress events. The cross-terms, the "everything goes wrong at once" scenario, are where true tail risk lives, and omitting them systematically underestimates it.

**No recursive dependency unwrapping**: Credora rates assets as single units. A token like wsrUSD receives a rating, but the framework does not recursively decompose it into its underlying constituents. The concentration risk, circular reflexivity, and compounding smart contract risk that our wsrUSD analysis (see Deep Risk Decomposition — wsrUSD (Reservoir) section, below) surfaced remain invisible. Our dependency unwrapping engine traces every wrapper, LP position, lending market, and pledge relationship programmatically, aggregating volatilities recursively through the full dependency graph.

**No governance and mutation risk factor**: Credora's framework does not include a dedicated factor for governance attack vectors, admin key abuse, timelock adequacy, or parameter mutation risk, precisely the category that drove the USD0++ and Resolv/USR incidents. Our model's Factor Z₆ (detailed below) reads timelock duration, multisig threshold ratios, admin transaction frequency, pause capability, and upgrade authority structure directly from onchain contract state.

**Opt-in visibility and update frequency**: Credora ratings appear on Morpho only when the vault curator explicitly opts in, creating a structural selection bias: curators with unfavourable ratings have no incentive to display them, and depositors in unrated vaults receive no risk signal at all. Ratings are also refreshed weekly at best, whereas the risk events that matter unfold within minutes. Our model updates per-block for market and oracle data, every five minutes for liquidity, hourly for counterparty and governance metrics, and immediately on contract events, with a circuit breaker forcing full recomputation on any return exceeding 3σ within a five-minute window.

## Description of the DeFi Collateral Asset Risk Model

The model presented is a fully quantitative, onchain-native framework for assessing the risk of any EVM collateral asset. **It requires no manual categorisation, contains no hardcoded parameters, and updates in real time from blockchain data**. A complete mathematical treatment including all equations, calibration procedures, and backtesting methodology is available in the full technical reference paper: [DeFi Collateral Asset Risk Model — Mathematical Reference v3.0](https://docsend.com/v/63dkk/defi-collat-asset-risk-model).

### Two Outputs, One Framework

The model produces two distinct but related outputs from the same underlying structure:
1. **Intrinsic Risk Grade ($o_{e}$)**: how risky an asset is to hold, independent of any lending context. This is expressed as an annualised effective volatility and mapped to a rating scale from **AAA** to **CCC** based on empirical quantiles across all monitored assets.
2. **Contextual Probability of Default (PDT)**: the probability that the asset causes a liquidation failure in a specific Morpho market over a given horizon T (1-day, 7-day, 30-day). This activates when the asset is deployed as collateral against a specific LTV structure and debt barrier.

The separation matters: a curator needs $o_{e}$ to decide whether to list an asset at all, and PDT to decide what cap and LLTV to set once listed.

### Six On-Chain Risk Factors

The model decomposes risk into six factors, each scored on a continuous [0, 1] scale derived entirely from onchain reads. All factor scores are cross-asset comparable by construction. Z = 0.9 on a stablecoin and Z = 0.9 on an LST both mean the same thing: current conditions are highly abnormal relative to that asset's own history.
- **Factor 1: Market Risk (Z₁)**: measures price and peg volatility. Computed from Chainlink spot prices and DEX TWAPs, blended across multiple time horizons, and normalised against the asset's own rolling volatility history. For stablecoins, peg deviation replaces raw price; for LSTs, exchange rate returns are used.
- **Factor 2: Counterparty Risk (Z₂)**: captures issuer, custodian, and validator risk. Metrics include holder concentration (Gini coefficient), supply velocity, backing ratios, market discounts, validator centralisation for LSTs, reserve health for CDP stablecoins, and redemption queue pressure. The applicable metric set is determined automatically through contract bytecode introspection no manual asset categorisation is required.
- **Factor 3: Smart Contract Risk (Z₃)**: estimates exploit probability using contract age, cumulative TVL (Lindy effect), bytecode complexity, upgrade recency and frequency, and similarity to a reference database of approximately 3,050 historically exploited contracts. This factor is mostly static and updates only on contract upgrade events.
- **Factor 4: Liquidity Risk (Z₄)**: measures exit depth and friction using position-size-aware Conditional Liquidity-at-Risk (LiqAR). Slippage is measured at a threshold calibrated to the asset's own volatility, conditioned on stress periods, and computed for the actual fund position size, not a generic notional.
- **Factor 5: Oracle Risk (Z₅)**: assesses price feed reliability through update frequency gaps, deviation between Chainlink prices and DEX TWAPs, and transmitter count. Oracle staleness under market stress is captured through correlation with Factor 1 in the model structure rather than as a separate adjustment.
- **Factor 6: Governance / Mutation Risk (Z₆)**: New in v3.0. Captures the risk of adverse parameter changes, admin key abuse, or governance attacks. Metrics include timelock duration, multisig threshold ratios, admin transaction frequency, pause capability history, and upgrade authority structure all read directly from onchain contract state.

### From Factor Scores to Risk Grade

Factor scores feed into a multi-step aggregation that produces $o_{e}$: 

**Factor regression with GARCH residuals**: each asset's returns are regressed on daily factor innovations using a GJR-GARCH(1,1)-t model. This yields sensitivity coefficients (βi) for each factor, a time-varying residual volatility (σ̂t) that reacts to volatility clustering in real time, and a tail thickness parameter (ν̂).

**Factor risk contributions**: each factor's contribution to total risk is the product of its estimated sensitivity and its current volatility: si(t) = |β̂i| · σZi(t). This is updated daily and expressed in annualised return units.

**Regime-switching correlation**: the six factors co-move under stress. A Hidden Markov Model with three regimes (calm, transition, crisis) runs continuously on the composite factor signal, producing filtered regime probabilities ξk(t). These blend regime-specific Vasicek correlations continuously, no discrete threshold switches, no discontinuous jumps.

**Effective volatility ($o_{e}$)**: the final risk measure assembles three components: **systemic risk** (all factors moving together in a market-wide crash), **idiosyncratic risk** (asset-specific factor shocks), and **dynamic unexplained risk** from the GARCH residual. When model fit falls below an R² of 0.30, a conservatism multiplier is automatically applied. The model degrades gracefully rather than silently.

**Intrinsic rating**: $o_{e}$ is mapped to a seven-tier rating (AAA → CCC) using empirical quantiles of the cross-sectional distribution across all monitored assets, recalculated quarterly.

### Contextual Probability of Default

When an asset is deployed as collateral in a Morpho market, the model activates a Merton-style default layer. The effective collateral value is stress-adjusted using the position-size-aware LiqAR and the six factor scores. A distance-to-default (DD) is computed from the ratio of adjusted collateral value to the debt barrier implied by the market's LLTV. The probability of default is then derived using a Student-t distribution, chosen because Gaussian assumptions produce tail probabilities orders of magnitude too small under realistic stress conditions.

Three horizons are computed simultaneously: PD₁d, PD₇d, PD₃₀d. A conditional stress test variant is also available, computing PD given a systemic shock of specified severity (e.g. z = −2 for moderate crisis, z = −3 for severe), enabling portfolio-level stress testing across a curator's full vault.

### Universal Coverage and Continuous Calibration

The model is designed to work on any EVM asset without manual intervention. Contract introspection automatically determines which metrics apply to each asset. For newly listed tokens with insufficient history, a Bayesian prior centred on category-average coefficients ensures stable estimates from day one, converging to full MLE as data accumulates.
All parameters are estimated via Maximum Likelihood or Bayesian Information Criterion there are no hardcoded constants. Factor scores update per-block for market and oracle data, every five minutes for liquidity, hourly for counterparty and governance, and immediately on contract events. A circuit breaker forces full model recomputation on any five-minute return exceeding 3σ.

For the complete mathematical specification, including all equations, likelihood functions, calibration schedules, and backtesting framework, refer to the [DeFi Collateral Asset Risk Model — Mathematical Reference v3.0](https://docsend.com/v/63dkk/defi-collat-asset-risk-model).

## Development

### Status 

Development of the risk model is actively underway. The mathematical framework covering the six-factor decomposition, GJR-GARCH-t calibration, and HMM regime-switching is currently being developed and refined. As of today, the team has built and validated the **yield source identification engine** and the **yield tree decomposition**, the dependency unwrapping layer that recursively maps any EVM asset to its underlying on-chain constituents as illustrated by the reUSD and wsrUSD examples below.

Integration work has begun across the primary DeFi protocol categories relevant to Morpho collateral assets: Chainlink oracle feeds, Uniswap V3 and Curve DEX depth queries, Lido and other LST exchange rate contracts, Morpho market parameters, and EIP-1967 upgrade event monitoring.
The dependency unwrapping engine which recursively decomposes composed assets such as vault tokens, Pendle PT/YT wrappers, and LP positions into their underlying risk constituents is operational and producing structured output on live assets.

### First results

The model produces two complementary outputs for any Morpho collateral asset: a **dependency graph** mapping the full chain of onchain relationships (wrappers, LP positions, lending markets, and pledge structures), and a **recursive risk decomposition** translating that structure into quantified, factor-level risk findings. Two examples are provided below.

#### Dependency Graph — reUSD

The graph below illustrates the recursive dependency unwrapping engine applied to reUSD (Re Protocol). It maps every wrapper, LP position, Pendle market, and Morpho lending market that either contains or depends on reUSD, tracing the full chain of onchain risk from the top-level asset down to individual collateral markets.

![reUSD_neigh](https://hackmd.io/_uploads/rkw5Eug2We.png)

The graph is generated programmatically from structured JSON output produced by the engine. Each node represents a distinct onchain entity, edges represent wrapping, containment, or pledge relationships.

#### Deep Risk Decomposition — wsrUSD (Reservoir)

The following is a full model output on **wsrUSD**, illustrating the kind of structured, actionable analysis the framework produces on any Morpho collateral asset.

**Asset structure.** wsrUSD is an ERC-4626 wrapper (current exchange rate: 1.074) around rUSD, itself a synthetic stablecoin issued by Reservoir backed by four sources:
```
Wrapped Savings rUSD (wsrUSD) [4626] | rate: 1.074463
└── rUSD [Reservoir]
    ├── steakUSDC (Morpho) [V1] — $20M
    │   ├── cbBTC/USDC (86% LLTV) — $109,717,726.65 (52.8%) @ 1.89% APY
    │   │   ├── [collateral] cbBTC
    │   │   └── [borrow] USDC
    │   ├── WBTC/USDC (86% LLTV) — $68,514,103.88 (33.0%) @ 1.89% APY
    │   │   ├── [collateral] WBTC
    │   │   └── [borrow] USDC
    │   ├── wstETH/USDC (86% LLTV) — $29,464,000.85 (14.2%) @ 1.89% APY
    │   │   ├── [collateral] wstETH [4626] (rate: 1.231065)
    │   │   │   └── stETH [4626] (rate: 1.0)
    │   │   │       └── ETH
    │   │   └── [borrow] USDC
    │   ├── WETH/USDC (86% LLTV) — $39,277.73 (0.0%) @ 1.89% APY
    │   │   ├── [collateral] WETH
    │   │   └── [borrow] USDC
    │   └── IDLE/USDC (0% LLTV) — $0.00 (0.0%) @ 0.00% APY
    │       ├── [collateral] IDLE [IDLE]
    │       └── [borrow] USDC
    ├── [PSM] PSM-USDC — $500K
    ├── steakRUSD — $53K
    ├── Steakhouse High Yield — $6
    └── smokeUSDT [V1] — $2
        ├── wsrUSD/USDT (94% LLTV) — $39,551,744.73 (99.6%) @ 3.37% APY
        │   ├── [collateral] wsrUSD
        │   └── [borrow] USDT
        ├── sUSDe/USDT (92% LLTV) — $43,943.56 (0.1%) @ 3.51% APY
        │   ├── [collateral] sUSDe [4626] (rate: 1.226156)
        │   │   └── USDe
        │   └── [borrow] USDT
        ├── PT-cUSD-23JUL2026/USDT (92% LLTV) — $82,057.94 (0.2%) @ 3.36% APY
        │   ├── [collateral] PT-cUSD-23JUL2026 [PT]
        │   │   └── SY-cUSD [SY] (rate: 1.0)
        │   │       └── cUSD
        │   └── [borrow] USDT
        ├── weETH/USDT (86% LLTV) — $15,079.95 (0.0%) @ 3.36% APY
        │   ├── [collateral] weETH [4626] (rate: 1.09191)
        │   │   └── eETH [4626] (rate: 1.0)
        │   │       └── ETH
        │   └── [borrow] USDT
        ├── sUSDS/USDT (96% LLTV) — $0.00 (0.0%) @ 2.99% APY
        │   ├── [collateral] sUSDS [Sky] (rate: 1.092186)
        │   │   ├── [Lending] Spark — $3.60B (return: 4.53%)
        │   │   │   ├── DAI — $273M @ 2.68% APY
        │   │   │   ├── USDT — $261M @ 1.99% APY
        │   │   │   ├── USDS — $144M @ 2.55% APY
        │   │   │   ├── PYUSD — $100M @ 0.72% APY
        │   │   │   ├── USDC — $18M @ 4.14% APY
        │   │   │   ├── sUSDS — $3M
        │   │   │   ├── wstETH — $468K
        │   │   │   ├── WETH — $264K @ 1.64% APY
        │   │   │   ├── sDAI — $38K
        │   │   │   ├── weETH — $31K
        │   │   │   ├── ezETH — $14K
        │   │   │   ├── rETH — $11K
        │   │   │   ├── rsETH — $4K
        │   │   │   ├── LBTC — $3K
        │   │   │   ├── cbBTC — $396 @ 0.03% APY
        │   │   │   ├── WBTC — $362 @ 0.00% APY
        │   │   │   └── tBTC — $23
        │   │   ├── [RWA] Bloom/Grove — $2.94B
        │   │   ├── [RWA] Obex — $606M
        │   │   └── [underlying] USDS [Sky] (price: 1.464347)
        │   │       ├── [PSM] Lite-PSM USDC — $4.85B
        │   │       ├── Allocator: Spark — $3.60B
        │   │       ├── Allocator: Bloom/Grove — $2.94B
        │   │       ├── Allocator: Obex — $606M
        │   │       ├── [CDP] ETH-C (ETH) — $317M
        │   │       ├── [CDP] ETH-A (ETH) — $164M
        │   │       ├── [CDP] RWA002-A (RWA) — $34M
        │   │       ├── [CDP] WSTETH-A (wstETH) — $20M
        │   │       ├── [CDP] WSTETH-B (wstETH) — $13M
        │   │       ├── [CDP] ETH-B (ETH) — $8M
        │   │       ├── [CDP] WBTC-C (WBTC) — $1M
        │   │       ├── [CDP] WBTC-A (WBTC) — $847K
        │   │       └── [CDP] WBTC-B (WBTC) — $186K
        │   └── [borrow] USDT
        ├── wstETH/USDT (86% LLTV) — $0.00 (0.0%) @ 3.00% APY
        │   ├── [collateral] wstETH [4626] (rate: 1.231065)
        │   │   └── stETH [4626] (rate: 1.0)
        │   │       └── ETH
        │   └── [borrow] USDT
        ├── WBTC/USDT (86% LLTV) — $0.00 (0.0%) @ 2.98% APY
        │   ├── [collateral] WBTC
        │   └── [borrow] USDT
        ├── cbBTC/USDT (86% LLTV) — $0.00 (0.0%) @ 2.20% APY
        │   ├── [collateral] cbBTC
        │   └── [borrow] USDT
        ├── syrupUSDT/USDT (92% LLTV) — $0.00 (0.0%) @ 1.66% APY
        │   ├── [collateral] syrupUSDT [4626] (rate: 1.122055)
        │   │   └── USDT
        │   └── [borrow] USDT
        ├── syrupUSDC/USDT (92% LLTV) — $0.00 (0.0%) @ 4.29% APY
        │   ├── [collateral] syrupUSDC [4626] (rate: 1.158423)
        │   │   └── USDC
        │   └── [borrow] USDT
        ├── PT-srUSDe-2APR2026/USDT (92% LLTV) — $231.48 (0.0%) @ 3.36% APY
        │   ├── [collateral] PT-srUSDe-2APR2026 [PT]
        │   │   └── SY-srUSDe [SY] (rate: 1.017289)
        │   │       └── srUSDe [4626]
        │   │           └── USDe
        │   └── [borrow] USDT
        ├── stcUSD/USDT (92% LLTV) — $0.11 (0.0%) @ 3.37% APY
        │   ├── [collateral] stcUSD [4626] (rate: 1.055424)
        │   │   └── cUSD
        │   └── [borrow] USDT
        ├── PT-stcUSD-23JUL2026/USDT (92% LLTV) — $0.11 (0.0%) @ 3.37% APY
        │   ├── [collateral] PT-stcUSD-23JUL2026 [PT]
        │   │   └── SY-stcUSD [SY] (rate: 1.055424)
        │   │       └── stcUSD [4626]
        │   │           └── cUSD
        │   └── [borrow] USDT
        ├── PT-srUSDe-25JUN2026/USDT (92% LLTV) — $0.06 (0.0%) @ 3.38% APY
        │   ├── [collateral] PT-srUSDe-25JUN2026 [PT]
        │   │   └── SY-srUSDe [SY] (rate: 1.017289)
        │   │       └── srUSDe [4626]
        │   │           └── USDe
        │   └── [borrow] USDT
        ├── PT-USDG-28MAY2026/USDT (94% LLTV) — $0.00 (0.0%) @ 2.94% APY
        │   ├── [collateral] PT-USDG-28MAY2026 [PT]
        │   │   └── SY-USDG [SY] (rate: 1.0)
        │   │       └── USDG
        │   └── [borrow] USDT
        └── IDLE/USDT (0% LLTV) — $0.00 (0.0%) @ 0.00% APY
            ├── [collateral] IDLE [IDLE]
            └── [borrow] USDT
```

In practice, rUSD is overwhelmingly a wrapper around steakUSDC. The PSM and remaining vaults are decorative at current scale.

**steakUSDC — the true underlying.**
steakUSDC is a Morpho vault allocating across four markets, all at 86% LLTV:

| Morpho Market | Allocation | LLTV | Collateral |
|---|---|---|---|
| cbBTC/USDC | 52.8% | 86% | cbBTC (Coinbase-custodied BTC) |
| WBTC/USDC | 33.0% | 86% | WBTC (BitGo-custodied BTC) |
| wstETH/USDC | 14.2% | 86% | wstETH → stETH → ETH |
| WETH/USDC | ~0% | 86% | ETH |

Uniform APY of 1.89% across all markets.

**Risk findings:**

**1. BTC concentration (85.8%)**
cbBTC and WBTC together represent 85.8% of steakUSDC exposure. wsrUSD is structurally a leveraged bet that BTC does not fall more than ~14% in a single move. In March 2020, BTC fell 40% within 24 hours.

**2. Wrapped BTC custodial risk**
cbBTC (52.8%) is custodied by Coinbase — a regulatory event or operational failure at Coinbase would propagate directly into wsrUSD. WBTC (33.0%) is custodied by BitGo, which already drew controversy in 2024 over a proposed transfer of custody to BiT Global. Two custodians, one risk category.

**3. Aggressive LLTV throughout**
86% LLTV across all markets leaves a 14% buffer before liquidation cascades begin. This is tight for assets with BTC-level tail volatility.

**4. Recursion depth of 4–5 layers**
Each layer adds independent smart contract risk and compounds liquidity friction in a mass redemption scenario:
```
wsrUSD → rUSD → steakUSDC → Morpho market → cbBTC / WBTC / wstETH
```

**5. Circular exposure in smokeUSDT**
The smokeUSDT vault holds $39.5M of wsrUSD as collateral to borrow USDT at 94% LLTV. wsrUSD is thus collateralising USDT that could theoretically be used to mint more rUSD — a classic reflexivity loop that amplifies stress in both directions.

**6. Yield inadequate for risk taken**
A base APY of ~1.89% (approximately 2–3% including the wsrUSD wrapper rate) for four layers of smart contract risk, concentrated BTC custodial exposure, and an aggressive LLTV profile represents poor risk-adjusted compensation.

**Summary scorecard.**

| Risk Dimension | Assessment |
|---|---|
| Smart contract risk | 🔴 4–5 layers of recursive depth |
| Concentration | 🔴 85.8% BTC (cbBTC + WBTC) |
| LLTV | 🟡 86% — aggressive but standard on Morpho |
| Yield / risk ratio | 🔴 ~2% for this complexity and tail exposure |
| Circularity | 🔴 wsrUSD as collateral in smokeUSDT |
| Exit liquidity | 🟡 PSM buffer of $500K only |


### Morpho integrations

The model will be deployed against the full universe of active Morpho collateral assets, with priority given to the 30 largest vaults by TVL. For each vault, the model will compute per-market risk scores, intrinsic ratings, and contextual PD estimates, giving curators and depositors a standardised, continuously updated view of the risk embedded in every position.

> Note on the 30 largest Morpho vaults by TVL: Full per-vault model output, including factor score breakdowns, $o_{e}$, intrinsic rating, and 1d/7d/30d PD estimates will be published as part of the grant deliverables. Vault coverage will include all major collateral types currently active on Morpho: wrapped BTC variants (cbBTC, WBTC), LSTs (wstETH, weETH, rsETH), synthetic stablecoins (USDe, sUSDe, USD0), and RWA-backed assets. Each vault entry will include a dependency graph of the type shown above, a full recursive risk decomposition, and a plain-language risk summary compatible with the format of the wsrUSD analysis above.

### Support and maintenance

Following the 12-month development and delivery period, the plan is to operate and maintain the model in a live production environment for a minimum of 12 additional months post-delivery. This includes daily recalibration of factor scores across all monitored assets, weekly rating updates published onchain or via a public API, monthly full MLE recalibration of sensitivity coefficients and GARCH parameters, and quarterly recalibration of Vasicek correlations, HMM parameters, and BIC-selected horizons in line with the schedule defined in the mathematical reference. Any new Morpho collateral asset reaching material TVL will be onboarded within 7 days of listing. Incident response including circuit-breaker activations, oracle anomalies, and exploit events will trigger immediate model updates and public alerts within the timeframes specified in the event-driven architecture.

## Proposed budget

**Total Grant Request: $250,000 in $MORPHO tokens, 12-month project, 6 bi-monthly tranches of ~$41,667**.

### Budget by Category

| Category | Description | Total |
|---|---|---|
| Research & Model Development | Factor model calibration, MLE/BIC pipeline, GARCH-HMM implementation, backtesting framework | $75,000 |
| Smart Contract / Onchain Infrastructure | Onchain data ingestion, event-driven update architecture, WebSocket subscriptions, contract introspection engine | $57,000 |
| Frontend / Dashboard | Risk score dashboard for curators and depositors, per-asset factor breakdown, vault-level PD display | $43,000 |
| Audits & Security | External code review of onchain components, model audit, backtesting validation by independent party | $33,000 |
| Team / Contributors | Core team compensation across 12 months | $32,000 |
| Operations & Legal | Infrastructure costs, RPC nodes, data archival (cryo), legal review | $10,000 |
| **Total** | | **$250,000** |

### Payment Schedule

| Tranche | Period | Amount ($MORPHO) | Key Milestone |
|---|---|---|---|
| 1 | Month 1–2 | ~$41,667 | Architecture design, data pipeline spec, factor model prototype (Z₁–Z₃) |
| 2 | Month 3–4 (**Public data & Dashboard available**) | ~$41,667 | Full 6-factor scoring live on testnet, GARCH-t calibration pipeline operational |
| 3 | Month 5–6 | ~$41,667 | HMM regime-switching integrated, σe and intrinsic rating live, internal backtesting complete |
| 4 | Month 7–8 | ~$41,667 | Contextual PD (Merton layer) live, dashboard v1 deployed, external audit initiated |
| 5 | Month 9–10 | ~$41,667 | Audit complete and remediated, stress testing framework live, coverage expanded to 50+ assets |
| 6 | Month 11–12 | ~$41,665 | Full mainnet deployment, public documentation, community handover and open-source release |
| **Total** | **12 months** | **$250,000** | |

*All amounts denominated in USD equivalent, paid in $MORPHO tokens at the 7-day TWAP price at the start of each tranche period.*

## Team

[Sigma Labs](https://) is a French quantitative research team with backgrounds from French grandes ecoles, and co-founders of DeFi protocols. The team combines academic expertise in stochastic modelling and quantitative finance with direct onchain experience, the gap this framework is built to close. The **DeFi Collateral Asset Risk Model v3.0** is our primary proof of work.

## Next Steps

For the next three weeks, this RFC is an open invitation for discussion. We welcome feedback from curators, depositors, risk researchers, and the broader Morpho community on the model design, scope, and roadmap before moving to a formal governance vote.

To engage: comment directly on this thread, reach out to the Sigma Labs team at [research@sigmalabs.xyz](https://). Once community feedback has been incorporated and rough consensus reached, this proposal will be submitted for onchain vote.

## Considerations

Models and all associated code will be fully open-sourced upon delivery under the [MIT License](https://opensource.org/license/mit). The GitHub repository will be public and accessible to the entire community including factor score pipelines, calibration scripts, backtesting framework, and dashboard code. Every model output will be traceable to a specific onchain read, a documented equation, and a versioned parameter set. 
Community members, independent researchers, and third-party auditors are explicitly encouraged to verify, challenge, and build on top of the framework.