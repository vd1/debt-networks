# [RFC] A Morpho-compatible DeFi Collateral Asset Risk Model

## Abstract

### What We Have Built

Sigma Labs has developed two operational tools for on-chain collateral risk transparency on Morpho. The **Dependency Graph Engine (DIG)** maps the full chain of on-chain relationships for any EVM asset — wrappers, LP positions, lending markets, and pledge structures — across 7+ protocol watchers, with live outputs covering hundreds of nodes per asset. The **Yield Tree Decomposition (YTD)** translates that graph into a structured, recursive breakdown of yield sources, collateral exposure, and risk concentration. The wsrUSD and reUSD analyses below are live outputs of these tools, not illustrations.

These tools already surface the structural signals that preceded the three most damaging Morpho incidents of the past 18 months. This RFC proposes to extend them into a full quantitative risk scoring and probability-of-default framework, transparently, openly, and in service of the entire Morpho ecosystem.

This diagnosis is not ours alone. Independent research published in April 2026 by Luca Prosperi (*Physics of On-Chain Lending*) documented 20–100x mispricing of collateral risk across major DeFi lending markets, providing external validation that the gap this framework addresses is both real and material.

### The Morpho Curationship Model and Its Promise

Morpho has redefined lending protocol design by separating infrastructure from risk management. Anyone can now spin up an ERC-4626 vault in a few clicks, define collateral exposure, set supply caps, choose oracles, and offer curated lending strategies to depositors. The protocol provides the rails; the curator provides the judgment.

The closest analogy in traditional finance is an asset manager: both set strategy and manage risk, but vault curators operate non-custodially. Execution is automated through smart contracts, and users can deposit and withdraw at will without anyone being able to prevent it.

The model's appeal is real. But its rapid expansion has exposed a structural gap: **the rigor of collateral risk assessment varies enormously across curators, and depositors have consistently lacked the tools to evaluate it**.

### A Growing Pattern of Confidence Failures

Three episodes made curator collateral risk impossible to ignore.

**January 10, 2025 — USD0++ / Usual Protocol.** Usual abruptly changed USD0++'s redemption mechanism, replacing unconditional 1:1 redemption with an $0.87 floor price without meaningful advance warning. Vault curators on Morpho had hardcoded USD0++:USDC prices at 1:1 parity rather than at market rate. Interest rates spiked, massive liquidations swept through lending markets, and TVL in the affected vault halved in hours. The DIG would have flagged the hardcoded oracle divergence; the governance factor (Z₆) would have captured the absence of a timelock on the redemption mechanism change.

**November 2025 — Stream Finance / xUSD.** Curators had routed USDC deposits into leverage loops backed by the synthetic stablecoin xUSD, leaving an estimated $285M–$700M at risk across multiple lending protocols when its oracle refused to update. The YTD would have surfaced the recursive leverage loop structure and flagged xUSD's oracle staleness under Z₅.

**March 2026 — Resolv / USR Exploit.** An attacker exploited a flaw in Resolv's USR minting contract, creating approximately 80 million unbacked tokens from roughly $200,000 in USDC. The oracle was hardcoded and never repriced: wstUSR was marked at $1.13 while trading at $0.63 on secondary markets. Fifteen Morpho vaults were impacted. Z₆ would have flagged the privileged single-EOA minting role; Z₅ would have flagged the hardcoded feed with no deviation circuit breaker.

A full on-chain signal analysis for each incident is provided in the Evidence Appendix.

### Motivation

These incidents share a common thread: they were not fundamentally unpredictable. The risks were present on-chain before each failure, in oracle configurations, minting mechanics, governance structures, liquidity depth, and backing ratios. What was missing was **a systematic framework to read those signals, quantify them, and translate them into actionable risk parameters for curators and depositors alike**.

Existing efforts have been a necessary starting point. But asset rating is not risk management. Rating tells you a score at a point in time; risk management tells you how that score changes under stress, how it correlates with other positions in a vault, and what it implies for liquidation probability at a given LTV. **These are the questions a curator must answer, and today, they answer them inconsistently, opaquely, and often only after something has already broken**.

This RFC proposes a dedicated, quantitative risk framework for Morpho collateral assets. Its goals are concrete:

- **Continuity:** risk scores update in real time from on-chain data, not from quarterly manual reviews.
- **Comparability:** a score of 0.8 on a stablecoin and 0.8 on a liquid staking token mean the same thing, i.e. current conditions are highly abnormal relative to that asset's own history.
- **Auditability:** every output is traceable to a specific on-chain read, a calibrated parameter, and a documented methodology.
- **Actionability:** the framework produces not just scores but probability of default estimates conditioned on actual vault LTV structures, the number a curator needs to set caps and the number a depositor needs to assess exposure.

The Morpho ecosystem has demonstrated that permissionless curation can scale. This RFC proposes the risk infrastructure that scaling responsibly requires.

## Prior Art: Credora by RedStone

Credora (acquired by RedStone in September 2025) introduced consensus-based risk ratings on Morpho in March 2025 and deserves credit for establishing that collateral risk transparency belongs in the protocol interface. We build on that foundation. Our work is complementary rather than competitive: where Credora provides point-in-time ratings, our framework produces continuous, fully on-chain-derivable risk scores and probability-of-default estimates that update in real time from live blockchain state. A detailed technical comparison is provided in the Technical Appendix.

## Description of the DeFi Collateral Asset Risk Model

The model is a fully quantitative, on-chain-native framework for assessing the risk of any EVM collateral asset. It requires no manual categorisation, contains no hardcoded parameters, and is designed to update in real time from blockchain data. A complete mathematical treatment is available in the full technical reference: [DeFi Collateral Asset Risk Model — Mathematical Reference v3.0](https://docsend.com/v/63dkk/defi-collat-asset-risk-model).

The model produces two outputs from the same underlying structure. The **Intrinsic Risk Grade (σe)** measures how risky an asset is to hold independent of any lending context, expressed as an annualised effective volatility and mapped to a seven-tier rating from AAA to CCC. The **Contextual Probability of Default (PDT)** measures the probability that the asset causes a liquidation failure in a specific Morpho market over a 1-day, 7-day, or 30-day horizon, activating when the asset is deployed as collateral against a specific LTV structure. A curator needs σe to decide whether to list an asset; they need PDT to decide what cap and LLTV to set once listed.

Risk is decomposed into six on-chain-observable factors: Market (Z₁), Counterparty (Z₂), Smart Contract (Z₃), Liquidity (Z₄), Oracle (Z₅), and Governance/Mutation (Z₆), each scored on a continuous [0,1] scale derived entirely from on-chain reads and cross-asset comparable by construction. Factor scores feed into a GJR-GARCH(1,1)-t regression with Hidden Markov Model regime-switching for correlation, producing a continuous distance-to-default that degrades visibly as conditions worsen rather than crossing a binary threshold.

Today, the Dependency Graph Engine and Yield Tree Decomposition are operational and producing live outputs. This grant funds the quantitative scoring layer, calibration, backtesting, live factor scoring, and the Merton PD layer, that transforms those structural outputs into rated, actionable risk scores. Full technical detail on all model components is provided in the Technical Appendix.

## Development

### What Is Built Today

Sigma Labs has built and validated two operational tools currently running on live on-chain data.

The **Dependency Graph Engine (DIG)** monitors 7+ protocol integrations and maps the full on-chain relationship graph of any EVM asset, including wrappers, LP positions, Pendle PT/YT structures, lending markets, and pledge relationships, producing structured JSON output and visual dependency trees. Live outputs cover assets with 500+ nodes in their dependency graph.

The **Yield Tree Decomposition (YTD)** translates the dependency graph into a recursive breakdown of yield sources, collateral allocation, LLTV exposure, and concentration risk at every layer of a composed asset. The wsrUSD and reUSD analyses below are direct outputs of these tools running on live blockchain state, not illustrations or mock-ups.

These two tools already surface the structural signals that preceded the USD0++, xUSD, and Resolv/USR incidents. The retroactive case studies demonstrating this are provided in full in the Evidence Appendix.

### What This Grant Delivers

The quantitative scoring layer, covering the six factor scores (Z₁–Z₆), GJR-GARCH-t calibration, HMM regime-switching, effective volatility (σe), intrinsic rating, and Merton-layer contextual PD, remains research design and is the primary funded deliverable of this grant. Concretely:

| Deliverable | Target |
|---|---|
| Z₁–Z₃ factor scoring live on testnet | Month 2 |
| Full 6-factor scoring + GARCH calibration | Month 4 |
| σe, intrinsic rating (AAA–CCC) live | Month 6 |
| Contextual PD (Merton layer) + dashboard v1 | Month 8 |
| External audit complete | Month 10 |
| Full mainnet deployment + open-source release | Month 12 |

By month 3–4, DIG and YTD tooling will be made available to at least one Morpho curator team for trial and direct feedback, providing a real-user validation checkpoint before the quantitative scoring layer is finalised.

### First Results

The model produces two complementary outputs for any Morpho collateral asset: a **dependency graph** mapping the full chain of on-chain relationships, and a **recursive risk decomposition** translating that structure into quantified, factor-level risk findings. Two live examples are provided below.

#### Dependency Graph — reUSD

The graph below illustrates the recursive dependency unwrapping engine applied to **reUSD (Re Protocol)**, tracing every wrapper, LP position, Pendle market, and Morpho lending market from the top-level asset down to individual collateral markets.

![reUSD_neigh](https://hackmd.io/_uploads/rkw5Eug2We.png)

#### Deep Risk Decomposition — wsrUSD (Reservoir)

wsrUSD is an ERC-4626 wrapper (exchange rate: 1.074) around rUSD, a synthetic stablecoin backed by four sources. In practice, rUSD is overwhelmingly a wrapper around steakUSDC (96%+ of backing). steakUSDC allocates across four Morpho markets, all at 86% LLTV:

| Morpho Market | Allocation | Collateral |
|---|---|---|
| cbBTC/USDC | 52.8% | cbBTC (Coinbase-custodied BTC) |
| WBTC/USDC | 33.0% | WBTC (BitGo-custodied BTC) |
| wstETH/USDC | 14.2% | wstETH → stETH → ETH |
| WETH/USDC | ~0% | ETH |

Key risk findings: 85.8% BTC concentration across two custodians; 86% LLTV leaves a 14% buffer before liquidation cascades; 4–5 layers of recursive smart contract risk; circular exposure (smokeUSDT holds $39.5M wsrUSD as collateral at 94% LLTV); and a base APY of ~1.89% inadequate for this structural complexity. Full yield tree, risk findings, and scorecard are provided in the Evidence Appendix.

### Morpho Integrations

The model will be deployed against the full universe of active Morpho collateral assets, with priority given to the 30 largest vaults by TVL. For each vault, the model will compute per-market risk scores, intrinsic ratings, and contextual PD estimates, giving curators and depositors a standardised, continuously updated view of the risk embedded in every position. Full per-vault output, including factor score breakdowns, σe, intrinsic rating, and 1d/7d/30d PD estimates, will be published as part of the grant deliverables.

### Support and Maintenance

Following the 12-month development and delivery period, Sigma Labs will operate and maintain the model in a live production environment for a minimum of 12 additional months. This includes daily recalibration of factor scores, weekly rating updates published on-chain or via a public API, monthly MLE recalibration of sensitivity coefficients and GARCH parameters, and quarterly recalibration of Vasicek correlations and HMM parameters. Any new Morpho collateral asset reaching material TVL will be onboarded within 7 days of listing. Exploit events, oracle anomalies, and circuit-breaker activations will trigger immediate model updates and public alerts.

## Proposed Budget

**Total Grant Request: $250,000 in $MORPHO tokens — 12-month project — 3 delivery-gated tranches.**

### Budget by Category

| Category | Description | Total |
|---|---|---|
| Research & Model Development | Six-factor scoring, GJR-GARCH-t calibration, HMM regime-switching, Merton PD layer, backtesting framework | $95,000 |
| Smart Contract / On-chain Infrastructure | On-chain data ingestion, event-driven update architecture, WebSocket subscriptions, contract introspection engine | $55,000 |
| Audits & Security | External code review of on-chain components, model audit, backtesting validation by independent party | $35,000 |
| Frontend / Dashboard | Risk score dashboard for curators and depositors, per-asset factor breakdown, vault-level PD display | $30,000 |
| Team / Contributors | Core team compensation across 12 months (2.5 FTE) | $25,000 |
| Operations & Legal | Infrastructure costs, RPC nodes, data archival (cryo), legal review | $10,000 |
| **Total** | | **$250,000** |

### Payment Schedule

Three delivery-gated tranches. Each tranche is released upon verified delivery of the stated milestones, not on a time basis.

| Tranche | Release Date | Amount ($MORPHO) | Delivery Gate |
|---|---|---|---|
| 1 | July 2026 | ~$83,333 | DIG + YTD tooling delivered to at least one Morpho curator for live trial; Z₁–Z₃ factor scoring live on testnet; retroactive case study (USD0++, xUSD, Resolv) published |
| 2 | October 2026 | ~$83,333 | Full 6-factor scoring operational; GJR-GARCH-t calibration complete with backtest results; σe and intrinsic rating (AAA–CCC) live; dashboard v1 deployed |
| 3 | February 2027 | ~$83,334 | Merton PD layer live (PD₁d, PD₇d, PD₃₀d); external audit complete and remediated; 50+ assets covered; full open-source release and public documentation |
| **Total** | | **$250,000** | |

*All amounts denominated in USD equivalent, paid in $MORPHO tokens at the 7-day TWAP price on the release date of each tranche.*

## Team — Sigma Labs

Sigma Labs is a French quantitative research team, combining academic backgrounds from **École Polytechnique**, **École Normale Supérieure (ENS)** and **Université Paris Dauphine–PSL** with hands-on experience co-founding and operating DeFi protocols. The **DeFi Collateral Asset Risk Model v3.0** and the live DIG/YTD outputs in this RFC are our primary proof of work.

**Vincent Danos** — École Polytechnique alumnus, co-founder of DeFi protocols and Research Director at CNRS. Leads overall research direction and model architecture.

**Amaury Denny** — Paris Dauphine–PSL, Quantitative Finance & Technology; 42 School Paris. Former VC research at KuCoin Labs, co-founder of a crypto family office with $20M+ AuM. Leads six-factor model calibration, GARCH/HMM implementation, and backtesting framework.

**Hamza E.** — ENS alumnus, PhD in Financial Markets, co-founder of DeFi protocols and quant prop shops. Leads quantitative research and model validation.

**Daniel J.** — ESILV alumnus, co-founder of a liquid fund, treasury manager and investor. Leads the Dependency Graph Engine and on-chain integration layer.

## Next Steps

This RFC is an open invitation for discussion. We welcome feedback from curators, depositors, risk researchers, and the broader Morpho community on the model design, scope, budget, and roadmap before moving to a formal governance vote. The feedback window is three weeks from the date of this post.

Following delivery of the first working scoring layer, Sigma Labs will publish model scores in **shadow mode** for 60–90 days, non-binding and publicly visible, running in parallel with existing risk frameworks. This builds a verifiable track record before any binding governance integration and gives the community a concrete basis for evaluating model quality ahead of Phase 2.

To engage: comment directly on this thread or reach out to the Sigma Labs team at [research@sigmalabs.xyz](mailto:research@sigmalabs.xyz). Once community feedback has been incorporated and rough consensus reached, this proposal will be submitted for on-chain vote.

## Considerations

The model and all associated code will be fully open-sourced upon delivery under the [MIT License](https://opensource.org/license/mit). The GitHub repository will be public and accessible to the entire community, including factor score pipelines, calibration scripts, backtesting framework, and dashboard code. Every model output will be traceable to a specific on-chain read, a documented equation, and a versioned parameter set. Community members, independent researchers, and third-party auditors are explicitly encouraged to verify, challenge, and build on top of the framework.

This RFC is accompanied by two appendices: a **Technical Appendix** containing the full six-factor model specification, GARCH/HMM equations, Merton PD layer, and calibration methodology; and an **Evidence Appendix** containing live DIG and YTD outputs, the full wsrUSD decomposition, the retroactive case study against USD0++, xUSD, and Resolv/USR, and the complete claim register.

---

# Evidence Appendix

## E1. Claim Register

| Component | Status | Evidence |
|---|---|---|
| Dependency Graph Engine (DIG) | **Built** | 7+ protocol watchers, live outputs (reUSD, wsrUSD, cUSD: 500+ nodes) |
| Yield Tree Decomposition (YTD) | **Built** | Structured JSON + visual trees (wsrUSD, reUSD, Sentora PYUSD) |
| Recursive risk decomposition (qualitative) | **Built** | wsrUSD analysis: 6 findings, scorecard, circular exposure identified |
| Six-factor scoring (Z₁–Z₆) | Research design | Mathematical specification complete; no calibrated live outputs |
| GJR-GARCH(1,1)-t calibration | Research design | Methodology specified; no fitted parameters or backtest results |
| HMM regime-switching (3 regimes) | Research design | Methodology specified; no calibrated transition matrix |
| Per-block effective volatility (σe) | Grant deliverable | Zero live outputs; Month 6 target |
| Merton-layer contextual PD | Grant deliverable | Zero live outputs; Month 8 target |
| Intrinsic rating (AAA–CCC) | Grant deliverable | No cross-sectional distribution yet; Month 6 target |
| Circuit breaker / event-driven architecture | Grant deliverable | Described in technical spec; not yet implemented |
| Dashboard | Grant deliverable | Not started; Month 8 target |

## E2. Full wsrUSD Yield Tree Decomposition

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

**Risk scorecard.**

| Risk Dimension | Assessment |
|---|---|
| Smart contract risk | 🔴 4–5 layers of recursive depth |
| Concentration | 🔴 85.8% BTC (cbBTC + WBTC) |
| LLTV | 🟡 86% — aggressive but standard on Morpho |
| Yield / risk ratio | 🔴 ~2% for this complexity and tail exposure |
| Circularity | 🔴 wsrUSD as collateral in smokeUSDT |
| Exit liquidity | 🟡 PSM buffer of $500K only |

## E3. Retroactive Case Study

The following analysis documents what was visible on-chain before each of the three major Morpho incidents of 2025–2026. The purpose is not to claim predictive certainty but to demonstrate that the structural signals behind each failure were present and readable from public blockchain state before the damage occurred.

### Case 1 — USD0++ / Usual Protocol (January 10, 2025)

**What happened.** Usual Protocol unilaterally changed USD0++'s redemption mechanism, replacing unconditional 1:1 redemption with an $0.87 floor price without advance notice. Vault curators on Morpho had hardcoded USD0++:USDC oracle prices at 1:1 parity. TVL in the affected vault halved within hours. Some curators exited positions ahead of the public announcement, prompting accusations of insider trading.

**What was visible on-chain before the incident.**

*Oracle configuration (Z₅).* The USD0++:USDC Morpho markets used a hardcoded oracle valuing USD0++ at exactly $1.00 with no deviation circuit breaker and no secondary market reference. This was readable from the oracle contract address stored in Morpho market parameters at any block before the incident.

*Governance structure (Z₆).* The redemption mechanism was controlled by a Usual Protocol admin role with no timelock on parameter changes. The specific change required no governance vote, no delay period, and no on-chain signal visible to curators or depositors before execution. All readable from the TimelockController contract.

*Dependency structure (DIG).* The DIG would have mapped USD0++ as a staked derivative of USD0 with a four-year maturity structure, flagging it as a bond-like instrument inconsistent with a hardcoded $1.00 oracle valuation.

| Signal | Factor | Readable before incident |
|---|---|---|
| Hardcoded oracle at $1.00, no market reference | Z₅ | ✅ Yes |
| No timelock on redemption mechanism | Z₆ | ✅ Yes |
| Bond-like maturity structure priced as spot | Z₂ | ✅ Yes |

### Case 2 — Stream Finance / xUSD (November 2025)

**What happened.** Curators routed USDC deposits into recursive leverage loops backed by xUSD. When xUSD's oracle refused to update, an estimated $285M–$700M was left at risk across Morpho, Euler, and Silo. Curators who had publicly claimed zero exposure were found to hold significant positions when the protocol collapsed.

**What was visible on-chain before the incident.**

*Leverage loop structure (YTD).* The recursive loop — USDC deposits → xUSD collateral → USDC borrow → repeat — was fully visible from Morpho market interactions and supply/borrow balances at each layer, with leverage ratios reportedly reaching 10x.

*Oracle staleness risk (Z₅).* xUSD's oracle depended on off-chain infrastructure with a documented maximum staleness window readable from the oracle contract's heartbeat parameter. Under stress, the oracle's failure to reprice was a documented design characteristic, not a surprise.

*Supply velocity and redemption queue (Z₂).* xUSD's totalSupply growth rate in the weeks before the collapse was abnormally high relative to its backing. The 65-day redemption queue was readable from the protocol contract and created a structural mismatch with 10x leverage positions.

| Signal | Factor | Readable before incident |
|---|---|---|
| Recursive leverage loop structure | Z₄ / YTD | ✅ Yes |
| Oracle staleness heartbeat window | Z₅ | ✅ Yes |
| Abnormal supply velocity | Z₂ | ✅ Yes |
| 65-day redemption queue | Z₂ | ✅ Yes |

### Case 3 — Resolv / USR Exploit (March 22, 2026)

**What happened.** An attacker exploited a flaw in Resolv's USR minting contract, minting 80 million unbacked USR tokens from approximately $200,000 in USDC and extracting roughly $25 million in ETH. The wstUSR oracle was hardcoded and never repriced: wstUSR was marked at $1.13 while trading at $0.63 on secondary markets. Fifteen Morpho vaults were impacted. Some curators' automated systems continued supplying liquidity into compromised markets hours after the exploit began.

**What was visible on-chain before the incident.**

*Privileged minting role (Z₆).* The USR minting function was controlled by a single EOA with no mint limits, no oracle checks, and no multisig requirement. A single private key held uncapped minting authority over a stablecoin integrated into $500M+ of DeFi lending, readable from the contract's access control storage slots.

*Hardcoded oracle (Z₅).* The wstUSR:USDC oracle was hardcoded with no secondary market reference and no circuit breaker. Once USR depegged, the oracle continued reporting stale prices while the asset traded at a steep discount, exactly the mechanism that enabled the collateral arbitrage.

*Leverage loop and contract age (YTD, Z₃).* Resolv's TVL grew from under $50M to over $650M in under three months, driven by leveraged looping on Morpho and Euler. The minting contract had high TVL relative to its age, a signal the Lindy-weighted Z₃ specifically penalises.

| Signal | Factor | Readable before incident |
|---|---|---|
| Single EOA minting role, no mint limits | Z₆ | ✅ Yes |
| Hardcoded oracle, no deviation circuit breaker | Z₅ | ✅ Yes |
| Recursive leverage loop, reflexive dependency | Z₄ / YTD | ✅ Yes |
| High TVL, low contract age | Z₃ | ✅ Yes |

### Summary

Across all three incidents, the structural risk signals were present on-chain before the damage occurred. None required inside information, off-chain intelligence, or predictive modelling to flag. They required systematic, factor-level reading of publicly available blockchain state.

| Incident | Primary signals | Factors |
|---|---|---|
| USD0++ / Usual (Jan 2025) | Hardcoded oracle, no timelock, bond mispriced as spot | Z₅, Z₆, Z₂ |
| Stream / xUSD (Nov 2025) | Leverage loop, oracle staleness, 65-day redemption queue | Z₄, Z₅, Z₂ |
| Resolv / USR (Mar 2026) | Single EOA minter, hardcoded oracle, leverage loop, low contract age | Z₆, Z₅, Z₄, Z₃ |

---

# Technical Appendix

## T1. Model Overview

The model decomposes the risk of any EVM collateral asset into six on-chain-observable factors, estimates all parameters via Maximum Likelihood Estimation on historical data, and integrates factor correlations through a Vasicek structure with Hidden Markov Model regime-switching. It produces two outputs: an intrinsic risk grade (σe) independent of any lending context, and a contextual probability of default (PDT) conditioned on a specific Morpho market's LTV structure.

The complete mathematical reference is available at: [DeFi Collateral Asset Risk Model — Mathematical Reference v3.0](https://docsend.com/v/63dkk/defi-collat-asset-risk-model).

## T2. Six On-Chain Risk Factors

Each factor Zᵢ ∈ [0,1] is constructed entirely from on-chain reads. All scores are cross-asset comparable: Z = 0.9 on a stablecoin and Z = 0.9 on an LST both mean current conditions are highly abnormal relative to that asset's own history.

### Factor 1 — Market Risk (Z₁)

Log-returns rₜ = ln(Pₜ/Pₜ₋₁) are computed from on-chain price series. For stablecoins, peg deviation returns are used; for LSTs, exchange rate returns. Realised volatility is blended across three BIC-calibrated horizons:

```
σ_blend = w₁·σ̂_τ₁ + w₂·σ̂_τ₂ + w₃·σ̂_τ₃
```

Z₁ normalises current short-horizon volatility against its own rolling history via Φ.

**Sources:** Chainlink `latestRoundData()`, Uniswap V3 `observe()`, LST exchange rate contracts. **Update:** per block.

### Factor 2 — Counterparty Risk (Z₂)

Captures issuer, custodian, and validator risk. The applicable metric set is determined automatically via contract bytecode introspection. Key metrics by asset type:

| Asset Type | Key Metrics |
|---|---|
| All assets | Holder Gini coefficient, supply velocity |
| Wrapped tokens | Backing ratio, market discount vs. reference asset |
| LSTs | Validator Gini, exchange rate trend, withdrawal queue pressure |
| CDP stablecoins | Reserve health, redemption flow |
| Vault tokens | TVL stability, yield anomaly vs. benchmark |

Sub-scores aggregated via MLE-optimised weights ωₘ. **Update:** hourly; per block during stress.

### Factor 3 — Smart Contract Risk (Z₃)

Exploit probability estimated from Lindy-weighted safety metrics and similarity to a reference database of ~3,050 historically exploited contracts:

```
sim(A, E) = cos(v_A, v_E) · |S_A ∩ S_E| / |S_E|
```

where v is the opcode frequency vector and S is the function selector set. Threshold θ* calibrated by AUC-ROC on temporal train/test split. **Update:** on contract upgrade events only.

### Factor 4 — Liquidity Risk (Z₄)

Position-size-aware Conditional Liquidity-at-Risk:

```
LiqARₚ(Q_fund) = Percentile_p [ slippage(Q_fund) | σ̂_1h > σ̂^(1-p)_1h ]
```

Slippage replayed against historical pool states for actual position size Q_fund. Depth threshold adaptive: k* · σ̂_7d (k* calibrated by BIC).

**Sources:** Uniswap V3 `quoteExactInputSingle()`, Curve `get_balances()`, Mint/Burn/Swap events via cryo. **Update:** every 5 minutes; per block during stress.

### Factor 5 — Oracle Risk (Z₅)

Φ-normalised across four dimensions: staleness (update gap vs. heartbeat), inaccuracy (Chainlink vs. DEX TWAP deviation), redundancy (transmitter count), and tail downtime risk (max staleness over rolling window). Oracle staleness under market stress is captured via the correlation ρ₁·ρ₅ in the Vasicek structure, not inside Z₅.

**Sources:** Chainlink `AnswerUpdated` events, `aggregator.transmitters()`. **Update:** hourly; per block during stress.

### Factor 6 — Governance / Mutation Risk (Z₆)

| Metric | Contract Source | Signal |
|---|---|---|
| Timelock duration | `TimelockController.getMinDelay()` | Short = high risk |
| Multisig threshold ratio | `Safe.getThreshold() / getOwners().length` | Low ratio = risk |
| Admin transaction frequency | Admin-role transactions over rolling window | High = instability |
| Pause capability | Presence of `pause()` + activation history | Emergency risk |
| Parameter mutations | Critical param change events (LTV, fees, oracles) | Governance activity |
| Upgrade authority | Owner / ProxyAdmin / Governance contract type | Centralisation |

**Update:** on contract events (TimelockCallScheduled, RoleGranted, OwnershipTransferred, Upgraded).

## T3. From Factor Scores to Effective Volatility (σe)

### Factor Regression with GJR-GARCH(1,1)-t

```
rₜ = α + Σᵢ βᵢ ΔZᵢ,ₜ + ϵₜ
ϵₜ = σₜ ηₜ,   ηₜ ~ t_ν(0,1)
σ²ₜ = ω + (αG + γ · 𝟙[ϵₜ₋₁<0]) ϵ²ₜ₋₁ + βG σ²ₜ₋₁
```

Parameters estimated by maximising the Student-t log-likelihood via L-BFGS-B. Outputs: β̂ᵢ, σ̂ₜ, ν̂, γ̂.

### Factor Risk Contributions

```
sᵢ(t) = |β̂ᵢ| · σ_Zᵢ(t)
```

### Vasicek One-Factor Structure with HMM Regime-Switching

```
Zᵢ = ρᵢ M + √(1 − ρ²ᵢ) ϵᵢ,   M ~ N(0,1)
```

A 3-regime HMM (calm / transition / crisis) runs on the composite factor signal Z̄(t). Filtered regime probabilities ξₖ(t) blend regime-specific correlations continuously:

```
ρᵢ(t) = Σₖ ξₖ(t) · ρ̂^(k)ᵢ
```

No discontinuous jumps. Correlations rise gradually as crisis regime probability increases.

### Effective Volatility

```
σ²e(t) = (Σᵢ sᵢ(t) ρᵢ(t))²      ← systemic risk
        + Σᵢ s²ᵢ(t)(1 − ρ²ᵢ(t))  ← idiosyncratic risk
        + σ̂²ₜ                      ← dynamic unexplained risk (GARCH)
```

When R²(t) < 0.30, a conservatism multiplier applies automatically:

```
σ^adj_e(t) = (1 + λ_cons · (0.30 − R²)) · σe(t)
```

### Intrinsic Rating

| Rating | Quantile | Interpretation |
|---|---|---|
| AAA | [0, q₅) | Minimal risk |
| AA | [q₅, q₁₅) | Very low |
| A | [q₁₅, q₃₅) | Low |
| BBB | [q₃₅, q₆₀) | Moderate |
| BB | [q₆₀, q₈₀) | Elevated |
| B | [q₈₀, q₉₅) | High |
| CCC | [q₉₅, 1] | Very high to extreme |

Empirical quantiles of σe cross-sectional distribution, recalculated quarterly.

## T4. Recursive Dependency Unwrapping

```
σ^(A)²_e = σ^(own)²_e + Σᵢ Σⱼ eᵢ eⱼ σ^(dᵢ)_e σ^(dⱼ)_e ρ̂_dᵢ,dⱼ(t)
ρ̂_dᵢ,dⱼ(t) = Σₖ ξₖ(t) · ρ̂^(k)_dᵢ,dⱼ
```

Recursion terminates at native L1 assets (ETH, BTC).

## T5. Contextual Probability of Default (Merton Layer)

```
V₀ = P₀ · (1 − s_stress(Q_fund))
B  = D / (Q · (1 − s_liq))
V^adj₀ = V₀ · Πᵢ (1 − Zᵢ · wᵢ · λ̂ᵢ)

DD = [ ln(V^adj₀ / B) − (σ²e / 2) · T ] / (σe · √T)

PDT = F_ν̂(−DD)
```

Three horizons computed simultaneously: PD₁d, PD₇d, PD₃₀d. Student-t tails used throughout: under Gaussian assumptions, a −50% daily move has probability ~10⁻¹⁵; under Student-t with ν̂ ≈ 3–5, tail events receive realistic probabilities.

Conditional PD under systemic stress:

```
PD(A | M = z) = F_ν̂ [ (F⁻¹_ν̂(PD_unconditional) − ρ̄ · z) / √(1 − ρ̄²) ]
```

Usage: z = −2 (moderate crisis) or z = −3 (severe).

## T6. Calibration Schedule

| Parameter | Scope | Frequency | Method |
|---|---|---|---|
| β̂ᵢ, GARCH params | Per asset | Monthly | Factor regression MLE |
| ρ̂^(k)ᵢ | Cross-asset | Quarterly | Vasicek MLE (HMM regime split) |
| HMM (A, μₖ, Σₖ) | Cross-asset | Quarterly | Baum-Welch EM |
| ν̂c | Per category | Quarterly | Student-t MLE |
| λ̂ᵢ | Global | Semi-annual | Stress episode MLE |
| (τ*, L*) horizons | Global | Quarterly | BIC grid search |
| (w*, ω*) blend weights | Per asset | Monthly | Joint MLE |
| θ*, α_penalty | Global | Semi-annual | AUC-ROC / cross-validation |

For newly listed tokens with fewer than L* days of history, MAP estimation is used with a Bayesian prior centred on category-average coefficients. β̂_MAP converges to β̂_MLE as data accumulates.

## T7. Backtesting Framework

1. **Kupiec test:** binomial test on PD exceedance consistency.
2. **Christoffersen test:** independence of exceedances (no clustering).
3. **Diebold-Mariano:** outperformance vs. pure historical VaR.
4. **PIT test:** uₜ = F_ν̂(−DDₜ) ~ Uniform(0,1), tested by Kolmogorov-Smirnov.
5. **Walk-forward validation:** expanding training window, 30-day test window, rolled daily. No future information leakage.
6. **Monte Carlo stress test:** joint factor trajectories under ρ̂^(crisis), distribution of PD under stress.
7. **Incident replay:** elevated σe and deteriorated ratings verified against USD0++, xUSD, and Resolv/USR using historical on-chain data.

## T8. Event-Driven Update Architecture

| Factor | Normal Mode | Stress Mode (Z₁ > 0.80) |
|---|---|---|
| Z₁ Market | Per block | Per block |
| Z₂ Counterparty | Hourly | Per block |
| Z₃ Smart Contract | On event | On event |
| Z₄ Liquidity | Every 5 min | Per block |
| Z₅ Oracle | Hourly | Per block |
| Z₆ Governance | On event | On event |

Circuit breaker: if |r_5min| > 3σ̂_5min, all factors are immediately recomputed and σe and PDT recalculated.

WebSocket subscriptions monitor continuously: `AnswerUpdated` events on Chainlink aggregators (Z₅); `Upgraded` events on EIP-1967 proxy slots (Z₃); `TimelockCallScheduled`, `RoleGranted`, `OwnershipTransferred` (Z₆); large `Transfer`, `Mint`, `Burn` events exceeding 1% of total supply (Z₂, Z₄).