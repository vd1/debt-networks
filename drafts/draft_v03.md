---
title: Defi-collat-v1

---

# [RFC] A Morpho-compatible DeFi Collateral Asset Risk Framework
*v0.3 — April 2026*

## Abstract

### What We Have Built

Before anything else, here is what our tooling produces today on a live Morpho collateral asset.

**wsrUSD — Yield Tree (live output, April 2026)**

```
Wrapped Savings rUSD (wsrUSD) [4626] | rate: 1.074463
└── rUSD [Reservoir]
    ├── steakUSDC (Morpho) [V1] — $20M   ← 96%+ of backing
    │   ├── cbBTC/USDC (86% LLTV) — $109M (52.8%) @ 1.89% APY
    │   ├── WBTC/USDC  (86% LLTV) — $68M  (33.0%) @ 1.89% APY
    │   ├── wstETH/USDC(86% LLTV) — $29M  (14.2%) @ 1.89% APY
    │   └── WETH/USDC  (86% LLTV) — ~$0   (~0%)   @ 1.89% APY
    ├── [PSM] PSM-USDC — $500K
    ├── steakRUSD — $53K
    └── smokeUSDT [V1] — $2
        └── wsrUSD/USDT (94% LLTV) — $39.5M (99.6%) @ 3.37% APY
            ├── [collateral] wsrUSD    ← circular: wsrUSD backs itself
            └── [borrow] USDT
            ...
```

What this surfaces immediately: 85.8% BTC concentration across two custodians (Coinbase, BitGo); 86% LLTV with a 14% buffer before liquidation cascades; 4–5 layers of recursive smart contract risk; a reflexivity loop where wsrUSD collateralises its own backing vault at 94% LLTV; and a base APY of ~1.89% that is inadequate compensation for this structural complexity. This output was produced by our **Dependency Graph Engine (DIG)** and **Yield Tree Decomposition (YTD)**, two operational tools running on live blockchain state. The full decomposition is in the Evidence Appendix.

> **[insert]** DIG = DeFi Integration graph

This is an observability layer Morpho currently lacks. This RFC proposes to build the full risk infrastructure stack on top of it.

### The Morpho Curationship Model and Its Promise

Morpho has redefined lending protocol design by separating infrastructure from risk management. Anyone can now spin up an ERC-4626 vault in a few clicks, define collateral exposure, set supply caps, choose oracles, and offer curated lending strategies to depositors. The protocol provides the rails; the curator provides the judgment.

> **[insert]** plus one can open a Morpho market where the vault's token (eg syrupUSDT, siUSD, etc) is collateral

The closest analogy in traditional finance is an asset manager: both set strategy and manage risk, but vault curators operate non-custodially. Execution is automated through smart contracts, and users can deposit and withdraw at will without anyone being able to prevent it.

The model's appeal is real. But its rapid expansion has exposed a structural gap: **the rigor of collateral risk assessment varies enormously across curators, and depositors have consistently lacked the tools to evaluate it**.

This gap is not a matter of opinion. Independent research published in April 2026 by Luca Prosperi ([*Physics of On-Chain Lending*](https://dirtroads.substack.com/p/68-the-physics-of-on-chain-lending)) documented 20–100x mispricing of collateral risk across major DeFi lending markets. Risk analytics firm Chaos Labs, in its post-mortem of the Resolv/USR exploit, documented the same structural failure: hardcoded oracles, unchecked minting roles, and leverage loops that were all readable from public blockchain state before any damage occurred. The problem is architectural, not incidental, and it recurs because no shared framework exists to read the onchain signals systematically.



### A Growing Pattern of Confidence Failures

Three episodes made curator collateral risk impossible to ignore:

- **January 10, 2025 — USD0++ / Usual Protocol:** Usual abruptly changed USD0++'s redemption mechanism, replacing unconditional 1:1 redemption with an $0.87 floor price without meaningful advance warning. Vault curators on Morpho had hardcoded USD0++:USDC prices at 1:1 parity rather than at market rate. Interest rates spiked, massive liquidations swept through lending markets, and TVL in the affected vault halved in hours. Our tooling is designed to detect hardcoded oracle divergence from market price and the absence of a timelock on governance-controlled redemption mechanisms.
- **November 2025 — Stream Finance / xUSD:** Curators had routed USDC deposits into leverage loops backed by the synthetic stablecoin xUSD, leaving an estimated $285M–$700M at risk across multiple lending protocols when its oracle refused to update. Our tooling is designed to detect recursive leverage loop structures and oracle staleness under the oracle risk factor (Z₅).

- **March 2026 — Resolv / USR Exploit:** An attacker exploited a flaw in Resolv's USR minting contract, creating approximately 80 million unbacked tokens from roughly $200,000 in USDC. The oracle was hardcoded and never repriced: wstUSR was marked at $1.13 while trading at $0.63 on secondary markets. Fifteen Morpho vaults were impacted. Chaos Labs founder Omer Goldberg documented the mechanism in real time: the oracle was hardcoded and thus never repriced, allowing traders to post cheap collateral at inflated valuations and drain USDC from lending vaults. Our tooling is designed to detect privileged single-EOA minting roles with no mint limits and hardcoded feeds with no deviation circuit breaker.

A full onchain signal analysis for each incident is provided in the Evidence Appendix.



### Motivation

These incidents share a common thread: they were not fundamentally unpredictable. The risks were present onchain before each failure, in oracle configurations, minting mechanics, governance structures, liquidity depth, and backing ratios. What was missing was **a systematic framework to read those signals, quantify them, and translate them into actionable risk parameters for curators and depositors alike**.

Existing efforts have been a necessary starting point. But asset rating is not risk management. Rating tells you a score at a point in time; risk management tells you how that score changes under stress, how it correlates with other positions in a vault, and what it implies for liquidation probability at a given LTV. **These are the questions a curator must answer, and today, they answer them inconsistently, opaquely, and often only after something has already broken**.

This RFC proposes a modular, open-source risk infrastructure stack for Morpho collateral assets, delivered as a public good and community-verified. Its goals are concrete:

- **Continuity:** risk scores update in real time from onchain data, not from quarterly manual reviews.
- **Comparability:** a score of 0.8 on a stablecoin and 0.8 on a liquid staking token mean the same thing, i.e. current conditions are highly abnormal relative to that asset's own history.
- **Auditability:** every output is traceable to a specific onchain read, a calibrated parameter, and a documented methodology.
- **Actionability:** the framework produces not just scores but probability of default estimates and decision support outputs conditioned on actual vault LTV structures, the numbers a curator needs to set caps and the numbers a depositor needs to assess exposure.

The Morpho ecosystem has demonstrated that permissionless curation can scale. This RFC proposes the risk infrastructure that scaling responsibly requires.

## Prior Art: Credora by RedStone

Credora (acquired by RedStone in September 2025) introduced consensus-based risk ratings on Morpho in March 2025 and deserves credit for establishing that collateral risk transparency belongs in the protocol interface. Our work operates at a different architectural layer: rather than producing periodic, curator-opt-in ratings, we produce a continuous, fully onchain-derivable infrastructure stack that updates in real time from live blockchain state and is available for any asset, any vault, without opt-in. The two approaches are complementary; ours does not replace Credora's ratings but fills the structural gaps — recursive dependency unwrapping, governance mutation risk, real-time oracle monitoring, and decision support — that point-in-time ratings cannot address by design. A detailed technical comparison is provided in the Technical Appendix.

## The Risk Infrastructure Stack

Rather than a single monolithic model, what we are building is a modular four-layer stack. Each layer has distinct users, use cases, and value propositions. Layers can be adopted independently or composed together.

**Layer 1: Observability**
The Dependency Graph Engine (DIG) and Yield Tree Decomposition (YTD) provide granular structural decomposition of any EVM asset or Morpho vault: wrapping layers, dependencies, collateral concentration, circularity, yield sources, and LLTV exposure at every recursive level. This layer is valuable as a standalone product, teams with their own internal risk frameworks can use it purely as a robust observability and decomposition engine, without adopting the scoring layer. The wsrUSD output in the Abstract is a live example of what Layer 1 produces today.

**Layer 2: Risk Translation**
The six-factor model (Z₁–Z₆) translates structural complexity into quantified, cross-asset comparable risk signals. This layer takes the dependency graph and yield tree as input and produces continuous factor scores, effective volatility (σe), intrinsic ratings (AAA–CCC), and contextual probability of default (PDT). It is presented as a standardised open-source baseline, not as the only possible implementation of this translation layer. Teams with existing quantitative risk engines can use Layer 1 outputs as input to their own models.

**Layer 3: Monitoring**
Continuous surveillance of onchain state, triggering alerts on regime changes, oracle staleness, governance mutations, leverage loop formation, and liquidity deterioration. The value here is operational, not just analytical: the monitoring layer turns static scores into a live early-warning system that tracks the evolution of risk between scoring cycles.

**Layer 4: Decision Support**
Once a risk signal is detected and quantified, the system produces actionable outputs for curators: cap adjustment recommendations based on current PDT, exposure reduction guidance under stress scenarios, alert workflows tied to specific factor threshold breaches, and vault-level risk dossiers summarising current posture and recommended actions. This layer closes the loop between analysis and execution. It is partially delivered in Phase 2 of this grant, with the full action layer proposed as a subsequent community initiative.

## Description of the DeFi Collateral Asset Risk Model

The risk translation layer (Layer 2) is a fully quantitative, onchain-native framework for assessing the risk of any EVM collateral asset. It requires no manual categorisation, contains no hardcoded parameters, and is designed to update in real time from blockchain data. A complete mathematical treatment is available in the full technical reference: [DeFi Collateral Asset Risk Model — Mathematical Reference v3.0](https://docsend.com/v/63dkk/defi-collat-asset-risk-model).

The model produces two outputs from the same underlying structure. The **Intrinsic Risk Grade (σe)** measures how risky an asset is to hold independent of any lending context, expressed as an annualised effective volatility and mapped to a seven-tier rating from AAA to CCC. The **Contextual Probability of Default (PDT)** measures the probability that the asset causes a liquidation failure in a specific Morpho market over a 1-day, 7-day, or 30-day horizon, activating when the asset is deployed as collateral against a specific LTV structure. A curator needs σe to decide whether to list an asset; they need PDT to decide what cap and LLTV to set once listed.

Risk is decomposed into six onchain-observable factors: Market (Z₁), Counterparty (Z₂), Smart Contract (Z₃), Liquidity (Z₄), Oracle (Z₅), and Governance/Mutation (Z₆), each scored on a continuous [0,1] scale derived entirely from onchain reads and cross-asset comparable by construction. Factor scores feed into a GJR-GARCH(1,1)-t regression with Hidden Markov Model regime-switching for correlation, producing a continuous distance-to-default that degrades visibly as conditions worsen rather than crossing a binary threshold.

Today, the DIG and YTD (Layer 1) are operational and producing live outputs. This grant funds Layers 2, 3, and the initial Layer 4 decision support outputs. Full technical detail on all model components is provided in the Technical Appendix.

## Development

### What Is Built Today

Sigma Labs has built and validated two operational tools currently running on live onchain data, forming the complete Layer 1 observability stack.

The **Dependency Graph Engine (DIG)** monitors 7+ protocol integrations and maps the full onchain relationship graph of any EVM asset, including wrappers, LP positions, Pendle PT/YT structures, lending markets, and pledge relationships, producing structured JSON output and visual dependency trees. Live outputs cover assets with 500+ nodes in their dependency graph.

The **Yield Tree Decomposition (YTD)** translates the dependency graph into a recursive breakdown of yield sources, collateral allocation, LLTV exposure, and concentration risk at every layer of a composed asset. The wsrUSD output in the Abstract and the reUSD graph below are direct outputs of these tools running on live blockchain state, not illustrations or mock-ups.

Layer 1 is already useful as a standalone product. The retroactive case studies demonstrating this are provided in full in the Evidence Appendix.

### What This Grant Delivers

This grant is structured in two phases, submitted as a single governance vote. Phase 2 is gated on verified delivery of Phase 1.

**Phase 1: Observability and Monitoring**

Phase 1 completes Layer 1 for the top 30 Morpho vaults by TVL and delivers Layer 3 monitoring. Every output is public, free, and community-verifiable from day one.

| Deliverable | Layer | Target |
|---|---|---|
| DIG + YTD live on top 30 Morpho vaults by TVL, open-sourced | Layer 1 | Months 1–2 |
| Oracle staleness, governance mutation, and leverage-loop alert system | Layer 3 | Months 3–4 |
| Dashboard v1: per-vault dependency graphs, yield trees, risk dossiers | Layer 1 + 3 | Months 5–6 |
| 2 named Morpho curator trials with direct feedback integration | Layer 1 + 3 | Month 5 |
| Retroactive case study published (USD0++, xUSD, Resolv/USR) | Layer 1 | Month 6 |

**Phase 2: Risk Translation and Decision Support (gated on Phase 1 delivery)**

Phase 2 delivers Layer 2 (quantitative scoring) and the initial Layer 4 decision support outputs. Scores run in shadow mode for a minimum of 60 days before any binding governance integration is proposed.

| Deliverable | Layer | Target |
|---|---|---|
| Z₁–Z₆ factor scoring live | Layer 2 | Months 7–8 |
| GJR-GARCH-t calibration + HMM regime-switching operational | Layer 2 | Month 9 |
| σe, intrinsic rating (AAA–CCC), and contextual PD (PD₁d, PD₇d, PD₃₀d) in shadow mode | Layer 2 | Month 10 |
| Cap adjustment recommendations and exposure reduction guidance based on live PDT | Layer 4 | Month 11 |
| External methodology review complete | Layer 2 | Month 11 |
| Full open-source release, public documentation, community handover | All | Month 12 |

### First Results

The stack produces two complementary Layer 1 outputs for any Morpho collateral asset: a **dependency graph** mapping the full chain of onchain relationships, and a **recursive risk decomposition** translating that structure into factor-level risk findings.

**Dependency Graph: reUSD**

The graph below illustrates the recursive dependency unwrapping engine applied to reUSD (Re Protocol), tracing every wrapper, LP position, Pendle market, and Morpho lending market from the top-level asset down to individual collateral markets.

![reUSD_neigh](https://hackmd.io/_uploads/rkw5Eug2We.png)

**Deep Risk Decomposition: wsrUSD (Reservoir)**

wsrUSD is 96%+ a wrapper around steakUSDC, itself a Morpho vault with 85.8% BTC concentration at 86% LLTV, 4–5 layers of recursive smart contract risk, a circular reflexivity loop via smokeUSDT, and a base APY of ~1.89% inadequate for this structural complexity. The full yield tree, risk findings, and scorecard are provided in the Evidence Appendix.

### Morpho Integrations

Phase 1 delivers Layer 1 and Layer 3 coverage of the top 30 Morpho vaults by TVL, including all major collateral types: wrapped BTC variants (cbBTC, WBTC), LSTs (wstETH, weETH, rsETH), synthetic stablecoins (USDe, sUSDe, USD0), and RWA-backed assets. Each vault entry includes a dependency graph, a full recursive yield tree, an alert configuration, and a plain-language risk dossier. Phase 2 extends this with per-market σe, intrinsic rating, 1d/7d/30d PD estimates, and cap recommendations across 50+ assets.

### Support and Maintenance

Following the 12-month delivery period, **Sigma Labs will operate and maintain the full stack in a live production environment for a minimum of 12 additional months**. This includes daily recalibration of factor scores, weekly rating updates published onchain or via a public API, monthly MLE recalibration of sensitivity coefficients and GARCH parameters, and quarterly recalibration of Vasicek correlations and HMM parameters. Any new Morpho collateral asset reaching material TVL will be onboarded within 7 days of listing. Exploit events, oracle anomalies, and circuit-breaker activations will trigger immediate model updates and public alerts.

## Proposed Budget

**Total Grant Request: $250,000 in $MORPHO tokens — 12-month project — 2 delivery-gated tranches — single governance vote.**

| Phase | Period | Amount | Gate |
|---|---|---|---|
| Phase 1: Observability and Monitoring | Months 1–6 | $120,000 | Governance approval |
| Phase 2: Risk Translation and Decision Support | Months 7–12 | $130,000 | Verified Phase 1 delivery |
| **Total** | **12 months** | **$250,000** | |

*All amounts denominated in USD equivalent, paid in $MORPHO tokens at the 7-day TWAP price on the disbursement date of each tranche.*

**Phase 1 Budget Detail ($120,000):**

| Category | Description | Amount |
|---|---|---|
| Infrastructure & Data | RPC nodes, archive access, cryo data pipeline, WebSocket monitoring | $25,000 |
| DIG / YTD Engineering | Protocol watchers, JSON output, dependency graph engine, yield tree engine | $40,000 |
| Dashboard & Alerts | Dashboard v1, vault dossiers, oracle/governance/leverage-loop alert system | $25,000 |
| Curator Trials | Integration with 2 named Morpho curator teams, feedback cycles | $10,000 |
| Operations & Legal | Entity costs, legal review, open-source licensing | $10,000 |
| Contingency | | $10,000 |
| **Phase 1 Total** | | **$120,000** |

**Phase 2 Budget Detail ($130,000):**

| Category | Description | Amount |
|---|---|---|
| Quantitative Research | Z₁–Z₆ calibration, GJR-GARCH-t, HMM regime-switching, Merton PD layer, backtesting framework | $60,000 |
| Engineering | Live factor scoring pipeline, shadow mode infrastructure, event-driven architecture | $25,000 |
| Decision Support | Cap recommendation engine, exposure reduction guidance, alert-to-action workflow | $15,000 |
| External Methodology Review | Independent review of model specification, calibration, and backtesting | $20,000 |
| Open-Source Release | Documentation, community handover, GitHub publication | $5,000 |
| Contingency | | $5,000 |
| **Phase 2 Total** | | **$130,000** |

## Team: Sigma Labs

Sigma Labs is a French quantitative research team, combining academic backgrounds from **École Polytechnique**, **École Normale Supérieure (ENS)** and **Université Paris Dauphine–PSL** with hands-on experience co-founding and operating DeFi protocols. The **DeFi Collateral Asset Risk Model v3.0** and the live DIG/YTD outputs in this RFC are our primary proof of work.

**Vincent Danos:** École Polytechnique alumnus, co-founder of DeFi protocols and Research Director at CNRS.

**Amaury Denny:** Paris Dauphine–PSL, Quantitative Finance & Technology; 42 School Paris. Former VC research at KuCoin Labs, co-founder of a crypto family office with $20M+ AuM.

**Hamza E.:** ENS alumnus, PhD in Financial Markets, co-founder of DeFi protocols and quant prop shops.

**Daniel J.:** ESILV alumnus, co-founder of a liquid fund, treasury manager and investor.

## Sustainability and Exit Clause

This framework is designed to outlast its initial grant. All code, models, and outputs will be fully open-sourced under the [MIT License](https://opensource.org/license/mit) upon Phase 1 delivery, with the Phase 2 quantitative layer following at Month 12. The GitHub repository will be public and forkable by any community member, curator, or independent researcher from Phase 1 completion onward.

If Phase 1 milestones are not delivered by Month 6, the Phase 2 tranche is not disbursed and the community retains full ownership of all Phase 1 outputs produced to date. If Sigma Labs ceases operations or is unable to continue at any point during the project, all data pipelines, model specifications, and documentation will be transferred to a community-designated maintainer. No single entity, including Sigma Labs, should be a point of failure for risk transparency infrastructure that the Morpho ecosystem depends on.

Post-grant sustainability: Layer 1 is a public good with no revenue model required. Layers 2 and 3 may in future support a paid API for institutional curators seeking higher-frequency or custom data access, with a free public tier maintained in perpetuity. Any such commercial layer would be proposed to the community separately and would not affect open access to core model outputs.

## Next Steps

This RFC is submitted as a single governance vote covering both phases. Phase 2 disbursement is gated on Phase 1 delivery, confirmed by a community checkpoint post at Month 6.

The feedback window is three weeks from the date of this post. We welcome input from **curators**, **depositors**, **risk researchers**, and the broader Morpho community on model design, scope, milestone structure, and budget before the vote is called.

Following Phase 2 delivery, Sigma Labs will publish quantitative model scores in **shadow mode** for a minimum of 60 days, non-binding and publicly visible, before any binding governance integration is proposed. This gives the community a concrete basis for evaluating model quality before it is used in any consequential decision.

To engage: comment directly on this thread or reach out at [research@sigmalabs.fi](mailto:research@sigmalabs.fi). Once rough consensus is reached, this proposal will be submitted for onchain vote.

## Considerations

The model and all associated code will be fully open-sourced upon delivery under the MIT License. The GitHub repository will be public and accessible to the entire community, including factor score pipelines, calibration scripts, backtesting framework, and dashboard code. Every model output will be traceable to a specific onchain read, a documented equation, and a versioned parameter set. Community members, independent researchers, and third-party auditors are explicitly encouraged to verify, challenge, and build on top of the framework.

This RFC is accompanied by two appendices: a **Technical Appendix** containing the full six-factor model specification, GARCH/HMM equations, Merton PD layer, and calibration methodology; and an **Evidence Appendix** containing the full wsrUSD yield tree, the reUSD dependency graph, the retroactive case study against USD0++, xUSD, and Resolv/USR, and the complete claim register.

---

# Evidence Appendix

## E1. Claim Register

| Component | Layer | Status | Evidence |
|---|---|---|---|
| Dependency Graph Engine (DIG) | Layer 1 | **Built** | 7+ protocol watchers, live outputs (reUSD, wsrUSD, cUSD: 500+ nodes) |
| Yield Tree Decomposition (YTD) | Layer 1 | **Built** | Structured JSON + visual trees (wsrUSD, reUSD, Sentora PYUSD) |
| Recursive risk decomposition (qualitative) | Layer 1 | **Built** | wsrUSD analysis: 6 findings, scorecard, circular exposure identified |
| Oracle / governance / leverage-loop alert system | Layer 3 | Phase 1 deliverable | Not yet built; Month 3 target |
| Dashboard v1 + vault dossiers | Layer 1 + 3 | Phase 1 deliverable | Not yet built; Month 5 target |
| Curator trials (2 named) | Layer 1 + 3 | Phase 1 deliverable | Not yet started; Month 5 target |
| Six-factor scoring (Z₁–Z₆) | Layer 2 | Phase 2 deliverable | Mathematical specification complete; no calibrated live outputs |
| GJR-GARCH(1,1)-t calibration | Layer 2 | Phase 2 deliverable | Methodology specified; no fitted parameters or backtest results |
| HMM regime-switching (3 regimes) | Layer 2 | Phase 2 deliverable | Methodology specified; no calibrated transition matrix |
| Per-block effective volatility (σe) | Layer 2 | Phase 2 deliverable | Zero live outputs; Month 10 target (shadow mode) |
| Merton-layer contextual PD | Layer 2 | Phase 2 deliverable | Zero live outputs; Month 10 target (shadow mode) |
| Intrinsic rating (AAA–CCC) | Layer 2 | Phase 2 deliverable | No cross-sectional distribution yet; Month 10 target |
| Cap recommendations and exposure reduction guidance | Layer 4 | Phase 2 deliverable | Not yet started; Month 11 target |
| External methodology review | Layer 2 | Phase 2 deliverable | Not started; Month 11 target |

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
    │       ├── [collateral] IDLE
    │       └── [borrow] USDC
    ├── [PSM] PSM-USDC — $500K
    ├── steakRUSD — $53K
    ├── Steakhouse High Yield — $6
    └── smokeUSDT [V1] — $2
        ├── wsrUSD/USDT (94% LLTV) — $39,551,744.73 (99.6%) @ 3.37% APY
        │   ├── [collateral] wsrUSD    ← circular exposure
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
        │   │   ├── [RWA] Bloom/Grove — $2.94B
        │   │   ├── [RWA] Obex — $606M
        │   │   └── [underlying] USDS [Sky]
        │   │       ├── [PSM] Lite-PSM USDC — $4.85B
        │   │       ├── [CDP] ETH-C (ETH) — $317M
        │   │       ├── [CDP] ETH-A (ETH) — $164M
        │   │       └── [... additional CDP vaults]
        │   └── [borrow] USDT
        └── [... additional markets]
```

**Risk scorecard:**

| Risk Dimension | Assessment |
|---|---|
| Smart contract risk | 🔴 4–5 layers of recursive depth |
| Concentration | 🔴 85.8% BTC (cbBTC + WBTC) |
| LLTV | 🟡 86% — aggressive but standard on Morpho |
| Yield / risk ratio | 🔴 ~2% for this complexity and tail exposure |
| Circularity | 🔴 wsrUSD as collateral in smokeUSDT at 94% LLTV |
| Exit liquidity | 🟡 PSM buffer of $500K only |


## E3. Retroactive Case Study

The following analysis documents what was visible onchain before each of the three major Morpho incidents of 2025–2026. The purpose is not to claim predictive certainty but to demonstrate that the structural signals behind each failure were present and readable from public blockchain state before the damage occurred. Our tooling is designed to detect each of these signal categories systematically.

### Case 1: USD0++ / Usual Protocol (January 10, 2025)

**What happened:** Usual Protocol unilaterally changed USD0++'s redemption mechanism, replacing unconditional 1:1 redemption with an $0.87 floor price without advance notice. Vault curators on Morpho had hardcoded USD0++:USDC oracle prices at 1:1 parity. TVL in the affected vault halved within hours. Some curators exited positions ahead of the public announcement, prompting accusations of insider trading.

**What was visible onchain before the incident:**

*Oracle configuration (Z₅):* The USD0++:USDC Morpho markets used a hardcoded oracle valuing USD0++ at exactly $1.00 with no deviation circuit breaker and no secondary market reference. Readable from the oracle contract address stored in Morpho market parameters at any block before the incident.

*Governance structure (Z₆):* The redemption mechanism was controlled by a Usual Protocol admin role with no timelock on parameter changes. The specific change required no governance vote, no delay period, and no onchain signal visible to curators or depositors before execution. Readable from the TimelockController contract.

*Dependency structure (DIG):* The DIG is designed to detect USD0++ as a staked derivative of USD0 with a four-year maturity structure, flagging it as a bond-like instrument inconsistent with a hardcoded $1.00 oracle valuation.

| Signal | Factor | Detectable by tooling |
|---|---|---|
| Hardcoded oracle at $1.00, no market reference | Z₅ | ✅ Yes |
| No timelock on redemption mechanism | Z₆ | ✅ Yes |
| Bond-like maturity structure priced as spot | Z₂ | ✅ Yes |

### Case 2: Stream Finance / xUSD (November 2025)

**What happened:** Curators routed USDC deposits into recursive leverage loops backed by xUSD. When xUSD's oracle refused to update, an estimated $285M–$700M was left at risk across Morpho, Euler, and Silo. Curators who had publicly claimed zero exposure were found to hold significant positions when the protocol collapsed.

**What was visible onchain before the incident:**

*Leverage loop structure (YTD):* The recursive loop — USDC deposits → xUSD collateral → USDC borrow → repeat — was fully visible from Morpho market interactions and supply/borrow balances at each layer, with leverage ratios reportedly reaching 10x. The YTD is designed to detect and flag this structure.

*Oracle staleness risk (Z₅):* xUSD's oracle depended on offchain infrastructure with a documented maximum staleness window readable from the oracle contract's heartbeat parameter. Under stress, the oracle's failure to reprice was a documented design characteristic, not a surprise.

*Supply velocity and redemption queue (Z₂):* xUSD's totalSupply growth rate in the weeks before the collapse was abnormally high relative to its backing. The 65-day redemption queue was readable from the protocol contract and created a structural mismatch with 10x leverage positions.

| Signal | Factor | Detectable by tooling |
|---|---|---|
| Recursive leverage loop structure | Z₄ / YTD | ✅ Yes |
| Oracle staleness heartbeat window | Z₅ | ✅ Yes |
| Abnormal supply velocity | Z₂ | ✅ Yes |
| 65-day redemption queue | Z₂ | ✅ Yes |

### Case 3: Resolv / USR Exploit (March 22, 2026)

**What happened:** An attacker exploited a flaw in Resolv's USR minting contract, minting 80 million unbacked USR tokens from approximately $200,000 in USDC and extracting roughly $25 million in ETH. The wstUSR oracle was hardcoded and never repriced: wstUSR was marked at $1.13 while trading at $0.63 on secondary markets. Fifteen Morpho vaults were impacted. Chaos Labs founder Omer Goldberg documented the mechanism: the oracle was hardcoded and thus never repriced, allowing traders to post discounted collateral at inflated valuations and drain USDC from lending vaults. Some curators' automated systems continued supplying liquidity into compromised markets hours after the exploit began.

**What was visible onchain before the incident:**

*Privileged minting role (Z₆):* The USR minting function was controlled by a single EOA with no mint limits, no oracle checks, and no multisig requirement. A single private key held uncapped minting authority over a stablecoin integrated into $500M+ of DeFi lending, readable from the contract's access control storage slots.

*Hardcoded oracle (Z₅):* The wstUSR:USDC oracle was hardcoded with no secondary market reference and no circuit breaker. Once USR depegged, the oracle continued reporting stale prices while the asset traded at a steep discount, exactly the mechanism that enabled the collateral arbitrage.

*Leverage loop and contract age (YTD, Z₃):* Resolv's TVL grew from under $50M to over $650M in under three months, driven by leveraged looping on Morpho and Euler. The minting contract had high TVL relative to its age, a signal the Lindy-weighted Z₃ is designed to penalise.

| Signal | Factor | Detectable by tooling |
|---|---|---|
| Single EOA minting role, no mint limits | Z₆ | ✅ Yes |
| Hardcoded oracle, no deviation circuit breaker | Z₅ | ✅ Yes |
| Recursive leverage loop, reflexive dependency | Z₄ / YTD | ✅ Yes |
| High TVL, low contract age | Z₃ | ✅ Yes |

### Summary

Across all three incidents, the structural risk signals were present onchain before the damage occurred. None required inside information, offchain intelligence, or predictive modelling to detect. They required systematic, factor-level reading of publicly available blockchain state.

| Incident | Primary signals | Factors |
|---|---|---|
| USD0++ / Usual (Jan 2025) | Hardcoded oracle, no timelock, bond mispriced as spot | Z₅, Z₆, Z₂ |
| Stream / xUSD (Nov 2025) | Leverage loop, oracle staleness, 65-day redemption queue | Z₄, Z₅, Z₂ |
| Resolv / USR (Mar 2026) | Single EOA minter, hardcoded oracle, leverage loop, low contract age | Z₆, Z₅, Z₄, Z₃ |

# Technical Appendix

## T1. Model Overview

The risk translation layer (Layer 2) decomposes the risk of any EVM collateral asset into six onchain-observable factors, estimates all parameters via Maximum Likelihood Estimation on historical data, and integrates factor correlations through a Vasicek structure with Hidden Markov Model regime-switching. It produces two outputs: an intrinsic risk grade (σe) independent of any lending context, and a contextual probability of default (PDT) conditioned on a specific Morpho market's LTV structure.

The complete mathematical reference is available at: [DeFi Collateral Asset Risk Model — Mathematical Reference v3.0](https://docsend.com/v/63dkk/defi-collat-asset-risk-model).

## T2. Six Onchain Risk Factors

Each factor Zᵢ ∈ [0,1] is constructed entirely from onchain reads. All scores are cross-asset comparable: Z = 0.9 on a stablecoin and Z = 0.9 on an LST both mean current conditions are highly abnormal relative to that asset's own history.

### Factor 1: Market Risk (Z₁)

Log-returns rₜ = ln(Pₜ/Pₜ₋₁) computed from onchain price series. For stablecoins, peg deviation returns are used; for LSTs, exchange rate returns. Realised volatility blended across three BIC-calibrated horizons:

```
σ_blend = w₁·σ̂_τ₁ + w₂·σ̂_τ₂ + w₃·σ̂_τ₃
```

Z₁ normalises current short-horizon volatility against its own rolling history via Φ.

**Sources:** Chainlink `latestRoundData()`, Uniswap V3 `observe()`, LST exchange rate contracts. **Update:** per block.

### Factor 2: Counterparty Risk (Z₂)

Captures issuer, custodian, and validator risk. Applicable metric set determined automatically via contract bytecode introspection.

| Asset Type | Key Metrics |
|---|---|
| All assets | Holder Gini coefficient, supply velocity |
| Wrapped tokens | Backing ratio, market discount vs. reference asset |
| LSTs | Validator Gini, exchange rate trend, withdrawal queue pressure |
| CDP stablecoins | Reserve health, redemption flow |
| Vault tokens | TVL stability, yield anomaly vs. benchmark |

Sub-scores aggregated via MLE-optimised weights ωₘ. **Update:** hourly; per block during stress.

### Factor 3: Smart Contract Risk (Z₃)

Exploit probability estimated from Lindy-weighted safety metrics and similarity to a reference database of ~3,050 historically exploited contracts:

```
sim(A, E) = cos(v_A, v_E) · |S_A ∩ S_E| / |S_E|
```

Threshold θ* calibrated by AUC-ROC on temporal train/test split. **Update:** on contract upgrade events only.

### Factor 4: Liquidity Risk (Z₄)

Position-size-aware Conditional Liquidity-at-Risk:

```
LiqARₚ(Q_fund) = Percentile_p [ slippage(Q_fund) | σ̂_1h > σ̂^(1-p)_1h ]
```

Slippage replayed against historical pool states for actual position size Q_fund. Depth threshold adaptive: k* · σ̂_7d (k* calibrated by BIC).

**Sources:** Uniswap V3 `quoteExactInputSingle()`, Curve `get_balances()`, Mint/Burn/Swap events via cryo. **Update:** every 5 minutes; per block during stress.

### Factor 5: Oracle Risk (Z₅)

Φ-normalised across four dimensions: staleness (update gap vs. heartbeat), inaccuracy (Chainlink vs. DEX TWAP deviation), redundancy (transmitter count), and tail downtime risk. Oracle staleness under market stress captured via the correlation ρ₁·ρ₅ in the Vasicek structure.

**Sources:** Chainlink `AnswerUpdated` events, `aggregator.transmitters()`. **Update:** hourly; per block during stress.

### Factor 6: Governance / Mutation Risk (Z₆)

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

Three horizons computed simultaneously: PD₁d, PD₇d, PD₃₀d. Student-t tails used throughout. Under Gaussian assumptions, a −50% daily move has probability ~10⁻¹⁵; under Student-t with ν̂ ≈ 3–5, tail events receive realistic probabilities.

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
7. **Incident replay:** elevated σe and deteriorated ratings verified against USD0++, xUSD, and Resolv/USR using historical onchain data.

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