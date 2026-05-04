---
title: '[RFC] A Morpho-compatible DeFi Collateral Asset Risk Framework - Phase 1'

---

# [RFC] A Morpho compatible DeFi Collateral Asset Risk Framework - Phase 1
*v0.5 — April 2026*
 
## Abstract
 
### The Problem: Four illusions at the heart of onchain lending
 
Morpho's isolated market architecture is a genuine advance: each market is a standalone unit, and a default in one market does not mechanically drain another. But this architectural compartmentalisation has created a false sense of independence among lenders and borrowers alike. The DeFi Integration Graph (DIG) and Yield Tree Decomposition (YTD), two tools we are building and are running on live blockchain states reveal a **quadruple root of illusion** that persists across lending platforms:
 
**1. The collateral independence illusion:** markets that appear isolated share hidden dependencies through their collateral assets. A vault may lend on market M₁: siUSD/USDC, where the collateral siUSD, a staked form of vault token contains wsrUSD (itself another staked form of vault token from Reservoir) in its yield tree decomposition. A depeg of wsrUSD propagates upward through the YTD and depegs siUSD; the default probability of M₁ therefore depends on the default probability of a market it has no direct contractual link to. Isolated markets are not independent markets when their collateral assets share recursive roots.
 
**2. The TVL illusion:** headline TVL figures count the same capital multiple times as it flows through layers of wrapping, vaulting, and re-depositing. A single dollar of USDC deposited into a vault, lent against collateral that is itself a vault token backed by the same USDC, inflates reported TVL at every layer. The aggregate number suggests depth and diversification that do not exist. The DIG traces capital through these recursive loops and reports the actual terminal exposure at each leaf.
 
**3. The diversification illusion:** a vault allocating across five Morpho markets appears diversified. But if 85% of the underlying collateral across those markets resolves to two BTC custodians (e.g., Coinbase and BitGo) through the YTD, the vault holds a concentrated BTC custody bet dressed up as a multi-market strategy. Diversification measured at the market level can vanish entirely when measured at the terminal asset level.
 
**4. The (non-)leverage illusion:** a passive depositor in a vault may believe their exposure is unleveraged. But if another user loops through the same vault, depositing the vault token as collateral to borrow the underlying, redepositing, and repeating, the passive depositor's capital is silently leveraged by the looper's activity. The increased risk is not visible at the investment surface. These dark loops are detectable from onchain state but are not surfaced by any existing interface.
 
These four illusions are not edge cases. They are structural features of composable DeFi that affect the majority of Morpho vaults by TVL today. Our tooling exists to make them visible.
 
## What we are building today
 
Sigma Labs has started to build and validate two operational tools currently running on live onchain data.
 
The **Dependency Graph Engine (DIG)** monitors 7+ protocol integrations and maps the full onchain relationship graph of any EVM asset, including wrappers, LP positions, Pendle PT/YT structures, lending markets, and pledge relationships, producing structured JSON output and visual dependency trees. **Live outputs cover assets with 500+ nodes in their dependency graph**.
 
The **Yield Tree Decomposition (YTD)** translates the dependency graph into a recursive breakdown of yield sources, collateral allocation, LLTV exposure, and concentration risk at every layer of a composed asset. 

The reUSD graph and the wsrUSD decomposition in First results below are direct outputs of these tools running on live blockchain state, not illustrations or mock-ups.
 
### First results
 
**Dependency Graph: reUSD**
 
The graph below illustrates the recursive dependency unwrapping engine applied to reUSD (Re Protocol), tracing every wrapper, LP position, Pendle market, and Morpho lending market from the top-level asset down to individual collateral markets.
 
![reUSD_neigh](https://hackmd.io/_uploads/rkw5Eug2We.png)
 
**Yield Tree Decomposition: wsrUSD (Reservoir)**

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
 
wsrUSD is 96%+ a wrapper around steakUSDC, itself a Morpho vault with 85.8% BTC concentration at 86% LLTV, 4–5 layers of recursive smart contract risk, a circular reflexivity loop via smokeUSDT, and a base APY of ~1.89% inadequate for this structural complexity.
 
## The Morpho curationship model and its structural gap
 
Morpho has redefined lending protocol design by separating infrastructure from risk management. Anyone can spin up an ERC-4626 vault, define collateral exposure, set supply caps, choose oracles, and offer curated lending strategies to depositors. The protocol provides the rails; the curator provides the judgment.
 
The model's appeal is real. But its rapid expansion has exposed a structural gap: **the rigor of collateral risk assessment varies enormously across curators, and depositors have consistently lacked the tools to evaluate it**.
 
This gap is not a matter of opinion. Independent research published in April 2026 by Luca Prosperi (*Physics of On-Chain Lending*) documented 20–100x mispricing of collateral risk across major DeFi lending markets. Risk analytics firm Chaos Labs, in its post-mortem of the Resolv/USR exploit, documented the same structural failure: hardcoded oracles, unchecked minting roles, and leverage loops that were all readable from public blockchain state before any damage occurred. The problem is architectural, not incidental, and it recurs because no shared framework exists to read the onchain signals systematically.
 
### A growing pattern of confidence failures
 
Two recent episodes made curator collateral risk impossible to ignore:
 
- **November 2025, Stream Finance / xUSD:** Curators routed USDC deposits into recursive leverage loops backed by the synthetic stablecoin xUSD, leaving an estimated $285M–$700M at risk across multiple lending protocols when its oracle refused to update. **Our tooling is designed to detect recursive leverage loop structures and oracle staleness**.
- **March 2026, Resolv / USR Exploit:** An attacker exploited a flaw in Resolv's USR minting contract, creating approximately 80 million unbacked tokens from roughly $200,000 in USDC. The oracle was hardcoded and never repriced: wstUSR was marked at $1.13 while trading at $0.63 on secondary markets. Fifteen Morpho vaults were impacted. Chaos Labs founder Omer Goldberg documented the mechanism in real time. **Our tooling is designed to detect privileged single-EOA minting roles with no mint limits and hardcoded feeds with no deviation circuit breaker**.

A full onchain signal analysis for each incident is provided in the Evidence Appendix.
 
### Motivation
 
These incidents share a common thread: they were not fundamentally unpredictable. The risks were present onchain before each failure, in oracle configurations, minting mechanics, governance structures, liquidity depth, and backing ratios. What was missing was **a systematic framework to read those signals and make them visible to curators and depositors before something breaks**.
 
This RFC proposes to build that framework: a modular, open-source disclosure infrastructure for Morpho collateral assets, delivered as a public good and community-verified. Its goals are concrete:
 
- **Continuity:** disclosures update from live onchain data, not from periodic manual reviews.
- **Comparability:** structural features are labelled consistently across asset types using categorical badges, not aggregate grades.
- **Auditability:** every output is traceable to a specific onchain read and a documented methodology.
- **Reproducibility:** any third party can verify any disclosure from the same blockchain state.

## Prior Art: Credora by RedStone
 
Credora (acquired by RedStone in September 2025) introduced consensus-based risk ratings on Morpho in March 2025 and deserves credit for establishing that collateral risk transparency belongs in the protocol interface. Our work operates at a different layer: rather than producing periodic, curator-opt-in ratings, **we produce a continuous, fully onchain-derivable disclosure infrastructure that updates from live blockchain state and is available for any asset, any vault, without opt-in**. The two approaches are complementary; ours does not replace Credora's ratings but fills structural gaps that point-in-time ratings cannot address by design.
 
## What this grant delivers
 
This grant covers the top whitelisted Morpho vaults by TVL (snapshot taken at Month 1), including all major collateral types: wrapped BTC variants (cbBTC, WBTC), LSTs (wstETH, weETH, rsETH), synthetic stablecoins (USDe, sUSDe, USD0), and RWA-backed assets. Each vault entry includes a dependency graph, a full recursive yield tree, categorical badges, a nutrition label, and an alert configuration. All recursive dependencies representing ≥5% of any covered vault's backing are traced to their terminal assets.

We are not building a risk authority. We are not producing safe/unsafe verdicts, aggregate grades, or composite risk scores. We are building a **disclosure layer**: structured, reproducible, machine-readable facts about what a collateral asset actually is, what it depends on, and how those dependencies change over time.
 
### The data intelligence layer
 
We are building a **data intelligence layer** for Morpho collateral assets, infrastructure that continuously reads, decomposes, and discloses the structural reality of onchain collateral. It has four concrete outputs:
 
**1. The collateral registry:** a public, machine-readable registry covering every collateral asset in the top whitelisted Morpho vaults (by TVL, snapshot at Month 1), plus all recursive dependencies representing ≥5% of any covered vault's backing. Each registry entry contains a full DIG dependency graph, a recursive YTD yield tree, terminal asset exposure, and categorical badges for structural features (`circularity`, `single-EOA roles`, `hardcoded oracles`, `leverage loops`, `concentrated custody`).
 
**2. The diff stream:** a continuous onchain monitoring feed that tracks mutations to registered assets: oracle configuration changes, governance role transfers, timelock modifications, supply velocity anomalies, new leverage loop formation, and liquidity deterioration. The diff stream turns static decompositions into a live surveillance layer. Alerts fire on specific structural events, not on aggregate score thresholds.
 
**3. Nutrition labels:** a standardised, human-readable summary for each covered vault, a one-page disclosure document that answers: what does this collateral actually depend on? How deep is the wrapping? Where is the concentration? What governance controls exist? What oracle architecture is in use? Nutrition labels use categorical badges only (e.g., "4+ recursive layers", "85%+ single-asset concentration", "no timelock on admin role"), no aggregate grade, no safe/unsafe language.
 
**4. The Manifest template:** an open-source template that any Morpho curator can adopt to produce standardised structural disclosures for their own vaults, regardless of whether Sigma Labs covers them. The manifest template is designed to be self-serve: a curator fills in the structural fields, and the template generates a nutrition label. The goal is to make disclosure the default, not the exception.
 
### Delivery Schedule
 
All deliverables are public, free, and community-verifiable from day one.
 
| Deliverable | Target |
|---|---|
| Yield Tree Decomposition live on whitelisted Morpho Vaults | Months 1 & 2 |
| DeFi Integration Graph live on whitelisted Morpho Vaults | Months 3 & 4 |
| Oracle staleness, governance mutation, leverage-loop alert system, and exhaustive evidence detection (diff stream) | Months 4 & 5 |
| [Dashboard v1](https://www.sigmalabs.fi/discover) (in progress): per-vault dependency graphs, yield trees, nutrition labels | Month 5 & 6 |
| Manifest template published for curator self-serve disclosure | Month 6 |
 
## Proposed Budget
 
**Total Grant Request: $110,000 in $MORPHO tokens**
 
*All amounts denominated in USD equivalent, paid in $MORPHO tokens at the 7-day TWAP price on the disbursement date.*
 
| Category | Description | Amount |
|---|---|---|
| Infrastructure & Data | RPC nodes, archive access, cryo data pipeline, WebSocket monitoring, API | $25,000 |
| DIG / YTD Engineering | Protocol watchers on major DeFi venues (top 15) and standards, dependency graph engine, yield tree engine | $45,000 |
| Dashboard & Alerts | Dashboard v1, nutrition labels, oracle/governance/leverage-loop alert system, integrable telegram alerting system pipeline | $20,000 |
| Operations & Legal | Entity costs, legal review, open-source licensing | $15,000 |
| Contingency | | $5,000 |
| **Total** | | **$110,000** |
 
## Team: Sigma Labs
 
Sigma Labs is a French quantitative research team, combining academic backgrounds from **École Polytechnique**, **École Normale Supérieure (ENS)**, **Université Paris Dauphine–PSL** and **ESILV** with hands-on experience co-founding and operating DeFi protocols. The live DIG/YTD outputs in this RFC are our primary proof of work.
 
**Vincent Danos:** École Polytechnique alumnus, co-founder of DeFi protocols and Research Director at CNRS.
 
**Amaury Denny:** Paris Dauphine–PSL, Quantitative Finance & Technology; 42 School Paris. Former VC research at KuCoin Labs, co-founder of a crypto family office with $20M+ AuM.
 
**Hamza E.:** ENS alumnus, PhD in Financial Markets, co-founder of DeFi protocols and quant prop shops.
 
**Daniel J.:** ESILV alumnus, co-founder of a liquid fund, treasury manager and investor.
 
## Sustainability and exit clause
 
This framework is designed to outlast its initial grant. All code, models, and outputs will be fully open-sourced under the [MIT License](https://opensource.org/license/mit) upon delivery, with the GitHub repository public and forkable by any community member, curator, or independent researcher from Month 1 onward.
 
If milestones are not delivered by Month 6, the community retains full ownership of all outputs produced to date. If Sigma Labs ceases operations or is unable to continue at any point during the project, all data pipelines, specifications, and documentation will be transferred to a community-designated maintainer. No single entity, including Sigma Labs, should be a point of failure for disclosure infrastructure that the Morpho ecosystem depends on.
 
## What Comes Next
 
This RFC covers a **six-month disclosure infrastructure build and one year maintenance**. We have further plans: a quantitative risk translation layer that takes the structural decompositions produced here and translates them into calibrated, cross-asset comparable risk signals, factor scores, effective volatility estimates, and contextual default probabilities conditioned on specific Morpho market structures. The mathematical foundation for this layer is documented in our [DeFi Collateral Asset Risk Model — Mathematical Reference v3.0](https://docsend.com/v/63dkk/defi-collat-asset-risk-model). Whether and when to propose this as a follow-on grant will depend on the evidence produced by this Phase 1 delivery, and will be submitted as a separate RFC for community evaluation on its own merits.
 
## Next steps
 
The feedback window is three weeks from the date of this post. We welcome input from **curators**, **depositors**, **risk researchers**, and the broader Morpho community on scope, milestone structure, and budget before the vote is called.
 
To engage: comment directly on this thread or reach out at [research@sigmalabs.fi](mailto:research@sigmalabs.fi). Once rough consensus is reached, this proposal will be submitted for onchain vote.
 
## Considerations
 
All code will be fully open-sourced upon delivery under the MIT License. The GitHub repository will be public and accessible to the entire community, including dependency graph engines, yield tree decomposition scripts, alert pipelines, dashboard code, and manifest templates. Every output will be traceable to a specific onchain read and a documented methodology. Community members, independent researchers, and third-party auditors are explicitly encouraged to verify, challenge, and build on top of the framework.
 
This RFC is accompanied by an **Evidence Appendix** containing the claim register, and the retroactive case studies against xUSD and Resolv/USR.
 
---
 
# Evidence Appendix
 
## E1. Claim Register
 
| Component | Status | Evidence |
|---|---|---|
| Dependency Graph Engine (DIG) | **Building** | 7+ protocol watchers, live outputs (reUSD, wsrUSD, cUSD: 500+ nodes) |
| [Yield Tree Decomposition (YTD)](https://www.sigmalabs.fi/discover) | **Building** | Structured JSON + visual trees (wsrUSD, reUSD, Sentora PYUSD) |
| Recursive structural decomposition | **Building** | wsrUSD analysis: 6 findings, scorecard, circular exposure identified |
| Oracle / governance / leverage-loop alert system | Phase 1 deliverable | Not yet built; Month 3 target |
| Dashboard v1 + nutrition labels | Phase 1 deliverable | Not yet built; Month 5 target |
| Manifest template | Phase 1 deliverable | Not yet built; Month 6 target |
 
## E2. Retroactive Case Study
 
The following analysis documents what was visible onchain before two major Morpho incidents. The purpose is not to claim predictive certainty but to demonstrate that the structural signals behind each failure were present and readable from public blockchain state before the damage occurred. **Our tooling is designed to detect each of these signal categories systematically**.
 
### Case 1: Stream Finance / xUSD (November 2025)
 
**What happened:** curators routed USDC deposits into recursive leverage loops backed by xUSD. When xUSD's oracle refused to update, an estimated $285M–$700M was left at risk across Morpho, Euler, and Silo. Curators who had publicly claimed zero exposure were found to hold significant positions when the protocol collapsed.
 
**What was visible onchain before the incident:**
 
- *Leverage loop structure (YTD):* the recursive loop, **USDC deposits → xUSD collateral → USDC borrow → repeat**, was fully visible from Morpho market interactions and supply/borrow balances at each layer, with leverage ratios reportedly reaching 10x. The YTD is designed to detect and flag this structure.
- *Supply velocity and redemption queue:* xUSD's `totalSupply` growth rate in the weeks before the collapse was abnormally high relative to its backing. The 65-day redemption queue was readable from the protocol contract and created a structural mismatch with 10x leverage positions.
 
### Case 2: Resolv / USR Exploit (March 22, 2026)
 
**What happened:** an attacker exploited a flaw in Resolv's USR minting contract, minting 80 million unbacked USR tokens from approximately $200,000 in USDC and extracting roughly $25 million in ETH. The wstUSR oracle was hardcoded and never repriced: wstUSR was marked at $1.13 while trading at $0.63 on secondary markets. Fifteen Morpho vaults were impacted. Some curators' automated systems continued supplying liquidity into compromised markets hours after the exploit began.
 
**What was visible onchain before the incident:**
 
- *Hardcoded oracle:* The wstUSR:USDC oracle was hardcoded with no secondary market reference and no circuit breaker. Once USR depegged, the oracle continued reporting stale prices while the asset traded at a steep discount, exactly the mechanism that enabled the collateral arbitrage.
- *Leverage loop and contract age:* Resolv's TVL grew from under $50M to over $650M in under three months, driven by leveraged looping on Morpho and Euler. The minting contract had high TVL relative to its age.

In both cases, the reserve shock created an onchain price that reflected the true value of the affected assets. The YTD's structural decomposition allowed us to identify the exposure upstream, while the DIG enabled optimal exit within 1–2 blocks of the market shock propagating onchain.