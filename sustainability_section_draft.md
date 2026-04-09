# Sustainability — Draft Section for RFC

## Post-Grant Revenue Model

The grant funds a **public transparency layer** — dependency graphs, vault dossiers, intrinsic ratings, and open-source methodology. This is the commons infrastructure that no single curator is incentivized to build alone.

The team sustains itself through a **paid real-time risk feed for curators**.

### Two tiers

| | Public (grant-funded) | Professional (paid) |
|---|---|---|
| **Update frequency** | Daily snapshots | Block-by-block |
| **Factor scores** | Weekly summary | Continuous streaming |
| **PD estimates** | 30-day horizon, updated weekly | 1d / 7d / 30d, updated per-block |
| **Alerts** | Post-incident analysis | Real-time circuit breaker alerts (oracle staleness, liquidity drain, governance mutation) |
| **Dependency graphs** | Static per-vault dossiers | Live recomposition on contract events |
| **API** | Rate-limited public endpoint | Dedicated WebSocket feed, SLA-backed |
| **Coverage** | Top 30 vaults | Full Morpho universe + custom assets |
| **Integration** | Dashboard | Direct feed into rebalancing agents and automated vault management systems |

### Why curators pay

Curators set LTV, supply caps, and allocation across markets. Today they do this with manual judgment and inconsistent data. A block-by-block risk feed lets them:

- **React before competitors** — detect oracle anomalies, liquidity drain, or collateral stress minutes before the market reprices
- **Automate rebalancing** — pipe factor scores directly into vault management agents for continuous, risk-aware allocation
- **Defend their depositors** — a curator who can point to a quantitative, auditable risk framework attracts more TVL than one who can't
- **Avoid the incidents that destroy reputation** — USD0++, xUSD, and Resolv/USR were all survivable for curators who saw the signals early

### Pricing

| Curator vault TVL | Suggested annual subscription |
|---|---|
| < $10M | $5K |
| $10M – $100M | $10K – $25K |
| $100M – $500M | $25K – $50K |
| > $500M | Custom |

For context: a curator managing $100M TVL at a 1-2% management fee earns $1-2M/yr. A $25K risk feed is 1-2.5% of revenue — trivially justified if it prevents a single incident or attracts additional deposits.

### Path to self-sustaining

- **10 curators at $20K avg = $200K ARR** — covers ongoing operations, recalibration, and infrastructure costs
- **20 curators at $20K avg = $400K ARR** — enables team expansion and multi-chain coverage
- The Morpho ecosystem currently has 50+ active curators, with the top 30 managing billions in aggregate TVL

### Why this avoids the Chaos Labs trap

Chaos Labs operated at negative margins for three years providing risk management to Aave at $5M+/yr because:
1. They were a single vendor accountable to DAO governance with no pricing power
2. Their infrastructure was purpose-built for Aave's architecture — when Aave rewrote V4, Chaos Labs' work was stranded
3. Revenue depended on a single client (the DAO)

Our model avoids all three failure modes:
- **Multiple buyers**: each curator is an independent customer — no single-client dependency
- **Architecture-stable**: Morpho's isolated markets and permissionless curation model are architecturally stable by design — the risk infrastructure doesn't get stranded by protocol upgrades
- **Open core**: the public methodology is open-source and community-owned. The paid product is the operational layer (speed, coverage, SLA), not the methodology itself. If the team disappears, the community retains the framework.

### Insurance as a downstream market

Accurate, continuous risk scoring enables a further revenue channel: **insurance pricing**. Protocols like Nexus Mutual and OpenCover need quantitative risk inputs to price cover on lending positions. A calibrated PD estimate for a specific vault at a specific LTV is exactly the input an insurer needs. This creates a second buyer category beyond curators, with pricing driven by actuarial value rather than subscription willingness.

This is a Phase 2+ opportunity — the grant period focuses on building the curator-facing product. But it shapes the model design: outputs should be structured for both human consumption (dashboard, dossiers) and machine consumption (API, streaming feeds) from day one.
