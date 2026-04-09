# Meeting Prep — RFC Review, April 7, 2026 (15h CET)

Participants: Vincent, Daniel, Amaury, Hamza

## Part 1: Debate Summary (Claude Opus 4.6 vs GPT-5.4, 5 rounds)

### Core Diagnosis

The RFC tries to be four documents at once — incident postmortem, academic model paper, product grant proposal, and team pitch — and each mode weakens the others. The central problem is **claim discipline**: the document reads as if most of the system is built, when in reality the team has built dependency decomposition tooling and the quantitative risk model remains research-stage.

### Claim Register

| Claim | Status | Evidence |
|---|---|---|
| Dependency graph engine (DIG) | **Built** | 7+ protocol watchers, live outputs (reUSD, cUSD: 551 nodes) |
| Yield tree decomposition (YTD) | **Built** | Structured JSON + visual trees (wsrUSD, Sentora PYUSD) |
| Recursive risk decomposition (qualitative) | **Built** | wsrUSD analysis: 6 findings, scorecard, circular exposure |
| Six-factor scoring (Z1-Z6) | Research design | Math spec v3.0 on paper; no calibrated outputs |
| GJR-GARCH(1,1)-t calibration | Research design | No backtest, no fitted parameters |
| HMM regime-switching (3 regimes) | Research design | No calibrated transition matrix |
| Per-block effective volatility | Future work | Zero live outputs |
| Merton-layer contextual PD | Future work | Zero live outputs, no backtest |
| Intrinsic rating (AAA-CCC) | Future work | No cross-sectional distribution |
| Circuit breaker / event-driven arch | Future work | Described, not built |
| Dashboard | Future work | Not started |

**~2 things built, ~4 designed on paper, ~6 not started. The RFC reads as if the ratio is inverted.**

### Top 10 Recommendations

1. **Retitle** → "Morpho Collateral Risk Transparency Layer"
2. **Lead with proof of work** — wsrUSD and Sentora PYUSD analyses on page 1, not page 7
3. **Split into 3 docs** — Main RFC (3-4 pages) + Technical Appendix (model math) + Evidence Appendix (live outputs, claim register)
4. **Shadow mode** — publish model scores for 60-90 days as non-binding; builds track record for Phase 2
5. **Budget**: $180K / 9 months, 3 delivery-gated tranches (~$60K each)
6. **Name the team**, drop "Sigma Labs" unless it's a real registered entity
7. **Credora: 3 sentences max**, complementary not adversarial — don't pick a fight with RedStone
8. **Cite Prosperi** ("Physics of On-Chain Lending," April 6 2026) as independent validation of 20-100x mispricing
9. **State 1.5-2 FTE** explicitly — delegates prefer honest capacity over implied full-time
10. **#1 pre-submission task**: retroactive case study showing tools would have caught USD0++/xUSD/Resolv before they blew up

### Recommended RFC Structure

```
MAIN RFC (3-4 pages)
1. Problem (0.5p) — 3 incidents + Prosperi citation + public-good argument
2. What Exists Today (1.5p) — DIG, yield trees, wsrUSD + Sentora examples
3. What This Grant Delivers (1p) — Phase 1: transparency layer / Phase 2: shadow-mode scoring
4. Relationship to Existing Work (0.25p) — Credora: complementary, 3 sentences
5. Team (0.25p) — named individuals, FTE commitment
6. Budget and Milestones (0.5p) — 3 tranches gated on delivery
7. Sustainability (0.25p) — open core + paid API

TECHNICAL APPENDIX — Six-factor model, GARCH/HMM, Merton PD, math reference link
EVIDENCE APPENDIX — Full decompositions, retroactive case study, claim register
```

### The Delegate Test

> "If a skeptical Morpho delegate asks 'what can you show me working this week?', the answer must be on page 1."

---

## Part 2: Context

### Chaos Labs Exits Aave (April 6, 2026)

Chaos Labs left Aave after 3 years — zero bad debt across $26B TVL, but operated at **negative margins** the entire time. Requested $8M for the V3→V4 transition; Aave offered $5M. Follows BGD Labs and ACI departures. Goldberg: *"Risk is downstream of architecture."*

**For us:** validates demand, proves risk-as-a-service economics are broken under single-client models, and creates political timing — Morpho governance just watched Aave lose its entire technical contributor base. Pitch: *"Fund the commons, don't rent a vendor."*

### Prosperi — "The Physics of On-Chain Lending" (April 6, 2026)

Independent validation using the same Merton/Black-Cox math as Daniel's model. Key finding: Morpho lending spreads are **20-100x too narrow** relative to structural credit models (0-20 bps observed vs 400+ bps required for ETH at 70% LTV / 86% LLTV). Five causes: depositor misperception, regulatory arbitrage, survivorship bias, bull market masking, token subsidies.

### Competitive Landscape

- **Odyssey Digital AM** — working on similar space; competitor, not client. Do not share IP.
- **Credora/RedStone** — incumbent. Opt-in, weekly, no recursive unwrapping.
- **Neither** has recursive dependency decomposition with live outputs.

### Business Model

Sell **block-by-block risk feed to curators**. Public tier (grant-funded): daily snapshots, graphs, dossiers. Paid tier: per-block scores, real-time PD, circuit breaker alerts, WebSocket API. **10 curators at $20K/yr = $200K ARR** — self-sustaining. Avoids Chaos Labs trap: multiple buyers, architecture-stable, open core. Downstream: insurance pricing (Nexus Mutual, OpenCover).

---

## Part 3: Open Questions

1. **Team identity** — "Sigma Labs"? Named individuals? Register a company first?
2. **Budget** — $180K/9mo (debate rec) vs $250K/12mo (current)? Can we show results in 3-4 months?
3. **Retroactive case study** — who builds it, on which incident? Amaury had onchain mMEV evidence pre-oracle.
4. **Prosperi** — cite in RFC? Reach out to Luca directly?
5. **Chaos Labs** — reference the Aave situation as market context?
6. **Odyssey** — what did Amaury learn? Do we need to accelerate?
7. **Morpho's criteria** — what does the DAO actually want to fund? (see Part 4, point 6)

## Part 4: Meeting Feedback

1. **Clarify post-hoc claims.** We are not claiming we would have predicted past incidents — we are saying that once the tooling is operational, we can demonstrate it would have flagged the structural weaknesses behind USD0++, xUSD, and Resolv/USR before they materialized. This does not imply we will catch every future risk.

2. **Separate what's done from what's ahead.** Draw a clear line between the DIG + YTD proof-of-concept (built) and the remaining work needed to reach our milestones — notably Amaury's risk factor model (calibration, backtesting, live scoring).

3. **Add ETAs to each goal.** Every claimed deliverable needs a concrete target date.

4. **Get Letters of Interest from curators.** Daniel has a list: Steakhouse, Gauntlet, Re7, Rockaway/MEV, etc. Curator endorsements materially strengthen a governance proposal.

5. **Add market-facing milestones.** Example: “In 3 months we deliver DIG + YTD tooling to [curator X]'s team for trial and feedback.” Real users testing real outputs.

6. **Understand what Morpho actually wants to fund.** Is the goal a retail-facing risk score on the Morpho GUI, or an active risk monitoring product for curators? According to Albist, Morpho leans toward user education. This shapes the entire deliverable framing.

7. **Move Amaury's technical model to a separate appendix.** Keep the main RFC focused on the transparency layer and proof of work.
