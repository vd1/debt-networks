# Ideas: defi incidents → framework; which do we catch?

**Goal:** assemble a corpus of DeFi incidents and map each one against the Sigma Labs framework (DIG, YTD, nutrition badges, diff-stream alerts) to answer: **which would we have caught? when? with which signal?** The output strengthens the Evidence Appendix of the RFC and bounds the framework's claimed coverage without overstating it.

The RFC currently documents two retroactive cases (Stream/xUSD Nov 2025, Resolv/USR Mar 2026). This task expands the corpus and turns it into a structured matrix.

## Deliverable

`incidents_matrix.md` (or CSV/JSON): one row per incident, with columns:

| Field | Notes |
|---|---|
| `id` | short slug (e.g., `stream-xusd-2025-11`) |
| `date` | YYYY-MM-DD of public failure |
| `protocol(s)` | issuer + affected lending venues |
| `loss_usd` | rough loss / at-risk |
| `category` | oracle / minting / leverage-loop / governance / liquidity / circular-collateral / RWA-opaque / bridge / other |
| `root_cause` | one-line mechanism |
| `onchain_signals_present` | comma-separated tags drawn from the badge set |
| `framework_module` | DIG / YTD / diff-stream / nutrition-label / none |
| `caught?` | yes / partial / no |
| `lead_time` | hours/days the signal was readable before the loss |
| `evidence_link` | postmortem, Etherscan tx, oracle config, etc. |
| `notes` | edge cases, caveats |

The explicit **`no`** rows are as important as the **`yes`** rows; they bound the claim and motivate Phase 2.

## Signal taxonomy (from the RFC badges)

- `circularity` - recursive loop in YTD
- `single-EOA-roles` - privileged mint/admin without timelock or multisig
- `hardcoded-oracle` - fixed price feed, no deviation circuit breaker
- `leverage-loop` - same-asset deposit/borrow recursion
- `concentrated-custody` - terminal asset concentration in one custodian
- `stale-oracle` - oracle update gap exceeds threshold
- `supply-velocity-anomaly` - totalSupply growth out of band vs backing
- `redemption-queue-mismatch` - withdrawal latency vs leverage ratio
- `governance-mutation` - role transfer, timelock removal, param change
- `liquidity-deterioration` - secondary-market depth collapse
- `liquidation-depth-ratio` - for each Morpho A/B market, measure aggregate DEX liquidity for an A -> B swap, corresponding to liquidation of collateral A into debt asset B, and compare it with total B debt. Working hypothesis: liquidity / total debt is around 10% (verify), meaning DEXes typically cannot absorb much liquidation flow. This is an important signal for a lender who lends B against collateral A.
- `oracle-lag-borrow-amplifier` - depeg risk can be amplified by fast players who buy discounted collateral while the oracle is still lagging, then borrow up to the limit against it before the oracle catches up. This turns oracle latency into an extraction channel and can accelerate bad debt.
- `young-contract-high-TVL` - TVL/age ratio outlier
- `RWA-opaque` - collateral terminates in offchain claim with no attestation

For each incident, mark which tags would have fired and via which module.

## Initial incident shortlist to populate

Already in RFC:
1. **Stream Finance / xUSD** - Nov 2025 - `leverage-loop`, `supply-velocity-anomaly`, `redemption-queue-mismatch`, `stale-oracle` → caught (YTD + diff-stream).
2. **Resolv / USR exploit** - Mar 22 2026 - `hardcoded-oracle`, `single-EOA-roles`, `young-contract-high-TVL`, `leverage-loop` → caught (nutrition badges + diff-stream).

Candidates to add (need verification on each):
3. **Mango Markets oracle manipulation** - Oct 2022 - `manipulable-oracle` (thin-liquidity TWAP). Probably *not caught* by current modules (no oracle-depth check yet); useful "no" row.
4. **Euler exploit** - Mar 2023 - donate/`liquidate` accounting bug. *Not caught*: code-level vulnerability, not structural. Bounds the claim: framework catches structural risk, not bytecode bugs.
5. **Curve re-entrancy / vyper compiler bug** - Jul 2023 - *Not caught*: same boundary.
6. **UST / Anchor depeg** - May 2022 - `circularity`, `concentrated-custody` (Luna backing), `supply-velocity-anomaly`. Likely *partial catch* (DIG would surface Luna→UST circular reflexivity; doesn't predict timing).
7. **CRV/Aave bad-debt episode** - Aug 2023 - `concentrated-collateral`, `liquidity-deterioration`. Diff-stream would flag liquidity drop; YTD would flag concentration. *Caught.*
8. **Prisma Finance hack** - Mar 2024 - delegate-call / migration bug. *Not caught.*
9. **Morpho-tied curator incidents (e.g. MEV Capital, Re7, Gauntlet) around xUSD/USR** - same root as #1/#2 but per-curator. *Caught* (same signals).
10. **eUSD / Lybra / Prisma mkUSD** mid-2024 depegs - `circularity`, `supply-velocity`. Probably *partial*.
11. **Iron Bank / Cream multi-protocol loops** - 2021–22 - `circularity`, `cross-protocol-loop`. *Caught.*
12. **Multichain bridge collapse** - Jul 2023 - operator key custody. *Not caught* (offchain custody).
13. **First Digital / FDUSD wobble** - Apr 2025 - issuer attestation gap. *Not caught* unless RWA-opaque badge is present (it would be).
14. **PT/YT mispricing on Pendle markets at maturity boundary** - recurring - `oracle-maturity-mismatch`. Edge case worth a row.
15. **Recent (Q1 2026) curator mis-allocation episodes** - anything Chaos Labs / LlamaRisk has post-mortemed since Resolv. Pull from the docs already in this dir.

## How to mine the corpus

- `chaos_labs_exits_aave.md`, `dirt_roads_68_physics_of_onchain_lending.md`, `llamarisk_aave_continuity.md` are already in this dir; extract incident references from them.
- Rekt.news leaderboard for losses ≥ $10M, filter for lending / collateral / oracle categories.
- DefiLlama hacks page.
- Chaos Labs blog post-mortems.
- Block analitica / Gauntlet incident write-ups.
- Tg/X threads from Omer Goldberg, Luca Prosperi (Dirt Roads), 0xngmi.

## Methodology rules (to keep the matrix calibrated)

1. **No retroactive overfitting.** A signal counts as "caught" only if (a) the rule is documented in the RFC badge set *before* this exercise, and (b) it could have been computed from onchain state at the time of the incident.
2. **Lead time must be falsifiable.** Cite the block range or timestamp where the signal was readable. If we can't, mark `lead_time = unknown`.
3. **"Partial" requires a clear gap.** State explicitly which part the framework misses (e.g., "DIG flags circularity but doesn't quantify reflexivity coefficient").
4. **Bytecode-level exploits get a "no" row.** Don't try to expand the framework to cover them; bound the claim.
5. **Per-incident note: would the diff-stream have alerted in time?** A signal that's only detectable after the fact is not a catch.

## Output integration

- Section in Evidence Appendix: `E3. Incident Corpus & Coverage Matrix` with the table + a coverage summary ("of N incidents in the corpus, framework catches K structural cases, partials M, misses bytecode/offchain L").
- Headline number for the RFC abstract / front matter, if defensible.
- Per-row links from the matrix back to the badge that would have fired, to make the framework's claims auditable.

## Open questions

- Cutoff for inclusion: minimum loss size? minimum protocol relevance?
- Do we include centralized-issuer stablecoin episodes (USDC March 2023, USDT historical) or scope to DeFi-native?
- Do we include aborted attacks (whitehat saves)?
- How do we handle incidents where the framework would have *flagged risk* but the curator might still have allocated? (Disclosure ≠ prevention; make this explicit.)
- Should the matrix live in this repo or be its own public dataset (CSV in a separate repo, citable)?

## Suggested first session

1. Skim `chaos_labs_exits_aave.md`, `llamarisk_aave_continuity.md`, `dirt_roads_68_physics_of_onchain_lending.md` and pull every incident reference into a flat list.
2. Cross-check against Rekt + DefiLlama for completeness.
3. Score the first 10 against the badge taxonomy.
4. Draft the "coverage summary" paragraph for the Evidence Appendix.
5. Decide format (markdown table vs CSV+rendered) before scaling to 30+ rows.
