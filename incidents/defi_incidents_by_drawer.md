# DeFi incidents — by drawer (Oct 2025 → Apr 2026)

Taxonomy view of the same incidents in `defi_incidents_2025-10_to_2026-04.md`. Each entry: date — name — amount — one-line cause. `↔` marks cross-listed incidents (primary drawer + secondary tag). Short Morpho-relevance note at the end of each drawer.

---

## 1. Smart contract bugs (pure code / logic)

Arithmetic, reentrancy, state-consistency, soundness, unsanitized calls.

- **2026-04-13** — Hyperbridge — ~$2.5M — Missing bounds check in MMR proof verifier → 1B bridged DOT minted. ↔ *bridge*
- **2026-04-13** — Dango perps — $410K (recovered) — Insurance-fund donation logic didn't check `amount > 0`.
- **2026-04-03** — Silo Finance — $392K — Oracle misconfig in a permissionless market. ↔ *oracle*
- **2026-03-05/06** — 1inch Fusion v1 — ~$5M — Calldata-length / buffer overflow in `_settleOrder`; end-user funds untouched.
- **2026-03** — Solv Protocol — ~$2.7M — Reentrancy / double-mint in BRO vault handling ERC-3525 deposits; 135 BRO → 567M counterfeit BRO.
- **2026-02-26** — Foom Cash — $2.26M ($1.84M returned) — Groth16 verifier `delta2 == gamma2` (skipped Phase-2 trusted setup) → forged nullifier hashes.
- **2026-02-20/22** — Veil Cash — ~$7K — Same `delta2 == gamma2` zkSNARK-soundness flaw (progenitor of Foom).
- **2026-01-25** — SwapNet — $13.43M — Arbitrary-call + unsanitized `transferFrom` abused user approvals. ↔ *approval surface*
- **2026-01-25** — Aperture Finance — $3.67M — Same root cause as SwapNet. ↔ *approval surface*
- **2026-01-21** — Saga (SagaEVM) — ~$7M — Bridge-validation bypass via IBC precompile manipulation; $D minted from thin air. ↔ *bridge*
- **2026-01-08** — Truebit — ~$26.44M — Integer overflow in legacy Solidity ^0.6.10 `getPurchasePrice()` (pre-SafeMath). **Archeological contracts.**
- **2026-01-05** — TMXTribe — $1.4M — Unverified contract, no sanity checks in LP staking/swap loop.
- **2025-12-30** — Unleash Protocol — ~$3.9M — Multisig governance takeover → malicious upgrade. ↔ *governance*
- **2025-12-27** — Flow blockchain — ~$3.9M — L1 execution-layer flaw → unauthorized FLOW + bridged-asset mint. ↔ *L1 infra*
- **2025-12-04** — USPD stablecoin — ~$1M — **Deployment-stage** CPIMP proxy takeover; shadow impl held admin silently 78 days.
- **2025-11-30** — Yearn Finance (yETH) — ~$9M — "16 wei → infinite yETH" cached-virtual-balance desync.
- **2025-11-03** — Balancer V2 — ~$120–128M — Rounding-direction inconsistency (`_upscaleArray` always rounds down) compounded via batchSwap.
- **2025-10-15** — Typus Finance (Sui) — ~$3.44M — Missing authorization assert on oracle `update_v2`; out-of-scope of the audit. ↔ *oracle*
- **2025-10-04** — Abracadabra (Cauldron V4) — ~$1.8M — `cook()` multi-action: Action 0 reset `CookStatus` after Action 5 set `needsSolvencyCheck=true` → solvency check bypassed.

**Morpho relevance:** Morpho Blue's surface here is small (minimal, immutable core), but *curator-added adapters/liquidator helpers* and *vault-wrapping tokens* sit on this risk pane. The Abracadabra "multi-action composition bypass" pattern generalizes to any lender accepting atomic action batches.

---

## 2. Oracle failures (misconfig + manipulation)

- **2026-04-03** — Silo Finance — $392K — Misconfigured feed on a permissionless market.
- **2026-03-16** — Venus Protocol — $3.7M extracted / $2.15M bad debt — Donation-attack bypassed supply cap + market-price oracle on thin THE.
- **2026-03-10** — Aave — $27.78M liquidated (reimbursed) — CAPO rate-limit misconfig under-reported wstETH/ETH (1.19 vs 1.23).
- **2026-02-22** — YieldBlox (Stellar Blend) — $10.2M ($7.2M frozen) — Thin-liquidity Reflector oracle manipulation; single USTRY offer at 501 USDC.
- **2026-02-15** — Moonwell — $1.78M bad debt — MIP-X43 wired cbETH/USD as cbETH/ETH only (no ETH/USD mult). **LST ratio-feed misconfig.**
- **2026-01-20** — MakinaFi — $4.2M — Permissionless `updateTotalAum()` + no TWAP/circuit breaker + live Curve spot; flash-loan $280M USDC, distort, lock NAV, withdraw.
- **2025-12-12** — Aevo / legacy Ribbon DOV — $2.7M — Oracle upgrade accidentally let anyone push expiry prices.
- **2025-10-15** — Typus Finance — $3.44M — Missing auth assert on `update_v2`.
- **2025-11-04** (secondary cause) — Stream Finance xUSD — Multiple lenders had **hardcoded xUSD oracle at $1** to prevent cascade liquidations; locked the depeg in as bad debt. ↔ *financial*

**Morpho relevance:** This is the pane with the highest Morpho-ecosystem tail risk. Every curator-listed LST/LRT/synthetic-stable market rides an oracle decision; Moonwell/Aave CAPO/MakinaFi/Aevo are all reproducible in MetaMorpho vaults via a single bad parameter. **Hardcoded $1 oracles on depegging collateral is the specific anti-pattern the user's `QFTaskList.tex` needs to flag.**

---

## 3. Bridge / cross-chain validation

- **2026-04-18** — KelpDAO (rsETH) — ~$292M — Single-verifier LayerZero DVN (1-of-1) → forged cross-chain msg → 116,500 rsETH minted → looped through Aave/Morpho/Fluid. **Cross-lender contagion.**
- **2026-04-13** — Hyperbridge — ~$2.5M — MMR proof-verifier bug; 1B bridged DOT minted on Ethereum.
- **2026-02-21** — IoTeX ioTube — ~$4.3M + 111M CIOTX minted — Compromised Ethereum-side Validator key. ↔ *credential*
- **2026-01-31 → 02-01** — CrossCurve — ~$3M — Spoofed Axelar messages via unvalidated `expressExecute` in PortalV2.
- **2026-01-21** — Saga (SagaEVM) — ~$7M — IBC precompile manipulation minted $D.

**Morpho relevance:** Morpho doesn't run a bridge, but *collateral that traveled over a bridge* can introduce an exogenous mint-event risk the lender inherits. KelpDAO is the canonical case — a vault lending against rsETH is exposed to the rsETH bridge regardless of the lender's own correctness.

---

## 4. Credential / key compromise

- **2026-04-14** — Zerion — ~$100K — UNC1069 AI-assisted SE; hot-wallet drain.
- **2026-02-21** — IoTeX Validator — see bridge drawer.
- **2026-01-31 (disclosed)** — Step Finance — ~$27.3M (261,854 SOL) — Exec device phished; keys drained multisig.
- **2025-11-20** — GANA Payment — ~$3.1M — Smart-contract ownership compromised; reward rates changed + unstake drain.
- **2025-10-30** — Garden Finance — ~$11.4M — Solver-layer infra compromise (EY: IPs from Japan/China).
- **2026-03-22** — Resolv Labs — $25M ETH + 80M unbacked USR — Compromised SERVICE_ROLE EOA (not multisig) + missing mint-cap/oracle checks. ↔ *contract bug*

**Morpho relevance:** Off-chain hygiene of *every* participant in a Morpho vault's dependency graph (curator signing keys, collateral issuer admin keys, oracle updater keys) is in scope. Resolv is the painful case where a stablecoin-issuer EOA compromise cascaded into Morpho vault bad debt.

---

## 5. Governance / multisig compromise (via social engineering)

- **2026-04-01** — Drift Protocol — ~$285M — Lazarus SE'd Security Council into pre-signing txs wrapped as Solana durable nonces; activated months later to list fake CVT as collateral.
- **2025-12-30** — Unleash Protocol — $3.9M — Multisig governance takeover → unauthorized upgrade. ↔ *contract bug*

**Morpho relevance:** Curator governance is the Morpho analog. A sufficiently patient SE campaign against a curator's signers could reproduce Drift-style "new collateral listed, caps raised, then drained." User's existing "Étude villain / attack Morpho" thread maps directly here.

---

## 6. Frontend / DNS / supply-chain

Protocol contracts untouched; losses come through user-side approvals or wallet key theft.

- **2026-04-14** — CoW Swap (cow.fi) — ~$1.2M — Forged IDs against Finland Traficom + Gandi → DNS points to phishing.
- **2026-03-19** — Neutrl — ~zero — `.fi` DNS-campaign hijack attempt.
- **2026-02-16** — OpenEden — no loss — AngelFerno DNS hijack.
- **2026-02-16** — Curvance — zero — AngelFerno DNS without DNSSEC; detected pre-drain.
- **2025-12 (exact day unclear)** — Trust Wallet extension v2.68 — >$6M — Malicious extension version exfiltrated keys.
- **2025-11-21/22** — Aerodrome + Velodrome — ~$700K–$1M+ — NameSilo internal compromise bypassed 3DNS multisig; DNSSEC removed.

**Morpho relevance:** Not a direct Morpho attack surface, but the user-approval model of any DeFi lender means a DNS hijack of a popular vault aggregator frontend (Morpho.org itself, or any MetaMorpho allocator's UI) would produce Aerodrome-style losses without touching Morpho contracts.

---

## 7. Financial / off-chain loss / stablecoin depeg

Not a contract bug — exposure via assets losing peg, off-chain fund-manager losses, recursive collateral loops.

- **2026-03-22** — Resolv Labs USR depeg — (see credential drawer; the depeg itself is the financial contagion arm) — Hit ~15 Morpho vaults; Gauntlet USDC Core held $4.95M in wstUSR/USDC = 98% of that market's lender liquidity.
- **2025-11-04 → 11-07** — Stables Labs USDX — -94% — Triggered by Balancer V2 drain of USDX/sUSDX pool + cascading leveraged liquidations on Lista/PancakeSwap.
- **2025-11-04 → 11-07** — Elixir deUSD — -98% — 65% of deUSD collateral was in private Morpho vaults where Stream was sole borrower using xUSD. Collapsed with Stream.
- **2025-11-04** — **Stream Finance (xUSD)** — $93M initial / ~$285M interconnected — Off-chain fund-manager loss. Hardcoded $1 oracles locked bad debt into books. Euler third-party curator vaults: $137M. TelosC ($123.6M) and Elixir ($68M) largest single exposures. **The canonical case: pinned oracles + risk-curator allocation + hidden-borrower private vaults + recursive loops + TVL illusion all in one episode.**

**Morpho relevance:** This is the most important drawer for the user's thesis. Every "illusion" in their `QFTaskList.tex` (collateral independence, TVL, diversification, leverage) shows up here. **Stream is the single best case study in the whole list.**

---

## 8. Market manipulation (perps / thin pools)

- **2025-11-13** — Hyperliquid HLP — $4.9M bad debt — $20M POPCAT buy-wall at $0.21, withdrawn, crashed; community vault absorbed bad debt. **Third such attack on Hyperliquid in 2025.**
- **2026-04-15 → 04-17** — Rhea Finance (NEAR) — $18.4M — Fake pools on Ref Finance + Rhea margin feature → borrow real vs phantom collateral. (Also: *oracle*-adjacent.)

**Morpho relevance:** Morpho Blue itself is slow-moving (no perps), but its *permissionless market creation* means a curator who lists a thin collateral with a market-price oracle reproduces the Venus/Rhea pattern. Supply-cap controls matter here.

---

## 9. L1 / chain-level

- **2025-12-27** — Flow — $3.9M — Execution-layer bug allowed native FLOW + bridged-asset mint. Foundation proposed 6h chain rollback.

**Morpho relevance:** Any chain Morpho deploys to inherits this risk level as a baseline. Worth flagging to curators the historical base rate of L1 incidents per chain.

---

## 10. Operational near-misses / non-exploits

- **2025-10-14** — PYUSD (Paxos) — accidentally minted ~$300 trillion then burned minutes later. Not an exploit. Worth filing as an operational base-rate signal for issuer mistakes.
- **2026-04-15** — Grinex exchange — $13.7M — Sanctioned CeFi venue drain; not DeFi but adjacent in the April narrative.

---

## Drawer sizes (period total)

| Drawer | # incidents | Order-of-magnitude $ |
|---|---|---|
| Smart contract bugs | 19 | ~$200M (Balancer dominates) |
| Oracle failures | 9 (overlapping) | ~$50M directly + $285M contagion enabled |
| Bridge / cross-chain | 5 | ~$310M (Kelp dominates) |
| Credential / key comp. | 6 | ~$70M |
| Governance / multisig | 2 | ~$290M (Drift dominates) |
| Frontend / DNS | 6 | ~$8M |
| Financial / depeg | 3 | ~$450M interconnected |
| Market manipulation | 2 | ~$23M |
| L1 / chain-level | 1 | $3.9M |
| Op. near-miss | 2 | negligible |

*(Dollar totals approximate and sometimes double-count — Kelp is in both "bridge" and drives the "financial contagion" around it. This is the point — the drawers overlap precisely where systemic risk lives.)*

---

## Cross-drawer patterns worth pulling out

1. **DPRK / Lazarus attribution on the big ones** — Drift ($285M), KelpDAO-related ($292M operational attribution debated), Zerion ($100K), Upbit CeFi ($36M Nov 2025). The pattern: increasingly patient, AI-assisted SE against signers rather than direct contract attacks.
2. **Archeological contracts** — Truebit (2021 code), Aevo legacy Ribbon, Yearn iEarn v1. Forks and legacy deployments carry tail risk for years.
3. **`.fi` / DNS-registrar weakness as a campaign** — CoW Swap + Steakhouse + HypurrFi + Neutrl. Registrar hardening (RegistryLock, DNSSEC) is the cheap mitigation.
4. **Curator concentration > diversity** — Stream, Resolv, Elixir all show that "vault N has exposure to asset X" aggregated across curators produces concentrated single-asset risk even when any individual vault looks small. Directly validates the **TVL illusion** and **diversification illusion** entries in `QFTaskList.tex`.
5. **Oracle-pinning as a cascade amplifier** — Hardcoding $1 on a depegging asset converts a price-discovery event into unrecoverable bad debt. Stream is the textbook case.
