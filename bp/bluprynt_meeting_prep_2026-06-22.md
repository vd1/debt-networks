# Bluprynt x Sigma Labs - call prep

When: today, 2026-06-22, 17:00
DD memo: `../../dueDil/2026-06-22-bluprynt-compliance-os.md`

## Room

- Sigma Labs: Vincent (vd210), Amaury Denny. Daniel Jean (framework co-author, group owner) is OoO til June 28 - bring him in for next week's architecture/structure discussion.
- Bluprynt: RG4 (sent the merge + funding message).

Read: today is a **YTD demo** (what we promised Rob), not a negotiation. You (Vincent + Amaury) drive it; merge/funding scoping lands next week when Daniel is back.

## Sigma Labs intro

Short opener:

> Sigma Labs is a quant team building DeFi vaults and risk infrastructure. We know
> the curator side well, have an active working relationship with Ellen Capital, and
> are currently negotiating an LP agreement with the Lido Ecosystem Foundation around
> our latest vault: https://www.sigmalabs.fi/vault/0x2746f31096f23670caf4043f8b30d8d02405a257
> The vault is whitelisted by Lagoon.

## Risk approach opener

Short transition:

> Coming to our risk approach: we developed a tool for recursive yield and
> dependency mapping, and I will show a few concrete examples.

Examples to show:
- **Our own Lagoon vault.** We used the tool on our own Lagoon vault,
  whitelisted by Lagoon:
  https://www.sigmalabs.fi/vault/0x2746f31096f23670caf4043f8b30d8d02405a257
  The vault view includes the complete dynamic yield tree, so a curator or LP can
  see what the vault's return stream depends on at each layer.
- **Ellen Capital vault.** We also applied the same YTD methodology to Ellen
  Capital's vault, which matters because we have an active working relationship with
  them and understand the curator workflow.
- **Top Morpho markets.** We run systematic YTD analysis on top Morpho markets:
  https://www.sigmalabs.fi/discover
  This is the broader version of the same approach: recursive dependency mapping
  across major yield markets rather than a single managed vault.

Current commercial context:
- We are talking to Steakhouse, a large curator in the Morpho ecosystem, about
  creating an `rcETH/ETH` vault to amplify the carry of our Lagoon Lido carry
  strategy.

Feature gap / next build:
- The machinery is in place, but we do not yet have a UI for running depeg
  propagation models on top of YTD trees. This is an obvious next layer: once the
  dependency graph exists, shocks can be propagated through it.

Pre-prod DIG tool:
- We also have a pre-prod tool, DIG, which lets a user discover what can be done
  with a token: strip, gauge, lend, swap, stake, and similar actions. This is the
  action/discovery complement to YTD's dependency view.

Bluprynt-specific bridge:
- Take `syrupUSDC` as the concrete example. It appears inside the YTD of PYUSD, even
  in `senPYUSD`, but our graph stops once the dependency enters the off-chain issuer
  and collateral layer. That is exactly where Bluprynt's asset graph, KYI, and Proof
  of Collateral can extend the tree rather than sitting beside it.
- Partnership thesis: Sigma can glue Bluprynt's off-chain certificates, reserve
  attestations, issuer credentials, and collateral documents into the live YTD/DIG
  graph. That would turn a hard off-chain boundary into a monitored node with
  provenance, credential state, and risk annotations.

## Call objective

Leave them wanting a funded integration pilot. Do not try to close architecture,
commercial structure, exclusivity, or IP today.

The useful outcome is:
- They understand YTD as the live recursive dependency graph for token yield.
- They see exactly where Bluprynt's asset graph can attach.
- They send concrete artifacts after the call: schema, sample asset graph, coverage
  list, API docs if any, and 5-10 target tokens.
- You agree to a next architecture call with Daniel.

## What we told Rob we'd do

> "We'd like to give you a full demo of our flagship tool YTD (recursive on-chain dependence tree for tokens) including the underlying data collection machine and the various risk measures which one can build on top. Even better if you have a bunch of tokens that you'd like us to run the tool on!"

So the call is demo-led: (1) YTD recursion, (2) the data-collection machine, (3) the risk measures on top, (4) run it live on their tokens.

## What they proposed (verbatim intent)

Merge Bluprynt's **asset graph** into Sigma's **yield (YTD)** and **dependency (DIG)** graphs, with Bluprynt **funding the integration** (or another partnership structure post-discussion). Note the direction: they merge *into* your graphs. You are the risk-analytics layer; they are a provenance/data source. Hold that posture.

## 5-minute spine

1. "We map what yield depends on recursively. The output is not a balance sheet
   label; it is a live graph of dependencies and failure paths."
2. "The hard boundary is the moment a token points into legal, issuer, custody, or
   collateral claims that are not directly observable onchain."
3. "Bluprynt seems to model that missing layer: issuer identity, credentials,
   disclosure documents, collateral structure, and potentially attestation state."
4. "The merge we should test is not cosmetic. It is a graph join between our
   dependency nodes and your issuer/collateral/credential nodes."
5. "If the sample works, the funded pilot should produce a merged graph for a small
   asset set, risk annotations, and a demoable analyst workflow."

## The fit (lead with this)

Sigma's YTD/DIG graphs bottom out at the on-chain layer; anything terminating in an off-chain claim is tagged `RWA-opaque` and goes dark (e.g. FDUSD-type issuer-attestation gaps are currently "not caught").

Bluprynt's asset graph is the attestation layer that resolves those dead-end leaves:
- KYI binds issuer legal entity <-> wallets <-> token contracts.
- Proof of Collateral describes the off-chain structure (legal wrapper, custody, receivables, servicing/waterfall, regulatory mapping).

So the merge is not two graphs side by side - it is your on-chain reflexivity/yield graph running continuously down into their attested off-chain collateral graph. One graph, on-chain to legal claim.

Bidirectional value:
- Sigma -> Bluprynt: if their credentials are point-in-time attestations, Sigma's
  diff-stream + nutrition badges add monitoring around them (supply velocity,
  governance mutation, liquidity deterioration, oracle and liquidation stress).
- Bluprynt -> Sigma: resolves the `RWA-opaque` misses into attested nodes, closing the exact gap the incident-coverage matrix is built to bound.

## Demo runbook

Arc: open on a token THEY care about, show the recursion, hit the off-chain boundary, let the merge sell itself.

1. **Open on PYUSD.** You already have `sigmaLabs/PYUSD_YTD.txt` - PYUSD is Bluprynt's flagship pilot (they ran a KYI pilot on it). Showing a token they have touched through YTD is the strongest possible opener. Walk the recursive dependence tree node by node.
2. **Show the data-collection machine.** They explicitly asked for this. How the tree is built from on-chain state, update cadence, what the diff-stream watches.
3. **Show the risk measures on top.** Nutrition badges (circularity, supply-velocity-anomaly, hardcoded-oracle, single-EOA-roles, liquidation-depth-ratio, oracle-lag-borrow-amplifier, etc.). Pick 2-3 that fire on a real token.
4. **Hit the `RWA-opaque` boundary live.** On PYUSD or a tokenized-RWA token, the tree terminates at an off-chain claim and goes dark. This is the money moment: "this leaf is exactly where your asset graph / KYI + Proof of Collateral plugs in." The merge case makes itself, in the demo, on their own asset.
5. **Run their tokens.** Whatever Rob brings - run YTD live. If he brings nothing, fall back to the pre-staged set below.

## PYUSD talk track

Use `sigmaLabs/PYUSD_YTD.txt` as the opener:

- Sentora PYUSD Main: `$392.7M`, net APY `2.37%`.
- Largest branches: `sUSDe/PYUSD` `$127.8M` (`32.5%`, `92% LLTV`),
  `cbBTC/PYUSD` `$113.5M` (`28.9%`, `86% LLTV`), `syrupUSDC/PYUSD`
  `$100.1M` (`25.5%`, `92% LLTV`), `weETH/PYUSD` `$38.9M`
  (`9.9%`, `86% LLTV`).
- The point: "PYUSD yield" is actually a portfolio of recursive collateral
  dependencies, not just exposure to PayPal-issued dollars.
- The sharp merge example is `sUSDe`: the YTD tree reaches Ethena backing, including
  off-chain delta-neutral perps via Copper/Ceffu and custodian balances. Bluprynt's
  graph could attach issuer, custodian, legal-wrapper, disclosure, and attestation
  state to those leaves.
- The second example is `sUSDS`: the tree pulls in Sky PSM, Spark, RWAs, and CDPs.
  Bluprynt can help resolve issuer and collateral provenance, while Sigma keeps the
  onchain reflexivity, LLTV, oracle, liquidation, and concentration risk view.

## Tokens to pre-stage (in case Rob brings none)

Bias toward Bluprynt's pilot/RWA world so the off-chain boundary shows up:
- **PYUSD** (already have the YTD - lead with it).
- A Circle/USDC or Paxos-issued asset (their other pilots).
- A tokenized-RWA stablecoin where the tree clearly hits `RWA-opaque` (best merge illustration).
- One reflexive DeFi token for contrast (sUSDe / sUSDS - you have `dig_sUSDe`, `dig_susds`) to show the recursion + circularity badges firing where the boundary is fully on-chain.

Have these loaded before the call so a live run never stalls.

## Discovery questions (weave into the demo, don't interrogate)

- What is the "asset graph" concretely - schema, entities, current coverage (how many issuers/tokens live)?
- Is Proof of Collateral self-attested, issuer-attested, third-party-attested, or
  regulator-attested? How fresh is it? This decides whether it is a usable risk feed
  vs. a marketing surface.
- Do they expose registry contract addresses, attestations, and revocations
  programmatically? If so, on which chains and with what API?
- Which PYUSD-related assets can they actually cover today: PYUSD, USDC/Paxos assets,
  cbBTC, sUSDe/USDe, sUSDS/USDS, syrupUSDC, tokenized RWAs?
- Which assets first? (stablecoins? Morpho-market collateral? - aligns with the Morpho-compatible RFC scope.)
- What does "funding the merge" mean to them - grant, services contract, equity, joint IP?
- How real/technical is the Chainlink ACE integration (could not verify from public sources)?

## Do NOT commit today

- IP ownership of the merged graph or the framework.
- Data licensing / exclusivity (do not lock Sigma to Bluprynt-only provenance).
- Funding structure - grant vs. equity vs. work-for-hire have very different consequences. Line: "let's scope the integration, then find the structure that fits, with Daniel in the room."

## Draft TG reply to RG4

> Great to connect - very much open to this. The merge makes a lot of sense from our side: our yield (YTD) and dependency (DIG) graphs currently bottom out at the on-chain layer and tag anything that terminates in an off-chain claim as "RWA-opaque." Your asset graph / KYI + Proof of Collateral is exactly the attestation layer that resolves those leaves - so a merged graph would run continuously from on-chain yield and reflexivity down into attested issuer and collateral structure, with our diff-stream adding monitoring around credential state.
>
> Happy to scope it properly next week and talk funding or whatever partnership shape fits once we've mapped the integration. Our framework lead Daniel is back from the 28th, so any time that week works well on our end - what suits you?

## Next-week agenda (with Daniel)

- Integration architecture: how the asset graph attaches to YTD/DIG nodes; shared schema for the `RWA-opaque` resolution.
- Coverage/freshness SLAs on Proof of Collateral.
- Partnership structure + IP ownership.
- Scope of the funded merge (first asset set, milestones).
