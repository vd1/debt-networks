# Bluprynt x Sigma Labs

## Bankable Collateral Disclosure For Tokenized RWAs

Bluprynt and Sigma Labs are complementary pieces of the same collateral-risk stack.

Bluprynt's Proof of Collateral and asset graph can resolve the issuer, legal, custody, collateral, and disclosure layer behind tokenized RWAs. Sigma Labs' YieldTree and DIG work can trace live on-chain dependencies through vaults, lending markets, recursive collateral, exit venues, and stress paths.

Composed well, the result is not a static disclosure report. It is a live graph from token to legal claim, with quantitative models for how losses propagate, what remains recoverable, and whether a position can be exited under stress.

## Core Thesis

Tokenized RWA collateral disclosure has two different problems:

1. Structure: what the asset is, who issued it, what backs it, where it sits, and what it depends on.
2. Loss: how a shock moves through that structure, how much value is recoverable, and what can be exited at size.

Bluprynt is strongest on the first problem: issuer-facing disclosure, Proof of Collateral, regulatory reporting, and the asset graph.

Sigma is strongest on the second problem: recursive yield and dependency traversal, exit-market analysis, depeg or stress propagation, and quantitative risk calibration.

The combined product should make the structure visible and the loss statement defensible.

## The Gap

### 1. Traversal Shows Structure, But Does Not Quantify Loss

Mapping the dependency tree tells an analyst what a token touches: vaults, collateral, lending markets, wrappers, off-chain claims, and nested exposures.

That does not by itself answer the harder questions:

- How does a partial failure propagate through the tree?
- Which dependencies are correlated?
- What recovery rate should be assumed for each edge?
- Where do liquidations cascade?
- What is the realizable exit value under stress?

Those questions require quantitative financial engineering on top of graph traversal.

### 2. Modeling Is The Scarce Layer

Graph adapters are serious engineering work, but the surface is defined: token standards, vault standards, protocols, disclosure schemas, and monitoring feeds.

Loss modeling across recursive collateral trees is less standardized. It requires assumptions about correlation, recovery, reflexivity, liquidation depth, exit slippage, and historical calibration. This is what turns a structural map into a risk statement that a regulator, insurer, allocator, or exchange can rely on.

### 3. Reporting Needs Continuous Materiality

Monthly reporting can satisfy a reporting calendar, but it can miss a mid-cycle dependency failure, a narrowing exit window, or a sudden change in vault composition.

Continuous monitoring only helps if alerts are calibrated to material loss risk. Otherwise the system either misses real problems or produces noise. The materiality thresholds should come from validated risk models, not only from raw graph changes.

## What Each Side Brings

### Bluprynt

- Proof of Collateral and issuer-facing disclosure workflows.
- Asset graph for linking tokenized assets to issuer, legal, custody, collateral, redemption, and regulatory facts.
- Token and vault standard coverage, including standards such as ERC-4626, ERC-3643, T-REX style infrastructure, Veda BoringVaults, and other RWA-associated formats.
- Reporting workflows for issuers, regulators, central banks, exchanges, and standards bodies.
- A regulator-facing posture informed by work with institutions such as the Bermuda Monetary Authority and the FCA.

Bluprynt solves the disclosure, provenance, reporting, and regulator-interface side of the stack.

### Sigma Labs

- YieldTree, a recursive on-chain dependency tree for vaults, collateral, lending markets, wrappers, queues, and off-chain boundaries.
- DIG-style exit-market analysis: where a token can be swapped, borrowed against, redeemed, queued, or otherwise unwound.
- Risk propagation on top of dependency trees, including depeg, contagion, and recursive collateral stress.
- Exit-liquidity-under-stress modeling: what can be realized, at size, during correlated outflow.
- Quantitative model calibration for risk tiers, monitoring thresholds, and insurance or capacity pricing.

Sigma solves the dependency traversal, stress modeling, exit value, and risk-pricing side of the stack.

## Combined Stack

| Layer | Combined Behavior |
| --- | --- |
| Asset graph | Bluprynt resolves issuer, legal, custody, collateral, and disclosure nodes. Sigma links those nodes to live on-chain dependencies when the token is deployed into vaults, lending markets, or nested yield structures. |
| Yield graph | Sigma traces recursive dependencies through protocols, collateral, loops, queues, and exit venues. Bluprynt attaches off-chain facts when a dependency terminates in an issuer, collateral pool, legal wrapper, or other disclosed claim. |
| Continuous monitoring | Bluprynt maintains disclosure and attestation state. Sigma supplies model-driven thresholds for material dependency change, depeg exposure, liquidity deterioration, and exit-risk alerts. |
| Risk frameworks | Sigma calibrates risk tiers and materiality floors against historical events and stress assumptions. Bluprynt presents the resulting risk state through issuer and regulator workflows. |
| Exit-market analysis | Sigma models realizable exit value under stress. Bluprynt can attach venue, reporting, and disclosure context where relevant. |
| Insurance and capacity | Sigma's loss-distribution models can support cover pricing, risk capacity, and attachment points. Bluprynt can package the model-backed disclosure for issuers, investors, insurers, and regulators. |
| Regulator interface | Bluprynt delivers the report and supervision interface. Sigma supplies the quantitative backing for dependency exposure, exit-liquidity posture, and loss scenarios. |
| Point-of-transaction checks | The resolved, model-scored graph can eventually be queried by exchanges, market infrastructure, or settlement venues before a transfer or trade. |

## Proposed Engagement

### 1. Joint Technical Review

Run a shared technical review of Sigma's loss-propagation methodology:

- Correlation structure.
- Recovery-rate assumptions.
- Cascade and reflexivity modeling.
- Liquidation and exit-slippage tails.
- Backtests against named historical depeg or collateral-stress events.

The goal is to define assumptions, sensitivity analysis, and acceptable false-positive and false-negative behavior before the models are embedded into a production workflow.

### 2. Reference Pilot

Use a real nested-vault or tokenized-RWA structure as the first integrated deployment.

Plume Nest is the best named candidate from the call because it is an active Bluprynt customer, uses BoringVault-style infrastructure, and wants this type of visibility.

The pilot should produce:

- A merged Bluprynt plus Sigma graph for the chosen asset or vault.
- Explicit off-chain boundary nodes and required disclosure fields.
- Risk propagation and exit-liquidity analysis across the resolved graph.
- A demoable analyst workflow for issuer, regulator, or exchange diligence.

### 3. Model Package

Sigma should deliver a model package that Bluprynt can evaluate and, if accepted, implement or operate within Proof of Collateral:

- Loss-propagation methodology.
- Assumptions and sensitivity analysis.
- Backtest validation.
- Risk-tier calibration.
- Monitoring materiality floors.
- Insurance loss-distribution or pricing framework, where relevant.

Each deliverable should have an acceptance test so the engagement rewards model quality, not only account volume.

### 4. Research Economics

Sigma's contribution should be compensated as research and model development, not only as sales origination.

A workable structure could combine:

- A fixed research fee or annual minimum for model development and maintenance.
- A model royalty on premium Proof of Collateral revenue enabled by Sigma's models.
- A separate revenue share for customers sourced by Sigma.

The exact economics should be scoped after the pilot asset set, production responsibilities, and model acceptance process are clear.

### 5. License And Channel

Possible license and channel terms:

- Sigma retains ownership of its methodology and can apply it outside Bluprynt collateral disclosure.
- Bluprynt receives a license to implement or use the accepted models inside Proof of Collateral.
- Bluprynt owns its graph, disclosure workflows, adapters, monitoring stack, and production code.
- Sigma may resell or introduce Proof of Collateral to customers it sources, with a defined revenue share.
- Accounts originated by Sigma should have protected economics and a trailing share if the research relationship ends.

### 6. IP And Non-Circumvention

The boundary should be clean:

- Bluprynt owns the disclosure product, graph infrastructure, adapters, registry, monitoring workflows, and regulator interface.
- Sigma owns the quantitative methodology, calibration approach, and model research.
- Implementation rights, exclusivity, sublicensing, and non-circumvention should be explicit before production integration.

## Immediate Next Steps

- Use the signed MNDA to exchange customer and pipeline details.
- Create a shared Google Doc for pilot scope, graph schema, commercial structure, and open issues.
- Select the first pilot asset, with Plume Nest as the leading candidate unless Bluprynt proposes a better one.
- Identify required Bluprynt fields for issuer, legal, custody, collateral, redemption, attestation freshness, and revocation state.
- Identify required Sigma fields for YTD/DIG nodes, dependency edges, risk propagation, exit markets, liquidity depth, and stress assumptions.
- Agree on acceptance tests for the first model package.

## Open Questions

- Is the first pilot Plume Nest, PYUSD, a Circle-related asset, or another RWA issuer?
- Which token and vault standards should be prioritized first?
- What schema joins a YTD/DIG node to a Bluprynt asset graph or Proof of Collateral node?
- What freshness, revocation, and monitoring guarantees can Bluprynt provide for off-chain disclosures?
- What latency is required for issuer reporting, exchange diligence, curator monitoring, and point-of-transaction checks?
- Which commercial structure best fits the pilot: research fee, license, revenue share, acquisition, or a mixed structure?
- Should exclusivity apply at all, and if so to which asset classes, customers, geographies, or use cases?
