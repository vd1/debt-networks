# Bluprynt + Sigma Labs Call Notes

Date: 2026-06-22

Source: auto-transcript of the Bluprynt and Sigma Labs call. This version reorganizes the conversation by topic, normalizes `Bluprynt`, and preserves the material points. Speaker attribution is inferred because the raw transcript mostly used `Me` and `Them`.

## Attendees

Listed in the recording transcript:

- Amaury Denny
- Robby Greenfield
- Jda 972
- Vincent Danos
- JC
- Jay
- Igor
- Hamza

## Short Summary

Bluprynt responded positively to Sigma Labs' YieldTree and DIG work. Both sides saw a strong fit between Sigma's recursive on-chain dependency mapping and Bluprynt's off-chain disclosure, issuer identity, and Proof of Collateral tooling.

The core shared thesis is that tokenized RWA risk cannot be explained by either on-chain graphs or off-chain disclosures alone. Sigma can trace live DeFi and vault dependencies until the graph reaches an off-chain claim. Bluprynt can describe the legal, issuer, collateral, custody, and disclosure layer behind that off-chain claim.

Bluprynt raised several possible commercial structures, including exclusive integration into Bluprynt, acquisition of the relevant technology, revenue share, or reciprocal use of each party's graph and disclosure layers. Nothing was agreed on the call. The MNDA is signed, so the immediate next step is a shared Google Doc and a pilot around one or more Bluprynt customer assets, with Plume Nest suggested as a strong candidate.

## Main Takeaways

- Sigma presented YTD as a recursive dependency tree for vaults, collateral, yield sources, and hidden exposure paths.
- The YTD engine can expose nested dependencies, loops, collateral composition, and changes in allocation over time.
- Sigma also described pre-production risk features: depeg or stress propagation across the tree, and DIG-style exit market analysis.
- Bluprynt said its asset graph and Proof of Collateral cover the part Sigma intentionally treats as opaque: off-chain issuer, legal, collateral, custody, and disclosure facts.
- Bluprynt's current market includes tokenized RWA issuers, regulators, central banks, standards bodies, and potentially exchanges.
- Bluprynt sees issuer reporting and regulator reporting as near-term paid use cases.
- Both sides saw real-time monitoring and point-of-transaction validation as a larger later use case.
- A natural adapter split emerged: Bluprynt can focus on token and vault standards plus off-chain disclosure adapters, while Sigma focuses on protocol, yield, risk, and dependency traversal adapters.
- Plume Nest was named as a good pilot target because it is an active Bluprynt customer, uses Veda BoringVault-style infrastructure, and wants this type of visibility.

## Organized Transcript

### Opening

Sigma opened with a short introduction and admitted additional team members from the waiting room. The call then moved directly into Sigma's demo and positioning.

Sigma described itself as a team spanning academic research, DeFi engineering, and financial engineering. The team emphasized its relationships with Morpho curators, especially Ellen Capital, and with the Lido ecosystem. Sigma also noted that its own vault work gives it practical exposure to the concerns curators have around risk, liquidity, and dependency management.

### Sigma's YTD Presentation

Sigma introduced YieldTree, or YTD, as a systematic way to unfold what an investor is actually exposed to after depositing into a vault or market.

The basic problem described was that a vault share is rarely a simple exposure. A deposit can be allocated into lending markets, collateralized positions, other vaults, yield strategies, queues, redemption mechanisms, or off-chain claims. Those child assets may themselves contain additional dependencies. YTD recursively opens those layers so the user can see the implicit portfolio behind a deposit.

Sigma emphasized several points:

- A single vault position can cascade through many layers of protocols and collateral types.
- Dependencies can include loops, which are relevant for systemic risk.
- The tree can be computed dynamically from adapters and updated as allocations change.
- The same machinery can be used for individual vaults, curator portfolios, or large market-level scans.

### Sigma Examples

Sigma showed its own Lagoon vault first. The example was used to illustrate how one deposited ETH can flow into different branches, including a carry trade and a Morpho vault branch. The point of the example was not the precise numbers shown in the stale copy of the data, but the shape of the dependency tree and the ability to compute it from live adapters.

Sigma then showed analysis of top Morpho markets, including a PYUSD-related market. The demo walked through first-layer lending exposures and then opened collateral nodes recursively. The tree included blue-chip collateral, stablecoin collateral, idle assets, more complex vault positions, and at least one branch that terminated in private credit or off-chain collateral.

That branch was used to show the boundary of Sigma's purely on-chain analysis. When a dependency reaches a private credit or off-chain collateral node, the right next layer is not another DeFi adapter. It is issuer disclosure, certification, proof of reserves or proof of collateral, and legal or regulatory context.

### Sigma Risk Layer

Sigma described existing and pre-production risk extensions built on top of YTD:

- Risk or depeg propagation across recursive dependency trees.
- Monitoring allocation changes at high cadence, potentially block by block with sufficient infrastructure.
- Exit market analysis through DIG, including where and how a position can be unwound.
- Liquidity depth analysis across DEXes, borrowing venues, redemption mechanisms, queues, and other exit paths.

The commercial idea before this call was to open source part of the work while selling fast data infrastructure and monitoring to curators. The value proposition for curators would be lower latency visibility into dependency changes, depeg exposure, and exits during stress.

### Bluprynt's Response

Bluprynt said the presentation was highly relevant to its own work. Robby pointed to Sigma's earlier public writing around systemic contagion risk in lending and collateral onboarding, saying that this risk is still often under-addressed even after market incidents.

Bluprynt described its customer and stakeholder base as including institutional issuers, central banks, regulators, the Bermuda Monetary Authority, the FCA, and standards-oriented groups such as tokenized asset coalitions and ERC-3643 or T-REX-related ecosystems.

Bluprynt's framing was that tokenized RWAs increasingly require a standardized disclosure registry that can satisfy investors and regulators. Many RWA products are hybrid: part on-chain vault logic, part off-chain collateral, legal wrapper, custodian, redemption rights, or issuer disclosure.

Bluprynt described two relevant pieces of its product:

- Proof of Collateral, which includes an asset disclosure form where the issuer provides off-chain details such as legal wrapper, redemption rights, and related claims.
- An asset graph, which traverses the nodes attached to an asset or vault, but does not perform Sigma's recursive yield traversal.

Bluprynt viewed the combination as regulator-grade disclosure: Sigma maps the live on-chain yield and dependency graph, while Bluprynt fills in the attested off-chain disclosure and collateral layer.

### Reporting And Compliance Use Case

Bluprynt said near-term demand is strongest from licensed tokenized RWA issuers that need ongoing automated reporting. These issuers may need to report monthly to regulators and provide several categories of artifacts, including asset structure, reserves or collateral, AML transaction activity, and cyber or crime-related monitoring outputs from other vendors.

Bluprynt said issuers and regulators are already willing to pay for reporting and compliance posture maintenance. Exchanges may also be a customer category because they need stronger diligence on listed assets.

### Integration Thesis

The parties converged on a complementary technical split.

Bluprynt can support token and vault standards, issuer disclosures, and off-chain claim resolution. Examples mentioned included major EVM vault standards, ERC-3643 or T-REX style infrastructure, Veda BoringVaults, and other RWA-associated token standards.

Sigma can support protocol traversal, yield traversal, YTD, DIG, risk propagation, liquidity analysis, and high-frequency monitoring.

Both sides acknowledged that adapters are still required. Sigma noted that each new protocol, vault, or investment structure may need a specific adapter, even when some interface patterns are reusable. Bluprynt said it has approached token and vault standards in a similar adapter-driven way.

The resulting product would let a user inspect a tokenized RWA from live on-chain dependency graph down into issuer, collateral, legal, and disclosure nodes.

### Commercial Discussion

Bluprynt expressed interest in integrating Sigma's technology into Bluprynt and making it available to customers. The call included several possible structures:

- Exclusive integration into Bluprynt.
- Outright acquisition of the relevant technology.
- Revenue share on customer revenue generated by the combined product.
- Reciprocal integration where Bluprynt's off-chain disclosures help Sigma users and Sigma's YTD/DIG helps Bluprynt users.
- Another partnership structure once scope and comfort are clearer.

No structure was selected. Bluprynt suggested moving the discussion into a shared Google Doc, then formalizing once the parties converge on scope and terms.

### Monitoring And Transaction-Time Validation

Bluprynt agreed with Sigma that fast monitoring is valuable. Beyond reporting, Bluprynt described a future use case where large transaction networks or market infrastructure could query the combined system at the point of transaction to validate that a tokenized asset is still acceptable and that counterparties are not exposed to newly visible risk.

DTCC was mentioned as an example of the kind of institution thinking about high-volume tokenized RWA markets, where millions of transactions per day could require near-real-time asset validity checks.

### Candidate Pilot Assets

When Sigma asked for a specific token or asset to demo against, Bluprynt suggested Plume Nest as a strong candidate.

Reasons given:

- Plume Nest is an active Bluprynt customer.
- Bluprynt believes Plume Nest wants this type of visibility.
- Plume Nest is already paying Bluprynt for related work.
- Plume Nest uses Veda BoringVault-style infrastructure and works with off-chain RWA issuers, making it a good example of nested vault and off-chain dependency structure.

Bluprynt also mentioned simpler examples such as Circle or PYUSD, and more complex products involving tokenized BoringVaults whose underlying assets may themselves rely on additional vaults.

## Action Items

- Bluprynt to create a shared Google Doc and give Sigma edit access.
- Both sides can now discuss customer pipelines under the signed MNDA.
- Bluprynt to share candidate tokens, entities, or customer assets for a focused demo or pilot.
- Sigma to be ready to run YTD against a Bluprynt-selected token or asset.
- Both sides to use the Google Doc to compare commercial structures and integration scope.
- Plume Nest to be considered as the first serious pilot candidate.

## Open Questions

- Which assets should be in the pilot set: Plume Nest, PYUSD, Circle-related assets, or another RWA issuer?
- Which token and vault standards should be prioritized first?
- What exact schema connects a YTD/DIG node to a Bluprynt asset graph or Proof of Collateral node?
- What freshness, revocation, and monitoring guarantees can Bluprynt provide for off-chain disclosures?
- What latency is required for issuer reporting, exchange diligence, curator monitoring, and point-of-transaction validation?
- What parts of the combined product would be open source, proprietary, licensed, or customer-specific?
- Would the right commercial structure be acquisition, revenue share, services, licensing, or a joint product?
- How should exclusivity be handled, if at all?

## Follow-Up Posture

The call ended with strong interest from Bluprynt. The immediate practical move is to avoid committing to IP, exclusivity, or acquisition terms until the technical integration and pilot asset set are clearer.

A good next discussion should focus on:

- The first pilot asset set.
- The adapter split between the teams.
- The graph join between Sigma's on-chain dependency nodes and Bluprynt's off-chain disclosure nodes.
- Required APIs, schemas, freshness, and permissioning.
- Commercial structure after the pilot scope is concrete.
