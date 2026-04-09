# Chaos Labs Exits Aave After Three-Year Risk Management Run

**Source:** [Omer Goldberg on X](https://x.com/omeragoldberg/status/2041185313163276302) — April 6, 2026

---

## Summary

Chaos Labs has officially departed as Aave's primary risk management partner after more than three years of service. Founder Omer Goldberg announced the departure citing misalignment on risk strategy, operational overload from other departing contributors, and unsustainable economics as Aave transitions to V4.

## Key Facts

- **Duration:** 3+ years as Aave's risk manager
- **Track record:** Zero material bad debt while Aave grew from $5.2B to $26B+ TVL, processed $2.5T+ in cumulative deposit volume, facilitated $2B+ in liquidations
- **Infrastructure built:** Risk Oracles, real-time parameter update systems, scaling Aave to 250+ markets across 19 blockchains

## Why Chaos Labs Left

### 1. Budget Misalignment
The V3 → V4 migration would substantially increase technical and operational demands. Chaos Labs requested $8M to manage the transition; Aave offered $5M — a $3M shortfall. Goldberg stated the firm operated at **negative margins for three years**; even a $1M increase would not achieve sustainability.

### 2. Architecture-Driven Risk Paradigm Shift
Goldberg explained: *"Risk is downstream of architecture. When the architecture changes completely, the risk engagement changes completely. Unlike turnkey solutions such as price oracles or proof-of-reserves, Risk Oracles and their accompanying systems are purpose-built for each protocol's architecture. When that architecture is rewritten from scratch, the risk infrastructure must follow."*

### 3. Contributor Exodus
The departure follows exits by other major Aave contributors:
- **BGD Labs** — main team handling V3, concluded engagement April 1, 2026
- **Aave Chan Initiative (ACI)** — stepped down amid governance tensions

The compounding departures increased Chaos Labs' workload and operational risk beyond what was sustainable.

### 4. Governance Tensions
Goldberg characterized a "deep misalignment" with Aave Labs over risk management strategy during the V4 rollout. The dispute reflects broader tensions within the protocol regarding centralized control over governance and revenue distribution.

## What Chaos Labs Built: Risk Oracles

Risk Oracles — first launched on Aave by Chaos Labs — allowed the protocol to **self-heal and update parameters in real time** in line with dynamic, volatile market conditions. This infrastructure enabled Aave to scale across 19 blockchains, streaming hundreds of parameter updates per month while maintaining operational rigor.

## Implications for DeFi Risk Management

- Aave must urgently identify replacement risk management expertise for both V3 and V4
- The departure highlights the **unsustainable economics of DeFi risk management** as a service — even at $5M+/year, Chaos Labs ran at negative margins
- The "risk is downstream of architecture" framing has direct implications for any protocol-specific risk model, including Morpho-focused proposals
- AAVE token traded at ~$95.38 (+3.9%) at time of announcement

## Relevance to Morpho Risk Model RFC

This departure is highly relevant context for Daniel's RFC:

1. **Validates demand:** If Aave's $26B protocol can't retain its risk manager, the market for risk infrastructure is real and underserved
2. **Pricing lesson:** Chaos Labs operated at negative margins for 3 years at $5M+/year — the RFC's $250K budget looks modest by comparison, but also raises questions about long-term sustainability of risk-as-a-service
3. **Architecture specificity:** Goldberg's point that risk systems must be purpose-built per protocol architecture supports the case for a Morpho-native risk model rather than a generic solution
4. **Governance risk:** The Aave governance dysfunction is a cautionary tale for any team seeking DAO grant funding — political alignment matters as much as technical quality

Sources:
- [Omer Goldberg on X](https://x.com/omeragoldberg/status/2041185313163276302)
- [Crypto Economy](https://crypto-economy.com/top-aave-risk-manager-chaos-labs-exits-amid-deepening-governance-rift/)
- [Crypto Times](https://www.cryptotimes.io/2026/04/07/chaos-labs-exits-aave-after-three-year-risk-management-run/)
