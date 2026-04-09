# LlamaRisk: Ensuring Continuity of Aave's Risk Management

**Source:** [Aave Governance Forum](https://governance.aave.com/t/llamarisk-ensuring-continuity-of-aaves-risk-management/24397)
**Posted:** April 7, 2026 — LlamaRisk

---

## Summary

Following Chaos Labs' departure, LlamaRisk (16-person team, Aave risk provider since 2024, no external investors) proposes absorbing all departing functions and transitioning Aave's risk management from delegated services into **protocol-owned infrastructure**.

## Key Points

### What they're absorbing

| Function | Readiness |
|---|---|
| Supply/Borrow cap automation | Transitioning to CRE automation |
| Interest rate parameter management | Full IRM coverage via manual AGRS |
| Liquidation parameter calibration | All markets + dynamic RWA models |
| E-Mode configuration | Active, including Liquid E-Mode |
| Oracle design & sanity checks | CAPO, adaptive feeds, CRE-native |
| Risk oracle infrastructure | Protocol-owned, replacing closed-source |
| New asset/chain evaluation | Full technical, risk, and legal |
| V4 risk architecture | Hub & Spoke, credit lines, Umbrella |
| Monitoring & alerting | Real-time deviation tracking |

### Core argument: protocol-owned > delegated

LlamaRisk critiques the old model for concentrating critical responsibility in a **closed-source external provider** with limited protocol visibility. Two incidents support this:

1. **March 10 wstETH CAPO Oracle Malfunction** — $1.03M in borrower damages, 47 wrongful liquidations, 4+ hours of depressed pricing. LlamaRisk couldn't detect or block it because of black-box architecture.
2. **February Slope2 Analysis** — significant divergence in risk oracle behavior during utilization spike, revealing undisclosed methodology gaps.

### Two-phase transition

- **Phase 1:** Immediate migration of Manual Risk Steward controls with co-ownership
- **Phase 2:** Progressive transition to protocol-owned CRE architecture — transparent, verifiable, no privileged access

### Resources

LlamaRisk: *"absorbing the full scope of a departing risk provider while simultaneously scaling protocol-owned infrastructure for V4 and Horizon cannot happen at current resource levels."* Detailed renewal proposal with budget coming soon.

---

## Relevance to Morpho RFC

1. **"Protocol-owned infrastructure"** is exactly the framing for our open-source, grant-funded transparency layer. LlamaRisk is making the same argument on Aave that we're making on Morpho.

2. **Closed-source risk providers are a liability.** The wstETH CAPO incident — $1M in damages because the risk oracle was a black box — is the strongest possible argument for auditable, reproducible risk tooling. Our RFC's "every output traceable to an onchain read" directly addresses this.

3. **16-person team absorbing Chaos Labs' scope** — shows the scale required for full risk management. Our RFC wisely targets the transparency layer only, not full parameter management.

4. **LlamaRisk's infrastructure stack** (LlamaGuard NAV, CRE automation, stress testing, VaR, liquidation cascade analysis) shows the mature version of what a risk provider looks like. Useful reference for our roadmap.

5. **The "protocol-owned" vs "vendor" debate** is live and active in DeFi governance right now. Our pitch — *"fund the commons, don't rent a vendor"* — rides this wave.
