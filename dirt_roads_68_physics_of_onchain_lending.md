# The Physics of On-Chain Lending (I)

**Developing Appropriate Risk Pricing Frameworks for Decentralized Lending**

By Luca Prosperi | April 6, 2026

---

## Overview

This article examines mathematical frameworks for pricing credit risk in decentralized lending markets, particularly within Morpho's architecture. The author argues that current market pricing significantly undercompensates lenders for the risks they bear.

## Key Thesis

"Overcollateralized lending on liquid crypto-native collateral is Morpho's bread and butter...we observe consistently narrower spreads (5-10x) over risk-free vs. what rational lenders would require."

---

## Mathematical Framework

### Merton Model Application

The analysis adapts Robert Merton's 1974 structural credit model to DeFi contexts. In simplified form, a firm with assets worth V and debt L defaults when asset value falls below L at maturity. This creates debt as equivalent to "a risk-free bond and a short put option."

### Black-Cox Extension: First-Passage Problem

Rather than observing collateral only at maturity, DeFi protocols monitor continuously via oracles. When collateral reaches a liquidation threshold (LLTV), positions are automatically liquidated. The probability model uses geometric Brownian motion:

**dC = μC dt + σC dW**

This yields a closed-form solution for first-passage probability—the likelihood that collateral touches the barrier within timeframe T before recovering.

### Credit Spread Formula

The annualized credit spread should be:

**s = −(1/T) ln(1 − LGD × P(τ_B ≤ T))**

Where LGD is loss given default. For liquid crypto collateral like ETH/BTC, this approximates the liquidation incentive.

---

## Critical Findings

### Simulation Results

For an ETH position at 70% LTV against 86% LLTV with 75% annualized volatility:

- **Distance to Default**: 0.59 sigma units (alarmingly close)
- **Without rebalancing**: 70-80% probability of liquidation within one year
- **Required spread**: 400+ basis points

However, observed depositor rates in Morpho's USDC markets range from 0-20 basis points—**a 20-100x underpricing relative to theoretical requirements**.

### Rebalancing Assumptions

- **Pure jump risk only** (continuous rebalancing): 45 bps minimum
- **20% extra capital available**: 350 bps spread needed
- **100% extra capital**: 130 bps spread needed
- **Realistic daily monitoring with finite capital**: 250-400 bps

---

## Why Is Credit Mispriced?

The article identifies five compounding factors:

1. **Depositor Misperception**: Retail users treat USDC deposits as risk-free savings, not as short puts on crypto collateral. They're "implicitly selling puts without recognizing it."

2. **Regulatory Arbitrage**: Stablecoin legislation restricts traditional intermediaries from offering competing yields, making Morpho the "most convenient, immediate, no-KYC non-custodial savings account."

3. **Survivorship Bias**: Major losses hit specific vaults; flagship Steakhouse and Gauntlet-curated USDC markets have avoided catastrophic losses, reinforcing a "safe yield" narrative.

4. **Bull Market Masking**: Under real-world (physical) market conditions, positive ETH/BTC returns reduce observed liquidation frequency, though risk-neutral pricing remains invariant to drift.

5. **Token Subsidies**: MORPHO incentives compress observed rates by subsidizing both borrowers and lenders.

---

## Leverage Looping Strategies

The analysis distinguishes looping trades (depositing asset A, borrowing B, converting back) as fundamentally different from simple lending:

- **Risk type**: Basis volatility, not directional price risk
- **Carry sources**: (Staking yield − borrow rate) × leverage
- **Historical example**: sUSDe looping at 7-10x leverage during high funding rates peaked at $1b+ TVL through Maker → Spark → Morpho → Ethena tower

At moderate leverage (3-5x), basis would need 15-30% moves to trigger liquidation. Above 10x, strategies become convex bets that "blow up precisely during liquidity crises."

---

## RWA Collateral: Models Break Completely

For non-crypto-native collateral, every Merton assumption fails:

1. **Unobservable Volatility**: Prices are quarterly marks, not market prices. Smoothed marks artificially suppress measured volatility, creating dangerous illusion of safety.

2. **Discrete Monitoring Defeats First-Passage**: Weekly or monthly updates allow collateral to fall far below LLTV before detection. The effective barrier shifts by approximately **β·σ·√(Δt)** where β ≈ 0.58.

3. **Liquidation Isn't Atomic**: Selling private credit takes weeks/months. Collateral deteriorates during delays, compounding losses with fire-sale discounts of 20-50%.

4. **Enforceability**: Tokenized claims still require legal enforcement across jurisdictions.

**Author's Position**: "I remain extremely bearish on non-crypto-native collateralized lending...adverse selection risk on those assets is high."

---

## Morpho v2: Intent-Based Markets

The upcoming upgrade introduces intent-based matching and fixed-rate, fixed-term loans. While ambitious, the author notes missing pieces:

- Insufficient solver depth for illiquid pairs
- Duration risk management systems remain underdeveloped
- Institutional capital requirements for counterparty certainty remain unmet

---

## Conclusion

Current DeFi lending spreads represent profound mispricing relative to mathematical models accounting for volatility, first-passage dynamics, and realistic rebalancing constraints. The gap reflects regulatory arbitrage, retail misperception, and survivorship bias rather than accurate risk pricing. The mispricing "will become visible when the market turns."
