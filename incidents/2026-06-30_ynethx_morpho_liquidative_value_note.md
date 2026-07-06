# ynETHx Morpho Market: Mark-To-Market Versus Liquidative Value

Data refreshed: 2026-06-30 20:55 UTC

Market: `0x3c16c2d107caade490b1d513ccc54cbd06a30f20ad6a9aaad8c744413872514a`
Collateral: `ynETHx`
Debt asset: `wstETH`
LLTV: `91.5%`

## Executive Summary

This market is a school case of the difference between mark-to-market value and liquidative value.

On the oracle mark, the main borrower is barely alive:

- Health factor: `1.0045`
- Borrowed: `1,296.03 wstETH`, about `$2.53m`
- Collateral: `1,621.46 ynETHx`, marked around `$2.78m`
- Utilization: `100%`
- Available lender liquidity: `$0`

On executable market depth, the position is already economically underwater. The visible exit market for `ynETHx` is too shallow to repay the debt. At Curve marginal price, the collateral is worth about `1,626.91 ETH` while the debt is about `1,604.86 ETH`, leaving only `22.06 ETH` of raw value before applying the `91.5%` LLTV. After applying LLTV, the liquidative health factor is only `0.928`.

The collateral would need to be worth about `1,753.94 ETH` to support the debt at the liquidation threshold. At Curve marginal value it is short by about `127.03 ETH`. For a real liquidation clip, the shortfall is much worse because the Curve pool has only about `34.76 WETH` on the exit side.

Conclusion: absent voluntary repayment, fresh collateral, private OTC liquidity, or protocol-level intervention, there is no realistic path for public liquidation proceeds to repay this debt in full.

## Current Position

The borrower driving the risk is:

`0x14bCD9da052Cdc6fE0b9446d5a616D5b7B4d4550`

Current position:

| Metric | Value |
| --- | ---: |
| Debt | `1,296.03 wstETH` |
| Debt, USD mark | `$2,532,807.71` |
| Collateral | `1,621.46 ynETHx` |
| Collateral, USD mark | `$2,780,356.25` |
| Health factor | `1.0045` |
| Borrow APY | `69.61%` |

The borrower is less than one percent from liquidation by oracle health. This is already a fragile state before considering exit liquidity.

## Lender Side Context

The market is fully utilized:

| Metric | Value |
| --- | ---: |
| Total borrow | `1,296.03 wstETH` |
| Total supply | `1,296.03 wstETH` |
| Utilization | `100%` |
| Liquidity available | `$0` |

Calendar May 2026 lender flows show clear withdrawal pressure:

| Flow | Value |
| --- | ---: |
| Gross supplied | `1,263.96 wstETH` |
| Gross withdrawn | `1,418.76 wstETH` |
| Net lender flow | `-154.80 wstETH` |

The largest outflow account was:

`0x833AdaeF212c5cD3f78906B44bBfb18258F238F0`

It supplied `1,261.82 wstETH`, withdrew `1,416.62 wstETH`, and net withdrew `154.80 wstETH`.

The result is a market with no spare lender liquidity and a single dominant borrower position sitting close to liquidation.

## Oracle Mark

Morpho oracle:

`0xB1E676190A86DA2Cb99afD0496538ABe1D4C164D`

The oracle is a `ChainlinkOracleV2` construction. Its current price is:

`0.877412 wstETH / ynETHx`

The quote feed used in the construction is:

`0x905b7dAbCD3Ce6B792D874e303D336424Cdb1421`

That feed reports the `wstETH/stETH` exchange rate. The oracle therefore marks `ynETHx` from vault or share conversion logic combined with the wstETH exchange rate. It is not reading the Curve pool as a direct liquidative price.

That is the core disconnect. The oracle mark can say the borrower is alive while the public exit market cannot actually convert the collateral into enough ETH or wstETH to repay the loan.

## Public Exit Liquidity

The only meaningful public exit market found is the Curve `ynETHx/WETH` pool:

`0xD65ed4BcE447195187f37cE7D82f56AdF1826F8F`

Pool balances:

| Asset | Balance |
| --- | ---: |
| `ynETHx` | `459.73` |
| `WETH` | `34.76` |

This pool is deeply imbalanced. It contains a lot of `ynETHx` but very little `WETH`. It can support small marks, but it cannot absorb a liquidation-sized sell.

Executable Curve quotes:

| Sell `ynETHx` | Receive `WETH` | Average `WETH / ynETHx` | Discount vs oracle |
| ---: | ---: | ---: | ---: |
| `0.1` | `0.1003` | `1.0034` | `-7.65%` |
| `1` | `1.0013` | `1.0013` | `-7.84%` |
| `5` | `4.9553` | `0.9911` | `-8.78%` |
| `10` | `9.7473` | `0.9747` | `-10.29%` |
| `25` | `22.1534` | `0.8861` | `-18.44%` |
| `50` | `31.0689` | `0.6214` | `-42.81%` |
| `100` | `33.6206` | `0.3362` | `-69.06%` |
| `250` | `34.4611` | `0.1378` | `-87.31%` |
| `500` | `34.6498` | `0.0693` | `-93.62%` |
| `1000` | `34.7175` | `0.0347` | `-96.80%` |

The table shows that the marginal public price and the liquidation-sized executable price are not the same object. A tiny trade is already below the oracle mark by about `8%`; a `100 ynETHx` sale clears around `69%` below the oracle; larger clips effectively drain the WETH side and receive almost no incremental value.

## CEX Check

No centralized exchange listing or quote was found.

Market-data APIs showed only DEX venues:

- CoinGecko markets: `Curve (Ethereum)`
- CoinPaprika markets: `Curve Finance`, `Curve TwoCrypto (Ethereum)`
- DexScreener and GeckoTerminal likewise show on-chain pools, mostly Curve

So the relevant public executable market is DEX-only. There is no visible CEX order book that can absorb liquidation flow.

## Liquidative Value

Using the Curve marginal price:

| Metric | Value |
| --- | ---: |
| Debt | `1,296.03 wstETH` |
| Debt in ETH terms | `1,604.86 ETH` |
| Collateral | `1,621.46 ynETHx` |
| Curve marginal price | `1.0034 WETH / ynETHx` |
| Collateral at Curve marginal price | `1,626.91 ETH` |
| Raw buffer before LLTV | `22.06 ETH` |
| Health factor at Curve marginal price | `0.928` |
| Shortfall to liquidation threshold | `127.03 ETH` |

Even the best small-trade Curve marginal price makes the position fail the Morpho liquidation threshold. Liquidation execution is worse because the position is far larger than the WETH side of the pool.

If the whole `1,621.46 ynETHx` collateral position were pushed through the main Curve exit pool, previous on-chain quotes showed proceeds of only about `34.7 WETH`. That is nowhere near the `1,296 wstETH` debt.

## Mark-To-Market Versus Liquidative Value

The mark-to-market value is the oracle or NAV-style value used for accounting and health checks. It answers the question:

What is the position worth if every unit can be valued at the reported reference price?

The liquidative value is the value that can actually be realized when collateral must be sold into available depth. It answers a different question:

How much debt can be repaid after selling the collateral through executable markets?

In liquid assets those two values are often close. Here they are not close.

The oracle says the collateral is worth enough to keep the position just above liquidation. The Curve pool says the first small exit is already below oracle, and that liquidation-sized exits rapidly collapse to the pool's limited `WETH` balance. The public market cannot turn the collateral into enough `wstETH` or `ETH` to repay the debt.

## Why Full Repayment Is Not A Realistic Liquidation Outcome

The debt can be repaid only if one of these happens:

- The borrower voluntarily repays.
- The borrower adds collateral or refinances.
- A private OTC buyer absorbs `ynETHx` near oracle value.
- The protocol or another actor provides external liquidity.
- The oracle or market structure changes before liquidation.

Without one of those external paths, public liquidation proceeds are insufficient.

The visible public market has about `34.76 WETH` on the Curve exit side, while the debt is about `1,604.86 ETH` in economic terms. That is the central fact. The collateral may have a high mark, but its liquidative value is much lower.

This is why the debt should not be treated as repayable from collateral liquidation under current market depth.

## Sources And Checks

- Morpho market: `https://app.morpho.org/ethereum/market/0x3c16c2d107caade490b1d513ccc54cbd06a30f20ad6a9aaad8c744413872514a/ynethx-wsteth#market`
- Morpho GraphQL market and position data: `https://api.morpho.org/graphql`
- Morpho oracle RPC call: `price()` on `0xB1E676190A86DA2Cb99afD0496538ABe1D4C164D`
- Curve pool RPC calls: `balances(uint256)` and `get_dy(int128,int128,uint256)` on `0xD65ed4BcE447195187f37cE7D82f56AdF1826F8F`
- CoinGecko tickers: `https://api.coingecko.com/api/v3/coins/yneth-max/tickers`
- CoinPaprika markets: `https://api.coinpaprika.com/v1/coins/ynethx-yneth-max/markets`
- DexScreener token pools: `https://api.dexscreener.com/latest/dex/tokens/0x657d9ABA1DBb59e53f9F3eCAA878447dCfC96dCb`
- GeckoTerminal token pools: `https://api.geckoterminal.com/api/v2/networks/eth/tokens/0x657d9ABA1DBb59e53f9F3eCAA878447dCfC96dCb/pools?page=1`
