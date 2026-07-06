# Summer Finance Ark Integrations: Donation-Sensitive Valuation

Observed: 2026-07-06
Chain: Ethereum
Estimated loss: around `$6M` per BlockSec Phalcon
Status: early external analysis, pending protocol postmortem

## Executive Summary

BlockSec Phalcon flagged a suspicious Ethereum transaction affecting Summer Finance on 2026-07-06, estimating losses around `$6M`.

The initial alert framed the issue as a share manipulation attack abusing deallocation mechanics in Morpho V2 vaults. Phalcon's later same-day analysis narrowed the suspected root cause to donation-sensitive valuation inside Summer Finance's Ark integrations, especially `MorphoV2VaultArk` and `SiloManagedVaultArk`.

The critical pattern is allocator accounting that reads a downstream vault's spot exchange rate through `convertToAssets(balanceOf(...))`. If that spot exchange rate can be distorted, or if the downstream receipt token can be acquired far below its accounting NAV, the parent allocator can import an inflated `totalAssets` value and allow withdrawals against value that is not economically real.

This is a clean vault-accounting incident: a local share-price distortion propagates upward into FleetCommander accounting and turns an integration-level valuation assumption into extractable funds.

## Phalcon Root-Cause Hypothesis

Phalcon's follow-up says both `MorphoV2VaultArk` and `SiloManagedVaultArk` appear to value positions with downstream vault spot exchange rates:

`convertToAssets(balanceOf(...))`

That makes reported assets donation-sensitive and receipt-token-price-sensitive. A donation, forced share-price movement, or gap between a deprecated receipt token's market price and accounting NAV can inflate the apparent value of the Ark's position even when executable value has not increased. The inflated value is then propagated into FleetCommander accounting.

Phalcon also noted that the attacker first accumulated a large amount of deprecated `vgUSDC`, likely at negligible cost, and that this appears to have been an important input to the later share-price distortion.

## vgUSDC Valuation Discrepancy

The clearest concrete price discrepancy is around `vgUSDC`, the `Varlamore USDC Growth` token at `0x8399C8Fc273bD165C346Af74A02e65f10e4FD78F`.

Blockscout transfer decoding for the exploit transaction shows:

- The attacker contract received about `476,265,053.026626 vgUSDC` from Balancer V3.
- The attacker contract later transferred about `19,551,517,226.711127 vgUSDC` to Summer's `SiloManagedVaultArk` at `0x61d7063041d83C8ca3E42c39181dFd14B3Bc76c2`.

This reframes the exploit as a classic oracle discrepancy. The attacker appears to have accumulated deprecated `vgUSDC` at a much lower economic price than the value reported by the downstream vault's ERC-4626 accounting. Summer then valued the Ark's `vgUSDC` exposure through the child vault's accounting NAV:

`convertToAssets(balanceOf(Ark))`

The problem is not that `convertToAssets()` is internally wrong. It is that this value was used as a price oracle for externally acquired, deprecated, and potentially illiquid receipt tokens. A child-vault NAV can be consistent with its own accounting while still being a bad liquidation value or parent-vault NAV input.

## Apparent Attack Flow

- Accumulate deprecated `vgUSDC` cheaply.
- Enter the Summer Finance accounting path and receive Summer vault exposure.
- Move a large amount of cheap `vgUSDC` into the Ark valuation path.
- Let Summer value that `vgUSDC` exposure at ERC-4626 accounting NAV through `convertToAssets(balanceOf(...))`.
- Redeem or withdraw against FleetCommander's inflated asset accounting.

## Risk Drawer

Primary drawer: smart contract / vault-accounting logic.

Secondary drawer: oracle and valuation failure. This is not a Chainlink-style oracle mistake, but it is the same class of error from a lender or allocator perspective: a manipulable spot value or accounting NAV is treated as a robust economic value.

## Morpho Relevance

The initial Phalcon alert explicitly mentions Morpho V2 vault deallocation mechanics, and the later update names `MorphoV2VaultArk`. The important distinction is that this is not described as a Morpho core exploit. The risk sits at the integration layer: an allocator adapter values a downstream vault position using a spot share conversion that can be distorted during reallocations.

For MetaMorpho and curator-risk work, this is a useful case study because it combines:

- downstream vault spot accounting,
- allocator-level `totalAssets` propagation,
- donation-sensitive share-price manipulation,
- deallocation flow complexity,
- stale or deprecated collateral inputs,
- receipt tokens whose acquisition price diverges from ERC-4626 accounting NAV.

## Open Checks

- Confirm the final loss amount from Summer Finance or a full BlockSec postmortem.
- Identify the exact affected FleetCommander vaults and assets.
- Verify whether both `MorphoV2VaultArk` and `SiloManagedVaultArk` were directly exploitable, or whether one was named as a structurally similar risk.
- Check whether any funds were frozen, returned, or recoverable.
- Review the exact transaction trace against the `convertToAssets(balanceOf(...))` accounting path.

## Sources

- BlockSec Phalcon initial alert: `https://x.com/Phalcon_xyz/status/2074019470918799737`
- BlockSec Phalcon follow-up analysis: `https://x.com/Phalcon_xyz/status/2074089672775725208`
- Phalcon Explorer transaction: `https://app.blocksec.com/phalcon/explorer/tx/eth/0x0db528c44f23fc7fa4544684a2fab81096450a14aae8bc89f42cd0592d43da12`
- Blockscout transaction: `https://eth.blockscout.com/tx/0x0db528c44f23fc7fa4544684a2fab81096450a14aae8bc89f42cd0592d43da12`
- `vgUSDC` token contract: `https://eth.blockscout.com/address/0x8399C8Fc273bD165C346Af74A02e65f10e4FD78F`
