# Summer Finance Ark Integrations: Donation-Sensitive Valuation

Observed: 2026-07-06
Chain: Ethereum
Confirmed loss: approximately `$6.04M` of depositor value
Status: official Summer.fi postmortem published; vault remediation, depositor reconciliation, and recovery remain pending
Last reviewed: 2026-07-10

## Executive Summary

On 2026-07-06, an attacker manipulated the net asset value of two Lazy Summer Protocol USDC vaults and extracted approximately `$6.04M` in a single atomic transaction:

- Mainnet USDC Lower Risk vault: approximately `$5.64M`
- Mainnet USDC Higher Risk vault: approximately `$0.40M`

The incident was not caused by compromised keys or a transient oracle update. It was a composition and offboarding failure. Arks whose allocation caps had been set to zero remained active and continued contributing to `FleetCommander.totalAssets()`. An external token transfer into an active Ark was therefore credited at the Ark's accounting value even though the transferred position had little realizable value and added no corresponding withdrawal liquidity.

The attacker accumulated impaired Silo `Varlamore USDC Growth` receipt tokens, commonly referred to as `vgUSDC`, at a deep market discount. Those tokens still reported a large ERC-4626 accounting value because stranded Silo debt continued accruing interest after the November 2025 Stream Finance collapse. The attacker then:

- flash-borrowed stablecoins and acquired most of the affected vault shares,
- donated the stale-valued receipt tokens directly into an active Silo Ark,
- increased the parent vault's reported NAV without adding equivalent realizable assets,
- redeemed at the inflated share price against real USDC held in other liquid Arks and buffers.

The higher-risk vault was affected through a Term Finance Ark and a recursive path back into the lower-risk vault. Herd Labs also reports that the attacker used permissionless Morpho V2 `forceDeallocate` calls to increase immediately available USDC before redemption. This liquidity step amplified extraction but did not create the false NAV.

The central lesson is that an allocation cap of zero is not equivalent to removing an integration from accounting. A child-vault NAV can be internally consistent while being unsafe as a parent-vault valuation input, especially when receipt tokens are externally transferable, deeply illiquid, or exposed to unresolved bad debt.

## Confirmed Root Cause

Lazy Summer vaults are ERC-4626 vaults managed by a `FleetCommander`. Each FleetCommander aggregates strategy adapters called Arks. Its share price is derived from the sum of reported assets across the active Ark set.

The relevant accounting pattern was:

`Ark.totalAssets() -> FleetCommander.totalAssets() -> vault share price`

For receipt-token Arks, the Ark valuation included externally transferred balances through logic equivalent to:

`convertToAssets(balanceOf(Ark))`

The affected Arks had allocation caps of zero but had not been removed from the active Ark set. As a result:

- normal allocator deposits into the Arks were blocked,
- direct token transfers into the Ark remained possible,
- donated token balances were still included in Ark `totalAssets()`,
- Ark `totalAssets()` remained included in FleetCommander NAV,
- inflated redemptions could be paid from unrelated liquid Arks.

Summer.fi explicitly rejects the early framing that the core problem was a mismatch between `totalAssets()` and `withdrawableTotalAssets()`. The manipulated Ark did not need to be withdrawable. Redemptions were satisfied from the buffer and other liquid Arks, while the manipulated illiquid Ark was left behind. The failure was allowing an impaired, donation-sensitive Ark to remain active and priced into NAV during offboarding.

## The Impaired vgUSDC Position

The token used against the lower-risk vault was Silo's `Varlamore USDC Growth` receipt token, `vgUSDC`, at:

`0x8399C8Fc273bD165C346Af74A02e65f10e4FD78F`

The underlying Silo market had exposure to `nbxUSD-155`, an `xUSD` wrapper affected by the November 2025 Stream Finance collapse. Borrowers had little economic reason to repay USDC debt to recover impaired `xUSD` collateral. The position was therefore effectively non-redeemable even as interest continued accruing in the market's accounting.

Herd Labs estimates that the Silo vault's original `$32.8M` allocation had grown to a reported value of roughly `$488M` through accrued interest, despite the USDC being stranded. This created a large gap between:

- reported ERC-4626 NAV,
- secondary-market acquisition cost,
- realizable redemption value.

Herd reconstructs five wallets acquiring nominally about `$7M` of `vgUSDC` exposure for roughly `$14.5K` through `USDT -> xUSD -> vgUSDC` swaps in an imbalanced Balancer pool. Summer.fi independently concludes that the wallet cluster was funded and accumulated the position over roughly three months, indicating planned pre-positioning rather than an opportunistic same-block purchase.

Blockscout decoding of the exploit transaction shows large `vgUSDC` transfers through the attacker contract and into the Summer Silo Ark at:

`0x61d7063041d83C8ca3E42c39181dFd14B3Bc76c2`

The precise token quantities are less informative than the valuation mismatch: the attacker could buy the receipt token near its distressed market value, while the Summer Ark credited it near the downstream vault's reported NAV.

## Affected Vaults

| Vault | Address | Pre-incident TVL reported by Herd | Confirmed net loss |
| --- | --- | ---: | ---: |
| Mainnet USDC Lower Risk | `0x98C49e13bf99D7CAd8069faa2A370933EC9EcF17` | approximately `$9.68M` | approximately `$5.64M` |
| Mainnet USDC Higher Risk | `0xE9cDA459bED6dcfb8AC61CD8cE08E2D52370cB06` | approximately `$482K` | approximately `$0.40M` |

Herd reports 67 configured Arks across the two vaults. Following their nested allocations produces more than 500 underlying positions, including repeated and correlated exposures. That complexity helps explain how an impaired token could remain several dependency levels below a user-facing USDC vault.

## Reconstructed Attack Flow

The transaction interleaved calls across both vaults. The grouping below follows the economic effect on each vault rather than the exact call order.

### Lower-risk vault

1. The attacker borrowed approximately `65.4M USDC` plus `1M USDT` through flash liquidity.
2. They deposited approximately `64.8M USDC` into the lower-risk vault at a pre-manipulation share price near `1.0665 USDC`.
3. The deposit gave the attacker roughly `86.4%` of the post-deposit share supply according to Herd's reconstruction.
4. The attacker transferred stale-valued `vgUSDC` directly into the Silo Ark. No corresponding FleetCommander shares were minted.
5. The donation increased reported NAV by roughly `9.5%`, moving the share price near `1.1678 USDC`, without adding comparable withdrawable value.
6. The attacker redeemed against the inflated NAV. The payout came from real USDC in the buffer and liquid Morpho, Spark, and Sky Arks, not from the impaired Silo position.
7. The lower-risk vault incurred approximately `$5.64M` of net loss.

### Higher-risk vault and recursive exposure

The higher-risk vault did not directly hold the Silo `vgUSDC` Ark. Its exposure arose through a Term Finance receipt token, `tsvSummerfiUSDC`, held by an ERC-4626 Ark. That Term strategy itself routed capital into the lower-risk vault.

The attacker used this recursive structure in the same atomic transaction:

1. The call sequence began with a near-zero-cost deposit and withdrawal round trip that moved about `$398K` of liquidity into the higher-risk vault's buffer for the later redemption.
2. A deposit into the Term strategy minted `tsvSummerfiUSDC` while sending underlying USDC into the lower-risk vault. Herd estimates this leg at roughly `$490K`.
3. The attacker recovered the routed USDC during the larger lower-risk redemption.
4. The attacker donated the Term receipt tokens into the higher-risk vault's Term Ark, and the FleetCommander credited them through its active Ark accounting.
5. Across the transaction, the attacker deposited approximately `29.517M USDC` into the higher-risk vault and redeemed approximately `29.916M USDC`.
6. The higher-risk vault incurred approximately `$399K` of loss.

The recursive path matters because the vault labels suggested separate risk products, but the higher-risk vault held a token whose strategy allocated into the lower-risk vault. Manipulating one product could therefore support manipulation of the other.

## Morpho V2 Liquidity Amplification

The initial Phalcon alert highlighted Morpho V2 deallocation mechanics. Herd's later trace analysis gives the specific explanation.

Morpho V2 vaults expose a permissionless `forceDeallocate` function. A caller can pay a fee denominated in vault shares to move assets from an adapter or market into the Morpho vault's idle liquidity. Some relevant vaults reportedly had a zero fee.

Herd reports that the attacker:

- made minimal deposits into downstream Morpho vaults where shares were required,
- called `forceDeallocate` across four Morpho vaults,
- moved substantial USDC from deployed positions into immediately redeemable idle balances,
- increased the lower-risk vault's accessible liquidity from roughly `$1.6M` to about `$5.64M`.

This did not transfer funds directly to the attacker. It made USDC available inside the downstream Morpho vaults so Summer's Ark redemption path could pull it during the inflated FleetCommander redemption.

Summer.fi's official postmortem describes the payout as coming from the buffer and liquid Morpho, Spark, and Sky Arks but does not isolate `forceDeallocate` as a root-cause component. The safest interpretation is:

- stale Ark valuation created the false share price,
- active-Ark offboarding failure allowed the donation to affect NAV,
- Morpho permissionless deallocation increased the liquid value that could be extracted.

Morpho core was not exploited and did not suffer bad debt in this transaction. Its liquidity-control feature was composed with Summer's inflated redemption path.

## Offboarding and Control Split

The affected Arks had already been capped:

- SiloV2 Ark cap set to zero on 2025-10-30
- Term Finance Ark cap set to zero on 2025-11-06

Block Analitica states that its `CURATOR_ROLE` could set exposure parameters and deposit caps but could not remove Arks, alter NAV accounting, freeze withdrawals, or change protocol implementation. Ark removal required governance.

The protocol's removal rules also required an Ark to have a zero cap and hold no assets before `removeArk` could complete. That creates an intermediate state in which an Ark is closed to allocator inflows but remains active, donation-sensitive, and included in NAV. In this case that intermediate state persisted for about eight months.

The operational lesson is that offboarding must be defined as a completed state transition, not a cap change. Ownership should be explicit for:

- detecting impaired downstream positions,
- neutralizing donations during wind-down,
- marking assets to realizable value,
- sweeping or socializing unrecoverable balances,
- completing governance removal within a bounded period.

## Permissions and Incident Response

The exploit did not use administrator privileges. Two separate emergency control systems should not be conflated:

- The Guardian Module is an `8`-signer, `6-of-8` multisig with narrow authority to pause vaults, set deposit caps to zero, and cancel governance proposals. It cannot move user funds.
- The Lazy Summer Foundation multisig executed the later sweep of donated `vgUSDC` from the lower-risk vault. Herd identifies this as a separate `3-of-5` Safe and notes that the response transaction could obtain the permissions needed to move the impaired position without a timelock.

The broader permissions observation is relevant even though it was not part of this exploit. Emergency response paths that can grant roles and move positions create separate key and governance risks that should be documented alongside economic attack paths.

Response actions included:

- Block Analitica setting affected and then broader fleet deposit caps to zero,
- Guardians pausing vaults across Ethereum, Base, Arbitrum, and Sonic,
- escalation to the Foundation on HyperEVM because the Guardian role was missing there,
- the Foundation sweeping donated `vgUSDC` from the lower-risk Ark to stop ongoing NAV distortion and crystallize the loss,
- outreach to the exploiter, tracing providers, exchanges, and SEAL 911.

## Impact and Recovery Status

Summer.fi reports approximately `$6.04M` extracted as DAI. A portion was later routed through Tornado Cash using an intermediary address, reducing the probability of voluntary recovery and limiting direct tracing.

After the sweep of the impaired position, the depositor loss was reflected directly in vault value. Summer.fi estimated roughly `$4M` of capital remained in the affected vaults, much of it illiquid. The DAO still needed to decide how to distribute remaining assets, whether to exclude attacker-held shares from incident snapshots, whether to compensate depositors, and how to unpause unaffected vaults.

Herd estimates that most of the lower-risk loss fell on a large depositor whose initial position was approximately `$7.8M` and was worth about `$3.13M` after the sweep. Summer.fi had not published a final per-user reconciliation at the time of its postmortem, so this should be treated as a third-party estimate.

## Risk Classification

Primary drawer: smart contract and vault-accounting design.

Secondary drawers:

- valuation and oracle-equivalent failure,
- liquidity and redemption-path composition,
- operational offboarding failure,
- governance and role fragmentation,
- recursive and transitive vault exposure.

Calling this an oracle failure does not imply a faulty Chainlink feed. The parent vault treated a downstream receipt token's accounting NAV as robust economic value even though its secondary-market price and realizable redemption value had collapsed.

## Design Lessons

- `depositCap == 0` must not be treated as complete offboarding.
- Direct token transfers must not create redeemable NAV unless the donated asset is valued conservatively and is economically realizable.
- Parent vaults need valuation rules for distressed, paused, queued, or non-redeemable child-vault shares.
- NAV, market value, and liquidative value should be tracked separately.
- Recursive vault exposure must be visible across all wrapper and adapter levels.
- Liquidity controls such as `forceDeallocate` should be threat-modeled as public redemption controls, not only as user conveniences.
- Curator, guardian, governor, and foundation responsibilities need explicit transition ownership and deadlines.
- Monitoring should flag active Arks with zero caps, impaired receipt tokens, direct donations, unexpected balance growth, and large gaps between accounting NAV and executable value.
- A contract-level audit is not sufficient when the economic vulnerability emerges across otherwise valid integrations.

## Source Assessment

The sources agree on the core mechanism: stale-valued tokens were donated into capped but active Arks, inflated FleetCommander NAV, and enabled redemption against other depositors' liquid USDC.

They differ in emphasis:

- Summer.fi frames the primary failure as an incomplete offboarding process rather than faulty vault accounting.
- Block Analitica emphasizes that caps had been zero for eight months and that only governance could remove the Arks.
- Phalcon emphasizes donation-sensitive adapter valuation and Morpho deallocation mechanics.
- Herd emphasizes dependency-graph complexity, recursive vault exposure, permissionless liquidity activation, and the permissions used during cleanup. Herd also sells dependency-analysis tooling, so its broad conclusions about graph-based monitoring have a commercial context, while its transaction-level claims remain independently checkable onchain.

The statement that there was no code bug should be read narrowly. The contracts behaved as implemented, but accepting externally transferred impaired assets into active NAV and allowing inflated redemption was still a protocol-design vulnerability.

## Open Checks

- Independently reproduce Herd's four `forceDeallocate` calls and quantify the liquidity released by each downstream Morpho vault.
- Reconcile Herd's approximate `$14.5K` acquisition-cost estimate with the full attacker wallet cluster and Summer.fi's pre-positioning timeline.
- Track the DAO's final depositor snapshot, remaining-asset distribution, and compensation decision.
- Confirm the final amount routed through Tornado Cash and any funds frozen or recovered elsewhere.
- Review the implemented mitigation for donations, active-Ark NAV, and time-bounded offboarding before any vault is reopened.
- Determine whether other Lazy Summer or third-party vaults contain capped but active integrations with externally transferable receipt tokens.

## Sources

- Summer.fi official postmortem: `https://blog.summer.fi/lazy-summer-usdc-vault-exploit-post-mortem-what-happened-and-what-comes-next/`
- Summer.fi technical postmortem: `https://gist.github.com/halaprix/52bd5e32b35be100dc40ba30539e4169`
- Block Analitica curator retrospective: `https://forum.summer.fi/t/lazy-summer-protocol-exploit-july-6-2026-ba-labs-risk-curator-retrospective/856`
- Herd Labs analysis: `https://paragraph.com/@herd-labs/the-dangers-of-modern-vault-design-what-summer-financeblock-analiticas-dollar6m-loss-teaches-us`
- Herd Labs transaction report: `https://herd.eco/doubleclick/forum/019f426e-a452-7000-9770-f21cca833567`
- Guardian Multisig transparency thread: `https://forum.summer.fi/t/guardian-multisig-transparency-reporting-thread/787/5`
- BlockSec Phalcon initial alert: `https://x.com/Phalcon_xyz/status/2074019470918799737`
- BlockSec Phalcon follow-up analysis: `https://x.com/Phalcon_xyz/status/2074089672775725208`
- Exploit transaction: `https://etherscan.io/tx/0x0db528c44f23fc7fa4544684a2fab81096450a14aae8bc89f42cd0592d43da12`
- Phalcon Explorer transaction: `https://app.blocksec.com/phalcon/explorer/tx/eth/0x0db528c44f23fc7fa4544684a2fab81096450a14aae8bc89f42cd0592d43da12`
- Blockscout transaction: `https://eth.blockscout.com/tx/0x0db528c44f23fc7fa4544684a2fab81096450a14aae8bc89f42cd0592d43da12`
- `vgUSDC` token contract: `https://eth.blockscout.com/address/0x8399C8Fc273bD165C346Af74A02e65f10e4FD78F`
- Foundation sweep transaction: `https://etherscan.io/tx/0x7bead580b8d610e56949fb4162384e6e31dec24ad3b4668d8f4bddc345f14fd4`
