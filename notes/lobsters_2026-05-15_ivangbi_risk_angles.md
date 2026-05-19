# Telegram thread — LobsterDAO 🦞
Source: https://t.me/lobsters_chat/574116

## Target message (#574116)

**ivangbi** · #574116 · 2026-05-15 12:45:06 UTC
DeFi peoples, brainraping time.

When you consider the risk of a protocol or an asset to deploy into, it comes through different angles:

1. code review: codebase itself (architecture, its simplicity, audits, bug bounties, test coverage, formal verification, etc)
2. liquidity of an asset: how liquid is a stablecoin, for example, when can you withdraw, how long to wait until maturity, etc. this isn't a risk in a hack sense but a risk nonetheless
3. leverage ratio: if is a lending protocol or something that underwrites loans/mints - the question is also how much is it looped. the more looped it is, the riskier sometimes an asset/protocol is (not always I guess). this can be similar to point (2) though
4. dependency on third-party things: oracles, keepers, curators, or any other jobs (onchain or offchain) that can influce the behavior of your position. influence, not "own" per se
5. admin access controls: admin permissions in contracts, who can mint or freeze, who can block assets or add new collaterals, etc. defiscan does that, for example
6. legal aspects: this is not per se point 5, but is an extension of it. what is the credibot-debtor relationship and how could that mess up your position even if no hack was done to your assets directly (like kelp L2 situation with bridge dependency, or how it was with Resolv to an extent where it wasn't a crystal-clear choice) 

Questions:

a) What are the 6/7/8 angles you can add to the list?
b) What are the services today that review these things? DeFiscan for (5), DeFi Saver / Chaos Labs / a few other for (2 or 3)... what else? What about the other pointers?

## Direct replies (34)

  **gspdnsobaka** · #574117 · 2026-05-15 12:46:39 UTC
  I conduct team background checks before investing. Basically look if their photos are aí generated and on what projects they worked before the current one

  **AJ777Josh** · #574118 · 2026-05-15 12:47:14 UTC
  Economic risk (Terra/luna)
  Counterparty risk for rwas
  data integrity/oracle risk
  Bridge risk

  **ivangbi** · #574119 · 2026-05-15 12:47:49 UTC
  These are all already inside 1-6

  **ivangbi** · #574120 · 2026-05-15 12:48:49 UTC
  That’s just a surface level screening from 2017 ico times. I dont remember when the last time it was that I saw many DeFi devs photos (even though they exist). You are supposed to look deeper into 1-6 vs just surface level screening. So no

  **cryptographicas** · #574121 · 2026-05-15 12:51:26 UTC
  Insolvency risk or how losses get absorbed in the system 
  Who gets rekt first were do we plebs stand in the order of wreckage 
  Contagion / rehypothification what’s the asset behind the asset behind the asset

  **nachollanillo** · #574123 · 2026-05-15 12:53:57 UTC
  Track record. How many years have the smart contracts been working vs how many attacks/hacks has the protocol suffered. 
  
  Basically the Lindy effect. Longer time signals more resiliency to changes in the industry.

  **cryptographicas** · #574124 · 2026-05-15 12:54:50 UTC
  Would also add redemption risk especially in the context of RWA’s and private credit

  **cryptographicas** · #574125 · 2026-05-15 12:55:23 UTC
  I don’t think this even exists anymore thanks to mythos

  **RobAnon** · #574128 · 2026-05-15 13:03:11 UTC
  This is what we use at infiniFi: https://www.notion.so/infiniFi-Risk-Assessment-Framework-28444c414f36808ca354e8b07fa1d22e

  **ivangbi** · #574129 · 2026-05-15 13:03:37 UTC
  Ye pretty much 1:1, nice.

  **xzhvyr** · #574133 · 2026-05-15 13:08:19 UTC
  herd(.)eco for architecture visualisation

  **andrew_core3** · #574135 · 2026-05-15 13:21:22 UTC
  So from my pov, you can run a decomposition across 6 columns: financial / security / operational / reputational / regulatory / dependency
  But I will dive into an operational risk (key mgmt, signer distribution, incident response readiness) and historical pattern match (does this protocol share design choices with things that already blew up? 1/1 DVN configs, unverified vault contracts, rounding in share accounting, etc.) not a code review, more "have we seen this movie before."

  **ivangbi** · #574136 · 2026-05-15 13:22:14 UTC
  I bet 10 LUSD this was written by AI :/

  **andrew_core3** · #574138 · 2026-05-15 13:25:04 UTC
  Nope, we analyse all this risks on our platform, I took it from the methodology https://core3.io/methodology/projects#metric-set

  **ivangbi** · #574144 · 2026-05-15 13:32:28 UTC
  This is cool potentially (and the task itself is very hard) but excuse me that seems rather surface level. It's understandable though if you try to mostly automate it by yourself, but I clicked on a few projects and the info was simply missing. That will need to be done by teams who'd submit their info I guess (can't all be done by you).
  
  It seems to me more like Credora (opinionated, one-score per protocol, composite scoring of multiple things) vs being laser-focused on every piece of the risk stack

  **andrew_core3** · #574145 · 2026-05-15 13:40:43 UTC
  Yes, it’s definitely not an easy task. We’re currently in the MVP phase and are still working on the data, but you’re right in pointing out that projects should also be able to submit and update their own information. If certain details aren’t publicly available, each project can come in and correct its own data

  **TheDr_TheDr** · #574147 · 2026-05-15 14:50:35 UTC
  Agreed, it cant be done centralised teams, so there has to mechanisms were things can get updated and corrected with quality adversarial  systems to make sure data integrity is validated with proofs and linked to authors

  **Ceazor** · #574148 · 2026-05-15 14:51:14 UTC
  7/ antidump mechanics. why hold, why farm, and who will buy it
  8/ intergrations with others, future potential and current. 
  9/ emergency repsonses, pauses (auto or not) , insurance, treasury/recovery funds 
  10/ 3rd party protocols dependancy, which projects depend on THIS one, as are willing to step in to help.

  **TheDr_TheDr** · #574150 · 2026-05-15 14:58:28 UTC
  beein working on number 10 actually - its probably the most interesting one for me at the moment - especially from exposure perspective

  **floowp** · #574151 · 2026-05-15 15:34:30 UTC
  Noting defiscan also covers (4) to an extent :)  #(5) in my experience is difficult to grasp as "legal" is really an overarching category and probably needs to be narrowed down more (eg user legal rights vs regulatory risk affecting a user vs x-jurisdictional, etc.), but would be interested if someone is aware of tools/services tracking/working on this (I'm sure more will be popping in the future as we get more dense regulatory requirements through Clarity and the like).

  **Lmk61_102** · #574154 · 2026-05-15 15:45:58 UTC
  I’d also like to evaluate the friction involved in entering and exiting the DeFi position — specifically bridge fees, deposit/withdrawal fees, redemption/lock-up periods, and opportunity cost. This will be an important input in my overall risk/reward evaluation. From my knowledge there is no tools for that.

  **andrew_core3** · #574155 · 2026-05-15 16:04:51 UTC
  Fair point, It would have been complete chaos if anyone on the team submitted the information without proper validation. This process is in place and is super critical

  **xmons** · #574156 · 2026-05-15 16:25:17 UTC
  7) where is the yield coming from (inflationary token emissions, demand for leverage, am i secretly being the backstop for something else, e.g. aave umbrella, tranching)

  **Stengarl** · #574157 · 2026-05-15 16:30:12 UTC
  Frontend implementation could be added to this list
  
  95%+ of DeFi users are using a frontend to interact with protocols. So if the frontend is shut down / censored, almost all users can't access to their funds anymore.
  
  Worse: the frontend can be compromised (DNS hijack, supply-chain attack on the frontend, malicious update), leading users to sign malicious transactions.
  
  The best-case scenario would be multiple independent frontends, including ENS + IPFS ones

  **taulantxx** · #574159 · 2026-05-15 16:32:43 UTC
  + a cli

  **safetylast** · #574162 · 2026-05-15 16:44:06 UTC
  separate by asset and protocol risk 
  
  different assets soemtimes also evaluated a bit differently. same for protoocls

  **JoeWait** · #574165 · 2026-05-15 16:52:38 UTC
  1. FE vibe coded?
  
  2. Scammer founders?
  
  3. KOL shilling?

  **OxJMG** · #574166 · 2026-05-15 17:02:57 UTC
  Some combination of DefiLlama TRF, Blockworks TTF or Aragon OTF.
  
  Want max possible transparency and onchain enforceability. More deets here: https://x.com/0xJMG/status/2051342977528942893

  **AntonAkentiev** · #574170 · 2026-05-15 17:23:30 UTC
  https://risklayer.online/ 
  https://defipunkd.com/
  https://pigi.finance
  https://xerberus.io/
  https://risk.zyf.ai
  https://pharos.watch/
  https://www.llamarisk.com
  ...

  **Zdeadex** · #574172 · 2026-05-15 18:14:54 UTC
  analytics.philidor.io / https://docs.philidor.io/docs/api-reference
  
  7. Asset composition / backing (off-chain, crypto-exo, endo, delta-hedged) 
  8. Issuer + counterparty quality (RWA: who actually issues) 
  9. Custodian / regulatory wrapper / KYC gate 
  10. Curator quality (governance, legal entity, audit firm), for Morpho/Aave-style vaults 
  11. Oracle health continuously (freshness, deviation, source diversity)
  12. Incident history + inherited exposure (Kelp/Resolv-style) 
  13. Composability / wrapper risk (4626 wrappers, vault-of-vault) 
  14. Reward sustainability (base vs incentives, who pays, duration) 
  15. Redemption mechanics / withdrawal queue / NAV cycles 
  16. TVL shape, depositor concentration, velocity (not absolute TVL) 
  17. Bridge / cross-chain dependency 
  18. Reserve transparency / attestation cadence

  **Staker1971** · #574173 · 2026-05-15 19:42:52 UTC
  I admire you!

  **dbadol** · #574194 · 2026-05-16 09:19:50 UTC
  6) Privacy.
  
  Onchain: How much visible and how easily traceable are my operations? How much frontrun, sandwich, etc. can I suffer? Frontends being likely centralized (almost all are), who manages them? How much can I trust them?
  
  Offchain: Considering the whole process, who monitors me, my IP, my accounts, my wallets? Where did I KYC? Are they able to keep that information safe?
  
  In other words, assume you live in France and you want to keep your 10 fingers… 😖

  **ivangbi** · #574201 · 2026-05-16 14:12:42 UTC
  Awesome thx. Are there corresponding dashboards for these, that you use? Beyond the ones mentioned above already

  **Zdeadex** · #574210 · 2026-05-16 16:35:15 UTC
  No we built Philidor because we didn’t find solutions covering them, happy to discuss what you would like/need to see inside

## Context before (last 15 messages)

**fred_letsgetonchain** · #574096 · 2026-05-15 09:03:28 UTC
https://x.com/credditxyz/status/2055200359400739009?s=20

**fred_letsgetonchain** · #574097 · 2026-05-15 09:03:56 UTC
hope the clarity act doesnt prevent this in the future somehow, the more yield pass through to onchain users the better

**fred_letsgetonchain** · #574098 · 2026-05-15 09:04:08 UTC
what is the status on that, anyone know?

**crypto_luciano** · #574099 · 2026-05-15 09:20:25 UTC
Is this the same thing they are doing on megascam?

**fred_letsgetonchain** · #574100 · 2026-05-15 09:26:35 UTC
i guess its the same playbook on megaeth, getting treasury yield kickback from USDm

**ivangbi** · #574101 · 2026-05-15 09:32:57 UTC
https://x.com/barnabemonnot/status/2055185422238593041?s=46

**schoad** · #574102 · 2026-05-15 10:01:39 UTC
thorchain exploited.

never a boring day in centralized services

**crypto_luciano** · #574103 · 2026-05-15 10:13:03 UTC
anyone have opinions on euphoria tap trading thing im not smart enough to understand it just looks like crazy way to lose money fast

**madjarevicn** · #574104 · 2026-05-15 10:22:12 UTC
Degenerate casino wrapped as a tradfi product

**madjarevicn** · #574105 · 2026-05-15 10:22:27 UTC
I am happy to meet a trader who has a trading thesis on 10s intervals 😂

**flipdazed** · #574106 · 2026-05-15 11:04:56 UTC
hi - I have a pretty strong thesis on microsecond fwiw. Read the book "Flash Boys". Low latency trading is the strongest thesis you can have tbh

**Caf0lla** · #574108 · 2026-05-15 11:25:17 UTC
What's that now, their 3rd or 4th exploit?

**Caf0lla** · #574109 · 2026-05-15 11:25:49 UTC
Used to be one of my biggest holdings but it's far too risky to invest in DeFi anymore. I just stick to infra mainly now

**madjarevicn** · #574110 · 2026-05-15 11:36:35 UTC
completely different games tbh. HFT has a thesis on microstructure and queue position, colocated servers, FPGA, etc. 
Tap products take the same timeframe, strip out the infra, and sell directional bets to retail wrapped in confetti and countdowns. One is edge, the other is a slot machine in trader clothing tbh.

**andrew_core3** · #574111 · 2026-05-15 11:44:10 UTC
https://x.com/banteg/status/2055231991507800334?s=20
True, but it was strange to see continued announcements about community podcasts without any mention of the security incident on their twitter page. The exploit might be related to their latest fix

## Context after (next 15 messages)

**gspdnsobaka** · #574117 · 2026-05-15 12:46:39 UTC
I conduct team background checks before investing. Basically look if their photos are aí generated and on what projects they worked before the current one

**AJ777Josh** · #574118 · 2026-05-15 12:47:14 UTC
Economic risk (Terra/luna)
Counterparty risk for rwas
data integrity/oracle risk
Bridge risk

**ivangbi** · #574119 · 2026-05-15 12:47:49 UTC
These are all already inside 1-6

**ivangbi** · #574120 · 2026-05-15 12:48:49 UTC
That’s just a surface level screening from 2017 ico times. I dont remember when the last time it was that I saw many DeFi devs photos (even though they exist). You are supposed to look deeper into 1-6 vs just surface level screening. So no

**cryptographicas** · #574121 · 2026-05-15 12:51:26 UTC
Insolvency risk or how losses get absorbed in the system 
Who gets rekt first were do we plebs stand in the order of wreckage 
Contagion / rehypothification what’s the asset behind the asset behind the asset

**arpangautam** · #574122 · 2026-05-15 12:53:43 UTC
OpSec - multisig signatures, etc. maybe part of 5, but can be more explicit. i know a few people have suggested best practices, and seal 911 (among others) are pointing protocols like us towards third party opsec auditors

more than just "who" can do these things as in 5, but how secure are these mechanisms

**nachollanillo** · #574123 · 2026-05-15 12:53:57 UTC
Track record. How many years have the smart contracts been working vs how many attacks/hacks has the protocol suffered. 

Basically the Lindy effect. Longer time signals more resiliency to changes in the industry.

**cryptographicas** · #574124 · 2026-05-15 12:54:50 UTC
Would also add redemption risk especially in the context of RWA’s and private credit

**cryptographicas** · #574125 · 2026-05-15 12:55:23 UTC
I don’t think this even exists anymore thanks to mythos

**ivangbi** · #574126 · 2026-05-15 12:58:26 UTC
https://x.com/uhr3al/status/2055270054238023789?s=46

**ivangbi** · #574127 · 2026-05-15 13:01:08 UTC
Ok @cryptographicas @nachollanillo @arpangautam @AJ777Josh now what are the services/dashboards you use to track those?

**RobAnon** · #574128 · 2026-05-15 13:03:11 UTC
This is what we use at infiniFi: https://www.notion.so/infiniFi-Risk-Assessment-Framework-28444c414f36808ca354e8b07fa1d22e

**ivangbi** · #574129 · 2026-05-15 13:03:37 UTC
Ye pretty much 1:1, nice.

**ivangbi** · #574130 · 2026-05-15 13:04:43 UTC
You redo research on every asset every time? Anything external you use that helps, or it's just non-existent across the board?

**AJ777Josh** · #574131 · 2026-05-15 13:05:20 UTC
Economic risks are independant audits I'm not sure any real time monitoring for economic risks exists! Would love to know more around this.

I think all others are all pretty much audits.

Only real time threat monitoring I'm familiar with are hypernative, hexagate and a few other providers
