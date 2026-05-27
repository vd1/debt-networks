# how a completely endogenous price model would work

A fully endogenous price model means $p$ is never imposed — it's solved for at every instant as the price that clears the
USDe market, given the state of the loop. Here's how it would actually be built, and why it's the hardest option.

The anchor: redemption arbitrage. USDe's peg isn't a law, it's an arbitrage equilibrium. Ethena lets (whitelisted) parties
mint/redeem USDe for ~\$1 of backing. If the secondary-market price $p < 1$, an arbitrageur buys USDe cheap and redeems it
with Ethena for \$1 of backing, capturing 1−p. That redemption buying pressure pushes $p$ back toward 1. So in an endogenous model, $p$ is pinned near 1 only while redemption is open and the backing is liquid enough to honor it at speed.

Where endogeneity bites — the backing is the loop. This is the whole point of the wash-lending paper. Redemption requires
Ethena to hand over liquid backing per USDe burned. But part of the backing is the looped Y — and to honor a wave of
redemptions Ethena must pull supply out of the markets M_k. Pulling supply raises utilization → raises r → moves r_eff and
Δ_v → changes loopers' incentives. And if the backing can't be liquidated within τ_exit, redemption effectively gates, the
arbitrage breaks, and p is no longer pinned. So p is a function of the loop state (Y, φ, τ_exit), while the loop state
evolves according to p.

The structure: a joint fixed point. At each instant you'd solve for the p that clears the USDe secondary market against
three forces: (i) holders wanting out (sell pressure), (ii) arbitrage redemption demand — bounded by how much liquid
backing is actually reachable, which depends on Y and τ_exit, (iii) loopers unwinding when Δ_v < 0 and dumping USDe
collateral. p and Y are determined simultaneously, not as driver-and-response.

Why it's the hardest — the chicken-and-egg problem. With no exogenous input, nothing perturbs the system. So a depeg has to
 emerge from the model itself, which forces you into one of two regimes:

- Self-fulfilling run: holders coordinate on "the peg will break," sell, and break it — a belief/sunspot equilibrium. This
is Diamond–Dybvig bank-run territory: you need a coordination/belief submodel, and "the dynamics" become equilibrium
selection — which basin, and what tips it.
- Bifurcation drift: a slow state variable (e.g. the basis yield s drifting down) eventually crosses a point where the
high-Y equilibrium ceases to exist, and the system falls to Y=0 with no choice. But then that drift is your de facto
exogenous input — so it's not truly fully endogenous.

That's why it's a genuinely different (and harder) paper: less "shock → cascade," more "theory of self-fulfilling depegs."
The paper already gestures at this with its multiple-equilibria result ("a small shock to S̄ or s can flip the pool") —
fully endogenous = formalizing the flip with even the trigger internalized.

Why your choice sidesteps this. "Exogenous trigger + endogenous propagation" lets you write p(0⁺) as given — a funding
spike, oracle wobble, large-holder exit, a USDT/USDC wobble — and then study propagation, where the price-impact spiral and
 the cascade math actually live. You get the full endogenous spiral without having to also answer "why did it start."
That's the clean 90% of the physics.