# Debt networks

Research notes and an OCaml simulation for clearing in debt networks.

## Layout

- `src/dn.ml`: clearing simulation from the original `debt-networks` repository.
- `notes/2020-lmfi/main.tex`: LMFI clearing-game notes from `DN`,
  Its local `vmacros.sty` is kept alongside it.
- `notes/2026-clearing/clearing_research_note.tex`: fractional prepayment and clearing note.
  The adjacent PDF preserves the untracked build found during consolidation.
- `notes/2026-onchain/Eisenberg-Noe.tex`: on-chain clearing note
  Its bibliography, reference PDFs, and `vmacros.sty` are kept alongside it.
- `references/simulation/trader1.pdf`: the distinct PDF from the original simulation repository.

Build a note from its own directory with `latexmk -pdf <filename>.tex`.
The LMFI note uses its adjacent `vmacros.sty`; it no longer depends on a personal Dropbox path.

## Source history

The Git history of this repository includes merge ancestry for the original `DN` repository at `edcdf5a`
and the `risk` repository at `04b2178`.
The `risk` history covers `Eisenberg-Noe/` in its original location.
The working copy of `clearing_research_note.tex` had an uncommitted edit,
and `clearing_research_note.pdf` was untracked; both were copied into this consolidation.
The original repositories remain available at `../DN` and `../risk` for comparison.
