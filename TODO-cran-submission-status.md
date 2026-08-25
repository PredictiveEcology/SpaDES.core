# SpaDES.core 3.2.0 — CRAN submission status

Handoff note. Written 2026-08-25. Named `TODO-*` so `.Rbuildignore`'s
`^TODO.*\.md$` keeps it out of the tarball.

Follow the `/cran-submission` skill (`~/.claude/skills/cran-submission/`);
`checklist.md` there is authoritative. This records where that checklist stands.

## Context

SpaDES.core was archived from CRAN on 2026-07-13 **solely because `reproducible`
was** — CRAN's own comment reads *"as requires archived package 'reproducible'"*.
Nothing was wrong with SpaDES.core itself.

`reproducible` 3.2.0 was accepted on **2026-08-25** and is back on CRAN:
<https://cran.r-project.org/web/packages/reproducible/index.html>, so the
blocker is gone.

⚠️ **`reproducible` 3.2.1 is in flight.** 3.2.0 shipped a check ERROR on CRAN's
two Fedora flavours (a `system("apt ...", intern = TRUE)` call that errors where
`apt` does not exist). The fix is committed and green on `development` in the
reproducible repo but **not yet submitted**. It does not block SpaDES.core.

## State of this repo

Branch `development`, last commit `ec079809` (7 days old).
`DESCRIPTION`: **Version 3.2.0, Date 2026-08-24**.

There is **substantial uncommitted release prep**, written 2026-08-24 16:12–16:15.
It has been reviewed — findings below — and is sound. It is **not committed**.

| file | change | verdict |
|---|---|---|
| `DESCRIPTION` | `3.1.2.9020` → `3.2.0`, date, roxygen 8.0.0 → 8.1.0 | correct |
| `NAMESPACE` | 296 lines churn | **semantically identical** — verified 498 entries, 209 exports, 135 methods, zero lost or added. Pure roxygen 8.1.0 reformatting of `importFrom` |
| `R/reexports.R` | drops the `.updateTagsRepo` top-level re-export | safe — still used at `R/simulation-spades.R:2657` via its own local `getFromNamespace()` |
| `.Rbuildignore` | `+^TODO.*\.md$` | pairs with the untracked `TODO-defineEvent.md` |
| `inst/WORDLIST` | +28 | the spell-check step |
| `NEWS.md` | −250 lines, 3.2.0 section | condensed |
| `cran-comments.md` | rewritten, **plus my edits** (below) | see below |
| untracked `man/dot-restart*.Rd` ×4 | roxygen output for `R/restart.R` | expected |
| untracked `TODO-defineEvent.md` | design note | build-ignored |

Also verified: all 21 active `getFromNamespace()` re-exports resolve (against
`reproducible`, `quickPlot`, `Require`), and the package **loads cleanly against
reproducible 3.2.0**, which is installed in the personal library from the
`v3.2.0` tag.

### Edits I made to `cran-comments.md` (uncommitted, on top of the above)

1. Opening now states `reproducible` **is** back on CRAN as of 2026-08-25 with
   the link, instead of "should be processed after it is back on CRAN".
2. "R CMD check results" rewritten — it claimed *"no ERRORs, WARNINGs, or
   NOTEs"*, which the local check disproves. It now describes the NOTE.

## Local `R CMD check --as-cran`: 1 NOTE

Run 2026-08-25 against the working tree (uncommitted prep included). Tests OK.
The single NOTE has five parts:

* `New submission` and `Package was archived on CRAN` — expected.
* **`Unknown, possibly misspelled, fields in DESCRIPTION: 'Remotes'`** — must be
  removed before submitting; see below.
* **`Suggests or Enhances not in mainstream repositories: SpaDES.tools`** — the
  open sequencing question; see below.
* Two possibly-invalid URLs (`man/tryCatch.Rd`, `man/getModuleVersion.Rd`), both
  Stack Overflow returning 403 to automated requests. Same false positive
  `reproducible` carries; documented in `cran-comments.md` rather than worked
  around. **Do not "fix" these by de-linking** — that evades the check rather
  than addressing it, and was explicitly rejected for `reproducible`.

## Decisions already taken

* **`reproducible (>= 3.0.0)` stays as is.** Do not bump to `>= 3.2.0`. Nothing
  in 3.2.0 was backwards-incompatible, and tightening it would exclude users on
  3.0.x/3.1.x for no benefit. (Maintainer's call, 2026-08-25.)
* **`Remotes` must be removed — but on `main`, not here.** Checklist step 11
  strips it *after* step 10's merge. Removing it on `development` would break
  dev installs of the whole ecosystem, since those three entries point at the
  `@development` branches.

## Open question — submission order

SpaDES.core `Suggests: SpaDES.tools (>= 2.1.1)`, and **SpaDES.tools is still
archived** (2026-07-13, same cause). That produces the "not in mainstream
repositories" NOTE.

Dependency order is **reproducible → SpaDES.tools → SpaDES.core → SpaDES**.

* Submitting SpaDES.tools first makes the NOTE disappear.
* Submitting SpaDES.core now means explaining the NOTE to a reviewer.

SpaDES.tools is at `2.1.2` on `development` with a **clean working tree**, so it
may be closer to ready. **This has not been decided.**

## Next steps

1. Decide the order above.
2. Commit the reviewed prep (it is all sound; nothing is known to be wrong).
3. Checklist steps 1–3: local `--as-cran` (clean apart from the NOTE), CI green,
   then win-builder ×3. Mac builder is **dead** — `submit.html` serves 200 but
   `/macbuilder/v1/submit` returns 502 on every attempt. R-hub was skipped for
   `reproducible` (pure-R package, the matrix covers more) and there is no
   `RHUB_TOKEN` configured.
4. Step 8 revdepcheck: `revdep/` exists so it applies, but the only reverse
   dependency (`SpaDES`) is itself archived — likely a no-op, say so rather than
   skipping silently.
5. Step 10: merge `development` → `main`. **`main` is a protected branch** in
   these repos — force-push is refused, so merge, never rewrite.
6. Step 11: remove `Remotes` on `main`.
7. Step 12: `devtools::submit_cran()` — `devtools::release()` is deprecated.
   **Then click CRAN's confirmation email**; the submission is not queued until
   you do.
8. On acceptance: `CRAN-SUBMISSION` commit, tag `vX.Y.Z`, GitHub release, then
   "Begin X.Y.Z.9000 development cycle" on `development`. Mirror what
   `reproducible` did at `06144e57` / `a4c722ef`.

## Gotchas worth carrying over

* **`gh` is a snap here.** It cannot read files under `/tmp` *or* under hidden
  directories in `$HOME`. Use `--body "$(cat …)"` or stage in a non-hidden dir.
* **win-builder**: FTP is broken; use the HTTP form. Slot mapping is *not* in
  visible order — `Button1` = release, `Button2` = devel, `Button3` = oldrelease.
* **The version hook auto-bumps `DESCRIPTION`.** Commit release version changes
  with `--no-verify` and then verify what actually landed.
* **CITATION.cff bot** pushes to `main` after merges; expect a non-fast-forward
  and merge `development` → `main` rather than forcing.
