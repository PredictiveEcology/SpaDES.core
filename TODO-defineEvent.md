# TODO: `defineEvent()` — replace `doEvent.X` switch dispatch

A proposed refactor of how a module declares its event handlers.
Replaces the `doEvent.<moduleName> <- function(sim, eventTime, eventType) switch(eventType, init = {...}, ...)`
pattern with a series of explicit registrations.

Captured 2026-06-15 from a design discussion. The goal is a small,
additive, backward-compatible change to SpaDES.core's user-facing
module-writing API that fixes several long-standing pain points
without altering the runtime, the simList, or any existing module.

## Motivation: the pain points it addresses

1. **String dispatch is typo-prone.** `scheduleEvent(sim, t, "M", "simLayres")` (typo)
   silently slides past the missing `switch()` case. The user discovers it by
   noticing `completed(results)` is shorter than expected, long after the run.
2. **Switch branches aren't testable in isolation.** You can't call a single
   event without going through `spades()`'s full event loop, because the
   branch's code lives inside an anonymous switch arm rather than a function.
3. **No per-event introspection.** `body(doEvent.X)` + manual switch traversal
   is the only way to discover "what events does module X support?".
4. **Per-event metadata leaks into other places.** Priority is set on every
   `scheduleEvent()` call; seed lives in `P(sim)$.seed$<event>` parameters.
   There's no single home for "this event's properties".

## The proposed API

```r
defineModule(sim, list(
  name = "RSFpredict",
  reqdPkgs = c("reproducible", "terra", "glmmTMB"),
  parameters = list(...),
  inputObjects = list(...),
  outputObjects = list(...)
))

defineEvent("init", function(sim) {
  sim <- scheduleEvent(sim, time(sim), "RSFpredict", "buildBaselineRSFmap")
  if (P(sim)$simulationProcess == "dynamic") {
    sim <- scheduleEvent(sim, P(sim)$predictStartYear, "RSFpredict", "simLayers")
  }
  invisible(sim)
})

defineEvent("buildBaselineRSFmap", function(sim) {
  sim$pred <- terra::predict(sim$modelLand, sim$model,
                             type = "response", re.form = NA)
  # ... etc
  invisible(sim)
})

defineEvent("simLayers", function(sim) {
  thisYear <- as.integer(time(sim))
  # ... layer building ...
  sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval,
                       "RSFpredict", "simLayers")
  invisible(sim)
})

defineEvent("simRSFmap", function(sim) { ... })
```

The framework synthesises the old
`doEvent.RSFpredict <- function(sim, eventTime, eventType) switch(eventType, ...)`
at module-load time from the registered events. The scheduler, `spades()`, and
every existing piece of runtime machinery see exactly what they always saw.
Zero change to the runtime.

### Optional per-event metadata

`defineEvent()` may also take metadata that today is scattered through
`scheduleEvent()` call sites and parameters:

```r
defineEvent("simLayers",
  priority = .normal,
  seed     = 42,        # was P(sim)$.seed$simLayers, hidden in parameters
  handler  = function(sim) { ... }
)
```

Opt-in only when the metadata is useful for that event.

## What the module writer notices

### Wins

1. **Typo-catching at the right level.** With registered events,
   ```r
   sim <- scheduleEvent(sim, time(sim), "RSFpredict", "simLayres")
   ```
   fails at scheduling time with
   `event 'simLayres' is not defined in module 'RSFpredict'; did you mean: simLayers, simRSFmap?`
   instead of silently no-oping.

2. **Per-event introspection.**
   ```r
   eventList("RSFpredict")
   #> c("init", "buildBaselineRSFmap", "simLayers", "simRSFmap")
   eventHandler("RSFpredict", "simLayers")
   #> the function, callable in isolation
   ```

3. **Testability.** Each event is now a real function:
   ```r
   testthat::test_that("simLayers builds correctly", {
     sim  <- mockSim(year = 2025, inputs = ...)
     sim2 <- eventHandler("RSFpredict", "simLayers")(sim)
     expect_named(sim2$simLand, "year2025")
   })
   ```

4. **Per-event metadata where it belongs** (see Optional section above).

### Friction

One extra `function(sim) {` line per event vs. `init = {`. ~5 characters. Cheap.

## Module scoping

The implicit "current module" is the cleanest API. `defineModule()` already
uses this hidden-state pattern (it pulls `sim` from the calling frame), so
the convention is established:

```r
defineModule(sim, list(name = "RSFpredict", ...))   # sets the current-module binding
defineEvent("init", function(sim) { ... })          # reads it
```

Module files are 1-module so the binding never gets confused.

Escape hatch for the rare 2-modules-per-file case:

```r
defineEvent("RSFpredict", "init", function(sim) { ... })
```

## Backward compatibility

Modules using the old `doEvent.X <- function(sim, ...) switch(...)` keep working
unchanged. The framework's event lookup becomes a two-step fallback:

```
1. Is there a registered handler for (moduleName, eventType)?  -> use it
2. Otherwise, fall through to doEvent.<moduleName>             -> existing behaviour
```

Migration is mechanical — every `switch(eventType, init = { body }, simLayers = { body })`
becomes a series of `defineEvent("init", function(sim) { body })` calls. AI-suitable.

## Implementation sketch

Three pieces, all small and localised:

1. **Registry env, scoped per module.** A package-internal env keyed by
   module name, each value an env keyed by event name -> handler function.

2. **`defineEvent()` writer.** Resolves the current module name (same way
   `defineModule()` does), writes the handler into the registry, and
   optionally attaches metadata as attributes on the function or in a
   parallel metadata env.

3. **One-line addition to the scheduler.** Wherever `do.call(doEvent.<mod>, ...)`
   is called today, first check the registry: if a registered handler exists
   for `(mod, eventType)`, call it; otherwise fall through to the old dispatch.
   Optionally synthesise `doEvent.<mod>` from the registry once at module-load
   time so the rest of the codebase doesn't have to know about the registry.

4. **Optional helpers**: `eventList(moduleName)`, `eventHandler(mod, event)`,
   improved error message in `scheduleEvent()` that suggests close matches
   via `agrep()` against `eventList(moduleName)`.

## Impact summary

This single change addresses 3 of the 4 long-standing module-API pain points
(from a separate discussion):

| Pain point                          | Addressed? |
|-------------------------------------|------------|
| String dispatch, typo-prone         | yes |
| Switch with silent default          | yes |
| Untestable individual events        | yes |
| `sim <-` reassignment ceremony      | no (orthogonal; separate proposal) |

It also lays a clean foundation if a future env-storage refactor wants to
drop the `sim <-` ceremony too: doesn't preclude it, doesn't depend on it.

## Next steps

- Sketch the registry + writer in a feature branch.
- Convert one existing module (e.g. `Biomass_speciesData`, small surface) as a
  proof-of-concept.
- Confirm `spades()`'s event loop accepts the synthesised `doEvent.X` without
  any other changes.
- If smooth: add `defineEvent()` to the `newModule()` template so new modules
  use the new form by default.
- Document the migration recipe in the SpaDES.core README / vignette.
