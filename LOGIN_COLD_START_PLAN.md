# Login Cold Start — Plan

## Problem
After entering credentials, users wait ~6 seconds on the same page before the dashboard loads.
`server.R` sources 30+ files synchronously inside `observeEvent(USER$logged_in, {...})`.

## Root Cause
Heavy files sourced at login:
- `deep_learning.R` (50KB)
- `transform_data.R` (52KB)
- `compare_trained_caret_models.R` (52KB)
- `user_defined_visualization.R` (43KB)

## Proposed Fix
1. Show animated progress overlay with status messages during login
2. Lazy-load heavy modules — defer sourcing until user navigates to that tab
3. Split into "essential" (sourced at login) vs "deferred" (sourced on tab visit)

## Status
Planning only — implementation to follow in a future sprint.
