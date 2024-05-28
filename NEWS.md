Keeper 0.1.0
============

Changes:

1. Change name from 'KEEPER' to 'Keeper', and `createKEEPER()` to `createKeeper()` for consistency with HADES.

2. Removed `vocabularyDatabaseSchema` argument of `createKeeper()`, which wasn't used, and the vocabulary should be in the CDM schema anyway.

3. Drop dependency on `stringr`.

4. Removed `exportFolder` argument from `createKeeper()`. The function now returns the Keeper output.

5. Keeper output column names now use camelCase in line with Hades recommendations.

6. Adding functions for converting Keeper output into prompts for large language models.
