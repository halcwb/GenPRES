# Implementation plan for issue #2006: read the missing columns of the SolutionRules sheet into the solution rule

2026-11-24.

## Problem description

The SolutionRules sheet has fifteen columns that `SolutionRule.parseSolutionRuleData`
(`src/Informedica.GenFORM.Lib/SolutionRule.fs`) does not read: Id, RowId, RowNo, RuleId, GPKs, HPKs, SourceText, ShelfLife,
MinPh, MaxPh, CombY, NonCombY, Notes, PrepText and AdminText. The parser reads the columns by name, so a workbook with these
columns loads, but what they hold is lost. A solution rule also has no id to name it by, where a dose rule has
one. Issue #2006 gives the meaning of each column.

## Approaches considered

1. Leave the columns unread until a feature uses them. The data stays in the sheet only.
2. Read the columns into `SolutionRuleData` only, and leave `SolutionRule` as it is. The values are then
   in the rows, and every user of a solution rule has to go back to the rows for them.
3. Read the columns into `SolutionRuleData`, give `SolutionRule` the values of the rule, and calculate the ids
   of a solution rule at load, as for a dose rule.

For the ids of approach 3:

- a. Read the ids from the sheet. The sheet then has to be kept in step with the rules by hand.
- b. Calculate the ids from the fields that identify the rule, as `DoseRuleData.setDataHashIds` does for a dose
  rule. The columns of the sheet are a place to write the ids to, not a source.

## Chosen approach

Approach 3 with b, with the decisions of the maintainer in the issue (2026-11-24):

- **The ids are hashes**, calculated at load, as for a dose rule.
- **RuleId** is a hash of every field that is the same for all rows of a rule, except SourceText: Generic,
  Form, Route, Indication, GPKs, HPKs, DoseType, DoseText, Loc, Dep, CVL, PVL, the age, weight and gestational age
  limits, MinDose, MaxDose, Diluents, AdjustUnit, Div, Volumes, the limits of the solution, ShelfLife, MinPh,
  MaxPh, CombY, NonCombY, Notes, PrepText and AdminText. SourceText is the text the rule is a translation of, so it does not
  identify the rule.
- **RowId** is a hash of the fields of the RuleId with the dilution target of the row: the substance when the
  row names one, otherwise the component, in lower case. That is the target an order looks up a dilution limit
  by. The diluent rows are expanded to their diluents first, so a diluent row and a row for one of its diluents
  have the same target. The dilution limits of the row are left out, so two rows of a rule with the same target
  have the same RowId. `SolutionRule.dedupRowsByRowId` keeps the first of them with a dilution limit, so a rule
  has one dilution limit for each target, and the load gives a warning when their dilution limits differ.
- **Id** is a hash of every field of the row except the ids and SourceText, the dilution limits included, in
  the order of the columns of the sheet. `SolutionRule.setDataId` calculates it before the diluent rows are
  expanded, so it identifies the row of the sheet.
- **Rows with one RuleId and different source texts are invalid.** They stay one rule, with the source text of
  its first row, and the load gives a warning.
- **The ids are of the type `HashId`**, a single-case union with a private case, for the rows of both sheets.
  `HashId.ofFields` hashes fields; `HashId.create` cleans a cell of a sheet: it trims the text, removes a leading
  apostrophe, with which a sheet marks a value as text, and makes it lower case. A text that is then not 12
  hexadecimal characters gives the empty HashId, because a sheet can turn a hash such as 0687555379e8 into a number.
  `DoseRule.DataId`, `DoseRule.GroupId` and `SolutionRule.Id` stay text and take `HashId.toString`.
- **A `SolutionRule` has an `Id` and no row number.** Only a `SolutionRuleData` row has a RowNo.
- **RowNo** of a row is the place of the row in its rule, counted from 0 in the order of the rows.
- **MinPh and MaxPh** are decimals with the unit of measure `pH`, a measure of `Informedica.GenCore.Lib.Measures`.
- **ShelfLife** is in minutes.
- **HPKs** holds trade products, not generic products: a GPK cannot tell two trade products with the same
  strength apart.
- **GPKs and HPKs narrow the products of the generic of the rule.** A component row of the generic attaches
  only the products with one of the GPKs, and of those only the trade products with one of the HPKs. Empty
  GPKs or HPKs keep every product. The diluents and the other components keep their products. HPKs that are
  not trade products of the GPKs are a warning of the load.
- **CombY and NonCombY** hold generics GenPRES knows: a known generic is a `Generic` of a Formulary product. A
  name that matches no known generic is a warning of the load.
- **No backward compatibility:** every one of the fifteen columns is required.
- **Nothing uses the new fields yet.**

What the code offers, checked on 2026-11-24:

- `String.sha1Short` hashes a list of fields without regard to case and repeated spaces, as the ids of a dose
  rule are hashed.
- `SolutionRule.map` groups the rows by the rule it builds from each row, so a value of the rule that differs
  between two rows gives two rules. The data record says so for Diluents and AdjustUnit.
- The warnings of the load map each row on its own (`SolutionRule.ruleOfRow`) and compare the rules, so a value
  that is different for every row of a rule cannot be on the rule a single row gives.
- `SolutionRule.unmarkedDiluents` compares the names of the Diluents column with the `Generic` of the Formulary
  products, without regard to case.

## Confidence

High for reading the columns and for the ids: the fields are decided, and the tests change every field of the
rule and of the row one at a time.

## Steps

### Step 1: read the columns and calculate the ids

One pull request.

- `SolutionRuleData` (`src/Informedica.GenFORM.Lib/Types.fs`) gets Id, RowId, RowNo, RuleId, GPKs, HPKs, SourceText,
  ShelfLife, MinPh, MaxPh, CombY, NonCombY, Notes, PrepText and AdminText, each with its `///` comment.
- `SolutionRule.parseSolutionRuleData` reads the fifteen columns.
- `SolutionRule` gets `Id`, `SourceText`, `GPKs`, `HPKs`, `ShelfLife` (a value in minutes), `MinPh`, `MaxPh`,
  `SameLine`, `NotSameLine`, `Notes`, `PrepText` and `AdminText`.
- `Measures` (`src/Informedica.GenCORE.Lib/Measures.fs`) gets the measure `pH`.
- `SolutionRule.ruleKeyFields` and `SolutionRule.rowKeyFields` return the fields of the two hashes.
  `SolutionRule.setDataHashIds` sets the RuleId and the RowId of each row from them and numbers the rows of
  each rule. `SolutionRule.map` calls it first and gives a rule the RuleId of its rows as its `Id`.
- `SolutionRule.mixedAdjustUnits` groups the rows without the id, because the id is a hash of the adjust unit
  too.
- `SolutionRule.mixedSourceTexts` gives one warning per rule whose rows give different source texts. The rule
  checks of the resources (`Keys.ruleChecks`) report it.
- `SolutionRule.dedupedRows` expands the diluent rows, sets the ids and calls `SolutionRule.dedupRowsByRowId`,
  which keeps the first row with a dilution limit of each RowId; `SolutionRule.map` starts with it. `SolutionRule.duplicateRows` gives one warning per RowId whose rows have different dilution
  limits, through `Keys.ruleChecks`.
- `SolutionRule.headers` and `SolutionRule.dataToCsv` write solution rule rows as TSV in the column order of the
  sheet, as `DoseRuleData.headers` and `DoseRuleData.dataToCsv` do for dose rule rows. `Export.exportSolutionData`
  sets the ids and the row numbers and puts the rows of a rule together; `Export.writeSolutionExport` writes the
  file. The parser reads an empty Diluents cell as no diluents, so an export parses back to the same rows.
  The script `src/Informedica.GenFORM.Lib/Scripts/ExportSolutionRules.fsx` exports the sheet twice and checks
  that the second export equals the first.
- `SolutionRule.narrowProducts` keeps the products with one of the GPKs and the trade products with one of the
  HPKs; `SolutionRule.map` applies it to the component rows of the generic. `SolutionRule.hpksOutsideGpks`
  gives one warning per rule with HPKs that are not trade products of its GPKs, through `Keys.ruleChecks`.
- `SolutionRule.unknownLineGenerics` gives one warning per generic with a name in CombY or NonCombY that the
  Formulary does not have. The rule checks of the resources (`Keys.ruleChecks`) report it.
- Tests in `tests/Informedica.GenFORM.Tests`: the fifteen columns in the column-contract test; the cells of a
  row; the values on the rule; a test for each field of the rule and of the row; the sort numbers; the
  warning.
- `docs/domain/genform-free-text-to-operational-rules.md`: the new columns in the solution rule table.
- Estimate: 150 to 180 lines.

## Issues that touch the steps

- #1953 asks for the same columns of the SolutionRules sheet, and for ShelfLife of the Reconstitution sheet.
  After this step only the Reconstitution column is left of it.
- #1350 and #1834, the preparation instructions: they can show the shelf life, the pH and the administration
  instructions.
