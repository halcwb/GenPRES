// Exports the rows of the DoseRules sheet, after the validation of GenFORM, as a TSV file with the columns of the
// sheet. The rows get their ids, their GrpNo and RowNo, and the G-Standaard check in FreqCheck and DoseCheck.
// A second pass exports the first export again and checks that the second export equals the first exactly, so the
// export is a fixed point: loading an export and exporting it again changes nothing.
//
// Issue #2006, implementation plan docs/implementation-plans/2006-solution-rule-columns.md: fill the ids and the
// numbers of the dose rule rows.
//
// The script runs in production mode: GenFORM reads the ZIndex cache files and checks against the whole
// G-Standaard instead of the demo files. The sheet is the one GENPRES_URL_ID names; a GENPRES_URL_ID set in the
// shell takes precedence over the one in the .env file.
//
// Usage, after dotnet run build, from this directory:
//     dotnet fsi ExportDoseRules.fsx

#I __SOURCE_DIRECTORY__
#load "load.fsx"
#r "../bin/Debug/net10.0/Informedica.GenFORM.Lib.dll"

open System
open System.IO
open Informedica.Utils.Lib
open Informedica.Utils.Lib.BCL
open Informedica.GenForm.Lib


// Production mode is set before the first use of ZIndex, which reads the setting when it loads the cache files.
Environment.SetEnvironmentVariable("GENPRES_PROD", "1")
Env.loadDotEnv () |> ignore


let dataUrlId = Environment.GetEnvironmentVariable "GENPRES_URL_ID"


/// The file of the first export, in the temporary folder of the system.
let exportPath = Path.Combine(Path.GetTempPath(), "DoseRules.tsv")


/// The file of the second export, made from the first export, in the temporary folder of the system.
let secondExportPath = Path.Combine(Path.GetTempPath(), "DoseRules2.tsv")


let provider = Api.getCachedProviderWithDataUrlId FormLogging.noOp dataUrlId


/// Exports rows and writes them as TSV to the file at path. Returns the lines of the file.
let export path data =
    let lines = data |> Export.exportData provider |> DoseRuleData.dataToCsv
    File.WriteAllText(path, lines |> String.concat "\n")
    lines


/// Parses the lines of an export into rows, with the same parser as the sheet.
let parseExport (lines: string list) =
    lines
    |> List.map (String.split "\t" >> List.toArray)
    |> List.toArray
    |> DoseRuleData.parseDoseRuleData


/// Prints the differences between the lines of the first and the second export: the number of lines, the lines of
/// one export only, and for each column the number of rows with the same RowId but a different cell, with up to
/// three examples.
let printDifferences (first: string list) (second: string list) =
    let header = first |> List.head |> String.split "\t" |> List.toArray
    let rows lines = lines |> List.tail |> List.map (String.split "\t" >> List.toArray)
    let firstRows, secondRows = rows first, rows second
    let onlyIn xs ys = xs |> List.except ys |> List.length

    printfn $"lines: %i{first.Length} in the first export, %i{second.Length} in the second"
    printfn $"lines only in the first export: %i{onlyIn first second}"
    printfn $"lines only in the second export: %i{onlyIn second first}"

    let secondById = secondRows |> List.map (fun r -> r[0], r) |> Map.ofList
    let pairs = firstRows |> List.choose (fun r -> secondById |> Map.tryFind r[0] |> Option.map (fun s -> r, s))
    printfn $"rows with a RowId in both exports: %i{pairs.Length}"

    for i, column in header |> Array.indexed do
        let differing = pairs |> List.filter (fun (r, s) -> r[i] <> s[i])

        if differing |> List.isEmpty |> not then
            printfn $"column %s{column}: %i{differing.Length} rows differ"

            for r, s in differing |> List.truncate 3 do
                printfn $"    %s{r[0]}: '%s{r[i]}' becomes '%s{s[i]}'"


match DoseRuleLoader.getData dataUrlId with
| Error msgs -> printfn $"The DoseRules sheet does not load: %A{msgs}"
| Ok data ->
    let first = data |> export exportPath
    printfn $"First pass: read %i{data.Length} rows, exported %i{first.Length - 1} rows to %s{exportPath}"

    match first |> parseExport with
    | Error msgs -> printfn $"The first export does not parse: %A{msgs}"
    | Ok exported ->
        let second = exported |> export secondExportPath

        printfn
            $"Second pass: read %i{exported.Length} rows, exported %i{second.Length - 1} rows to %s{secondExportPath}"

        if second = first then
            printfn "The second export equals the first exactly."
        else
            printfn "The second export differs from the first."
            printDifferences first second
