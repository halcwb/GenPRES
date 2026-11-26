// Prototype for issue #1834: lay out the parenteralia view as clear preparation instructions.
//
// Prints the text of SolutionRule.Print.toMarkdown for generics of the sheet, to review it. The text is Dutch, with
// one fixed sequence of sections in the order of the work, after the monographs of the hospital parenteralia handbook: the title, the routes and the
// departments, the products, the reconstitution, the preparation and administration per patient category and
// access, the shelf life, the pH, the generics for the same line and the notes. A section without data is left
// out. The server keeps sending markdown, and the text stays Dutch.
//
// Usage, after dotnet run build, from this directory:
//     dotnet fsi Parenteralia.fsx meropenem ...   prints the text of each generic from the sheet
//     dotnet fsi Parenteralia.fsx --all           writes the text of every generic to one file

#I __SOURCE_DIRECTORY__
#load "load.fsx"
#r "../bin/Debug/net10.0/Informedica.GenFORM.Lib.dll"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

open Informedica.Utils.Lib
open Informedica.GenForm.Lib


/// Loads the solution rules of the sheet that GENPRES_URL_ID names, with the resource provider.
let loadRules () =
    Env.loadDotEnv () |> ignore
    let dataUrlId = Environment.GetEnvironmentVariable "GENPRES_URL_ID"
    let provider = Api.getCachedProviderWithDataUrlId FormLogging.noOp dataUrlId
    provider, Api.getSolutionRules provider


/// Returns the text of a generic, as the parenteralia view gets it, or the message of the exception.
let textOf provider rules generic =
    try
        rules
        |> Api.filterSolutionRules provider { Filter.solutionFilter with Generic = Some generic }
        |> SolutionRule.Print.toMarkdown ""
        |> Ok
    with e ->
        Error e.Message


/// Prints the text of each generic.
let printGenerics generics =
    let provider, rules = loadRules ()

    for g in generics do
        match g |> textOf provider rules with
        | Ok md -> printfn $"==================== %s{g}\n%s{md}"
        | Error msg -> printfn $"==================== %s{g}\nERROR: %s{msg}"


/// Writes the texts of every generic to one Markdown file in the temporary folder of the system, and prints the
/// generics whose text fails.
let writeAll () =
    let provider, rules = loadRules ()
    let results = rules |> SolutionRule.generics |> Array.map (fun g -> g, g |> textOf provider rules)
    let path = IO.Path.Combine(IO.Path.GetTempPath(), "Parenteralia.md")

    results
    |> Array.choose (fun (_, r) -> r |> Result.toOption)
    |> String.concat "\n\n---\n\n"
    |> fun md -> IO.File.WriteAllText(path, md)

    let errors = results |> Array.filter (snd >> Result.isError)
    printfn $"%i{results.Length} generics, %i{errors.Length} failed, the texts in %s{path}"
    errors |> Array.iter (fun (g, r) -> printfn $"%s{g}: %A{r}")


match fsi.CommandLineArgs |> Array.toList |> List.tail with
| [] -> ()
| [ "--all" ] -> writeAll ()
| generics -> printGenerics generics
