/// Builds page text from translated terms.
module Informedica.GenPRES.Client.Core.Helpers.TermText

open Informedica.GenPRES.Shared


/// Fills {0}, {1}, ... in a translated sentence with the given values, in order.
let fill (args: string list) (s: string) =
    args
    |> List.indexed
    |> List.fold (fun s (i, a) -> s |> String.replace $"{{{i}}}" a) s


/// Joins translated sentences with a space, skipping empty ones.
let sentences (xs: string list) = xs |> List.filter String.notEmpty |> String.concat " "
