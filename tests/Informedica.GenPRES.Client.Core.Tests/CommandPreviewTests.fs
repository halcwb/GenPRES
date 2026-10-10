module Informedica.GenPRES.Client.Core.Tests.CommandPreviewTests

open Expecto
open Expecto.Flip
open Informedica.GenPRES.Shared.Types
open Informedica.GenPRES.Shared.Api


[<Tests>]
let tests =
    let twoGenerics =
        { Informedica.GenPRES.Shared.Models.OrderContext.empty with
            OrderContext.Filter.Generics = [| "ibuprofen"; "paracetamol" |]
        }

    testList
        "CommandPreview.shown"
        [
            test "a command that cannot change the context shows it as it is" {
                twoGenerics
                |> CommandPreview.shown (
                    OrderViewCommand.SetNthFilterProperty(Informedica.GenPRES.Shared.Models.OrderContext.Generic, 2)
                )
                |> Expect.equal "an index out of range" twoGenerics
            }
        ]
