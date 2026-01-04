namespace Informedica.GenForm.Lib

module SolutionLimit =

    open Informedica.GenCore.Lib.Ranges
    open Utils

    /// An empty SolutionLimit.
    let limit =
        {
            SolutionLimitTarget = NoLimitTarget
            Quantity = MinMax.empty
            QuantityAdj = MinMax.empty
            Quantities = None
            Concentration = MinMax.empty
            Products = [||]
        }


    let minMaxToString (minMax : MinMax) =
        if minMax = MinMax.empty then ""
        else
            minMax
            |> MinMax.toString
                "min "
                "min "
                "max "
                "max "


    let toString (sl: SolutionLimit) =
        [
            sl.Quantity |> minMaxToString
            sl.QuantityAdj |> minMaxToString
            sl.Concentration |> minMaxToString
        ]
