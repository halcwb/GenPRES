
#time

// load demo or product cache


System.Environment.SetEnvironmentVariable("GENPRES_DEBUG", "1")
System.Environment.SetEnvironmentVariable("GENPRES_PROD", "1")
System.Environment.SetEnvironmentVariable("GENPRES_URL_ID", "1JHOrasAZ_2fcVApYpt1qT2lZBsqrAxN-9SvBisXkbsM")

#load "load.fsx"

open MathNet.Numerics
open Informedica.Utils.Lib.BCL
open Informedica.GenCore.Lib.Ranges
open Informedica.GenForm.Lib
open Informedica.GenUnits.Lib
open Informedica.GenOrder.Lib



module Types =


    /// Commands that can be executed on an order for calculations
    type OrderCommand =
        | CalcMinMax of Order
        | CalcValues of Order
        | ReCalcValues of Order
        | SolveOrder of Order
        | ChangeProperty of Order * ChangePropertyCommand

    and ChangePropertyCommand =
        | DecreaseFrequency
        | IncreaseFrequency
        | DecreaseDoseQuantity of cmp: string * ntimes: int
        | IncreaseDoseQuantity of cmp: string * ntimes: int
        | DecreaseDoseRate of ntimes: int
        | IncreaseDoseRate of ntimes: int


module OrderProcessor =

    open Types
    open Order
    open Informedica.GenOrder.Lib.OrderProcessor

    module Name = Informedica.GenSolver.Lib.Variable.Name
    module Quantity = OrderVariable.Quantity
    module Time = OrderVariable.Time
    module Frequency = OrderVariable.Frequency
    module Dose = Orderable.Dose




    let orderPropertyIncrOrDecrDoseRate step ord =
        ord
        |> OrderPropertyChange.proc
            [
                OrderableDose Dose.setRateAdjustToNonZeroPositive
                // clear all dependent dose rates
                ComponentDose ("", Dose.setRateToNonZeroPositive)
                ItemDose ("", "", Dose.setRateToNonZeroPositive)
                // clear time
                ScheduleTime Time.setToNonZeroPositive
            ]
        |> OrderPropertyChange.proc
            [
                // apply dose rate constraints again
                OrderableDose step
            ]



    let orderPropertyIncrOrDecrDoseQuantity step cmp ord =
        ord
        |> OrderPropertyChange.proc
            [
                if ord.Schedule |> Schedule.hasTime then
                    ScheduleTime OrderVariable.Time.setToNonZeroPositive
                    OrderableDose Dose.setRateToNonZeroPositive

                // only clear other components than the one being changed
                for c in ord.Orderable.Components do
                    if c.Name |> Name.toString <> cmp then
                        ComponentDose (c.Name |> Name.toString, Dose.setQuantityToNonZeroPositive)
                    else
                        ComponentDose (cmp, Dose.setQuantityAdjustToNonZeroPositive)
                        ComponentDose (cmp, Dose.setPerTimeToNonZeroPositive)
                
                OrderableDose Dose.setQuantityToNonZeroPositive
                ItemDose ("", "", Dose.setQuantityToNonZeroPositive)

                OrderableDose Dose.setPerTimeToNonZeroPositive
                ComponentDose ("", Dose.setPerTimeToNonZeroPositive)
                ItemDose ("", "", Dose.setPerTimeToNonZeroPositive)

                OrderableQuantity Quantity.setToNonZeroPositive
                OrderableDoseCount OrderVariable.Count.setToNonZeroPositive
                ComponentOrderableCount ("", OrderVariable.Count.setToNonZeroPositive)
                ComponentOrderableQuantity ("", Quantity.setToNonZeroPositive)
                ItemOrderableQuantity ("", "", Quantity.setToNonZeroPositive)
            ]
        |> OrderPropertyChange.proc
            [
                OrderableQuantity Quantity.applyConstraints
                ComponentOrderableQuantity ("", Quantity.applyConstraints)
                ItemOrderableQuantity ("", "", Quantity.applyConstraints)
                OrderableDoseCount OrderVariable.Count.applyConstraints

                if ord.Schedule |> Schedule.hasTime then
                    ScheduleTime Time.applyConstraints
                    OrderableDose Dose.setStandardRateConstraints

                ComponentDose (cmp, step)
            ]


    let orderPropertyIncrOrDecrFrequency step ord =
        ord
        |> OrderPropertyChange.proc
            [
                OrderableDose Dose.setPerTimeToNonZeroPositive
                ComponentDose ("", Dose.setPerTimeToNonZeroPositive)
                ItemDose ("", "", Dose.setPerTimeToNonZeroPositive)
            ]
        |> OrderPropertyChange.proc
            [
                ScheduleFrequency step
            ]


    let processChangeProperty cmd ord =
        match cmd with
        | DecreaseFrequency -> ord |> orderPropertyIncrOrDecrFrequency Frequency.decrease
        | IncreaseFrequency -> ord |> orderPropertyIncrOrDecrFrequency Frequency.increase
        | DecreaseDoseQuantity (cmp, n) -> ord |> orderPropertyIncrOrDecrDoseQuantity (Dose.decreaseQuantity n) cmp
        | IncreaseDoseQuantity (cmp, n) -> ord |> orderPropertyIncrOrDecrDoseQuantity (Dose.increaseQuantity n) cmp
        | DecreaseDoseRate n -> ord |> orderPropertyIncrOrDecrDoseRate (Dose.decreaseRate n)
        | IncreaseDoseRate n -> ord |> orderPropertyIncrOrDecrDoseRate (Dose.increaseRate n)


    /// <summary>
    /// Process an order through a pipeline of steps
    /// depending on the command given
    /// </summary>
    /// <param name="logger">The logger</param>
    /// <param name="normDose">An optional norm dose adjustment</param>
    /// <param name="cmd">The command to process</param>
    /// <returns>A Result with the processed Order or a list of error messages</returns>
    let processPipeline logger normDose cmd =

        let runStep (step: Step) (ord: Order) =
            if step.Guard (classify ord) then
                $"\n=== PIPELINE STEP {step.Name} ===\n"
                |> Events.OrderScenario
                |> Logging.logInfo logger

                ord |> stringTable |> Events.OrderScenario |> Logging.logInfo logger
                step.Run ord
                |> function
                | Ok ord ->
                    ord |> stringTable |> Events.OrderScenario |> Logging.logInfo logger
                    Ok ord
                | Error (ord, msgs) ->
                    $"Error in {step.Name}"
                    |> Events.OrderScenario
                    |> Logging.logInfo logger

                    Error (ord, msgs)
            else Ok ord

        let runPipeline (ord: Order) (steps: Step list) =
            (Ok ord, steps)
            ||> List.fold (fun acc step -> acc |> Result.bind (runStep step))

        // Helper guards matching legacy active-pattern logic
        // NoValues is defined as: not empty, no values, and not solved
        let isNoValues (s: OrderState) = not s.IsEmpty && not s.HasValues && not s.DoseIsSolved

        // Core step functions
        let calcMinMaxStep increaseIncrement ord =
            match calcMinMax logger normDose increaseIncrement ord with
            | Ok o -> Ok o
            | Error (o, errs) ->
                o |> stringTable |> Events.OrderScenario |> Logging.logInfo logger
                Error (o, errs)

        let calcValuesStep ord = ord |> minIncrMaxToValues false true logger |> Ok

        let solveStep ord = solveOrder true logger ord

        let processClearedStep ord =
            match processClearedOrder logger ord with
            | Ok o -> Ok o
            | Error _ -> solveOrder true logger ord

        let applyConstraintsStep ord = ord |> applyConstraints |> Ok

        let processChangeProperty cmd ord = ord |> processChangeProperty cmd |> Ok

        match cmd with
        | CalcMinMax ord ->
            [ { Name = "calc-minmax: calc-minmax"; Guard = (fun s -> s.IsEmpty); Run = calcMinMaxStep true } ]
            |> runPipeline ord
        | CalcValues ord ->
            // Legacy behavior: only when NoValues (not for empty orders)
            [ { Name = "calc-values: calc-values"; Guard = isNoValues; Run = calcValuesStep } ]
            |> runPipeline ord
        | SolveOrder ord ->
            // Legacy behavior: do NOT run min/max here; use values-only flow
            [
                { Name = "solve-order: ensure-values-1"; Guard = isNoValues; Run = calcValuesStep };
                { Name = "solve-order: solve-1"; Guard = (fun s -> s.HasValues); Run = solveStep };
                { Name = "solve-order: process-cleared"; Guard = (fun s -> s.DoseIsSolved && s.IsCleared); Run = processClearedStep };
                { Name = "solve-order: final-solve"; Guard = (fun s -> s.OrderIsSolved |> not); Run = solveStep }
            ]
            |> runPipeline ord
        | ReCalcValues ord ->
            [
                { Name = "recalc-values: apply-constraints"; Guard = (fun _ -> true); Run = applyConstraintsStep };
                { Name = "recalc-values: calc-minmax"; Guard = (fun _ -> true); Run = calcMinMaxStep false };
                { Name = "recalc-values: calc-values"; Guard = (fun _ -> true); Run = calcValuesStep }
            ]
            |> runPipeline ord

        | ChangeProperty (ord, cmd) -> //ord |> processChangeProperty cmd |> Ok
            [
                { Name = "change-property: process-change"; Guard = (fun _ -> true); Run = processChangeProperty cmd }
                { Name = "change-property: solve-order"; Guard = (fun _ -> true); Run = solveStep }
            ]
            |> runPipeline ord


module HelperFunctions =

    let print sl = sl |> List.iter (printfn "%s")


    let inline printOrderTable order =
        order
        |> Result.iter (Order.printTable ConsoleTables.Format.Minimal)
        
        order


    let solveOrder order = 
        match order with
        | Error e -> $"Error solving order: {e}" |> failwith
        | Ok o ->
            o
            |> Order.solveMinMax true OrderLogging.noOp


open Types
open HelperFunctions



let morfCont =
    let au = Units.Weight.kiloGram
    let fu = Units.Volume.milliLiter
    let su = Units.Mass.milliGram
    let du = Units.Mass.microGram |> Units.per au |> Units.per Units.Time.hour
    let cu = su |> Units.per fu
    let ru = fu |> Units.per Units.Time.hour

    { Medication.order with
        Id = "1"
        Name = "morfin pump"
        Route = "INTRAVENEUS"
        Quantities = 50N |> ValueUnit.singleWithUnit fu |> Some
        Components = [
            { Medication.productComponent with
                Name = "morfin"
                Form = "iv fluid"
                Quantities = 1N |> ValueUnit.singleWithUnit fu |> Some
                Divisible = Some 10N
                Substances = [
                    { Medication.substanceItem with
                        Name = "morfin"
                        Concentrations = 
                            10N
                            |> ValueUnit.singleWithUnit cu
                            |> Some
                        Dose = 
                            { DoseLimit.limit with
                                DoseLimitTarget = "morfin" |> SubstanceLimitTarget
                                AdjustUnit = au |> Some
                                RateAdjust = 
                                    { MinMax.empty with
                                        Min = 10N |> ValueUnit.singleWithUnit du |> Limit.inclusive |> Some
                                        Max = 40N |> ValueUnit.singleWithUnit du |> Limit.inclusive |> Some
                                    }
                            }
                            |> Some
                        Solution =
                            { SolutionLimit.limit with
                                SolutionLimitTarget = "morfine" |> SubstanceLimitTarget
                                Quantity = 5N |> ValueUnit.singleWithUnit su |> MinMax.createExact 
                            }
                            |> Some
                    }
                ]
            }
            { Medication.productComponent with
                Name = "saline"
                Form = "iv fluid"
                Quantities = 1N |> ValueUnit.singleWithUnit fu |> Some
                Divisible = Some 10N
            }
        ]
        OrderType = ContinuousOrder
        Adjust = 10N |> ValueUnit.singleWithUnit au |> Some
        Dose = 
            { DoseLimit.limit with
                DoseLimitTarget = OrderableLimitTarget
                AdjustUnit =  None
            }
            |> Some
    }


morfCont 
|> Medication.toString
|> print


morfCont
|> Medication.toOrderDto
|> Order.Dto.fromDto
|> printOrderTable
|> Result.map Order.applyConstraints
|> printOrderTable
|> solveOrder
|> printOrderTable
|> Result.bind (fun o -> 
    (o, IncreaseDoseRate 20) 
    |> ChangeProperty
    |> OrderProcessor.processPipeline OrderLogging.noOp None
)
|> printOrderTable
|> Result.bind (fun o -> 
    (o, IncreaseDoseRate 1) 
    |> ChangeProperty
    |> OrderProcessor.processPipeline OrderLogging.noOp None
)
|> printOrderTable
|> ignore



let pcmDrink =
    let au = Units.Weight.kiloGram
    let fu = Units.Volume.milliLiter
    let su = Units.Mass.milliGram
    let cu = su |> Units.per fu
    let tu = Units.Time.day

    { Medication.order with
        Id = "pcm-drank"
        Name = "paracetamol drank"
        Components =
            [
                {
                    Medication.productComponent with
                        Name = "paracetamol"
                        Form = "drank"
                        Quantities =
                            5N
                            |> ValueUnit.singleWithUnit fu
                            |> Some
                        Divisible = Some 1N
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "paracetamol"
                                        Concentrations =
                                            24N
                                            |> ValueUnit.singleWithUnit cu
                                            |> Some
                                        Dose =
                                            { DoseLimit.limit with
                                                DoseLimitTarget = "paracetamol" |> SubstanceLimitTarget
                                                AdjustUnit = su |> Some
                                                PerTimeAdjust =
                                                    MinMax.createInclIncl
                                                        (60N |> ValueUnit.singleWithUnit (su |> Units.per au |> Units.per tu))
                                                        (90N |> ValueUnit.singleWithUnit (su |> Units.per au |> Units.per tu))
                                            }
                                            |> Some
                                }
                            ]
                }
            ]
        Route = "or"
        OrderType = DiscontinuousOrder
        Adjust = 10N |> ValueUnit.singleWithUnit au |> Some
        Frequencies =
            [|3N; 4N |]
            |> ValueUnit.withUnit (Units.Count.times |> Units.per tu)
            |> Some
        DoseCount = 1N |> ValueUnit.singleWithUnit Units.Count.times |> MinMax.createExact
    }


open Types

pcmDrink
|> Medication.toString
|> print


pcmDrink
|> Medication.toOrderDto
|> Order.Dto.fromDto
|> printOrderTable
|> Result.map Order.applyConstraints
|> printOrderTable
|> solveOrder
|> printOrderTable
|> Result.bind (fun o -> 
    (o, IncreaseFrequency) 
    |> ChangeProperty
    |> OrderProcessor.processPipeline OrderLogging.noOp None
)
|> printOrderTable
|> Result.bind (fun o -> 
    (o, DecreaseDoseQuantity ("paracetamol", 20)) 
    |> ChangeProperty
    |> OrderProcessor.processPipeline OrderLogging.noOp None
)
|> printOrderTable
|> ignore


let cotrim =
    let au = Units.Weight.kiloGram
    let fu = Units.Volume.milliLiter
    let su = Units.Mass.milliGram
    let cu = su |> Units.per fu
    let tu = Units.Time.day

    {
        Medication.order with
            Id = "1"
            Name = "cotrimoxazol"
            Components =
                [
                    {
                        Medication.productComponent with
                            Name = "cotrimoxazol"
                            Form = "drank"
                            Quantities =
                                1N
                                |> ValueUnit.singleWithUnit fu
                                |> Some
                            Divisible = Some 1N
                            Substances =
                                [
                                    {
                                        Medication.substanceItem with
                                            Name = "sulfamethoxazol"
                                            Concentrations =
                                                [| 40N; 400N; 800N |]
                                                |> ValueUnit.withUnit cu
                                                |> Some
                                            Dose =
                                                { DoseLimit.limit with
                                                    DoseLimitTarget = "sulfamethoxazol" |> SubstanceLimitTarget
                                                    AdjustUnit = su |> Some
                                                    QuantityAdjust =
                                                        MinMax.createInclIncl
                                                            (27N |> ValueUnit.singleWithUnit (su |> Units.per au))
                                                            (30N |> ValueUnit.singleWithUnit (su |> Units.per au))
                                                }
                                                |> Some
                                    }
                                    {
                                        Medication.substanceItem with
                                            Name = "trimethoprim"
                                            Concentrations =
                                                [| 8N; 80N; 160N |]
                                                |> ValueUnit.withUnit cu
                                                |> Some
                                            Dose =
                                                { DoseLimit.limit with
                                                    DoseLimitTarget = "trimethoprim" |> SubstanceLimitTarget
                                                    AdjustUnit = su |> Some
                                                    QuantityAdjust =
                                                        MinMax.createInclIncl
                                                            (6N - 6N / 10N |> ValueUnit.singleWithUnit (su |> Units.per au))
                                                            (6N |> ValueUnit.singleWithUnit (su |> Units.per au))
                                                }
                                                |> Some
                                    }
                                ]
                    }
                ]
            Route = "or"
            OrderType = DiscontinuousOrder
            Frequencies =
                [|2N |]
                |> ValueUnit.withUnit (Units.Count.times |> Units.per tu)
                |> Some
            Adjust = 10N |> ValueUnit.singleWithUnit au |> Some
            DoseCount = 1N |> ValueUnit.singleWithUnit Units.Count.times |> MinMax.createExact
            Dose =
                { DoseLimit.limit with
                    DoseLimitTarget = OrderableLimitTarget
                    AdjustUnit = au |> Some
                    QuantityAdjust =
                        { MinMax.empty with
                            Max =
                                10N
                                |> ValueUnit.singleWithUnit (fu |> Units.per au)
                                |> Limit.inclusive
                                |> Some
                        }
                }
                |> Some
    }


cotrim
|> Medication.toString
|> print


cotrim
|> Medication.toOrderDto
|> Order.Dto.fromDto
|> printOrderTable
|> Result.map Order.applyConstraints
|> solveOrder
|> printOrderTable
|> ignore

let tpnComplete =
    { Medication.order with
        Id = "f1adf475-919b-4b7d-9e26-6cc502b88e42"
        Name = "samenstelling c"
        Route = "INTRAVENEUS"
        OrderType = TimedOrder
        Adjust =
            11N
            |> ValueUnit.singleWithUnit Units.Weight.kiloGram
            |> Some
        Frequencies =
            1N
            |> ValueUnit.singleWithUnit (Units.Count.times |> Units.per Units.Time.day)
            |> Some
        Time =
            { MinMax.empty with
                Min =
                    20N
                    |> ValueUnit.singleWithUnit Units.Time.hour
                    |> Limit.inclusive
                    |> Some
                Max =
                    24N
                    |> ValueUnit.singleWithUnit Units.Time.hour
                    |> Limit.inclusive
                    |> Some
            }
        Dose =
            { DoseLimit.limit with
                DoseLimitTarget = OrderableLimitTarget
                AdjustUnit = Units.Weight.kiloGram |> Some
                QuantityAdjust =
                    { MinMax.empty with
                        Max =
                            (755N / 10N)
                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                            |> Limit.inclusive
                            |> Some
                    }
            }
            |> Some
        DoseCount =
            { MinMax.empty with
                Min = 1N |> ValueUnit.singleWithUnit Units.Count.times |> Limit.inclusive |> Some
                Max = 1N |> ValueUnit.singleWithUnit Units.Count.times |> Limit.inclusive |> Some
            }
        Components =
            [
                // Samenstelling C component
                {
                    Medication.productComponent with
                        Name = "Samenstelling C"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Dose =
                            { DoseLimit.limit with
                                DoseLimitTarget = "Samenstelling C" |> ComponentLimitTarget
                                AdjustUnit = Units.Weight.kiloGram |> Some
                                QuantityAdjust =
                                    { MinMax.empty with
                                        Min =
                                            10N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                        Max =
                                            25N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                    }
                            }
                            |> Some
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "energie"
                                        Concentrations =
                                            (32N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Energy.kiloCalorie |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "eiwit"
                                        Concentrations =
                                            (8N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Mass.gram |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "eiwit" |> SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            5N / 100N
                                                            |> ValueUnit.singleWithUnit (Units.Mass.gram |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "natrium"
                                        Concentrations =
                                            (1N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "natrium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "kalium"
                                        Concentrations =
                                            (2N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "kalium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "calcium"
                                        Concentrations =
                                            (3N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "fosfaat"
                                        Concentrations =
                                            (2N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "magnesium"
                                        Concentrations =
                                            (1N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "chloor"
                                        Concentrations =
                                            (7N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                            ]
                }
                // NaCl 3% component
                {
                    Medication.productComponent with
                        Name = "NaCl 3%"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Dose =
                            { DoseLimit.limit with
                                DoseLimitTarget = "NaCl 3%" |> LimitTarget.ComponentLimitTarget
                                AdjustUnit = Units.Weight.kiloGram |> Some
                                QuantityAdjust =
                                    { MinMax.empty with
                                        Min =
                                            6N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                        Max =
                                            6N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                    }
                            }
                            |> Some
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "natrium"
                                        Concentrations =
                                            (5N / 10N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "natrium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "chloor"
                                        Concentrations =
                                            (5N / 10N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                            ]
                }
                // KCl 7,4% component
                {
                    Medication.productComponent with
                        Name = "KCl 7,4%"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Dose =
                            { DoseLimit.limit with
                                DoseLimitTarget = "KCl 7,4%" |> LimitTarget.ComponentLimitTarget
                                AdjustUnit = Units.Weight.kiloGram |> Some
                                QuantityAdjust =
                                    { MinMax.empty with
                                        Min =
                                            2N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                        Max =
                                            2N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                    }
                            }
                            |> Some
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "kalium"
                                        Concentrations =
                                            1N
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "kalium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "chloor"
                                        Concentrations =
                                            1N
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                            ]
                }
                // gluc 10% component
                {
                    Medication.productComponent with
                        Name = "gluc 10%"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "energie"
                                        Concentrations =
                                            (4N / 10N)
                                            |> ValueUnit.singleWithUnit (Units.Energy.kiloCalorie |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "koolhydraat"
                                        Concentrations =
                                            (1N / 10N)
                                            |> ValueUnit.singleWithUnit (Units.Mass.gram |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                            ]
                }
            ]
    }



let tpn =
    { Medication.order with
        Id = "f1adf475-919b-4b7d-9e26-6cc502b88e42"
        Name = "samenstelling c"
        Route = "INTRAVENEUS"
        OrderType = TimedOrder
        Adjust =
            11N
            |> ValueUnit.singleWithUnit Units.Weight.kiloGram
            |> Some
        Frequencies =
            1N
            |> ValueUnit.singleWithUnit (Units.Count.times |> Units.per Units.Time.day)
            |> Some
        Time =
            { MinMax.empty with
                Min =
                    20N
                    |> ValueUnit.singleWithUnit Units.Time.hour
                    |> Limit.inclusive
                    |> Some
                Max =
                    24N
                    |> ValueUnit.singleWithUnit Units.Time.hour
                    |> Limit.inclusive
                    |> Some
            }
        Dose =
            { DoseLimit.limit with
                DoseLimitTarget = OrderableLimitTarget
                AdjustUnit = Units.Weight.kiloGram |> Some
                QuantityAdjust =
                    { MinMax.empty with
                        Max =
                            (755N / 10N)
                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                            |> Limit.inclusive
                            |> Some
                    }
            }
            |> Some
        DoseCount =
            { MinMax.empty with
                Min = 1N |> ValueUnit.singleWithUnit Units.Count.times |> Limit.inclusive |> Some
                Max = 1N |> ValueUnit.singleWithUnit Units.Count.times |> Limit.inclusive |> Some
            }
        Components =
            [
                // Samenstelling C component
                {
                    Medication.productComponent with
                        Name = "Samenstelling C"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Dose =
                            { DoseLimit.limit with
                                DoseLimitTarget = "Samenstelling C" |> LimitTarget.ComponentLimitTarget
                                AdjustUnit = Units.Weight.kiloGram |> Some
                                QuantityAdjust =
                                    { MinMax.empty with
                                        Min =
                                            10N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                        Max =
                                            25N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                    }
                            }
                            |> Some
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "eiwit"
                                        Concentrations =
                                            (8N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Mass.gram |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "eiwit" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 100N)
                                                            |> ValueUnit.singleWithUnit (Units.Mass.gram |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "natrium"
                                        Concentrations =
                                            (1N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "natrium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                                {
                                    Medication.substanceItem with
                                        Name = "kalium"
                                        Concentrations =
                                            (2N / 100N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "kalium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                            ]
                }
                // NaCl 3% component
                {
                    Medication.productComponent with
                        Name = "NaCl 3%"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Dose =
                            { DoseLimit.limit with
                                DoseLimitTarget = "NaCl 3%" |> LimitTarget.ComponentLimitTarget
                                AdjustUnit = Units.Weight.kiloGram |> Some
                                QuantityAdjust =
                                    { MinMax.empty with
                                        Min =
                                            6N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                        Max =
                                            6N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                    }
                            }
                            |> Some
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "natrium"
                                        Concentrations =
                                            (5N / 10N)
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "natrium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                            ]
                }
                // KCl 7,4% component
                {
                    Medication.productComponent with
                        Name = "KCl 7,4%"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Dose =
                            { DoseLimit.limit with
                                DoseLimitTarget = "KCl 7,4%" |> LimitTarget.ComponentLimitTarget
                                AdjustUnit = Units.Weight.kiloGram |> Some
                                QuantityAdjust =
                                    { MinMax.empty with
                                        Min =
                                            2N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                        Max =
                                            2N
                                            |> ValueUnit.singleWithUnit (Units.Volume.milliLiter |> Units.per Units.Weight.kiloGram)
                                            |> Limit.inclusive
                                            |> Some
                                    }
                            }
                            |> Some
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "kalium"
                                        Concentrations =
                                            1N
                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                        Solution =
                                            { SolutionLimit.limit with
                                                SolutionLimitTarget = "kalium" |> LimitTarget.SubstanceLimitTarget
                                                Concentration =
                                                    { MinMax.empty with
                                                        Max =
                                                            (5N / 10N)
                                                            |> ValueUnit.singleWithUnit (Units.Molar.milliMole |> Units.per Units.Volume.milliLiter)
                                                            |> Limit.inclusive
                                                            |> Some
                                                    }
                                            }
                                            |> Some
                                }
                            ]
                }
                // gluc 10% component
                {
                    Medication.productComponent with
                        Name = "gluc 10%"
                        Form = "vloeistof"
                        Quantities =
                            1N
                            |> ValueUnit.singleWithUnit Units.Volume.milliLiter
                            |> Some
                        Divisible = Some (1N)
                        Substances =
                            [
                                {
                                    Medication.substanceItem with
                                        Name = "koolhydraat"
                                        Concentrations =
                                            (1N / 10N)
                                            |> ValueUnit.singleWithUnit (Units.Mass.gram |> Units.per Units.Volume.milliLiter)
                                            |> Some
                                }
                            ]
                }
            ]
        Quantities = failwith "Not Implemented"
    }


tpn
|> Medication.toString
|> print


let tpnConstraints =
    [
        OrderAdjust OrderVariable.Quantity.applyConstraints

        ScheduleFrequency OrderVariable.Frequency.applyConstraints
        ScheduleTime OrderVariable.Time.applyConstraints

        OrderableQuantity OrderVariable.Quantity.applyConstraints
        OrderableDoseCount OrderVariable.Count.applyConstraints
        OrderableDose Order.Orderable.Dose.applyConstraints

        ComponentOrderableQuantity ("", OrderVariable.Quantity.applyConstraints)

        ItemComponentConcentration ("", "", OrderVariable.Concentration.applyConstraints)
        ItemOrderableConcentration ("", "", OrderVariable.Concentration.applyConstraints)
    ]



let logger = OrderLogging.createConsoleLogger ()


let applyPropChange msg propChange ord =
    printfn $"=== Apply PropChange {msg} ==="
    let ord =
        ord
        |> Order.OrderPropertyChange.proc propChange
    ord
    |> Order.solveMinMax true Logging.noOp
    |> function
        | Ok ord -> ord
        | _ ->
            printfn $"=== ERROR {msg} ==="
            ord
    |> fun ord ->
        ord
        |> Order.printTable ConsoleTables.Format.Minimal

        ord


let run
    proteinPerc
    potassiumPerc
    sodiumPerc
    glucPerc
    tpn =

    tpn
    |> Medication.toOrderDto
    |> Order.Dto.fromDto
    |> Result.map (fun ord ->
        let ord =
            ord
            |> Order.OrderPropertyChange.proc tpnConstraints
    //        |> Order.applyConstraints

        ord
        |> Order.printTable ConsoleTables.Format.Minimal

        let ord =
            ord
            |> Order.solveMinMax true Logging.noOp //logger
            //|> Result.bind (Order.solveMinMax true logger)

        ord
        |> Result.iter (Order.printTable ConsoleTables.Format.Minimal)

        let ord =
            ord
            |> Result.map (fun ord ->
                ord
                |> applyPropChange
                    "Samenstelling C"
                    [
                        ComponentOrderableQuantity ("Samenstelling C", OrderVariable.Quantity.setPercValue proteinPerc)
                    ]
            )

        let ord =
            ord
            |> Result.map (fun ord ->
                ord
                |> applyPropChange
                    "KCl 7,4%"
                    [
                        ComponentOrderableQuantity ("KCl 7,4%", OrderVariable.Quantity.setPercValue potassiumPerc)
                    ]
            )

        let ord =
            ord
            |> Result.map (fun ord ->
                ord
                |> applyPropChange
                    "NaCl 3%"
                    [
                        ComponentOrderableQuantity ("NaCl 3%", OrderVariable.Quantity.setPercValue sodiumPerc)
                    ]
            )

        let ord =
            ord
            |> Result.map (fun ord ->
                ord
                |> applyPropChange
                    "gluc 10%"
                    [
                        ComponentOrderableQuantity ("gluc 10%", OrderVariable.Quantity.setPercValue glucPerc)
                    ]
            )

        ord
    )


tpn
|> run 50 0 5 0
|> ignore

