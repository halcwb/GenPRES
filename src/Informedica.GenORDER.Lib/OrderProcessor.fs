namespace Informedica.GenOrder.Lib


module OrderProcessor =

    open Informedica.Utils.Lib
    open ConsoleWriter.NewLineNoTime
    open Order

    module Quantity = OrderVariable.Quantity
    module Concentration = OrderVariable.Concentration
    module Time = OrderVariable.Time
    module Frequency = OrderVariable.Frequency
    module Dose = Orderable.Dose


    let (|FrequencyCleared|RateCleared|TimeCleared|ConcentrationCleared|DoseQuantityCleared|DosePerTimeCleared|NotCleared|) (ord: Order) =
        let frq = ord.Schedule |> Schedule.getFrequency
        let tme = ord.Schedule |> Schedule.getTime

        match frq |> Option.map Frequency.isCleared |> Option.defaultValue false,
              ord.Orderable.Dose |> Dose.isRateCleared,
              tme |> Option.map Time.isCleared |> Option.defaultValue false,
              ord.Orderable |> Orderable.isConcentrationCleared,
              ord.Orderable |> Orderable.isDoseQuantityCleared,
              ord.Orderable |> Orderable.isItemDosePerTimeCleared with
        | true,  false, false, false, false, false -> FrequencyCleared
        | false, true,  false, false, false, false -> RateCleared
        | false, false, true,  false, false, false -> TimeCleared
        | false, false, false, true,  false, false -> ConcentrationCleared
        | false, false, false, false, true,  false -> DoseQuantityCleared
        | false, false, false, false, false, true  -> DosePerTimeCleared
        | res ->
            $"{res} was not matched!" |> writeWarningMessage
            NotCleared


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


    let orderPropertySetFrequency printErr logger step ord =
        ord
        // clear frequency and dependent properties
        |> OrderPropertyChange.proc
            [
                // clear schedule frequency
                ScheduleFrequency Frequency.setToNonZeroPositive
                // Clear dependent per-time dose variables
                OrderableDose Dose.setPerTimeToNonZeroPositive
                ComponentDose ("", Dose.setPerTimeToNonZeroPositive)
                ItemDose ("", "", Dose.setPerTimeToNonZeroPositive)
            ]
        // re-apply constraints
        |> OrderPropertyChange.proc
            [
                // Apply constraints to frequency first
                ScheduleFrequency Frequency.applyConstraints
            ]
        // re-calc min max
        |> solveMinMax printErr logger
        // step to a min, median or max value
        |> Result.map 
            (
                OrderPropertyChange.proc
                    [
                        // Set frequency to min value
                        ScheduleFrequency step
                    ]
            )


    let orderPropertySetDoseQuantity printErr logger step cmp ord =
        ord
        // clear dose quantity and dependent properties
        |> OrderPropertyChange.proc
            [
                // clear schedule time if applicable
                if ord.Schedule |> Schedule.hasTime then
                    ScheduleTime Time.setToNonZeroPositive
                    OrderableDose Dose.setRateToNonZeroPositive
                // clear dose quantity                
                OrderableDose Dose.setQuantityToNonZeroPositive
                ComponentDose ("", Dose.setQuantityToNonZeroPositive)
                ItemDose ("", "", Dose.setQuantityToNonZeroPositive)
                // clear dose per timke
                OrderableDose Dose.setPerTimeToNonZeroPositive
                ComponentDose ("", Dose.setPerTimeToNonZeroPositive)
                ItemDose ("", "", Dose.setPerTimeToNonZeroPositive)
                // clear orderable quantity and dose count
                OrderableQuantity Quantity.setToNonZeroPositive
                OrderableDoseCount OrderVariable.Count.setToNonZeroPositive
                ComponentOrderableCount ("", OrderVariable.Count.setToNonZeroPositive)
                ComponentOrderableQuantity ("", Quantity.setToNonZeroPositive)
                ItemOrderableQuantity ("", "", Quantity.setToNonZeroPositive)
            ]
        // re-apply constraints
        |> OrderPropertyChange.proc
            [
                // apply schedule constraints
                if ord.Schedule |> Schedule.hasTime then
                    ScheduleTime Time.applyConstraints
                    OrderableDose Dose.setStandardRateConstraints
                // apply dose constraints
                OrderableDose Dose.applyQuantityConstraints
                ComponentDose ("", Dose.applyQuantityMaxConstraints)
                // if the orderable doesn't have a max constraint, then
                // use the per-time constraints
                if ord.Orderable |> Orderable.hasMaxDoseQuantityConstraint |> not then
                    OrderableDose Dose.applyPerTimeConstraints
                    ComponentDose ("", Dose.applyPerTimeConstraints)
                    ItemDose ("", "", Dose.applyPerTimeConstraints)            
                // apply quantity and count
                OrderableQuantity Quantity.applyConstraints
                ComponentOrderableQuantity ("", Quantity.applyConstraints)
                ItemOrderableQuantity ("", "", Quantity.applyConstraints)
                OrderableDoseCount OrderVariable.Count.applyConstraints
            ]
        // re-calc min max
        |> solveMinMax printErr logger
        // step to a min, median or max dose quantity
        |> Result.map 
            (
                OrderPropertyChange.proc
                    [
                        // Set dose quantity to the specified value
                        ComponentDose (cmp, step)
                    ]
            )


    let orderPropertySetDoseRate printErr logger step ord =
        ord
        // clear dose rates and dependent properties
        |> OrderPropertyChange.proc
            [
                OrderableDose Dose.setRateAdjustToNonZeroPositive
                // Clear all dependent dose rates
                ComponentDose ("", Dose.setRateToNonZeroPositive)
                ItemDose ("", "", Dose.setRateToNonZeroPositive)
                // Clear time
                ScheduleTime Time.setToNonZeroPositive
            ]
        // re-apply constraints
        |> OrderPropertyChange.proc
            [
                // Apply constraints to dose rate first
                OrderableDose Dose.applyConstraints
                ComponentDose ("", Dose.applyConstraints)
                ItemDose ("", "", Dose.applyConstraints)

                ScheduleTime Time.applyConstraints
            ]
        // re-calc the min max 
        |> solveMinMax printErr logger
        // step to a min, median or max rate
        |> Result.map 
            (
                OrderPropertyChange.proc
                    [
                        // Set dose rate to the specified value
                        OrderableDose step
                    ]
            )


    let processChangeProperty printErr logger cmd ord =
        match cmd with
        | DecreaseFrequency -> ord |> orderPropertyIncrOrDecrFrequency Frequency.decrease |> Ok
        | IncreaseFrequency -> ord |> orderPropertyIncrOrDecrFrequency Frequency.increase |> Ok
        | SetMinFrequency -> ord |> orderPropertySetFrequency printErr logger Frequency.setMinValue 
        | SetMedianFrequency -> ord |> orderPropertySetFrequency printErr logger Frequency.setMedianValue
        | SetMaxFrequency -> ord |> orderPropertySetFrequency printErr logger Frequency.setMaxValue
        | DecreaseDoseQuantity (cmp, n) -> ord |> orderPropertyIncrOrDecrDoseQuantity (Dose.decreaseQuantity n) cmp |> Ok
        | IncreaseDoseQuantity (cmp, n) -> ord |> orderPropertyIncrOrDecrDoseQuantity (Dose.increaseQuantity n) cmp |> Ok
        | SetMinDoseQuantity cmp -> ord |> orderPropertySetDoseQuantity printErr logger (Dose.setMinDose ord.Schedule) cmp
        | SetMaxDoseQuantity cmp -> ord |> orderPropertySetDoseQuantity printErr logger (Dose.setMaxDose ord.Schedule) cmp
        | SetMedianDoseQuantity cmp -> ord |> orderPropertySetDoseQuantity printErr logger (Dose.setMedianDose ord.Schedule) cmp
        | DecreaseDoseRate n -> ord |> orderPropertyIncrOrDecrDoseRate (Dose.decreaseRate n) |> Ok
        | IncreaseDoseRate n -> ord |> orderPropertyIncrOrDecrDoseRate (Dose.increaseRate n) |> Ok
        | SetMinDoseRate -> ord |> orderPropertySetDoseRate printErr logger (Dose.setMinDose ord.Schedule)
        | SetMaxDoseRate -> ord |> orderPropertySetDoseRate printErr logger (Dose.setMaxDose ord.Schedule)
        | SetMedianDoseRate -> ord |> orderPropertySetDoseRate printErr logger (Dose.setMedianDose ord.Schedule)


    let orderPropertyChangeFrequency ord =
        ord
        |> OrderPropertyChange.proc
            [
                ScheduleFrequency Frequency.setToNonZeroPositive

                OrderableDose Dose.setPerTimeToNonZeroPositive
                ComponentDose ("", Dose.setPerTimeToNonZeroPositive)
                ItemDose ("", "", Dose.setPerTimeToNonZeroPositive)
            ]
        |> OrderPropertyChange.proc
            [
                ScheduleFrequency Frequency.setStandardValues
            ]


    let orderPropertyChangeDoseQuantity ord =
        ord
        |> OrderPropertyChange.proc
            [
                if ord.Schedule |> Schedule.hasTime then
                    ScheduleTime OrderVariable.Time.setToNonZeroPositive
                    OrderableDose Dose.setRateToNonZeroPositive

                OrderableDose Dose.setQuantityToNonZeroPositive
                ComponentDose ("", Dose.setQuantityToNonZeroPositive)
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
                OrderableDose Dose.applyQuantityConstraints
                ComponentDose ("", Dose.applyQuantityMaxConstraints)
                OrderableDoseCount OrderVariable.Count.applyConstraints

                if ord.Schedule |> Schedule.hasTime then
                    ScheduleTime OrderVariable.Time.applyConstraints
                    OrderableDose Dose.setStandardRateConstraints

                // if the orderable doesn't have a max constraint, then
                // use the per-time constraints
                if ord.Orderable |> Orderable.hasMaxDoseQuantityConstraint |> not then
                    OrderableDose Dose.applyPerTimeConstraints
                    ComponentDose ("", Dose.applyPerTimeConstraints)
                    ItemDose ("", "", Dose.applyPerTimeConstraints)
            ]


    let orderPropertyChangeRate ord =
        ord
        |> OrderPropertyChange.proc
            [
                // clear all dose rates
                OrderableDose Dose.setRateToNonZeroPositive
                ComponentDose ("", Dose.setRateToNonZeroPositive)
                ItemDose ("", "", Dose.setRateToNonZeroPositive)
                // clear time
                ScheduleTime Time.setToNonZeroPositive
            ]
        |> OrderPropertyChange.proc
            [
                // apply dose rate constraints again
                OrderableDose Dose.setStandardRateConstraints
                OrderableDose Dose.applyRateConstraints
                ComponentDose ("", Dose.applyRateConstraints)
                ItemDose ("", "", Dose.applyRateConstraints)
                // apply time constraints again
                ScheduleTime Time.applyConstraints
            ]


    /// Process an order that has been cleared
    /// by setting relevant variables to non-zero positive
    /// values and solving the order again
    let processClearedOrder logger ord =
        // small helpers to clarify post-processing after property changes
        let solveAndToValues minTime =
            solveMinMax true logger
            >> Result.map (minIncrMaxToValues true minTime logger)

        let defaultInc = 100
        let solveIncrIncrAndToValues minTime =
            solveMinMax true logger
            >> Result.bind (increaseIncrements logger defaultInc defaultInc)
            >> Result.map (minIncrMaxToValues true minTime logger)

        let logUnmatched (kind: string) =
            $"===> no match for {kind} cleared " |> writeWarningMessage
            ord |> stringTable |> Events.OrderScenario |> Logging.logWarning logger

        match (ord |> inf).Schedule with
        | Continuous _ ->
            match ord with
            | TimeCleared
            | RateCleared ->
                ord
                |> orderPropertyChangeRate
                |> solveAndToValues true
            | ConcentrationCleared ->
                ord
                |> OrderPropertyChange.proc
                    [
                        // clear all item dose rates
                        ComponentDose ("", Dose.setRateToNonZeroPositive)
                        ComponentDose ("", Dose.applyRateConstraints)

                        ItemDose ("", "", Dose.setRateToNonZeroPositive)
                        ItemDose ("", "", Dose.applyRateConstraints)
                        ItemDose ("", "", Dose.setQuantityToNonZeroPositive)

                        // clear the item- and component-orderable quantities
                        // causing these to be recalculated
                        ComponentOrderableConcentration ("", Concentration.setToNonZeroPositive)
                        ComponentOrderableCount ("", OrderVariable.Count.setToNonZeroPositive)
                        ComponentOrderableQuantity ("", Quantity.setToNonZeroPositive)
                        ComponentOrderableQuantity ("", Quantity.applyConstraints)
                        ComponentDose ("", Dose.setQuantityToNonZeroPositive)

                        ItemOrderableConcentration ("", "", Concentration.setToNonZeroPositive)
                        ItemOrderableQuantity ("", "", Quantity.setToNonZeroPositive)
                    ]
                |> solveAndToValues true
            | _ ->
                logUnmatched "continuous"
                ord |> solveOrder true logger
        | Once
        | Discontinuous _ ->
            match ord with
            | FrequencyCleared ->
                ord
                |> orderPropertyChangeFrequency
                // solve min/max and min/incr/max to values
                |> solveAndToValues true
            | DosePerTimeCleared
            | DoseQuantityCleared ->
                ord
                |> orderPropertyChangeDoseQuantity
                // solve min/max, increase increments, and min/incr/max to values
                |> solveIncrIncrAndToValues true
            | _ ->
                logUnmatched "discontinuous"
                ord |> solveOrder true logger
        | OnceTimed _
        | Timed _ ->
            match ord with
            | FrequencyCleared ->
                ord
                |> orderPropertyChangeFrequency
                // solve min/max and min/incr/max to values
                |> solveAndToValues false
            | RateCleared
            | TimeCleared ->
                ord
                |> orderPropertyChangeRate
                |> solveAndToValues false
            | DosePerTimeCleared
            | DoseQuantityCleared ->
                ord
                |> orderPropertyChangeDoseQuantity
                // solve min/max, increase increments, and min/incr/max to values
                |> solveIncrIncrAndToValues false

            | _ ->
                logUnmatched "timed"
                ord |> solveOrder true logger


    type GenSolverExceptionMsg = Informedica.GenSolver.Lib.Types.Exceptions.Message


    let (|IsEmpty|NoValues|HasValues|DoseSolvedNotCleared|DoseSolvedAndCleared|) ord =
        match ord with
        | _ when ord |> isEmpty -> IsEmpty
        | _ when ord |> hasValues -> HasValues
        | _ when ord |> doseIsSolved && ord |> isCleared |> not -> DoseSolvedNotCleared
        | _ when ord |> doseIsSolved && ord |> isCleared -> DoseSolvedAndCleared
        | _ -> NoValues


    let printState = function
        | IsEmpty -> "IsEmpty"
        | NoValues -> "NoValues"
        | HasValues -> "HasValues"
        | DoseSolvedNotCleared -> "DoseSolvedNotCleared"
        | DoseSolvedAndCleared -> "DoseSolvedAndCleared"


    // New: A lightweight classification and step-driven pipeline
    type PrescriptionKind =
        | PKOnce
        | PKOnceTimed
        | PKDiscontinuous
        | PKContinuous
        | PKTimed


    /// The state of an Order used for classification
    /// in the processing pipeline
    /// IsEmpty: whether the order is empty
    /// HasValues: whether the order has values
    /// DoseIsSolved: whether the dose is solved
    /// IsCleared: whether the order is cleared
    /// PrescriptionKind: the kind of prescription
    /// (Once, OnceTimed, Discontinuous, Continuous, Timed)
    /// This is used to determine which steps to run
    /// in the processing pipeline
    /// and in which order
    type OrderState = {
        IsEmpty: bool
        HasValues: bool
        DoseIsSolved: bool
        OrderIsSolved: bool
        IsCleared: bool
        PrescriptionKind: PrescriptionKind
    }


    /// Classify an order into an OrderState
    let classify (ord: Order) : OrderState =
        let kind =
            match ord.Schedule with
            | Once -> PKOnce
            | OnceTimed _ -> PKOnceTimed
            | Discontinuous _ -> PKDiscontinuous
            | Continuous _ -> PKContinuous
            | Timed _ -> PKTimed

        {
            IsEmpty = ord |> isEmpty
            HasValues = ord |> hasValues
            DoseIsSolved = ord |> doseIsSolved
            OrderIsSolved = ord |> isSolved
            IsCleared = ord |> isCleared
            PrescriptionKind = kind
        }


    /// A processing step in the pipeline
    /// with a name, a guard function to check
    /// whether to run the step, and a run function
    /// that takes an Order and returns a Result
    /// with the processed Order or a list of error messages
    type Step = {
        Name: string
        Guard: OrderState -> bool
        Run: Order -> Result<Order, Order * GenSolverExceptionMsg list>
    }


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
                { Name = "change-property: process-change"; Guard = (fun _ -> true); Run = processChangeProperty false logger cmd }
                { Name = "change-property: solve-order"; Guard = (fun _ -> true); Run = solveStep }
            ]
            |> runPipeline ord
