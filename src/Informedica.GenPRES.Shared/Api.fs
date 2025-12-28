namespace Shared


module Api =


    open Types


    type Command =
        | OrderContextCmd of OrderContextCommand
        | TreatmentPlanCmd of TreatmentPlanCommand
        | FormularyCmd of Formulary
        | ParenteraliaCmd of Parenteralia

    and OrderContextCommand =
        | UpdateOrderContext of OrderContext
        | SelectOrderScenario of OrderContext
        | UpdateOrderScenario of OrderContext
        | ResetOrderScenario of OrderContext
        | ReloadResources of OrderContext
        // Frequency property commands
        | DecreaseOrderFrequencyProperty of OrderContext
        | IncreaseOrderFrequencyProperty of OrderContext
        | SetMinOrderFrequencyProperty of OrderContext
        | SetMaxOrderFrequencyProperty of OrderContext
        | SetMedianOrderFrequencyProperty of OrderContext
        // DoseQuantity property commands (cmp = component, ntimes = number of times to adjust)
        | DecreaseOrderDoseQuantityProperty of OrderContext * cmp: string * ntimes: int
        | IncreaseOrderDoseQuantityProperty of OrderContext * cmp: string * ntimes: int
        | SetMinOrderDoseQuantityProperty of OrderContext * cmp: string
        | SetMaxOrderDoseQuantityProperty of OrderContext * cmp: string
        | SetMedianOrderDoseQuantityProperty of OrderContext * cmp: string
        // DoseRate property commands
        | DecreaseOrderDoseRateProperty of OrderContext * ntimes: int
        | IncreaseOrderDoseRateProperty of OrderContext * ntimes: int
        | SetMinOrderDoseRateProperty of OrderContext
        | SetMaxOrderDoseRateProperty of OrderContext
        | SetMedianOrderDoseRateProperty of OrderContext


    and TreatmentPlanCommand =
        | UpdateTreatmentPlan of TreatmentPlan
        | FilterTreatmentPlan of TreatmentPlan

    type Response =
        | OrderContextResp of OrderContextResponse
        | TreatmentPlanResp of TreatmentPlanResponse
        | FormularyResp of Formulary
        | ParentaraliaResp of Parenteralia

    and OrderContextResponse =
        | OrderContextSelected of OrderContext
        | OrderContextUpdated of OrderContext
        | OrderContextRefreshed of OrderContext
        | ResourcesReloaded of OrderContext

    and TreatmentPlanResponse =
        | TreatmentPlanFiltered of TreatmentPlan
        | TreatmentPlanUpdated of TreatmentPlan


    module Command =

        let toString = function
            | OrderContextCmd cmd ->
                match cmd with
                | UpdateOrderContext _ -> "UpdateOrderContext"
                | SelectOrderScenario _ -> "SelectOrderScenario"
                | UpdateOrderScenario _ -> "UpdateOrderScenario"
                | ResetOrderScenario _ -> "ResetOrderScenario"
                | ReloadResources _ -> "ReloadResources"
                // Frequency property commands
                | DecreaseOrderFrequencyProperty _ -> "DecreaseOrderFrequencyProperty"
                | IncreaseOrderFrequencyProperty _ -> "IncreaseOrderFrequencyProperty"
                | SetMinOrderFrequencyProperty _ -> "SetMinOrderFrequencyProperty"
                | SetMaxOrderFrequencyProperty _ -> "SetMaxOrderFrequencyProperty"
                | SetMedianOrderFrequencyProperty _ -> "SetMedianOrderFrequencyProperty"
                // DoseQuantity property commands
                | DecreaseOrderDoseQuantityProperty _ -> "DecreaseOrderDoseQuantityProperty"
                | IncreaseOrderDoseQuantityProperty _ -> "IncreaseOrderDoseQuantityProperty"
                | SetMinOrderDoseQuantityProperty _ -> "SetMinOrderDoseQuantityProperty"
                | SetMaxOrderDoseQuantityProperty _ -> "SetMaxOrderDoseQuantityProperty"
                | SetMedianOrderDoseQuantityProperty _ -> "SetMedianOrderDoseQuantityProperty"
                // DoseRate property commands
                | DecreaseOrderDoseRateProperty _ -> "DecreaseOrderDoseRateProperty"
                | IncreaseOrderDoseRateProperty _ -> "IncreaseOrderDoseRateProperty"
                | SetMinOrderDoseRateProperty _ -> "SetMinOrderDoseRateProperty"
                | SetMaxOrderDoseRateProperty _ -> "SetMaxOrderDoseRateProperty"
                | SetMedianOrderDoseRateProperty _ -> "SetMedianOrderDoseRateProperty"
            | TreatmentPlanCmd cmd ->
                match cmd with
                | UpdateTreatmentPlan _ -> "UpdateTreatmentPlan"
                | FilterTreatmentPlan _ -> "FilterTreatmentPlan"
            | FormularyCmd _ -> "FormularyCmd"
            | ParenteraliaCmd _ -> "ParenteraliaCmd"


    /// Defines how routes are generated on server and mapped from the client
    let routerPaths typeName method = $"/api/%s{typeName}/%s{method}"


    /// A type that specifies the communication protocol between client and server
    /// to learn more read the docs at https://zaid-ajaj.github.io/Fable.Remoting/src/basics.html
    type IServerApi =
        {
            processCommand: Command -> Async<Result<Response, string[]>>
            testApi: unit -> Async<string>
        }