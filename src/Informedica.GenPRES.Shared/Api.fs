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
        | DecreaseScheduleFrequencyProperty of OrderContext
        | IncreaseScheduleFrequencyProperty of OrderContext
        | SetMinScheduleFrequencyProperty of OrderContext
        | SetMaxScheduleFrequencyProperty of OrderContext
        | SetMedianScheduleFrequencyProperty of OrderContext
        // DoseQuantity property commands (ntimes = number of times to adjust)
        | DecreaseOrderableDoseQuantityProperty of OrderContext * ntimes: int
        | IncreaseOrderableDoseQuantityProperty of OrderContext * ntimes: int
        | SetMinOrderableDoseQuantityProperty of OrderContext
        | SetMaxOrderableDoseQuantityProperty of OrderContext
        | SetMedianOrderableDoseQuantityProperty of OrderContext
        // DoseRate property commands (ntimes = number of times to adjust)
        | DecreaseOrderableDoseRateProperty of OrderContext * ntimes: int
        | IncreaseOrderableDoseRateProperty of OrderContext * ntimes: int
        | SetMinOrderableDoseRateProperty of OrderContext
        | SetMaxOrderableDoseRateProperty of OrderContext
        | SetMedianOrderableDoseRateProperty of OrderContext
        // Orderable Quantity property commands (ntimes = number of times to adjust)
        | DecreaseOrderableQuantityProperty of OrderContext * ntimes: int
        | IncreaseOrderableQuantityProperty of OrderContext * ntimes: int
        | SetMinOrderableQuantityProperty of OrderContext
        | SetMaxOrderableQuantityProperty of OrderContext
        | SetMedianOrderableQuantityProperty of OrderContext
        // Component Quantity property commands (cmp = component, ntimes = number of times to adjust)
        | DecreaseComponentQuantityProperty of OrderContext * cmp: string * ntimes: int
        | IncreaseComponentQuantityProperty of OrderContext * cmp: string * ntimes: int
        | SetMinComponentQuantityProperty of OrderContext * cmp: string
        | SetMaxComponentQuantityProperty of OrderContext * cmp: string
        | SetMedianComponentQuantityProperty of OrderContext * cmp: string


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
            | OrderContextCmd _ -> "OrderContextCmd"
            | TreatmentPlanCmd _ -> "TreatmentPlanCmd"
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
