namespace Informedica.ZForm.Lib


[<AutoOpen>]
module Types =

    open MathNet.Numerics

    open Informedica.GenUnits.Lib

    type Route = Informedica.ZIndex.Lib.Types.Route.Route
    type MinMax = Informedica.GenCore.Lib.Ranges.MinMax


    /// A patient category
    type PatientCategory =
        {
            /// The min max gestational age of the patient
            GestAge : MinMax
            /// The min max age of the patient
            Age : MinMax
            /// The min max weight of the patient
            Weight : MinMax
            /// The min max BSA of the patient
            BSA : MinMax
            /// The gender of the patient
            Gender : Gender
        }

        static member GestAge_ :
            (PatientCategory -> MinMax) * (MinMax -> PatientCategory -> PatientCategory) =
            _.GestAge, (fun a p -> { p with GestAge = a })

        static member Age_ :
            (PatientCategory -> MinMax) * (MinMax -> PatientCategory -> PatientCategory) =
            _.Age, (fun a p -> { p with Age = a })

        static member Weight_ :
            (PatientCategory -> MinMax) * (MinMax -> PatientCategory -> PatientCategory) =
            _.Weight, (fun w p -> { p with Weight = w })

        static member BSA_ :
            (PatientCategory -> MinMax) * (MinMax -> PatientCategory -> PatientCategory) =
            _.BSA, (fun b p -> { p with BSA = b })

        static member Gender_ :
            (PatientCategory -> Gender) * (Gender -> PatientCategory -> PatientCategory) =
            _.Gender, (fun g p -> { p with Gender = g })

    and Gender = Male | Female | Undetermined


    /// Dose range limits
    type DoseRange =
        {
            /// Normal limits
            Norm : MinMax
            /// Normal limits adjusted by weight (Unit = weight unit)
            NormWeight : MinMax * Unit
            /// Normal limits adjusted by BSA (Unit = BSA unit)
            NormBSA : MinMax * Unit
            /// Absolute limits
            Abs : MinMax
            /// Absolute limits adjusted by weight (Unit = weight unit)
            AbsWeight : MinMax * Unit
            /// Absolute limits adjusted by BSA (Unit = bsa unit)
            AbsBSA : MinMax * Unit
        }
        static member Norm_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            _.Norm,
            (fun mm dr -> { dr with Norm = mm })

        static member NormWeight_ :
            (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
            _.NormWeight,
            (fun mm dr -> { dr with NormWeight = mm })

        static member NormBSA_ :
            (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
            _.NormBSA,
            (fun mm dr -> { dr with NormBSA = mm })

        static member Abs_ :
            (DoseRange -> MinMax) * (MinMax -> DoseRange -> DoseRange) =
            _.Abs,
            (fun mm dr -> { dr with Abs = mm })

        static member AbsWeight_ :
            (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
            _.AbsWeight,
            (fun mm dr -> { dr with AbsWeight = mm })

        static member AbsBSA_ :
            (DoseRange -> MinMax * Unit) * (MinMax * Unit -> DoseRange -> DoseRange) =
            _.AbsBSA,
            (fun mm dr -> { dr with AbsBSA = mm })


    /// Dosage, DoseRanges for different dosage types.
    type Dosage =
        {
            /// Identifies the substance
            Name : string
            /// Dosage at the start
            StartDosage : DoseRange
            /// Dosage per administration
            SingleDosage : DoseRange
            /// Dosage rate
            RateDosage : DoseRange * RateUnit
            /// Total dosage per time period
            TotalDosage : DoseRange * Frequency
            /// List of original DoseRules
            Rules : Rule list
        }
        static member Name_ :
            (Dosage -> string) * (string -> Dosage -> Dosage) =
            _.Name,
            (fun s d -> { d with Name = s })

        static member StartDosage_ :
            (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
            _.StartDosage,
            (fun dr d -> { d with StartDosage = dr })

        static member SingleDosage_ :
            (Dosage -> DoseRange) * (DoseRange -> Dosage -> Dosage) =
            _.SingleDosage,
            (fun dr d -> { d with SingleDosage = dr })

        static member RateDosage_ :
            (Dosage -> DoseRange * RateUnit) * (DoseRange * RateUnit -> Dosage -> Dosage) =
            _.RateDosage,
            (fun dr d -> { d with RateDosage = dr })

        static member TotalDosage_ :
            (Dosage -> DoseRange * Frequency) * (DoseRange * Frequency -> Dosage -> Dosage) =
            _.TotalDosage,
            (fun dt d -> { d with TotalDosage = dt })

        static member Rules_ :
            (Dosage -> Rule list) * (Rule list -> Dosage -> Dosage) =
            _.Rules,
            (fun rs d -> { d with Rules = rs })
    /// The frequencies of a TotalDosage
    and Frequency =
        {
            /// The list of possible frequencies
            Frequencies : Frequencies
            /// The TimeUnit to use for the frequencies
            TimeUnit : TimeUnit
            /// The minimal interval between frequencies
            MinimalInterval : ValueUnit Option
        }
        static member Frequencies_ :
            (Frequency -> Frequencies) * (Frequencies -> Frequency -> Frequency) =
            _.Frequencies,
            (fun frs fr -> { fr with Frequencies = frs })

        static member TimeUnit_ :
            (Frequency -> Unit) * (Unit -> Frequency -> Frequency) =
            _.TimeUnit,
            (fun tu fr -> { fr with TimeUnit = tu })

        static member MinimalInterval_ :
            (Frequency -> ValueUnit Option) * (ValueUnit Option -> Frequency -> Frequency) =
            _.MinimalInterval,
            (fun mi fr -> { fr with MinimalInterval = mi })

    and Frequencies = BigRational list

    and TimeUnit = Unit

    and RateUnit = Unit
    /// The original Z-Index or PedForm rule
    and Rule = GStandRule of string | PedFormRule of string


    /// A patient dosage, a patient category with
    /// associated shape an substance dosages.
    type PatientDosage =
        {
            /// The patient group the doserules applies
            Patient : PatientCategory
            /// A Dosage for the Shape, note this only applies
            /// when there is only one shape and one generic product
            /// TODO make this optional?
            ShapeDosage : Dosage
            /// The Dosages for the Substances
            SubstanceDosages : Dosage list
        }
        static member Patient_ :
            (PatientDosage -> PatientCategory) * (PatientCategory -> PatientDosage -> PatientDosage) =
            _.Patient,
            (fun pat pd -> { pd with Patient = pat })

        static member ShapeDosage_ :
            (PatientDosage -> Dosage) * (Dosage -> PatientDosage -> PatientDosage) =
            _.ShapeDosage,
            (fun sd pd -> { pd with ShapeDosage = sd })

        static member SubstanceDosages_ :
            (PatientDosage -> Dosage list) * (Dosage list -> PatientDosage -> PatientDosage) =
            _.SubstanceDosages,
            (fun d sd -> { sd with SubstanceDosages = d })


    /// A trade product id and label for a ShapeDosage
    type TradeProductLabel =
        { HPK : int; Label : string }
        static member HPK_ :
            (TradeProductLabel -> int) * (int -> TradeProductLabel -> TradeProductLabel) =
            _.HPK,
            (fun hpk tp -> { tp with HPK = hpk })


        static member Label_ :
            (TradeProductLabel -> string) * (string -> TradeProductLabel -> TradeProductLabel) =
            _.Label,
            (fun lbl tp -> { tp with Label = lbl })


    /// A generic product id and label for a ShapeDosage
    type GenericProductLabel =
        { GPK : int; Label : string }
        static member GPK_ :
            (GenericProductLabel -> int) * (int -> GenericProductLabel -> GenericProductLabel) =
            _.GPK,
            (fun hpk tp -> { tp with GPK = hpk })


        static member Label_ :
            (GenericProductLabel -> string) * (string -> GenericProductLabel -> GenericProductLabel) =
            _.Label,
            (fun lbl tp -> { tp with Label = lbl })


    /// A ShapeDosage the shapes that can be given
    /// by a route and associated GenericProducts,
    /// TradeProducts and PatientDosages.
    type ShapeDosage =
        {
            /// Name of the shape the doserule applies to
            Shape : string list
            /// TradeProducts the doserule applies to
            TradeProducts : TradeProductLabel list
            /// GenericProducts the doserule applies to
            GenericProducts : GenericProductLabel list
            /// Patients to which the doserule applies to
            PatientDosages : PatientDosage list
        }

        static member Shape_ :
            (ShapeDosage -> string list) * (string list -> ShapeDosage -> ShapeDosage) =
            _.Shape,
            (fun s rd -> { rd with Shape = s })

        static member TradeProducts_ :
            (ShapeDosage -> TradeProductLabel list) * (TradeProductLabel list -> ShapeDosage -> ShapeDosage) =
            _.TradeProducts,
            (fun tps sd -> { sd with TradeProducts = tps |> List.distinct })

        static member GenericProducts_ :
            (ShapeDosage -> GenericProductLabel list) * (GenericProductLabel list -> ShapeDosage -> ShapeDosage) =
            _.GenericProducts,
            (fun tps sd -> { sd with GenericProducts = tps |> List.distinct })

        static member PatientDosages_ :
            (ShapeDosage -> PatientDosage list) * (PatientDosage list -> ShapeDosage -> ShapeDosage) =
            _.PatientDosages,
            (fun pdl rd -> { rd with PatientDosages = pdl })


    /// A RouteDosage the administration routes
    /// and associated ShapeDosages.
    type RouteDosage =
        {
            /// Administration route
            Route : string
            /// The dosage rules per shape
            ShapeDosages : ShapeDosage list
        }
        static member Route_ :
            (RouteDosage -> string) * (string -> RouteDosage -> RouteDosage) =
            _.Route,
            (fun s rd -> { rd with Route = s })

        static member ShapeDosages_ :
            (RouteDosage -> ShapeDosage list) * (ShapeDosage list -> RouteDosage -> RouteDosage) =
            _.ShapeDosages ,
            (fun pdl rd -> { rd with ShapeDosages = pdl })


    type IndicationDosage =
        {
            // The indication(-s) the dose rule applies to
            Indications : string list
            // The dosage rules per administration route
            RouteDosages : RouteDosage list
        }
        static member Indications_ :
            (IndicationDosage -> string list) * (string list -> IndicationDosage -> IndicationDosage) =
            _.Indications,
            (fun sl inds -> { inds with Indications = sl })

        static member RouteDosages_ :
            (IndicationDosage -> RouteDosage list) * (RouteDosage list -> IndicationDosage -> IndicationDosage) =
            _.RouteDosages,
            (fun rdl inds -> { inds with RouteDosages = rdl })


    /// A DoseRule for a generic. The DoseRule applies to
    /// a generic and has a list of IndicationDosages.
    /// The full hierarchy is:
    /// DoseRule -> IndicationDosage -> RouteDosage -> ShapeDosage -> PatientDosage
    /// A PatientDosage has a ShapeDosages and/or a list of SubstanceDosages.
    type DoseRule =
        {
            /// Generic the DoseRule applies to
            Generic : string
            /// List of synonyms for the generic
            Synonyms : string list
            /// The ATC code
            ATC : string
            /// ATCTherapyGroup the DoseRule applies to
            ATCTherapyGroup : string
            /// ATCTherapySubGroup the DoseRule applies to
            ATCTherapySubGroup : string
            /// The generic group the DoseRule applies to
            GenericGroup : string
            /// The generic subgroup the DoseRule applies to
            GenericSubGroup : string
            /// The DoseRules per indication(-s), i.e. IndicationDosages
            IndicationsDosages : IndicationDosage list
        }
        static member Generic_ :
            (DoseRule -> string) * (string -> DoseRule -> DoseRule) =
            _.Generic,
            (fun s dr -> { dr with Generic = s })

        static member Synonyms_ :
            (DoseRule -> string list) * (string list -> DoseRule -> DoseRule) =
            _.Synonyms,
            (fun sns dr -> { dr with Synonyms = sns |> List.distinct })


        static member IndicationDosages_ :
            (DoseRule -> IndicationDosage list) * (IndicationDosage list -> DoseRule -> DoseRule) =
            _.IndicationsDosages,
            (fun inds dr -> { dr with IndicationsDosages = inds })


    /// Maps the DosageTypes
    type DoseMapping =
        | Norm
        | Abs
        | NormKg
        | AbsKg
        | NormM2
        | AbsM2


    /// Maps Units to Z-Index units
    /// or MetaVision units.
    type UnitMapping =
        {
            ZIndexLong : string
            ZIndexShort : string
            MetaVision : string
            Unit : Unit
        }


    /// Maps Frequencies to Z-Index units
    /// or MetaVision units.
    type FrequencyMapping =
        {
            ZIndex : string
            ZIndexFreq : BigRational
            ZIndexUnit : string
            MetaVision1 : string
            MetaVision2 : string
            Frequency : BigRational
            Unit : Unit
        }


    /// Can be used to create a DoseType.
    type CreateConfig =
        {
            GPKs : int list
            IsRate : bool
            SubstanceUnit : Unit Option
            TimeUnit : Unit Option
        }


