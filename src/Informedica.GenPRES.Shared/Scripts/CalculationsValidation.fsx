// ============================================================
// Calculations Validation Script
//
// Purpose: Validate clinical calculation formulas in
//          Shared/Calculations.fs against published medical
//          reference values.  Run interactively in FSI or via
//          `dotnet fsi CalculationsValidation.fsx` from this
//          directory.
//
// References:
//   BSA – Mosteller (N Engl J Med 1987;317:1098),
//         Du Bois (Arch Intern Med 1916;17:863),
//         Haycock (J Pediatr 1978;93:62),
//         Gehan & George (Cancer Chemother Rep 1970;54:225),
//         Fujimoto (Nihon Eiseigaku Zasshi 1968;23:443)
//   Age – AAP Pediatrics 2004 (PMA); ISMP preterm dosing guide
//   eGFR – CKD-EPI 2021 (Inker, NEJM 2021;385:1737),
//           CKD-EPI 2009 (Levey, Ann Intern Med 2009;150:604),
//           MDRD (Levey, Ann Intern Med 1999;130:461),
//           Schwartz (JASN 2009;20:629)
// ============================================================

#I __SOURCE_DIRECTORY__
#load "load.fsx"

#r "nuget: Expecto, 9.0.4"
#r "nuget: Expecto.FsCheck, 9.0.4"

open System
open Expecto
open Expecto.Flip
open Shared
open Shared.Types
open Shared.Calculations


// ============================================================
// Helpers
// ============================================================

/// Float closeness at ±2 % relative tolerance.
let closePct (msg: string) (expected: float) (actual: float) =
    let tol = abs expected * 0.02
    actual |> Expect.floatClose msg { absolute = tol; relative = 0.02 } expected

/// Float closeness at ±5 % relative tolerance (used for eGFR rounding).
let closePct5 (msg: string) (expected: float) (actual: float) =
    let tol = abs expected * 0.05
    actual |> Expect.floatClose msg { absolute = tol; relative = 0.05 } expected


// ============================================================
// BSA tests
//
// Reference patient: adult male  weight 70 kg  height 170 cm
//                    weight 70 000 g   height 170 cm
//
// Computed reference values (formula definitions applied to 70 kg / 170 cm):
//   Mosteller      ≈ 1.818 m²   sqrt(70 × 170 / 3600)
//   Du Bois        ≈ 1.812 m²   0.007184 × 70^0.425 × 170^0.725
//   Haycock        ≈ 1.825 m²   0.024265 × 70^0.5378 × 170^0.3964
//   Gehan & George ≈ 1.832 m²   0.0235 × 70^0.51456 × 170^0.42246
//   Fujimoto       ≈ 1.765 m²   0.008883 × 70^0.444 × 170^0.663
// ============================================================

let bsaTests =
    let w = 70_000<gram> // 70 kg
    let h = 170<cm>

    testList
        "BSA formulas – adult reference patient (70 kg / 170 cm)"
        [
            test "Mosteller ≈ 1.818 m²" {
                BSA.calcMosteller w h
                |> float
                |> closePct "Mosteller" 1.818
            }

            test "Du Bois ≈ 1.812 m²" {
                BSA.calcDuBois w h
                |> float
                |> closePct "Du Bois" 1.812
            }

            test "Haycock ≈ 1.825 m²" {
                BSA.calcHaycock w h
                |> float
                |> closePct "Haycock" 1.825
            }

            test "Gehan & George ≈ 1.832 m²" {
                BSA.calcGehanAndGeorge w h
                |> float
                |> closePct "Gehan & George" 1.832
            }

            test "Fujimoto ≈ 1.765 m²" {
                BSA.calcFujimoto w h
                |> float
                |> closePct "Fujimoto" 1.765
            }

            // Paediatric spot-check: 10 kg / 75 cm neonate/infant
            // Mosteller: sqrt(10 * 75 / 3600) = sqrt(0.2083) ≈ 0.456 m²
            test "Mosteller paediatric 10 kg / 75 cm ≈ 0.456 m²" {
                BSA.calcMosteller 10_000<gram> 75<cm>
                |> float
                |> closePct "Mosteller paediatric" 0.456
            }
        ]


// ============================================================
// Age tests
// ============================================================

let ageTests =
    testList
        "Age calculations"
        [
            // Preterm born at 28w+2d, now 70 chronological days old
            //   PMA = (28w × 7d + 2d + 70d) / 7 = (196 + 2 + 70) / 7 = 268/7 = 38w (integer)
            test "PMA: 28w+2d GA, 70 chronological days → 38 weeks" {
                Age.postMenstrualAge 28<week> 2<day> 70<day>
                |> Expect.equal "PMA should be 38 weeks" 38<week>
            }

            // Adjusted age: born at 28w+0d, chronological age 112 days
            //   prematurity = 40w – 28w = 12 weeks = 84 days
            //   adjusted age = 112 – 84 = 28 days
            test "Adjusted age: 28w GA, 112 chronological days → 28 days" {
                Age.adjustedAge 28<week> 0<day> 112<day>
                |> Expect.equal "Adjusted age should be 28 days" 28<day>
            }

            // Full-term infant born at 40w, any chronological age → adjusted = chronological
            test "Adjusted age at full term equals chronological age" {
                Age.adjustedAge 40<week> 0<day> 45<day>
                |> Expect.equal "At 40 w adjusted = chronological" 45<day>
            }

            // Chronological age between known dates
            test "Chronological age: Dec 7 2022 → Jan 25 2023 = 49 days" {
                Age.chronologicalAgeDays (DateTime(2022, 12, 7)) (DateTime(2023, 1, 25))
                |> Expect.equal "Should be 49 days" 49<day>
            }

            // Week ↔ day conversions
            test "weeksToDays 2<week> = 14<day>" {
                Age.weeksToDays 2<week>
                |> Expect.equal "2 weeks = 14 days" 14<day>
            }

            test "daysToWeeks 21<day> = 3<week>" {
                Age.daysToWeeks 21<day>
                |> Expect.equal "21 days = 3 weeks" 3<week>
            }
        ]


// ============================================================
// Renal Conversion tests
// ============================================================

let renalConversionTests =
    testList
        "Renal unit conversions"
        [
            // 1 mg/dL = 88.42 µmol/L
            test "creatinine 1.0 mg/dL → 88.42 µmol/L" {
                RenalConversions.creatMgDlToMicroMolL 1.0<mg / dL>
                |> float
                |> closePct "to µmol/L" 88.42
            }

            test "creatinine 88.42 µmol/L → 1.0 mg/dL" {
                RenalConversions.creatMicroMolLToMgDl 88.42<microMol / L>
                |> float
                |> closePct "to mg/dL" 1.0
            }

            // BUN 10 mg/dL = 3.571 mmol/L
            test "urea BUN 10 mg/dL → 3.571 mmol/L" {
                RenalConversions.ureaMgDlToMmolL 10.0<mg / dL>
                |> float
                |> closePct "urea to mmol/L" 3.571
            }
        ]


// ============================================================
// eGFR tests
//
// Reference patients — values computed from formula definitions
// (consistent with NKF online CKD-EPI calculator).
//
// Patient A: Female, age 57 y, creatinine 1.4 mg/dL
//            CKD-EPI 2021:
//              ratio = 1.4/0.7 = 2.0; 2.0^-1.2 ≈ 0.435
//              0.9938^57 ≈ 0.701
//              142 × 0.435 × 0.701 × 1.012 ≈ 44 mL/min/1.73m²
//
// Patient B: Male, age 45 y, creatinine 1.0 mg/dL
//            CKD-EPI 2021:
//              ratio = 1.0/0.9 = 1.111; 1.111^-1.2 ≈ 0.881
//              0.9938^45 ≈ 0.756
//              142 × 0.881 × 0.756 ≈ 94.6 mL/min/1.73m²
//
// Patient C (paediatric): height 140 cm, creatinine 0.6 mg/dL
//            Bedside Schwartz: 0.413 × 140 / 0.6 ≈ 96.3 mL/min/1.73m²
// ============================================================

let egfrTests =
    testList
        "eGFR formulas"
        [
            testList
                "CKD-EPI 2021 (no race coefficient)"
                [
                    test "Female 57 y creatinine 1.4 mg/dL → ≈44 mL/min/1.73m²" {
                        let creat = Creatinine.MgPerDl 1.4<mg / dL>

                        EGfr.ckdEpi2021 Sex.Female 57.0<year> creat
                        |> float
                        |> closePct5 "CKD-EPI 2021 Female" 44.0
                    }

                    test "Male 45 y creatinine 1.0 mg/dL → ≈95 mL/min/1.73m²" {
                        let creat = Creatinine.MgPerDl 1.0<mg / dL>

                        EGfr.ckdEpi2021 Sex.Male 45.0<year> creat
                        |> float
                        |> closePct5 "CKD-EPI 2021 Male" 95.0
                    }
                ]

            testList
                "CKD-EPI 2009"
                [
                    // CKD-EPI 2009 Female Other 57y creat 181 µmol/L:
                    // ratio = (181/88.42) / 0.7 ≈ 2.924; 2.924^-1.209 ≈ 0.273
                    // 0.993^57 ≈ 0.670; 141 × 0.273 × 0.670 × 1.018 ≈ 26 mL/min/1.73m²
                    test "Female Other 57 y creatinine 181 µmol/L → ≈26 mL/min/1.73m²" {
                        let creat = Creatinine.MicroMolPerL 181.0<microMol / L>

                        EGfr.ckdEpi2009 Sex.Female EGfr.Race2009.Other 57.0<year> creat
                        |> float
                        |> closePct5 "CKD-EPI 2009 Female Other" 26.0
                    }
                ]

            testList
                "MDRD 4-variable"
                [
                    // MDRD: 175 × sCr^-1.154 × age^-0.203 × sexFactor × raceFactor
                    // Male Other 45y creat 1.0 mg/dL:
                    // 175 × 1.0^-1.154 × 45^-0.203 ≈ 175 × 1 × 0.462 ≈ 81 mL/min/1.73m²
                    test "Male Other 45 y creatinine 1.0 mg/dL → ≈81 mL/min/1.73m²" {
                        let creat = Creatinine.MgPerDl 1.0<mg / dL>

                        EGfr.mdrd Sex.Male EGfr.Race4v.Other 45.0<year> creat
                        |> float
                        |> closePct5 "MDRD Male" 81.0
                    }
                ]

            testList
                "Bedside Schwartz (paediatric)"
                [
                    // 0.413 × height_cm / sCr_mgdl
                    // height 140 cm, creat 0.6 mg/dL → 0.413 × 140 / 0.6 = 96.3
                    test "height 140 cm creatinine 0.6 mg/dL → ≈96.3 mL/min/1.73m²" {
                        EGfr.schwartz 140.0<cm> (Creatinine.MgPerDl 0.6<mg / dL>)
                        |> float
                        |> closePct "Schwartz" 96.3
                    }
                ]
        ]


// ============================================================
// GFR Classification tests
// ============================================================

let classifyTests =
    testList
        "GFR classification (KDIGO 2012)"
        [
            test "≥90 → Normal" {
                classifyGfr 95.0<mL / minute / normalM2>
                |> Expect.equal "" GfrClassification.Normal
            }

            test "60–89 → MildlyDecreased" {
                classifyGfr 75.0<mL / minute / normalM2>
                |> Expect.equal "" GfrClassification.MildlyDecreased
            }

            test "45–59 → MildToModeratelyDecreased" {
                classifyGfr 52.0<mL / minute / normalM2>
                |> Expect.equal "" GfrClassification.MildToModeratelyDecreased
            }

            test "30–44 → ModerateToSeverelyDecreased" {
                classifyGfr 38.0<mL / minute / normalM2>
                |> Expect.equal "" GfrClassification.ModerateToSeverelyDecreased
            }

            test "15–29 → SeverelyDecreased" {
                classifyGfr 20.0<mL / minute / normalM2>
                |> Expect.equal "" GfrClassification.SeverelyDecreased
            }

            test "<15 → KidneyFailure" {
                classifyGfr 10.0<mL / minute / normalM2>
                |> Expect.equal "" GfrClassification.KidneyFailure
            }
        ]


// ============================================================
// Run all tests
// ============================================================

let allTests =
    testList
        "Shared.Calculations validation"
        [
            bsaTests
            ageTests
            renalConversionTests
            egfrTests
            classifyTests
        ]

runTestsWithCLIArgs [] [||] allTests |> ignore
