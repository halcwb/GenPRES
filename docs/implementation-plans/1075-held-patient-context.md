# Implementation plan for issue #1075

Hold the patient context from the first new or changed order to the sign, so that every order a
signed version adds rests on one patient context. The use case is
[UC-12](../scenarios/integration/uc-12-held-patient-context.md); this implementation plan builds
it.

Builds on [the two patient modes](976-two-patient-modes.md), which holds an identified
patient's age from the open to the sign, and on the idle end of a Session (#1061).

- [Problem description](#problem-description)
- [What the code does today](#what-the-code-does-today)
- [Approaches considered](#approaches-considered)
- [Chosen approach](#chosen-approach)
- [Confidence](#confidence)
- [Steps](#steps)
- [Verification, per step](#verification-per-step)
- [To settle in review](#to-settle-in-review)
- [Related, not a member](#related-not-a-member)

## Problem description

A change to the patient context after an order is on the order plan re-evaluates the order
plan's totals and leaves each order as it was computed. The order plan's patient, its totals and
the signed version's patient data follow the edit; nothing compares them with the patient each
order was computed on. So a User can sign orders composed on different data, and the version
says they rest on the last of it.

## What the code does today

- **Client.** `updatePatient` in `App.fs` sends `PatientChanged` to the order context and the
  order plan. `OrderPlanMachine.step` puts the new patient on the order plan and asks
  `Recalculate`; the contexts keep the patient they were created with. `PlanWork`
  (`PlanWorkPolicy.fs`) counts the commands that changed the order plan since the version last
  opened or signed, for the guard that asks before leaving the page; it does not say which
  contexts are new or changed.
- **Server.** Every context of the order plan carries the patient it was computed on
  (`PlanContext.Context.Patient`). `Session.challenge` and `Session.commit` check the Session,
  the Role, the token, the head and the challenge, and refuse an order plan that names an order
  twice; nothing compares a context's patient with the order plan's. The Server keeps nothing of
  the order plan between requests.
- **Data notice.** When the EHR reads other data at the sign, the accept sets the draft to the
  notice's data and challenges over it, orders or not.
- **Age.** An identified patient's age is held by the Server from the open to the sign and put
  on every request; the Client's age does not count.

## Approaches considered

### Where the hold lives

**Chosen: derived from the order plan, by the Client for the panel and by the Server at the
signature.** Only the order plan is signed, as a version. A new or changed context is one the
version last opened or signed does not hold as it is. The Client knows that version and the
order plan; the Server gets both at the challenge and the commit, the head from its store and
the order plan from the request.

- *A row when the first new or changed order arrives*: rejected. The Server would keep a fact
  about the order plan it otherwise never keeps, and still has to check the order plan at the
  signature, since the row cannot say what the order plan holds.
- *In the Client alone*: rejected. The Server signs; a Client that does not lock, or an old
  Client, must not get a version with mixed contexts past it.

### What the Server checks

**Chosen: every new or changed context carries the order plan's patient context**, compared on
the data the rules read. Contexts the head holds unchanged are exempt: they are part of a signed
version, computed on the data it was signed on.

- *Every context, the head's included*: rejected for now. A head's orders were signed on the
  data of that sign; refusing them would refuse every sign after a weight change until the
  replacement of #672 exists.
- *Against the Session's patient instead of the order plan's*: the order plan's patient is what
  the version is signed on, and the Session's is already put on the order plan's age; comparing
  with the order plan's keeps the check a function of the request and the head alone.

### The data notice under the hold

**Chosen: signed on the held context, recorded as not verified.** The notice is shown as today;
while the context is held the accept does not replace the draft, and the version records that
the data it rests on is not the EHR's reading. The new data applies once the context is
released.

- *The accept replaces the draft, as today*: rejected; it changes the context under the new and
  changed orders, which is the problem.
- *Refuse the sign until the User drops the new and changed orders*: rejected; the EHR moving
  must not make a correct order plan unsignable.

### The age

**Chosen: unchanged.** The #976 rule holds the age from the open to the sign whatever the order
plan holds. Letting it follow the clock while released would change a rule that just landed, for
a case the idle end of #1061 already bounds.

## Chosen approach

- **Hold** (Client.Core): the order plan state keeps the contexts of the version last opened or
  signed; `held` is whether the order plan has a context that version does not hold as it is.
  Pure, tested under Expecto.
- **Panel** (Client): the panel is read-only while held, with a notice naming the three ways
  out: sign, remove the new and changed orders, refresh. `updatePatient` ignores an edit while
  held, as a second guard.
- **Check** (Server): at the challenge and the commit, an order plan whose new or changed
  contexts carry another patient context than the order plan's is refused `ContextDiffers`, a
  new `SigningRefusal`. The comparison is on the data the rules read.
- **Data notice**: while held, the accept keeps the draft and the version is signed on it as not
  verified.
- **Refresh**: a Session command that reads the EHR again, projects it at the clock and makes it
  the Session's patient; the Client asks first and drops the new and changed orders.

## Confidence

Medium. The check at the signature is a pure function of the order plan and the head and carries
the safety of the change. The Client's hold needs the order plan state to keep the version it
opened or signed, which it does not today. The refresh adds a Session command and a second place
the EHR is read.

## Steps

One pull request per step unless the step says two. Everything outside
`src/Informedica.GenPRES.Client/` is first a script, then a migration.

1. **The hold (script).** `src/Informedica.GenPRES.Client.Core/Scripts/HeldContext.fsx`: the
   order plan state keeps the contexts of the version last opened or signed; `changed` names the
   contexts that version does not hold as it is; `held` is whether any are left.

   Tests: a launch on a head is released; an added order holds; a changed head order holds;
   removing every new and changed order releases; a sign releases; a version opened releases;
   removing a head order alone does not hold.

2. **The hold migrated** into `OrderPlanMachine.fs` and a policy module beside
   `PlanWorkPolicy.fs`, tests in `Informedica.GenPRES.Client.Core.Tests`.

3. **The panel.** `Views/Patient.fs` read-only while held, with a notice and its three actions;
   the leave-page guard unchanged. `updatePatient` ignores an edit while held. New terms for the
   notice and the actions.

4. **The check (script).** `src/Informedica.GenPRES.Server/Scripts/HeldContext.fsx`: the patient
   context of each context of the order plan compared with the order plan's on the data the
   rules read; `changed` against the head; `challenge` and `commit` shadowed to refuse
   `ContextDiffers` before the challenge is issued and before the PIN is checked.

   Tests over the order plan scenarios: an order plan whose orders share the order plan's
   context passes; a new order on another weight, department or renal function is refused at
   both; a head order on an older context passes; an order plan without a head, every context
   new, is checked whole; a refusal costs no PIN attempt.

5. **The check migrated.** `SigningRefusal.ContextDiffers` into `GenPRES.Shared`, the check
   into `ServerApi.Session.fs`, the refusal's sentence into `SigningPolicy.fs`; tests in both
   test projects.

6. **The data notice under the hold (script, then migration; two pull requests).** The accept
   keeps the draft while held; the challenge over an accepted notice signs the held context as
   not verified.

   Tests: a notice accepted while held leaves the draft; the version is signed on it with
   `Verified` false; a notice accepted while released replaces the draft as today.

7. **Refresh (script, then migration; two pull requests).** A Session command that reads the EHR
   again, projects it at the clock with the user's measurements over it, and returns the
   Session's patient with a fresh token; the Client asks first, drops the new and changed orders
   and reopens the head on the new patient. Closes #1075.

   Tests: a refresh reads the EHR once and projects at the clock; the age is the one at the
   refresh; the measurements stand; a refresh with no EHR data leaves the patient as it was.

## Verification, per step

- Script steps: `dotnet fsi` on the script, checking that Expecto reports `Status: Ok`; the
  script stays in the repository.
- Migration steps: `dotnet run build`, `dotnet run servertests` and
  `dotnet fsi scripts/CheckDependencyRule.fsx`, each checked for its success line.
- Client steps: `dotnet run clientbuild`, the generated JSX checked for the changed element,
  then the browser, by the user.

| Step | Check |
|------|-------|
| 3 | In the browser: add an order, the panel goes read-only with its notice; remove it, the panel is released; sign, the panel is released. |
| 5 | An order plan with a new order on another weight, sent by hand, is refused `ContextDiffers` at the challenge. |
| 6 | With the stub's EHR data changed after the open and an order on the order plan, the version is signed on the held data, not verified. |
| 7 | A refresh after a change of the stub's EHR data shows the new data and no new or changed orders. |

## To settle in review

- **Which fields.** The data the rules read: weight, height, gestational age, department, renal
  function, access, gender and the age. Confirm, or name the ones to leave out.
- **Which Sessions.** Every Session, identified and anonymous; the url mode without a Session is
  unchanged. Confirm.
- **Re-prescribing on a new context.** Deferred to #672; until then a new bedside weight means
  signing the order plan or dropping its new and changed orders first. Accept the cost, or pull
  #672 in.
- **Recording the difference from the EHR.** The `Verified` flag, or a field of its own on the
  version.
- **Refresh and the measurements.** The measurements the user recorded stand over a refresh, as
  they stand over a sign. Confirm.

## Related, not a member

- **#672** the replacement of a context for new patient data, and a guard at the signature for
  the head's contexts.
- **#976** the age held from the open to the sign; unchanged here.
- **#1061** the idle end of a Session, which bounds a hold left behind.
- **#598** client testing: would let step 3 be tested without the browser.
- **#518** the carry-over of an order plan's changes into a relaunched tab, which would carry
  the hold with it.
