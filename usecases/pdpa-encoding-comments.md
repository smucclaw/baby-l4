# Comments on the encoding process of PDPA
Part VIA - Notification of Data Breaches & Personal Data Protection (Notification of Data Breaches) Regulations 2021

## General comments about class declarations
1. To think about what kinds of legal concepts should be packaged as primitive classes, and also when a user should be advised to extend a primitive class instead of creating a new class

2. Also not sure how to articulate the guidelines for grouping legal concepts into a general class with subclasses instead of creating multiple classes. What are the downstream effects on the rule declarations when taking the first approach over the second?

3. Rather unsure as to when I should use characteristic predicates (ie. isNotifiableDataBreach) vs just declaring NotifiableDataBreach and (not NotifiableDataBreach)

4. How to access the attributes in a class declaration? For example:

```
class DataBreach {
    UnauthorisedAccess: Boolean
    UnauthorisedCollection: Boolean
    UnauthorisedUse: Boolean
    UnauthorisedDisclosure: Boolean
    UnauthorisedCopying: Boolean
    UnauthorisedModification: Boolean
    UnauthorisedDisposal: Boolean
    LossofStorageMedium: Boolean
    }

```
Do I access Unauthorised Access with DataBreach.UnauthorisedAccess?

## Other general comments
1. Docstrings like in Python would greatly help documentation for the rule declarations

2. I find that I'm scrolling alot up and down to check whether I've declared a certain class to represent a legal concept when writing the rules. Is there a way to "automatically" create the classes on the go when the user declares a rule below? And later he can edit it

3. Can I just declare a variable without a corresponding class? For example, s26C states that *This section applies to a data breach that occurs on or after the date of commencement of section 13 of the PDPA.*. So can I declare the date of commencement as (example syntax):

```
DateofCommencement: 13062019
```

4. Is there a way to represent the direction of an obligation? i.e. Whether the intermediary or the organisation is obliged to conduct an assessment

Also who needs to send a notification to whom, e.g. from the data intermediary to the organisation.

Would be helpful in cases such as s26C(3): *Where a data intermediary has reason to believe that a data breach has occurred in relation to personal data that the data intermediary is processing on behalf of and for the purposes of another organisation:
(a) the data intermediary must notify that other organisation of the occurrence of the data breach; and
(b) that other organisation, must upon notification by the data intermediary conduct an assessment of whether the data breach is a notifiable data breach.*

The current formulation is this:
```
# s26C(3)(a): Duty for data intermediary to inform other organisation
rule <s26c_3_a_applies>
for di: DataIntermediary, rb: ReasonToBelieve, db: DataBreach, pa: PublicAgency, s26c_1_applies: Boolean
if (    
    dataintermediary di && reasontobelieve rb && databreach db && (not PublicAgency) && s26c_1_applies
)
then NotificationofOtherOrg


# s26C(3)(b): Duty for other organisation to conduct assessment
rule <s26c_3_b_applies>
if s26c_3_a_applies
then AssessmentOfDataBreach
```
But not sure whether it fully captures the meaning of the section; ```AssessmentofDataBreach``` does not explicitly refer to the other organisation. 

Being able to express direction of obligation would also be useful for contracts too; for example party A represents that X is true, as long as party B continues to do Y.

## Specific comments on class declarations

### What information constitutes data

1.
```
class DataBreach  extends Event {
    UnauthorisedAccess: Boolean
    UnauthorisedCollection: Boolean
    UnauthorisedUse: Boolean
    UnauthorisedDisclosure: Boolean
    UnauthorisedCopying: Boolean
    UnauthorisedModification: Boolean
    UnauthorisedDisposal: Boolean
    LossofStorageMedium: Boolean
}
```
2.
```class UnauthorizedAccessBreach extends DataBreach```

MS: alternative?
```
isDataBreach: Event -> Boolean
isUnauthorizedAccessBreach: Event -> Boolean

for x: Event
if isDataBreach x
then isUnauthorizedAccessBreach
```

### "Reasonability concepts"

1. 
```
class LikelytoOccur
```
To be written as predicate? 

MS: we should try to make it a subclass of a general notion, like Event or Action

2.
```
class DataBreachRelatesTo
```
Probably need to clarify what "relates to" entails exactly, does it just mean "if X is true"?

3.
```
class Reasonable
```
Maybe this should be a primitive class, since there are alot of such "reasonable" qualifiers in law

4.
```
class SignificantHarmToIndividual  {
    FullName: Boolean
    IDNumber: Boolean
    PersonalDataInPart1ofScheduleSubjectToPart2ofSchedule: Boolean
    AccountIdentifier: Boolean
    PasswordtoAccount: Boolean
}
```
MS: isn't that rather the individual and not the harm to the individual? Significantharm should be a relation: 

```
decl significantharm: DataBreach -> IndividualData -> Boolean
```

You're right, the key linking statement is that it is significant harm to individual if "data breach relates to" etc...

But conceptually, the attributes don't belong to the individual, but relate to the individual's data.

I have formalised it as such:

```
class IndividualData  {
    FullName: Boolean
    IDNumber: Boolean
    PersonalDataInPart1ofScheduleSubjectToPart2ofSchedule: Boolean
    AccountIdentifier: Boolean
    PasswordtoAccount: Boolean
}
```

### Actions taken by parties involved

1. Created new class to account for a direction / instruction given by an organisation to another organisation

```class DirectionBy```

### Notification requirements
1. How to account for r5(4), which specifies that notification to commission must be in a particular format

### Time classes and misc
1. Would be good to have an comparison operator that signifies whether an event happens before or after another event (like > operator for ints)

MS: the missing notion of time is one of the big problems of current L4. 

2. 
```
PriorTo
```
MS: "prior to" looks like a relation between two events / time points and should not be a class.

I would replace PriorTo with a comparison operator for time, but since we don't have it, just retain for now?

## Declarations
1. Good to have some documentation on when something should be formalised as a relation vs a class declaration. (See the point on significant harm above).

### s26D: Duty to notify occurrence of data breach
1. 
```
decl actiontaken: ActionTaken -> Boolean
decl prescribedrequirements: PrescribedRequirements -> Boolean
decl technologicalmeasure: TechnologicalMeasure -> Boolean
```

Maybe these functions that convert the classes to booleans can be part of the local scope of the general rule? (ie. defined within the rule called s26D)

2. MS: The following are just rulenames and do not have to be declared -- unless one wants to reason about applicability of rules
```
decl s26d_1_applies: NotifiableDataBreach -> NotificationtoCommission
decl s26d_5_a_applies: NotifiableDataBreach -> ActionTaken -> PrescribedRequirements -> SignificantHarmToIndividual -> NotificationtoCommissionWithoutNotificationToIndividual
decl s26d_5_b_applies: NotifiableDataBreach -> TechnologicalMeasure -> PriorTo -> SignificantHarmToIndividual -> NotificationtoCommissionWithoutNotificationToIndividual
```

So for rules there is no explicit "type declaration" needed?

## Rules

### s26B - Notifiable Data Breach

#### s26B(1)

1. 
```
rule <s26b_1_applies>
for db: DataBreach, indiv: IndividualData
if significantHarm db indiv || isOfSignificantScale db
then isNotifiableDataBreach db
```

M: ```isNotifiableDataBreach``` is the characteristic predicate of ```NotifiableDataBreach```. The question is whether in the end, we will keep the class ```NotifiableDataBreach``` at all

Another formulation of rule s26b_1 that uses on the applicability of the other rules?

```
rule <s26b_1_applies>
for s26b_2_applies: Boolean, s26b_3_applies: Boolean, s26b_4_applies: Boolean
if (
    (s26b_2_applies || s26b_3_applies) && (not s26b_4_applies)
)
then NotificationtoCommission
```

#### s26B(2) - Significant Harm
MS: not clear what "without limiting subsection (1)(a)" means here. Rule still in a messy state ...

My interpretation is that "without limiting" means that there could be other conditions (not currently specified in the legislation) that could also constitute significant harm under ss(1)(a). Perhaps we need another ```class OtherConditions```?

### s26C: Duty to Conduct Assessment of Breach

#### s26C(1)

Draft formulation, given that we cannot currently represent time

Applies to data breach that occurs on or after date of commencement of s13 of PDPA. \>= here is used to represent "on or after"

```
rule <s26c_1_applies>
if (
    DateofDataBreach >= Commencementofs13ofPDPA
 )
then True
```

#### s26C(4)

Organisation must carry out the assessment mentioned in ss (2) or (3)(b) in accordance with the prescribed requirements

Can this be represented as the attribute of class AssessmentOfDataBreach? Or is it another issue of currently not being able to represent state in L4?

### s26D: Duty to Notify Occurrence of Notifiable Data Breach

#### s26D(1)

Text from the legislation: *Where an organisation assesses that a data breach is a notifiable data breach, the organisation must notify the Commission as soon as practicable, but in any case no later than 3 days after the day the organisation makes the assessment.*

How to represent the following when it is a requirement instead of a predicate: "no later than 3 days after the day that the organisation makes the assessment"?
Also how to represent "less than 3 days" and "best of knowledge and belief"?

Proposed formulation: Define another construct that means that the organisation is in breach of the PDPA and use the else branch? Example below:
```
decl bestofknowledgeandbelief: BestofKnowledgeAndBelief -> Boolean
decl days: Days -> Integer -> Boolean

class InBreachofPDPA

rule <s26d_1_applies>
for ndb: NotifiableDataBreach, bfb: BestofKnowledgeAndBelief, d: Days
if (
    notifiabledatabreach ndb && bestofknowledgeandbelief bfb && days 3
)
then NotificationtoCommission


rule <s26d_1_else_in_breach>
for ndb: NotifiableDataBreach, bfb: BestofKnowledgeAndBelief, d: Days
if (
    notifiabledatabreach ndb && (not (bestofknowledgeandbelief bfb) || not (days 3))
)
then InBreachOfPDPA
```

#### s26D(2)

Should we allow "if then else" statements:

```
rule <s26d_2_applies>
for ndb: NotifiableDataBreach, 
    s26d_5_a_applies: Boolean, s26d_5_b_applies: Boolean, 
    s26d_6_a_applies: Boolean, s26d_6_b_applies: Boolean, s26d_7_applies: Boolean
if (
    notifiabledatabreach ndb && (s26d_5_a_applies || s26d_5_b_applies || s26d_6_a_applies || s26d_6_b_applies || s26d_7_applies)
)
then NotificationtoCommissionWithoutNotificationToIndividual
# else NotificationtoCommission
```

MS: The else-case is spelled out below

```
rule <s26d_2_applies_else>
for ndb: NotifiableDataBreach, 
    s26d_5_a_applies: Boolean, s26d_5_b_applies: Boolean, 
    s26d_6_a_applies: Boolean, s26d_6_b_applies: Boolean, s26d_7_applies: Boolean
if not (
    notifiabledatabreach ndb && (s26d_5_a_applies || s26d_5_b_applies || s26d_6_a_applies || s26d_6_b_applies || s26d_7_applies)
)
then NotificationtoCommission
```

In that case, do we need to add a predicate that if (not s26d_2_applies) then NotificationtoCommission in the rule <\s26d_2_applies_else> so that the rules don't conflict?


#### s26D(8)

Text from the legislation: *An organisation is not, by reason only of notifying the Commission under subsection (1) or an affected individual under subsection (2), to be regarded as being in breach of -
(a) any duty or obligation under any written law ...
(b) any rule of professional conduct ...*

Not too sure how to express the concept "is not, by reason only of notifiying the Commission to be regarded in breach of any duty or obligation ..."

Seems to me that this implies that there is a not exhaustive list of predicates that could result in the rule being applied. I.e. There could be other reasons in which the organisation would be in breach of any duty or obligation.

Current formulation (which assumes that there are exhaustive conditions):

```
rule <s26d_8_a_applies>
for s26d_1_applies: Boolean, s26d_2_applies: Boolean
if (
    s26d_1_applies || s26b_2_applies
)
then (not ObligationUnderLawNotToDisclose)


# s26D(8)(b) - Organisation not in breach of professional conduct
rule <s26d_8_b_applies>
for s26d_1_applies: Boolean, s26d_2_applies: Boolean
if (
    s26d_1_applies || s26d_2_applies
)
then (not RuleOfProfessionalConduct)
```
