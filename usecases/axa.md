# Remarks about the Axa case study #

## Modelling language (aka L4) ##

### Minor extensions of the existing language ###

* TBD: Support for metric types (time and space). 
  In particular not clear: dimensionality of space
  (1-dimensional: location; ... 2-dimensional: surface / territory; 3-dimesional)
* TBD: For a class C, create predicates isC
* TBD: Enumeration types with generation of axioms of distinctness of its members
* TBD: decide on what to do with the components of classes

### Novel features, major extensions ###

* Definitional / declarative contexts (also see proof contexts below) 
  For example for formalisation of statements like:
  "The cover in this section will only apply if you have paid the premium as
  shown on your policy schedule." 
* Modules, possibly with first-class operations such as module extensions and
  parameterization by modules. Purpose: structuring of contracts with generic
  insurance contracts which can progressively be extended (vehicle contract,
  vehicle breakdown contract etc.), possibly with country-specific adapations
  (mixins). Take inspiration from Haskell type classes, Isabelle axiomatic
  type classes, Scala subtype polymorphism.

## Modelling style ##

* Attempt to separate model into a generic part (could be reused for other,
  similar contracts) and a specific part (for this contract)
* TBD: Might be interesting to explicitly model as a state machine
* Question: should contracts be introduced explicitly as class, with subclassing
  for different types of contracts, such as time-restricted contracts,
  contracts with restricted number of claims per time period 
  ("We will only cover up to two misfuelling claims each year") etc.?
* Explicit notion of contract may also be useful because one client may have
  different contracts with different claims. 


## Limits of modelling ##

### Not made precise in the current model ###

... but that could easily be added:

* More precise modelling of countries, locations etc. 


### Situations the modelling currently cannot capture ###

* Granularity of temporal situations: what if a breakdown event occurs and the
  insurance policy loses validity before a repair action can be taken.

## Questions regading the case study ##

### Unclear wordings or descriptions ###

* Excess: "If you have selected this to apply to your policy, means the amount
  that you must pay": Not clear whether this is a Boolean option (the
  client chooses either an excess policy where certain amounts are at his
  charge, or a full coverage policy where all dammages are reimbursed), or a
  numerical value.

* In SecA.3, unclear associativity:
  "we will recover your vehicle, the driver and passengers to our Authorized
  Operator’s base or home/local repairer within 20 miles." 
  possible readings:
  * (to our Authorized Operator’s base or home/local repairer) within 20 miles.
  * to our Authorized Operator’s base or (home/local repairer within 20 miles). 

* In SecA.4: "We will pass on ...": under which conditions?
  Not modelled in L4.
  
* SecB: Is the coverage not subject to having paid the premium (as for SecA)?

* The word "misfuelling" is not precisely defined, nor what it means that a
  "misfuelling occurs" somewhere/ sometime: for example, does "misfuelling
  occurs in the UK" mean that filling in the wrong type of fuel happens in the
  UK, or that the effect becomes felt in the UK?


## Verification ##

* TBD: introduce local declarations and conditions in the style
  for (decls) if (preconds) then assertion
  
* TBD: local proof contexts for describing complex scenarios locally

