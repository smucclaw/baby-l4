# Remarks about the Axa case study

## Modelling language (aka L4) ##

* TBD: Support for metric types (time and space). 
  In particular not clear: dimensionality of space
  (1-dimensional: location; ... 2-dimensional: surface / territory; 3-dimesional)
* TBD: For a class C, create predicates isC
* TBD: Enumeration types with generation of axioms of distinctness of its members
* TBD: decide on what to do with the components of classes


## Modelling style ##

* Attempt to separate model into a generic part (could be reused for other,
  similar contracts) and a specific part (for this contract)
* TBD: Might be interesting to explicitly model as a state machine

## Limits of modelling ##

### Not made precise in the current model

... but that could easily be added:

* More precise modelling of countries, locations etc. 
  Better to be done 

### Situations the modelling currently cannot capture

* Granularity of temporal situations: what if a breakdown event occurs and the
  insurance policy loses validity before a repair action can be taken.

## Questions regading the case study

### Unclear wordings or descriptions

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


## Verification ##
