# Revision history for type-of-html

## 1.4.0.0  -- 2018-05-01

* add fixity for >
* remove ghc8.0.2 compatibility for perf reasons
* remove CPP
* error early on functions as children

## 1.3.4.0  -- 2018-04-03

* reduce allocations
* add compilation benchmarks
* small bug fixes

## 1.3.3.2  -- 2018-03-25

* reduce allocations for smaller pages

## 1.3.3.0  -- 2018-02-07

* fix the 'input' element

## 1.3.2.2  -- 2018-01-29

* ghc 8.4 compatibility
* better allocation strategy
* allow more lists

## 1.3.1.0  -- 2017-12-28

* internal cleanup
* make constraints more concise

## 1.3.0.0  -- 2017-12-12

* add Either
* add Maybe
* internal refactor
* cleaner api of the Convert class

## 1.2.0.0  -- 2017-11-28

* remove argument from boolean attributes

## 1.1.0.1  -- 2017-11-17

* factor out CPP

## 1.1.0.0  -- 2017-11-04

* add support for ghc 802
* simplify types
* set up ci

## 1.0.1.0  -- 2017-10-29

* export Document
* use double conversion

## 1.0.0.0  -- 2017-09-18

* perf increase
* more compile time optimizations
* more test cases
* more Convert instances

## 0.5.1.0  -- 2017-09-13

* perf increase
* better compile times

## 0.5.0.0  -- 2017-09-12

* type attributes
* don't allow invalid attributes
* perf increase
* better compile times

## 0.4.2.0  -- 2017-09-11

* don't remove omittable tags
* simplify internals

## 0.4.0.0  -- 2017-09-09

* new api: attributes are now monoids
* builder based: higher perf

## 0.3.0.0  -- 2017-09-05

* Overhaul of api
* Monomorphic render functions
* Remove inefficient builders
* Predifined attributes
* Single polymorphic implementation for all renderers

## 0.2.1.0  -- 2017-08-04

* Escape strings

## 0.2.0.0  -- 2017-07-30

* Full implementation

## 0.1.0.0  -- 2017-07-09

* First draft
