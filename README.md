# sparse

A library to allow the creation of sparse bit arrays from primitive types.

The theory of Heirarchical Temporal Memory espoused by Jeff Hawkins of
[Numenta](http://numenta.com) and implemented in
[NuPIC](http://github.com/numenta/nupic) calls for the use of sparse arrays of
bits representing all manner of data that can be passed into and out of an
artifical representation of the neocortex. A nascent reimplementation of
this theory in clojure is [clortex](http://github.com/fergalbyrne/clortex).

The intent of this library is to provide a mechanism for creating suitable 
sparse bit arrays, for input into a neo-cortical model, from primitive data types.

The basic principles are that a sparse array has a particular size in bits, always has
a certain number of bits turned on and represents values within two bounds. For a
positive whole number the simplest range approach is for values to be allowed from
zero to a specified maximum.

For example, suppose a sparse array represents a long value, has 21 bits and
5 bits must be set and can deal with a value from zero to 1024. Here
are some example transformations:

<table>
  <tr><td>Value</td><td>Sparse array</td></tr>
  <tr><td>0</td><td>(0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1)</td></tr>
  <tr><td>100</td><td>(0 0 0 0 1 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1)</td></tr>
  <tr><td>567</td><td>(0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0)</td></tr>
  <tr><td>1024</td><td>(0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1)</td></tr>
</table>

## Usage

FIXME

## License

Copyright © 2014 Matt Daley

Distributed under the Eclipse Public License, the same as Clojure.
