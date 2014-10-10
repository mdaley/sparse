# sparse

A library to allow the creation of sparse bit sequences from primitive types.

The theory of Heirarchical Temporal Memory (HTM) espoused by Jeff Hawkins of
[Numenta](http://numenta.com) and implemented in
[NuPIC](http://github.com/numenta/nupic) calls for the use of sparse arrays of
bits representing all manner of data that can be passed into and out of an
artifical representation of the neocortex. A nascent reimplementation of
this theory in clojure is [clortex](http://github.com/fergalbyrne/clortex).

The intent of this library is to provide a mechanism for creating suitable 
sparse bit sequences, for input into a neo-cortical model, from primitive data types.

The basic principles are that a sparse sequence has a particular size in bits, always has
a certain number of bits turned on and represents values within two bounds. For a
positive whole number the simplest range approach is for values to be allowed from
zero to a specified maximum.

For example, suppose a sparse sequence represents a long value, has 21 bits with
5 that must be set, and can encode a value from zero to 1024. Here
are some example transformations of integers into the sparse sequence:

<table>
  <tr><th>Value</th><th>Sparse array</th></tr>
  <tr><td>0</td><td>(0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1)</td></tr>
  <tr><td>100</td><td>(0 0 0 0 1 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1)</td></tr>
  <tr><td>567</td><td>(0 0 0 1 0 0 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0)</td></tr>
  <tr><td>1024</td><td>(0 0 1 0 0 0 0 0 1 0 0 0 1 0 0 0 1 0 0 0 1)</td></tr>
</table>

Note that the bit sequence is always 21 bits in length and always has
5 bits set. As the number to be transformed changes within the range, so
the positions of the set bits change.

The mechanism is such that a small integer range can be transformed into a very
large bit sequence - for example with thousands of bits - so there will
be one particular bit pattern for every input value. However, if the number range
has many more values than the number of bit patterns in a small bit sequence,
each bit pattern will represent many numbers. The intention is to create a pattern
that represents a number but not neccessarily to create an encoding that can be
reversed back to the original input value.

## Usage

Import from [clojars](https://clojars.org/sparse) using:

```clj
[sparse 0.1.2]
```

then:

```clj
(:require [sparse.core :refer [long->sparse]])

...

(long->sparse 1000 17 455 1024)
```

This generates 1000 bit sparse representation with 17 bits set of the number 455 within the
range 0 to 1024.

See the [documentation](http://mdaley.github.io/sparse/docs/uberdoc.html).

## License

Copyright © 2014 Matt Daley

Distributed under the Eclipse Public License, the same as Clojure.
