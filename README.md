guile-toxcore
=============

Guile Scheme bindings for [libtoxcore](https://github.com/irungentoo/toxcore).

Usage
-----

There are 2 ways to use guile-toxcore.  The `(tox)` module exports a
high-level API that performs error checking and abstracts away the handling of
foreign data types and pointers.  This is almost certainly the API that you
want to use.  However, if you desire to build up your own abstractions on top
of the low-level C bindings or the high level bindings do not do what you
want, use the `(tox bindings)` module.

To use the high-level API:

```
(use-modules (tox))

(with-tox (make-tox)
  ;; TODO: Call your friend to ask if they've read their SICP today.
  (display "Hello, Tox!\n"))
```

To use the low-level API:

```
(use-modules (tox bindings))

(define tox (tox-new 1))

(tox-kill tox)
```

Dependencies
------------

* libtoxcore
* GNU Guile >= 2.0.5

Install
-------

```
./autogen.sh &&
./configure &&
make &&
sudo make install
```

License
-------

Copyright © 2014 David Thompson <davet@gnu.org>

guile-toxcore is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

guile-toxcore is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.
