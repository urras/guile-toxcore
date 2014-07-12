guile-toxcore
=============

Guile Scheme bindings for [libtoxcore](https://github.com/irungentoo/toxcore).

Usage
-----

```
(use-modules (tox))

(define tox (tox-new))

;; TODO: Call your friend to ask if they've read their SICP today.

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
