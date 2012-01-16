;;;; This file is part of MULCH.
;;;;
;;;;MULCH is free software: you can redistribute it and/or modify
;;;;it under the terms of the GNU General Public License as published by
;;;;the Free Software Foundation, either version 3 of the License, or
;;;;(at your option) any later version.
;;;;
;;;;MULCH is distributed in the hope that it will be useful,
;;;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;GNU General Public License for more details.
;;;;
;;;;You should have received a copy of the GNU General Public License
;;;;along with MULCH.  If not, see <http://www.gnu.org/licenses/>.

;;;;io.lisp





;;OK, now I have to make sure I can identify the stream the server is reading from....see next comment and IRC logs
;;This should work with a custom REPL, one that loops through a list of connections. Do that later, call them mulch-read, mulch-eval, mulch-print. Mulch-print will likely use princ. We'll need some custom exception handling for mulch-read so that "(look" won't screw up the system, and to prevent reader macros
;;For the custom REPL--we'll use dolist and set up a list of username-variables, so something like rep-ing on dolist, then looping. Mulch-print will need to have some way of getting a stream....probably hard-wired through the dolist.