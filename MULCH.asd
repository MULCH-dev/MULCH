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

;;;; MULCH.asd

(asdf:defsystem #:mulch
  :serial t
  :depends-on (#:usocket #:ironclad) ;Once I find a good time library, or how to work with CL's internal time stuff, I'll probably add that here
  :components ((:file "package")
               (:file "mulch")))

