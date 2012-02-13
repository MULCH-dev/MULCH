#| This file is part of MULCH.

MULCH is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version. |# #|

MULCH is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
;along with MULCH.  If not, see <http://www.gnu.org/licenses/>.|#

;structs.lisp 

;Rooms, people, objects...
(defstruct locale key name light north east south west northeast southwest northwest southeast players objects)))) 
;Key identifies it for me.
;Name is the name associated with it in-game, so while key could be "living-room-1", name would be "Fred Foobar's Living Room"
;! Put this in a hash table, just like users! 
(defparameter *rooms* (make-hash-table))

;To prevent bugs with players and objects, create a "build-room" function that takes a name, setfs a var to the room, calls players-at and objects-at to define the objects and players....
;;The following chunk of code was something I made naïvely. It won't worķ and for good reason, too.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun players-at-aux (location-name) (defparameter #'(players-at-aux-1 location-name) #'(list nil))) ;This needs an aux function itself due to being a list....Ugly, I know....
;(defun players-at-aux-1 (location-name) (concatenate 'string "players-" location-name))
;(defun players-at (name-room list-players)
;  (dolist (players-i (mapcar #'username-variable *registered-usernames*))
;    (if (equalp (player-location players-i) name-room)
;	(cons players-i (players-at-aux-1 name-room)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun players-at ()
  (labels ((players-at-indiv ()
	     
;Objects-at will probably have a cleaner design--I'll make an alist of objects and their locations, so it'll be easier to go through it...
(defstruct player name password gender species health mana level experience stream location saved-location con cha cre str dex inventory) ;Set some default values for these, maybe change to defclass. ALSO: make int wis str dex cha etc....
;Also, write a function that dolists through the players and if their experience is above, say, 100, then incf their level and setf the experience to their experience minus 100. Have this running at all times.
(defun find-player-from-stream (stream)
  (labels ((find-player-from-stream-aux (players-i)
	     (if (equalp (player-stream players-i) stream)
		 players-i)))
    (maphash #'find-player-from-stream-aux *users*)))
