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
(defstruct locale key name light north east south west northeast southwest northwest southeast players objects) 
;Key identifies it for me.
;Name is the name associated with it in-game, so while key could be "living-room-1", name would be "Fred Foobar's Living Room"
;! Put this in a hash table, just like users! 
(defparameter *rooms* (make-hash-table))

;Create an analogous objects-at
(defun players-at ()
  (let ((player-list ()))
    (labels ((players-at-indiv (room vals)
	       (maphash #'(lambda (players-i vals) 
			    (if (equalp (player-location players-i) (locale-name room))
				(cons players-i player-list))) *users*)
	       (setf (locale-players room) player-list)))
      (maphash #'players-at-indiv *rooms*))))
;;Maybe a hash table wasn't the *best* idea. I don't seem to be doing anything with the values
      
(defstruct player 
  name 
  password 
  gender 
  species 
  char-class 
  (health 500) 
  (mana 100)
  (level 0)
  (experience 0)
  stream 
  (location *starting-location*)  
  (saved-location *starting-location*)
  conviction 
  charisma 
  credibility 
  strength 
  dexterity 
  (inventory ()))
;Objects-at will probably have a cleaner design--I'll make an alist of objects and their locations, so it'll be easier to go through it...
;Also, write a function that dolists through the players and if their experience is above, say, 100, then incf their level and setf the experience to their experience minus 100. Have this running at all times.

(defun find-player-from-stream (stream)
  (labels ((find-player-from-stream-aux (players-i vals)
	     (if (equalp (player-stream players-i) stream)
		 players-i)))
    (maphash #'find-player-from-stream-aux *users*)))



