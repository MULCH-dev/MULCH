#| This file is part of MULCH.

MULCH is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or 
 (at your option) any later version.

MULCH is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with MULCH.  If not, see <http://www.gnu.org/licenses/>.|#

;;;;io.lisp





;;OK, now I have to make sure I can identify the stream the server is reading from....see next comment and IRC logs
;;This should work with a custom REPL, one that loops through a list of connections. Do that later, call them mulch-read, mulch-eval, mulch-print. Mulch-print will likely use princ. We'll need some custom exception handling for mulch-read so that "(look" won't screw up the system, and to prevent reader macros
;;For the custom REPL--we'll use dolist and set up a list of username-variables, so something like rep-ing on dolist, then looping. Mulch-print will need to have some way of getting a stream....probably hard-wired through the dolist.
(defun mulch-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))
(defun mulch-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))
(defparameter user-stream nil "The stream to the current user it's reading from")
(defun mulch-repl ()
  (labels ((poll-repl (users-i) 
	     (if (player-stream users-i)
		 (let ((cmd (mulch-read users-i)))
		   (setf user-stream (player-stream users-i))
		   (mulch-print (eval cmd))))))
    (maphash #'poll-repl *users*))
  (mulch-repl))
;;This alist lists some names to be redirected to appropriate functions. Migrate the "go ..." over here. I'll need to check with mulch-read to see if it should be a string or not. For right now, let's assume it's a symbol
(defparameter *syms* nil)
(acons 'get 'pickup *syms*)

;;Currently, we'll be using regular eval, but it should be replaced once we have a defcommand macro.

;;Now we must create a defcommand macro (in order to simplify the task of limiting certain commands to certain groups of players, e.g. level 40 and above or only Occultists... It will also be used for the basic communication commands: say, tell. We'll need to implement channels with this as well.
;Find some way to create synonyms, e.g. "get" (which is a LISP function already...) for "pickup". This would also reduce redundancy in later, inelegant "moving" code.
(defmacro defcommand (name (&optional level c-class species gender gold city newbie) (&rest args) &body body)
  "Specialized DEFUN for commands to reduce code duplication"
(let ((player (gensym)))
  `(defun ,@name ,args 
     (let ((,player (find-player-from-stream user-stream)))
       (if 
	(and 
	 (or (> (player-gold ,player) ,@gold) (null ,@gold))
	 (or (> (player-level ,player) ,@level) (null ,@level))
	 (or (equalp (player-class ,player) ,@c-class) (null ,@c-class))
	 (or (equalp (player-species ,player) ,@species) (null ,@species))
	 (or (equalp (player-gender ,player) ,@gender) (null ,@gender))
	 (or (equalp (player-city ,player) ,@city) (null ,@city))
	 (or (<= (player-level ,player) 20) (null ,@newbie)))
	,body
	(mulch-print "I do not know that command"))))))

(defun say (&rest words) 
    (dolist (users-i (remove (find-player-from-stream user-stream) (locale-players (player-location (find-player-from-stream user-stream))))))
      (format (player-stream users-i) "~:(~A~) says: ~:(~{~A~^ ~}~)~%" (find-player-from-stream user-stream) words))
(defun tell (player &rest words) 
  (let ((recip-stream (player-stream (username-variable player))))
    (format recip-stream "~:(~A~) tells you: ~:(~{~A~^ ~}~)~%" (find-player-from-stream user-stream) words)))
;;How will I make channels? Maybe I'll make a function for each channel that conses a user to a list of people on the channel if they meet such-and-such condition, and have a channel-say command for each of them such that it prints it to all the streams on the channel? This is enough code reuse that it probably warrants a macro.
(defmacro channel (name level c-class species gender city newbie)
  `(defparameter ,@name nil)
  `(let ((channel-loop (intern (string-upcase (concatenate 'string ,@name "-update")))))
     (defun ,channel-loop ()
       (labels 
	   ((channel-loop (players-i) 
	      (if
	       (and 
		(or (> (player-gold players-i) ,@gold) (null ,@gold))
		(or (> (player-level players-i) ,@level) (null ,@level))
		(or (equalp (player-class players-i) ,@c-class) (null ,@c-class))
		(or (equalp (player-species players-i) ,@species) (null ,@species))
		(or (equalp (player-gender players-i) ,@gender) (null ,@gender))
		(or (equalp (player-city players-i) ,@city) (null ,@city))
		(or (<= (player-level players-i) 20) (null ,@newbie)))
	       (cons (player-name players-i) ,@name))))
	 (maphash #'channel-loop *users*))
       (,channel-loop)))
  `(let ((channel-say (intern (concatenate 'string ,@name "-say"))))
     (defcommand (,channel-say ,@level ,@c-class ,@species ,@gender ,@gold ,@city ,@newbie (&rest words))
	 (dolist (in-channel-i (mapcar #'player-stream ,@name))
	   (format in-channel-i "~:@(~A~) ~:(~A~) says: ~:(~{~A~^ ~}~)~%" ,@name (find-player-from-stream user-stream) words)))))
	      
(defun look ()
  (let ((player-room (player-location (find-player-from-stream user-stream))))
   (princ (locale-description player-room) user-stream)
   ;;I could probably seperate this in a different function to avoid code duplication
   ;;the car of locale-DIRECTION is the room, the cadr the implement, the caddr the accessiblity predicate.
   (if (locale-north player-room)
       (describe-exit (cadr (locale-north player-room)) "north"))
   (if (locale-east player-room)
       (describe-exit (cadr (locale-east player-room)) "east"))
   (if (locale-west player-room)
       (describe-exit (cadr (locale-west player-room)) "west"))
   (if (locale-south player-room)
       (describe-exit (cadr (locale-south player-room)) "south"))
   (if (locale-northeast player-room)
       (describe-exit (cadr (locale-northeast player-room)) "northeast"))
   (if (locale-northwest player-room)
       (describe-exit (cadr (locale-northwest player-room)) "northwest"))
   (if (locale-southeast player-room)
       (describe-exit (cadr (locale-southeast player-room)) "southeast"))
   (if (locale-southwest player-room)
       (describe-exit (cadr (locale-southwest player-room)) "southwest"))
   (if (locale-up player-room)
       (describe-exit (cadr (locale-up player-room)) "up"))
   (if (locale-down player-room)
       (describe-exit (cadr (locale-down player-room)) "down"))
   (if (locale-in player-room)
       (describe-exit (cadr (locale-in player-room)) "in"))
   (if (locale-out player-room)
       (describe-exit (cadr (locale-out player-room)) "out"))
   (princ describe-inhabitants user-stream)))
   ;Add more directions here
(defun describe-exit (implement direction)
  (princ (concatenate 'string "There is a " implement " going" direction " from here") user-stream))
(defun north () (move north))
(defun n () (move north))
(defun east () (move east))
(defun e () (move east))
(defun west () (move west))
(defun w () (move west))
(defun south () (move south))
(defun s () (move south))
(defun up () (move up))
(defun u () (move up))
(defun down () (move down))
(defun d () (move down))
(defun in () (move in))
(defun out () (move out))
(defun o () (move out))
(defun nw () (move northwest))
(defun northwest () (move northwest))
(defun northeast () (move northeast))
(defun ne () (move northeast))
(defun sw () (move southwest))
(defun southwest () (move southwest))
(defun southeast () (move southeast))
(defun move (direction)
  (let ((direct (intern (string-upcase (concatenate 'string "locale-" direction)))))
    (if (direct (player-location (find-player-from-stream user-stream)))
	(progn (setf player-location (find-player-from-stream user-stream (car (locale-out (player-location (find-player-from-stream user-stream))))))
	       (look))
      (princ (concatenate 'string "There is no exit leading" direction "here") user-stream))))
	