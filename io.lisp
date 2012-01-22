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
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eql item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            (caps (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
(defun mulch-print (lst)
    (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string lst)) 'list) t nil) 'string))
    (fresh-line))
(defun mulch-read ()
    (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
         (flet ((quote-it (x)
                    (list 'quote x)))
             (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))
(defparameter user-stream nil "The stream to the current user it's reading")
(defun alist (what)
  (ecase what (:keys (function car)) (:values (function cdr))))
(defun mulch-repl ()
       (loop
	  (dolist (users-i (mapcar #'username-variable (map 'list (alist :keys)  *registered-usernames*)))
	    (if (player-stream users-i)
		(let ((cmd (mulch-read users-i)))
		  (setf user-stream (player-stream users-i))
		  (mulch-print (eval cmd))))))) ;I have no idea whether or not this will work.
;;Currently, we'll be using regular eval, but it should be replaced once we have a defcommand macro.

;;Now we must create a defcommand macro (in order to simplify the task of limiting certain commands to certain groups of players, e.g. level 40 and above or only Occultists... It will also be used for the basic communication commands: say, tell. We'll need to implement channels with this as well.
(defmacro defcommand (name (&rest args) player level c-class species gender gold city newbie  &body body)
  "Specialized DEFUN for commands to reduce code duplication"
  `(defun ,@name ,args 
     (if 
      (and 
       (or (> (player-gold player) ,@gold) (null ,@gold))
       (or (> (player-level player) ,@level) (null ,@level))
       (or (equalp (player-class player) ,@c-class) (null ,@c-class))
       (or (equalp (player-species player) ,@species) (null ,@species))
       (or (equalp (player-gender player) ,@gender) (null ,@gender))
       (or (equalp (player-city player) ,@city) (null ,@city))
       (or (<= (player-level player) 20) (null ,@newbie)))
      ,body
      (mulch-print "I do not know that command"))))

(defun say (&rest words) 
  (let ((users-at-room (remove (find-player-from-stream user-stream) (locale-players (player-location (find-player-from-stream user-stream))))))
    (dolist (users-i (users-at-room))
      (format recip-stream "~:(~A~) says: ~(~({~A~^ ~}~)~%" (find-player-from-stream user-stream) words)))
(defun tell (player &rest words)
  (let ((recip-stream (player-stream (username-variable player))))
    (format recip-stream "~:(~A~) tells you: ~(~({~A~^ ~}~)~%" (find-player-from-stream user-stream) words)))
;;How will I make channels? Maybe I'll make a function for each channel that conses a user to a list of people on the channel if they meet such-and-such condition, and have a channel-say command for each of them such that it prints it to all the streams on the channel? This is enough code reuse that it probably warrants a macro.
(defmacro channels (name 
    
  
		      
	  
  