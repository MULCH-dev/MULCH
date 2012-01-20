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

;;;;server.lisp
'

;;;This is for setting up the multiplexer
;23:28:01 <sea4ever> Follow from server-tick function. It calls server-accept-loop on the listening sockets, which returns a list of all new connections. It then iterates that list and makes a unique struct for each. (appending those to the global 'state')
(defun accept-sock-if-capable (socket) ;Much of the multiplexer design is cribbed from sea4ever's IRC server
  (if (usocket:state socket)
      (usocket:socket-accept socket)
      nil))
(defun server-accept-loop (listeners &optional (accum nil)
			   (if (not listeners)
			       accum
			       (server-accept-loop (cdr listeners)
						   (let ((new (accept-sock-if-capable (car listeners))))
						     (if new
							 (progn
							   (push new accum)
							   accum)
							 accum))))))
(defstruct server-state ;This is also from the IRC server, and will probably have to be heavily modified/destroyed to fit in a MULCH
  (listening-sockets nil)
  (connections nil)
  (users))

(defun welcome-to-mulch (stream)
  (princ "Welcome to the MULCH, v0.0" stream)
  (princ #\Newline stream)
  (princ "Enter your username:" stream)
  (let ((username (read-line stream)))
    (if (assoc username *registered-usernames*) ;*registered-usernames* is an alist...
	(progn
	  (princ "Enter your password:" stream)
	  (let ((password (read-line)))
	    (if (equal (cdr (assoc username *registered-usernames*)) password)
		(progn 
		  (princ #\Newline stream)
		  (princ "Welcome, " stream)
		  (princ username stream))
		(progn
		  (princ "Incorrect username or password")
		  (welcome-to-mulch stream)))))
	(build-player username stream))))

(defun build-player (username stream)
  (princ "Creating new user..." stream)
  (princ #\Newline stream)
  (princ "Enter your password" stream)
  (let ((password1 (read-line)))
    (princ "Confirm your password" stream)
    (let ((password2 (read-line)))
      (if (equal password1 password2)
	  (progn
	    (princ "Do you wish to be (physically) male or female? Note that Spivak pronouns will be used throughout the game" stream)
	    (let ((gender (read-line)))
	      (cond ((or (equalp gender "m") (equalp gender "male"))
		     (build-player-aux username password1 "male" stream))
		    ((or (equalp gender "f") (equalp gender "female"))
		     (build-player-aux username password1 "female" stream))
		    (t (princ "Sorry, didn't catch that. Let's try again: enter 'male' or 'female'.")
		       (build-player username stream)))))
	  (progn
	    (princ "Passwords do not match" stream)
	    (welcome-to-mulch stream))))))
(defun build-player-aux (username password gender stream)
  (princ "Which species are you? Choose from the following list:" stream)
  (princ species-list stream);;Add species with the game engine.
  (let ((species (read-line)))
    (cond ((equalp species "human")
	   (build-player-aux-1 username password ender "human" stream))
	  ((equalp species "elf")
	   (build-player-aux-1 username password gender "elf" stream))
	  ((equalp species "dwarf")
	   (build-player-aux-1 username password gender "dwarf" stream))
	  ((equalp species "orc")
	   (build-player-aux-1 username password gender "orc" stream))
	  ((equalp species "centaur")
	   (build-player-aux-1 username password gender "centaur" stream))
	  ;To be added with default stat mods.
	  (t (princ "Sorry, didn't catch that. Let's try again")
	     (build-player-aux username password gender stream))))) ;This may seem unnecessary, but it's for exception handling and general cleanliness, especially as species affects stat modifiers
(defun build-player-aux-1 (username password gender species stream)
  (defparameter `@,(username-variable username) (make-player :name username :password password :gender gender :species species :health 500 :mana 100 :level 0 :experience 0 :stream stream :location starting-location :saved-location starting-location)) ;Maybe change default health and mana...
  (pairlis username password *registered-usernames*))
(defun username-variable (username) (concatenate 'string "*user-" username "*"));This will be bound to the structs, so it deserves a function...
(defstruct connection ;;Also from IRC server
  (socket nil)
  (output-list) ;;List of strings to push out of this socket.
  (user))
(defun server-tick (server-state)
  (dolist (new-user-i (server-accept-loop (server-state-listening-sockets server-state))) ;IRC server: server-state was a struct and listening-sockets one of its slots...It was in structs.lsp
    ;;For each new user that has just connected.
    (push (make-connection :socket new-user-i)
	  (server-state-connections server-state))
    (welcome-to-mulch (stream-usocket-stream new-user-i))))

