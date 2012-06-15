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
    along with MULCH.  If not, see <http://www.gnu.org/licenses/>.

    server.lisp|#
'

;;;This is for setting up the multiplexer
;23:28:01 <sea4ever> Follow from server-tick function. It calls server-accept-loop on the listening sockets, which returns a list of all new connections. It then iterates that list and makes a unique struct for each. (appending those to the global 'state')
(defun accept-sock-if-capable (socket) ;Much of the multiplexer design is cribbed from sea4ever's IRC server
  (if (usocket:state socket)
      (usocket:socket-accept socket)
      nil))
(defun server-accept-loop (listeners &optional (accum nil))
			   (if (not listeners)
			       accum
			       (server-accept-loop (cdr listeners)
						   (let ((new (accept-sock-if-capable (car listeners))))
						     (if new
							 (progn
							   (push new accum)
							   accum)
							 accum)))))
(defstruct server-state ;This is also from the IRC server, and will probably have to be heavily modified/destroyed to fit in a MULCH
  (listening-sockets nil)
  (connections nil)
  (users))

(defun welcome-to-mulch (stream)
  (princ "Welcome to the MULCH, v0.0" stream)
  (princ #\Newline stream)
  (princ "Enter your username:" stream)
  (let ((username (read-line stream)))
    (if (gethash username *users*) 
	(progn
	  (princ "Enter your password:" stream)
	  (let ((password (read-line)))
	    (if (equal (player-password (gethash username *users*)) password)
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
	    (princ "Do you wish to be referred to by male, female, or spivak pronouns?" stream)
	    (let ((gender (read-line)))
	      (cond ((or (equalp gender "m") (equalp gender "male"))
		     (build-player-aux username password1 "male" stream))
		    ((or (equalp gender "f") (equalp gender "female"))
		     (build-player-aux username password1 "female" stream))
		    ((equalp gender "spivak")
		     (build-player-aux username password1 "spivak" stream))
		    (t (princ "Sorry, didn't catch that. Let's try again: enter 'male' or 'female', or 'spivak'.")
		       (build-player username stream)))))
	  (progn
	    (princ "Passwords do not match" stream)
	    (welcome-to-mulch stream))))))
(defun build-player-aux (username password gender stream)
  (princ "Which species are you? Choose from the following list:" stream)
  (format stream "~{~@(~a~)~%~}"  *species-list*);;Add species with the game engine.
  (let ((species (read-line stream)))
    (if (not (member species *species-list* :test equalp))
	(progn (princ "Sorry, didn't catch that. Let's try again" stream)
	       (build-player-aux username password gender stream))
      (build-player-aux* username password gender species stream))))
(defun build-player-aux* (username password gender species stream)
  (princ "If you wish to choose a character class now, do so:" stream)
  (format stream "~{~@(~a~)~%~}" *c-class-list*)
  (let ((c-class (read-line stream)))
    (if (and (not (member c-class *c-class-list* :test equalp)) (not (null c-class)))
	(progn (princ "Sorry, didn't catch that. Let's try again" stream)
	       (build-player-aux* username password gender species stream))
      (build-player-aux** username password gender species c-class stream))))
(defun build-player-aux** (username password gender species c-class stream) 
 (setf (gethash username *users*) (make-player :name username :password password :gender gender :species species :char-class c-class :stream stream))) ;Maybe change default health and mana...and add other things.
  
  (newbie-tut (username))))
;;Define a newbie-tutorial 


;;Some cities will only be open to people of certain character classes, and such
;;We want to emphasize roleplay, and give each city a local flavor. This is, after all, based on D&Dis.
;;Similarly, though, as we'd be characterized as gods, we'd need to intefere very indirectly, and have some random bad stuff (e.g., natural disasters) happen.
;;We'll need a way of classifying character classes, and we'd probably have to limit the number of them 
#|List of (suggested) Character Classes:
Platonist
Aristotlean
Confucian
Spinozist
Cartesian
Kantian
Hegelian
Buddhist
Schopenhauerist
Positivist
Atheistic Existentialist
Theistic Existentialist
Lockean
Hobbesian
Postmodernist
Objectivist
Mohist

We'll need to shorten this list...

|#
;We'll also need to find a way so that they can take on the roles of, say, Druids or Merchants in Ateraan. Maybe they produce some goods

(defun username-variable (username) (gethash username *users*));This will be bound to the structs, so it deserves a function...
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

;;;OK, I'm going to completely redefine the way I handle users. I'll put the structs in a hash table, *users*...
(defparameter *users* (make-hash-table))
