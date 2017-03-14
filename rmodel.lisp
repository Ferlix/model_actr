#|

    Model:    Second-order theory of mind reinforcement learning

    Authors:  Arvid Lindström (s2740761)
              Federico Ferlito (s2936860)
              Francesco Dal Canton (s2935120)

    Info:     The comments made by us are the ones that use the
              multi-line specification (i.e. like this comment box).

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;	REINFORCEMENT LEARNING ACT-R model in order to show the developmental transitions in reasoning about false beliefs of others.
;;;;;;  These stand from a child's reasoning from his/her own point of view (zero-order) to taking into consideration
;;;;;;  an other agent’s beliefs (first-order) and later to taking into consideration an other agent’s beliefs about
;;;;;;  other agents’ beliefs (second-order).
;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;	Instructions on using this model:
;;;;;;	  1. Make sure you have ACT-R 6.0
;;;;;;	  2. Call (fbt) if you run the model for 1 child doing the experiment 1 time
;;;;;;    3. Call (do-fbt) if you run the model for 1 child doing the experiment 100 times
;;;;;;    4. Call (do-fbt-n N) if you run the model for N children doing the experiment 100 times
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defvar *response* nil) ;; global variable that stores model output
(defvar *model* nil)
(defvar *hold-responses* nil)  ;; global variable that stores model output



;; do experiment 1 time
(defun fbt (&optional who)

(goal-focus goal)
  (if (eq who 'human)
      (setf *model* nil)
         (setf *model* t)
   )
        (run 30 :real-time t)
)


;; do experiment 100 times
(defun do-fbt ()
	(dotimes (i 100)
            (setf *response* nil)
			(fbt)
               (push *response* *hold-responses*)
         )
)

;; do experiment nx100 times
(defun do-fbt-n (n)
	  (dotimes (i n)
              (setf *hold-responses* nil)
                        (reset)
			(do-fbt)
                        (write-to-file (concatenate 'string "dat-" (write-to-string i)) (nreverse *hold-responses*))
;(setf *hold-response* nil)
         )
)

;; write results to a file
(defun write-to-file (name lst)
	(with-open-file
		(out
			(ensure-directories-exist
				(merge-pathnames
					(make-pathname :name name :directory '(:relative "ACTR_model_output") :type "txt")
					"~/")
                        )
			:direction :output :if-does-not-exist :create :if-exists :supersede
                 )
			(dolist (line lst)
				(format out "~{~a~^~t~}~%" line))
         )
)



(clear-all) ;; clear model settings

(define-model FBT_2   ;name of the model

;;Model Parameters:

;; Below you see the related parameters for the model. You’re expected to assign values to them or keep them default or turn them on (t) or off (nil)
;; you can change the parameters once you have a complete model at the end of the course.
;; when you write your scientific report, you are expected to explain your decisions about the parameters.

(sgp
  #|
  Here we commented out the parameters that we did not need for this task, while we specified those that we needed.
  |#
  :esc t ;  sub-symbolic level
  :ol t  ;  optimised learning
  :rt -20  ;  retrieval threshold
  :ans nil ;  instantaneous noise
  :egs 0 ;  utility noise
  :ul t  ;  utility learning
  :pas nil; permanent noise

;; The parameters below for tracking the model’s trace. You can change them if you need to turn off the details etc.
;; See reference manual of ACT-R for further details, which is in the “docs” folder of ACT-R’s main folder

        :ult nil ;turn on the trace for the utilities
        :v  t   ; trace detail
        :act nil  ; activation trace parameter
        :sact low ; save activation trace

)

;Chunk-types of the Model:

;You will use the following chunk-type for the story facts. Story facts are the sentences from the story, i.e., see lines 141-146.
;The slot "subject" is for the subjects (i.e., "maxi", "sally", "mother").
;The slot "negation" is for negating the verbs (i.e., either nil or "not").
;The slot "verb" is for the verbs (i.e., "put", "see").
;The slot "object" is for the object (i.e., "chips").
;The slot "location" is for the locations (i.e., "cupboard", "oven", "trashbin").
;The slot "time" is for when the story is happened (i.e., 1,2,3, which represent time t0, t1, t2)(see below the temporal order chunk explanation, lines 141-147).
;The slot "type" is holding the type of a verb (i.e., action, perception).
;The slot "self-ref" refers to the chunks name itself. Each story fact chunk will refer to itself.
;The slot "ref" refers to other story facts which occur at the same time. (see below the temporal order chunk explanation, i.e., lines 141-146).
;The slot "level" is to hold the previously retrieved level of reasoning chunk in order to use it when it is necessary. (You might need this at some point in first-order and second-order reasoning)

(chunk-type story subject negation verb object location time type self-ref ref level)

;For the time sequences of the events. (You might need these but it is not necessary if you find other ways to model).

(chunk-type time-fact t0 t1 t2 t3 t4)

; The following chunk type is for the goals and their states.
; The slot "type" is for the where question (action)
; The slot "output"  is for holding the output if the model (i.e., cupboard, oven, trashbin)
(chunk-type goal state type output)


(add-dm

;The story fact chunks
#|
Here we added the relevant chunks for specifying the slots of the story chunks
|#
(not isa chunk) (action isa chunk) (perception isa chunk) (subject isa chunk)
(location isa chunk) (verb isa chunk) (object isa chunk) (negation isa chunk)
(put isa chunk) (see isa chunk) (type isa chunk) (Maxi isa chunk) (Sally isa chunk)
(chips isa chunk) (mother isa chunk) (oven isa chunk) (cupboard isa chunk)
(trashbin isa chunk) (r0 isa chunk) (r1 isa chunk) (r2 isa chunk) (r3 isa chunk)
(r4 isa chunk)

#|
Here we added the states that are specific to the goal chunk (i.e. the values for the state slot).
|#
(start isa chunk) (retrieve0 isa chunk) (retrieved0 isa chunk) (done isa chunk)
(retrieve-zero-percept isa chunk) (retrieve-one-percept isa chunk)
(retrieved1 isa chunk) (retrieve2 isa chunk) (retrieved2 isa chunk)

;temporal order chunk. There are three seperate time points in the story:
;at t0 Maxi put the chips into the cupboard.
;at t1 Sally put the chips into the oven.
;at t1 Maxi saw Sally.
;at t1 Sally did not see Maxi.
;at t2 The mother put the chips into the trashbin.
(t0 ISA time-fact t0 1 t1 2 t2 3)

#|
Here we defined the five story fact chunks. Each chunk follows the specification of the story chunk-type.
Each chunk corresponds to one of the five sentences of the story, in order to correctly represent the way
a child might remember the story after he reads or hears it.
|#
(story0 ISA story subject Maxi negation nil verb put object chips location cupboard time 1 type action self-ref story0 ref nil)
(story1 ISA story subject Sally negation nil verb put object chips location oven time 2 type action self-ref story1 ref nil)
(story2 ISA story subject Maxi negation nil verb see object Sally location nil time 2 type perception self-ref story2 ref story1)
(story3 ISA story subject Sally negation not verb see object Maxi location nil time 2 type perception self-ref story3 ref story2)
(story4 ISA story subject mother negation nil verb put object chips location trashbin time 3 type action self-ref story4 ref nil)

#|
Below chunk assigns the goal at the beginning of the trial. The state is set to start so that the start-response production
can fire, while the type is set to action because this reflects the question posed to this model (i.e. "Where are the chips?").
This because a 'where' question implies knowledge of where someone put something, and 'put' itself is an action, not a perception.
|#
(goal isa goal state start type action)

)

; For the Assignment 2, you are expected to write production rules to apply zero-order reasoning and gives
; the answer "trashbin" (as if the model reasons about the question "Where is the chips?").

; The production rule that gives the answer should also have the following functions for the output of the model:
; To put 0 (zero) as a strategy level representing the zero-order strategy to the variable response:  !safe-eval! (push 0 *response*)
; To write the name of values of the reasoning chunks:
; !safe-eval! (push (sdp (reasoning-zero reasoning-zero-0 reasoning-zero-0-1) :name :utility :u :at :reward) *response*)

#|
This production starts the reasoning process. After checking whether the goal chunk
has state 'start', it issues a request to the retrieval module to retrieve the story
chunk relevant for the action type specified in the goal chunk, which also needs to have
happened at time 2 (so we know it's the last story fact).  The goal state is set to
'retrieve' for the next production to start.
|#
(p start-zero
   =goal>
      ISA         goal
      state       start
      type        =type
==>
   +retrieval>
      ISA         story
      type        =type
      time        3
   =goal>
      state       retrieve0
)

#|
This production fires if the goal is in the 'retrieve' state, if the retrieval of the
story chunk was successful (multiple slots are checked for content), and if the imaginal
model is not busy in other tasks.
If that is the case, the retrieved chunk is added to the imaginal module, while
the goal state is set to 'respond'.
|#
(p retrieve-zero
   =goal>
      ISA         goal
      state       retrieve0
   =retrieval>
      ISA         story
      subject     =sub
      negation    nil
      verb        =verb
      object      =obj
      location    =loc
      time        =time
      type        =type
   ?imaginal>
      state       free
==>
   +imaginal>
      ISA         story
      subject     =sub
      negation    nil
      verb        =verb
      object      =obj
      location    =loc
      time        =time
      type        =type
   =goal>
      state      retrieved0
)

#|
This production starts when the goal chunk has state 'response', and when the relevant
story fact was successfully stored in the imaginal buffer (again, multiple slots are checked
for this condition).
If these conditions are met, the goal state is set to 'done' to signify
the end of the process, the output slot is finally set to the location contained in the chunk
that is in the imaginal buffer. Along with this, a few functions are included to print the output
in a separate file.
|#
(p respond-zero
   =goal>
      ISA         goal
      state       retrieved0
  =imaginal>
      ISA         story
      subject     =sub
      negation    nil
      verb        =verb
      object      =obj
      location    =loc
      time        =time
      type        =type
==>
  =goal>
      state       done
      output      =loc
  !output!        (=loc)
  !safe-eval!     (push 0 *response*)
  !safe-eval! (push (spp (respond-zero start-first start-second) :name :utility :u :at :reward) *response*)
)

#|
The model will attempt to simply retrieve the last time that Maxi saw something
in order to take on his perspective, which is necessary to answer the first-order theory of mind question.
|#
(p start-first
  =goal>
      ISA         goal
      state       retrieved0
  =imaginal>
      ISA         story
==>
  +retrieval>
      ISA         story
      subject     Maxi
      negation    nil
      type        perception
      verb        see
  =goal>
      state       retrieve-one-percept
)

#|
After having retrieved the chunk relative to Maxi's last perception, the model will retrieve
the action that happened at the same time as Maxi's perception. This action will be the one
which contains the location of the chips.
|#
(p retrieve-first
  =goal>
      ISA         goal
      state       retrieve-one-percept
  =retrieval>
      ISA         story
      subject     Maxi
      negation    nil
      type        perception
      time        =time
      ref         =r
==>
  =goal>
      state       retrieved1
  +retrieval>
      ISA         story
      self-ref    =r
)

#|
In this production the model will check whether it has a chunk in the retrieval module,
and if so it will produce an output which contains the location of the chips in that chunk.
|#
(p respond-first
  =goal>
      ISA         goal
      state       retrieved1
  =retrieval>
      ISA         story
      subject     =sub
      negation    nil
      verb        =verb
      object      =obj
      location    =loc
      time        =time
      type        =type
==>
  =goal>
      state       done
      output      =loc
  !output!        (=loc)
  !safe-eval!     (push 1 *response*)
  !safe-eval! (push (spp (respond-zero start-first start-second) :name :utility :u :at :reward) *response*)
  )

#|
At this point, the model is reasoning from the mother's point of view: it has the
chunk in the immaginal buffer containing the position "trashbin", where she put them.
The following rule, "start-second", will compete  with two rules:
 "respond-zero" (the one for responding with zero order TOM strategy) and "start-first"
 (the one that starts the 1st order TOM strategy).

 To answer the question:
  "Where does Sally think that Maxi will look for the chips?"
 in psychological plausible way, the model needs to retrieve the last perceived thing by Sally,
 since she's the last person that moved the chips before the mother (but Sally hadn't seen her!).
 Thus, if she knows that Maxi saw her when she moved the chips in the oven, this place will
 be the answer. Otherwise, if she haven't seen him, then Maxi doesn't know that the chips
 were moved there, so the answer will be the last action he did (or the last perceived movement of
 the chips that he saw before).

|#
(p start-second
  =goal>
      ISA         goal
      state       retrieved0
  =imaginal>
      ISA         story
==>
  =goal>
      state       retrieve2
  +retrieval>
      ISA         story
      subject     Sally
      type        perception
      verb        see
)

#|
Since the retrieved chunk shows that Sally did not see Maxi when she moved the chips,
this production issues a retrieval request for the chunk at time 1 in which an action
 regarding Maxi was carried out, since this will be the first occurrence of the chips being moved
 in which he's involved.
|#
(p retrieve-second
  =goal>
      ISA        goal
      state      retrieve2
  =retrieval>
      ISA         story
      subject     =sub
      negation    not
      verb        =verb
      object      =obj
      time        =time
      type        =type
==>
  =goal>
      state       retrieved2
  +retrieval>
      ISA         story
      subject     Maxi
      time        1
      type        action
)

#|
Here the chunk relative to Maxi moving the chips has been recalled, and the model thus
produces the answer both in the output and in the output file.
|#
(p respond-second
  =goal>
      ISA         goal
      state       retrieved2
  =retrieval>
      ISA         story
      subject     =sub
      negation    nil
      verb        =verb
      object      =obj
      location    =loc
      time        =time
      type        =type
==>
  =goal>
      state       done
      output      =loc
  !output!        (=loc)
  !safe-eval! (push 2 *response*)
  !safe-eval! (push (spp (respond-zero start-first start-second) :name :utility :u :at :reward) *response*)
)

(spp respond-zero :u 70)
(spp respond-zero :reward 0)

#|
Here we set the utility and reward values for the first-order reasoning. The utility value is lower than for the zero-order,
because it needs to be executed after the latter. The reward is set to a small value so that the improvement in performance is gradual.
|#
(spp start-first :u 20)
(spp respond-first :reward 0)

#|
Here we set the utility and reward values for the second-order reasoning. The utility value is very low, because it is the reasoning
that is executed last, after zero and first-order reasoning. The reward is quite high, because it is the correct reasoning to use
and the model needs to have positive feedback to reinforce it.
|#
(spp start-second :u 5)
(spp respond-second :reward 15)

)

;; The assignment will be graded in terms of the following criteria:
;; 1) Output
;; 2) Cognitive Plausibility of the production rules
;; 3) The quality of the code document in terms of clear explanations.
