#lang plai

(require "practica4-base.rkt")

(print-only-errors true)


(define-type RCFAEL
  [idR (name symbol?)]
  [numR (n number?)]
  [boolR (b boolean?)]
  [MListR (l MList?)]
  [withR (bindings (listof bind?))
         (body RCFAEL?)]
  [recR (bindings (listof bind?))
         (body RCFAEL?)] 
  [funR (params (listof symbol?))
        (body RCFAEL?)]
  [appR (fun RCFAEL?)
        (args (listof RCFAEL?))]
  [ifR (con RCFAEL?) (then RCFAEL?) (else RCFAEL?)]
  [equalR? (r RCFAEL?) (l RCFAEL?)]
  [opR (f procedure?)
         (l RCFAEL?)]
  [binopR (f procedure?)
         (l RCFAEL?)
         (r RCFAEL?)])
  
  
#|
<RCFAEL>::= 
**|<id>
**| <num>**
**| <bool>
**| <MList>
**| {with {{<id> <RCFAEL>}+} <RCFAEL>}
**| {rec {{<id> <RCFAEL>}+} <RCFAEL>}
**| {fun {<id>*} <RCFAEL>}
**| {if <RCFAEL> <RCFAEL> <RCFAEL>}
**| {equal? <RCFAEL> <RCFAEL>}
**| {<op> <RCFAEL>}
**| {<binop> <RCFAEL> <RCFAEL>}
| {<RCFAEL> <RCFAEL>*}
|#