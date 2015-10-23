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

(define-type opV
  [inc (i procedure?) (n number?)]
  [dec (d procedure?) (n number?)]
  [Mzero? (z procedure?) (n number?)]
  [Mnum? (n procedure?) (m number?)]
  [neg (n procedure?) (m number?)]
  [bool? (b procedure?) (v bool?)]
  [Mfirst (f procedure?) (l MList?)]
  [Mrest (f procedure?) (l MList?)]
  [Mempty? (f procedure?) (l MList?)]
  [Mlist? (f procedure?) (l MList?)])
  





