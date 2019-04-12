#lang sicp

; using generic operations with obvious dispatch will cause changes in every selector to handle new type
; new operation will have to know about all types and how to handle them
; in this case operations are more important for the system than types

; if we will use data-directed programming we`d have to add every type as a package with its procedures
; adding new procedure for multiple types involves adding this procedures for every package

; message-passing style is similar to data-directed programming
; every new type is a new object with its own dispatch
; every new procedure will cause adding it to all related types

; adding new type is more convinient with data-directed and message-passing style
; cause every type incapsulates its realization and doesn`t have to know about other types

; adding new operation is more convinient in generic way
; cause adding new operation will change only this operation
; we don`t have to walk through every type to add new operation

; in addition to that adding new procedures in message-passing constructor will not cause updates for old objects
; this objects have scoped their dispatch procedure as it was before and will act with an error on unlisted messages
; to update our data structure we will have to create new object using selectors for the old values

