# AttributeErlang
Programming with attribute-based communication in Erlang

## Introduction
AErlang is a middleware and an extension of Erlang concurrent constructs for enabling Input and Output actions of the AbC process calculus (http://arxiv.org/pdf/1602.05635.pdf) in Erlang programs.
* attribute environment: in an AbC systems, components are equipped with a set of attributes describing some interesting aspects that they want to expose to each other. An attribute environment maps attributes to specific values. 
* attribute-based send: A send is decorated with a predicate over attributes of partners, which allows dynamic selecting the group of communication partners at the communication time.
* attribute-based receive: Similarly, a predicate at receiver side is used to filter out uninteresting senders.

## How to use

Start AErlang in the application by:
    aerlang:start().

Erlang processes need to declare their attribute environment in the form of a list of 2-tuples
