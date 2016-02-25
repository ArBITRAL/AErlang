# AttributeErlang
Programming attribute-based communication in Erlang

## Introduction
AErlang is a middleware and an extension of Erlang concurrent constructs for enabling Input and Output actions of the AbC process calculus (http://arxiv.org/pdf/1602.05635.pdf) in Erlang programs.
* attribute environment: in an AbC system, components are equipped with a set of attributes describing some interesting aspects that they want to expose to each other. Attributes have specific values that depends on the system to be modelled.
* attribute-based send: A send is decorated with a predicate over attributes of partners, which allows dynamic selecting the group of partners at the communication time.
* attribute-based receive: Similarly, a predicate at receiver side can be used to filter out uninteresting senders.

## How to use
Start AErlang in your application by calling function start/0 from aerlang module:

    aerlang:start().

Erlang processes should define an attribute environment in the form of a list of 2-tuples, for example:

    Env = [{'Color', red},{'Role', explorer},{'Battery', 30}].

Then each process needs to register their information by calling function register/2:

    aerlang:register(Key, Env).
    
Processes can update their attribute values by calling update/2:

    aerlang:update(Key, Data).

where Data has the similar form to Environment: [{'Attribute1', Value1},....].
    
