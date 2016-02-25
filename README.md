# AttributeErlang
Programming attribute-based communication in Erlang

## Introduction
AErlang is a middleware and an extension of Erlang concurrent constructs for enabling Input and Output actions of the AbC process calculus (http://arxiv.org/pdf/1602.05635.pdf) in Erlang programs.
* attribute environment: in an AbC system, components are equipped with a set of attributes describing some interesting aspects that they want to expose to each other. Attributes have specific values that depends on the system to be modelled.
* attribute-based send: A send is decorated with a predicate over attributes of partners, which allows dynamic selecting the group of partners at the communication time.
* attribute-based receive: Similarly, a predicate at receiver side can be used to filter out uninteresting senders.

## How to use

### Registering process's attribute environment
Start AErlang in your application by calling function start/0 from aerlang module:

    aerlang:start().

Erlang processes should define an attribute environment in the form of a list of 2-tuples, for example:

    Env = [{'Color', red},{'Role', explorer},{'Battery', 30}].

Then each process needs to register their information by calling function register/2:

    aerlang:register(Key, Env).
    
Key should be unique for each process.

Processes can update their attribute values by calling update/2:

    aerlang:update(Key, Data).

where Data has the similar form to Environment: [{'Attribute1', Value1},....].

### Attribute-based send
In the module that you are going to use attribute-based primitives, you should include the aerlang transformation module:

    -module(test_AbC).
    -compile({parse_transform, aerl_trans}).

Then in the code, one process can send with predicate!. For example:

    Predicate = "Battery > 30",
    to(Predicate) ! Msg
That means, send message Msg to all processes whose attributes satisfy the predicate Predicate.

### Attribute-based receive: you can declare a Predicate before Erlang receive construct. And use it to filter out all senders do not satisfy the predicate:

    from(Predicate),
    receive
        ....
    end.

### See test and example folder for more details on how to use
