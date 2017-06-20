# AttributeErlang
Programming attribute-based communication in Erlang

## Introduction
AErlang is a middleware and an extension of Erlang concurrent constructs for enabling Input and Output actions of the AbC process calculus (http://arxiv.org/pdf/1602.05635.pdf) in Erlang programs.
* attribute environment: in an AbC system, components are equipped with a set of attributes describing some interesting aspects that they want to expose to each other. Attributes have specific values that depends on the system to be modelled.
* attribute-based send: A send is decorated with a predicate over attributes of partners, which allows dynamic selecting the group of partners at the communication time.
* attribute-based receive: Similarly, a predicate at receiver side can be used to filter out uninteresting senders.

### Compilation Prerequisites

- rebar tool https://github.com/rebar/rebar
(if you are using Mac OSx: https://gist.github.com/recr0ns/ba1d0f1dc859c9789a23)
- Erlang OTP >= 18

At the top-level directory:
rebar compile

### startup

Start erl passing the path to ebin/ directory of aerl:

    erl -pa aerl/ebin

Start aerl:

    aerl:start().

This will start mnesia first and then components process registry, message broker

### Registering process's attribute environment

AErlang processes are normal Erlang one but they need an attribute environment in the form of either a proper list or a map, for example:

    Env = [{'Color', red},{'Role', explorer},{'Battery', 30}].

Each process needs to register their information by calling function register/1:

    aerl:register(Env).

Registration can also be done by other process invoking function register/2:

    aerl:register(Pid,Env).    

Processes can handle their attribute environment via setter and getter functions:

    aerl:setAtt(Name,Value).
    aerl:getAtt(Name).
    aerl:setAtt(TupleList).
    aerl:getAtt(NameList).

where TupleList has the similar form to Environment: [{'Attribute1', Value1},....].

### Attributes
Attributes are Erlang atoms
Attribute values can be one of the followings data types: atom(), number(), list()

### Predicates

In general, predicates are boolean expressions over attributes. 


### Attribute-based send
In the module that you are going to use attribute-based primitives, you include the aerlang transformation module:

    -module(...).
    -compile({parse_transform, aerl_trans}).

In the code, one process can send with predicates over attributes of registrated processes!. For example:

    Predicate = "battery > 30",
    to(Predicate) ! Msg
That means, send the message Msg to all processes whose attributes satisfy the predicate Predicate.
Predicates can be over user's variables using a prefix symbol $, provided that variables are declared in the same scope with predicates, e.g.,:
    
    X = 30,
    Ps = "battery > $X",
    to(Ps) ! content.

### Attribute-based receive 
You can declare a Predicate before Erlang receive construct. And use it to filter out all senders do not satisfy the predicate:

    Predicate = "color = red and battery < 30",
    from(Predicate),
    receive
        ....
    end.
Note that whenever a attribute-based receive is used, "from" must be followed by a "receive" construct.

## Running Examples
In this repo, there are aerl programs for stable matching problems.
Please visit their folder to see how to run them

## Bugs
aerlang is a prototype, built as a proof-of-concept, for experimenting with ideas of predicate-based message passing.

If you try aerlang and find any problems, please create issues (https://help.github.com/articles/creating-an-issue/)
so as they could be addressed
