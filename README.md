# AttributeErlang
Programming attribute-based communication in Erlang

## Introduction
AErlang is a middleware and an extension of Erlang concurrent constructs for enabling Input and Output actions of the AbC process calculus (http://arxiv.org/pdf/1602.05635.pdf) in Erlang programs.
* attribute environment: in an AbC system, components are equipped with a set of attributes describing some interesting aspects that they want to expose to each other. Attributes have specific values that depends on the system to be modelled.
* attribute-based send: A send is decorated with a predicate over attributes of partners, which allows dynamic selecting the group of partners at the communication time.
* attribute-based receive: Similarly, a predicate at receiver side can be used to filter out uninteresting senders.

## How to use

### Registering process's attribute environment
Start AErlang in your application by calling function start/1 from aerlang module:

    aerl:start(broadcast).

Erlang processes should define an attribute environment in the form of either a list of 2-tuples or a map, for example:

    Env = [{'Color', red},{'Role', explorer},{'Battery', 30}].

Each process needs to register their information by calling function register/1:

    aerl:register(Env).

Registration can also be done by other process invoking function register/2:

    aerl:register(Pid,Env).    

Processes can handle their attribute environment via setter and getter functions:

    aerl:setAtt(Name,Value).
    aerl:getAtt(Name).
    aerl:setAtts(TupleList).
    aerl:getAtts(NameList).

where TupleList has the similar form to Environment: [{'Attribute1', Value1},....].

### Attribute-based send
In the module that you are going to use attribute-based primitives, you should include the aerlang transformation module:

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

See the example folder for case studies and details on how to use.

### Other features:
####Receiving predicates can be over elements of message itself (similar to selective receive):
    
    Predicate = "$Money > 100 and battery < 30",
    from(Predicate),
    receive
        {Money,Y} -> do_work
    end.

####Function start/1 can accept two other operation mode instead of broadcast
- pushing: It keeps tracks of process attribute values whenver processes update their environments. AErlang preselects the interested groups of receivers.
- pulling: It keeps tracks of receiving predicates. AErlang preselects the interested groups of senders.

