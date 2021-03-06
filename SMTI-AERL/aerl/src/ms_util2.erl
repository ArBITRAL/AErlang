%% This module automatically generated - do not edit

%%% This module provides utilities for use in building
%%% match specifications from records

-module(ms_util2).

-export([get_index/2,get_variable/2,no_of_fields/1]).

no_of_fields(aerl_store) -> 5;
no_of_fields(Other) -> exit({error,"Invalid Record Name: "++Other}).


get_index(aerl_store,pid) -> 1;
get_index(aerl_store,id) -> 2;
get_index(aerl_store,env)-> 3;
get_index(aerl_store,ref) -> 4;
get_index(aerl_store,pred) -> 5;

get_index(aerl_store,F) -> exit({error,"Record: aerl_store has no field called "++atom_to_list(F)});
get_index(Record,_Field) -> exit({error,"Invalid Record Name: "++Record}).

get_variable(aerl_store,pid)-> '$1';
get_variable(aerl_store,id)-> '$2';
get_variable(aerl_store,env)-> '$3';
get_variable(aerl_store,ref)-> '$4';
get_variable(aerl_store,pred)-> '$5';
get_variable(Record,_Field) -> exit({error,"Invalid Record Name: "++Record}).
