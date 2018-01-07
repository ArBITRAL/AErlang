%% This module automatically generated - do not edit

%%% This module provides utilities for use in building
%%% match specifications from records

-module(ms_util2).

-export([get_index/2,no_of_fields/1]).

no_of_fields(aerl_store) -> 8;
no_of_fields(aerl_pub) -> 8;
no_of_fields(aerl_sub) -> 3;
no_of_fields(Other) -> exit({error,"Invalid Record Name: "++Other}).


get_index(aerl_store,pid)-> '$1';
get_index(aerl_store,id)-> '$2';
get_index(aerl_store,env)-> '$3';
get_index(aerl_store,body)-> '$4';
get_index(aerl_store,wealth)-> '$5';
get_index(aerl_store,eyes)-> '$6';
get_index(aerl_store,hair)-> '$7';
get_index(aerl_store,prefs)-> '$8';
get_index(aerl_store,F) -> exit({error,"Record: aerl_store has no field called "++atom_to_list(F)});
get_index(aerl_pub,pid)-> '$1';
get_index(aerl_pub,id)-> '$2';
get_index(aerl_pub,env)-> '$3';
get_index(aerl_pub,body)-> '$4';
get_index(aerl_pub,wealth)-> '$5';
get_index(aerl_pub,eyes)-> '$6';
get_index(aerl_pub,hair)-> '$7';
get_index(aerl_pub,prefs)-> '$8';
get_index(aerl_pub,F) -> exit({error,"Record: aerl_pub has no field called "++atom_to_list(F)});
get_index(aerl_sub,pid)-> '$1';
get_index(aerl_sub,timestamp)-> '$2';
get_index(aerl_sub,pred)-> '$3';
get_index(aerl_sub,F) -> exit({error,"Record: aerl_sub has no field called "++atom_to_list(F)});
get_index(Record,_Field) -> exit({error,"Invalid Record Name: "++Record}).
