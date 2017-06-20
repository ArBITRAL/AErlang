Compilation prerequisites
  rebar
  Erlang OTP >= 18
  
At the top-level directory
  rebar compile
  erl -pa aerl/ebin asmp/ebin

Run the application:
  application:start(asmp).
