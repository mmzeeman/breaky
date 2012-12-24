Breaky (work in progress)
=========================

Breaky provides a way to supervise and manage modules and processes depending on external
resources. 

For these kind of processes standard OTP supervisors will not work. The problem is that if 
something fails the supervisor can't return the state of the external process in to a known
good state. When that happens the supervisor of the process will fail too and if the 
supervisor is restarted often enough it can take the whole system down.

Breaky acts as a "middleman" for these kind of processes. The process is started and managed
under an OTP supervision tree, but managed by a circuit breaker FSM. When the FSM detects 
a failure it will restart the process just like a normal supervisor would do. When a certaint
threshold of restarts has been reached the restarts will be done slower. Any call or cast to
the failing process will result in ```open``` to notify other processes about the situation.

Example:

    %% Start the circuit breaker for the database. It is an external resource.
    {ok, _Pid} = breaky:start_circuit_breaker(db, {start_link, database, 
        [{user, "me"}, {user, "1234"}]}),


    ...
    case breaky:call(db, "select * from my_table;") of
        open ->
            error_logger:error_msg("Db unavailable"); 
            {error, open};
        {ok, Result} ->
            Result
    end,
    ...

