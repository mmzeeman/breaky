Breaky
======

Breaky provides a way to supervise and manage modules and processes depending on external
resources. For these kind of processes standard OTP supervisors will not work. The problem 
is that if something fails, the supervisor can't return the state of the external process 
in to a known good state by restarting because it has no control over it. When that happens 
the supervisor of the process can fail too. If the supervisor is restarted often enough it 
can take the whole system down.

Breaky acts as a circuit breaker "middleman" for these kind of processes. The process are
started and managed in a normal OTP supervision tree, but restarts are managed by a circuit 
breaker FSM. When the FSM detects too many failures it will restart the process just like 
a normal supervisor would do, but when a certaint threshold of restarts has been reached 
the restarts will be done slower and calls or casts to the failing process will result 
in ```open``` to notify that the circuit breaker is open and the process is not available.
This will prevent the fault from cascading through the system.

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

