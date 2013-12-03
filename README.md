# Breaky

Breaky provides a way to supervise and manage modules and processes depending on external
resources. For these kind of processes standard OTP supervisors will not work. The problem 
is that if something fails, the supervisor can't return the state of an external process 
into a known good state by restarting. The supervisor has no control over it. When that happens 
too often, the supervisor will fail too, and if a supervisor is restarted often enough it 
can take down the whole system down.

Breaky acts as a automatic circuit breaker "middleman" for these kind of processes. The 
process are started and managed in a normal OTP supervision tree. Restarts are managed 
by a circuit breaker FSM. When the FSM detects too many failures it will try to re-establish
the service, but when a certaint threshold of restarts has been reached the restarts will be 
done in a slower rate and calls or casts to the failing process will result in ```open``` to notify 
that the circuit breaker is open and the process is not available. This will prevent the fault 
from cascading through the system.

Example:

```erlang
    %% Start the circuit breaker for the database. It is an external resource.
    Args = [{user, "me"}, {password, "1234"}],
    {ok, _Pid} = breaky:start_circuit_breaker(db, {start_link, database, Args}),
 
    ...
    
    case breaky:call(db, "select * from my_table;") of
        open ->
            error_logger:error_msg("database unavailable"); 
            {error, db_unavailable};
        {ok, Result} ->
            Result
    end,
    ...
```

## Circuit Breaker States

| State |  Description |
| -------: | :---------- |
| Open | In open state the process is not available |
| Closed | The process is running normally |
