{application, fuzed,
  [{description, "Fuzed Master"},
   {vsn, "0.1.0"},
   {modules, [fuzed_app, fuzed_sup, rails_connection_pool, pool_sweeper]},
   {registered, [fuzed_app, fuzed_sup, rails_connection_pool, pool_sweeper]},
   {applications, [kernel, stdlib]},
   {mod, {fuzed_app, []}},
   {start_phases, []}
  ]}.