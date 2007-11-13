{application, fuzed_node,
  [{description, "Fuzed Handler Node"},
   {vsn, "0.2.0"},
   {modules, [fuzed_node_app, fuzed_node_sup, resource_manager]},
   {registered, [fuzed_node_app, fuzed_node_sup, resource_manager]},
   {applications, [kernel, stdlib]},
   {mod, {fuzed_node_app, []}},
   {start_phases, []}
  ]}.