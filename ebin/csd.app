%%-*- mode: erlang -*-
{application, csd,
 [
  {description, "csd"},
  {vsn, "1"},
  {modules, [
             csd,
             csd_app,
             csd_sup,
             csd_resource,

             % ErlyDTL templates
             sample_dtl
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto,
                  mochiweb,
                  webmachine
                 ]},
  {mod, { csd_app, []}},
  {env, []}
 ]}.
