{application,veda@client,
             [{description,"veda@client - veda web client application"},
              {vsn,"0.0.1a"},
              {modules,[hello_dtl,veda@client,veda@client_app,
                        veda@client_resource,veda@client_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,inets,crypto,mochiweb,webmachine]},
              {mod,{veda@client_app,[]}},
              {env,[]}]}.
