{application,gloom,[
    {description,"Gloom: Simple Job Distribution"},
    {vsn,"0.0.1"},
    {modules,[
        gloom,
        gloom_app,
        gloom_db,
		gloom_master,
		gloom_server,
		gloom_slave,
		gloom_sup,
        gloom_util,
		mochijson2,
		mochinum
    ]},
    {registered,[gloom, gloom_master_server, gloom_client_server]},
    {applications,[kernel,stdlib,crypto]},
    {mod, {gloom_app, []}},
    {env, [
        {master_port, 9998},
		{slave_port, 9999},
        {ip, "0.0.0.0"},
        {lib, "/var/lib/gloom/"},
        {log, "/var/log/gloom/"}
    ]}    
]}.
