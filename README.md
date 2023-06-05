# ErlyTalk - Messaging Service

It's a distributed WebSocket server in Erlang that handles incoming frames and dispatches them to the designated destination. It provides a scalable and fault-tolerant solution for real-time communication over WebSocket connections in a distributed environment.

- Messages between the Erlang nodes cluster are routed by the Distributed Erlang System.
- Users’ process IDs (PIDs) are stored and synchronized across the Erlang nodes cluster using Syn.
- The HTTP server is handled by Cowboy.
- New Erlang nodes are being discovered automatically by using Kubernetes headless service.

Requirements
-----
- Erlang OTP 26

Build
-----
    $ rebar3 compile
    $ rebar3 shell

Environment variables
-----
- MSGSVC_HTTP_BIND_ADDRESS: The address the HTTP server should bind to.
- MSGSVC_HTTP_BIND_PORT: The port the HTTP server should bind to.
- MSGSVC_CLUSTER_SECRET: A secret string used for inter-node communication within the cluster.
- NODE_IP: The IP address of the current node.

Contact
-----
For any questions or inquiries, please visit https://www.aliyousefian.com/contact.

For more information, visit the [project blog post](https://www.aliyousefian.com/880/erlytalk-project).
