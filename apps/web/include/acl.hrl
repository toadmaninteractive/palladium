% Roles
-define(role_consumer, consumer).
-define(role_maintainer, maintainer).
-define(role_admin, admin).
-define(role_superadmin, superadmin).

-record(acl, {
    project :: atom() | 'undefined',
    database :: atom() | 'undefined',
    get :: web_protocol:access_role() | 'undefined',
    post :: web_protocol:access_role() | 'undefined',
    put :: web_protocol:access_role() | 'undefined',
    patch :: web_protocol:access_role() | 'undefined',
    delete :: web_protocol:access_role() | 'undefined'
}).

-type acl() :: #acl{}.
