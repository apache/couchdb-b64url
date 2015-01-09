==============
couchdb-b64url
==============

This is a pretty simple NIF that is just responsible for encoding and decoding
Base46 URL values::

    1> Thing = b64url:encode("Hello, CouchDB!").
    <<"SGVsbG8sIENvdWNoREIh">>
    2> b64url:decode(Thing).
    <<"Hello, CouchDB!">>

License
=======

Apache 2.0
