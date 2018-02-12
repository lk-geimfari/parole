![](https://raw.githubusercontent.com/lk-geimfari/parole/master/logo.png)


An OTP client-server application for local password managing based on **gen_server**.

Usage
-----

```erlang
Erlang/OTP 18 [erts-7.3] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V7.3  (abort with ^G)

2> parole:start().
{ok,<0.40.0>}

3> Resource = <<"www.github.com">>.
<<"www.github.com">>

4> Password = <<"MyNotEncryptedPassword">>.
<<"MyNotEncryptedPassword">>

5> parole:add(Resource, Password).
ok

6> parole:lookup(Resource).
true

7> parole:get(Resource, decrypted).
<<"MyNotEncryptedPassword">>
```


TODO
----

- [ ] Use DETS instead gb_trees
- [ ] Add safe encryption algorithm support


LICENSE
-------

```
            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
                   Version 2, December 2004

Copyright (C) 2018 Likid Geimfari

Everyone is permitted to copy and distribute verbatim or modified
copies of this license document, and changing it is allowed as long
as the name is changed.

            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION

0. You just DO WHAT THE FUCK YOU WANT TO.

```