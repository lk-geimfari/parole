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
{value,<<"MyNotEncryptedPassword">>}

7> parole:get(Resource).
<<"MyNotEncryptedPassword">>
```


TODO
----

- [] Use DETS instead gb_trees
- [] Add safe encryption algorithm support