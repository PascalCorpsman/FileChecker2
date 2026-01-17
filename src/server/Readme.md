# Server docu

If you want to use filechecker with a remote server you also need to compile the server.lpi file.

The filechecker server is a simple server that opens a TLS port to upload and download the database.

In order to work properly you need to provide a settings.ini file that at the beginning should at least have the following content

```ini
[Server]
Port=443
Certificate=server.crt
PrivateKey=server.key

[User]
Count=1
Name0=Admin
PW0=1234
Rights0=255
```

| Value | description |
| --- | --- |
| Port | TCP-Port where the server should listen
| Certificate, PrivateKey | Optional key pair created with command below
| Count | Number of registered users, need to be at least 1
| Name\<number> | Name of user with \<number> + 1
| PW\<number> | Cleartext password of user with \<number> + 1
| Rights\<number> | Bitwise encoded rights for user with \<number> + 1 (see below table)

On some machines the server is not able to create TLS connections by its own, or if you want to use a specific certificate you can create one using the following command:
```bash
openssl req -x509 -newkey rsa:2048 -nodes \
  -keyout server.key \
  -out server.crt \
  -days 365
```
### User rights
By default all users have the following rights:
- Right to request a challenge (needed to log in)
- Right to request a token (needed to log in)
- Right to get the list of available databases
- Right to download a arbiture database

| Bit | Decimal representation | Description |
| --- | --- | --- |
|  0  |  1  | Right to upload a database
|  1  |  2  | Right to reload settings (needed to refres settings.ini from remote without restarting the server)
|  2  |  4  | Right to edit users (needed to add / delete users, change rights of users)


