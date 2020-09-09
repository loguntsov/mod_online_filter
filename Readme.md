# Ejabberd API to get online users available for chatting

This module provides http API to get online users with specific status (online, available for chatting).

Module tracks presence with this status and returns list of JIDs (without resources) who are online and have specific status of this presence. 

It provides http API by URI: ``/online_filter``.

It returns list of strings (JIDs) in JSON format

## Configure

## Step 1

Add line for request_handlers to the config:

```
listen:
  ...
  -
    port: 5280
    module: ejabberd_http
    request_handlers:
      ...
      /online_filter/: mod_online_filter
      ...
  ...
```

Module will work as standard http interface in 5280 port for URL: /online_filter

## Step 2

Enable module:

```
modules:
    ...
    mod_online_filter:{}
```



## Requirements

* installed ejabbberd

Tested with Ejabberd 20.07

* make 

    ```sudo apt-get install make```
* rebar3 

```
    wget https://rebar3.s3.amazonaws.com/rebar3
    ./rebar3 local install
    export PATH=/home/loguntsov/.cache/rebar3/bin:$PATH    
```
* git
    
    ```sudo apt-get install git```
    
   
## Compilation

Just use make command to build everything.    

## Installation

Put folders with compiled beam files:
 
* _build/default/lib/http_gateway  
* _build/default/lib/jsx

to folder with all ejabberd's dependencies.

## Testing

To tesh http API just call:

```wget --no-check-certificate https://localhost:5280/online_filter```

### Ejabberd 20

# Installation Ejabberd from sources

## Installation dependencies

```
apt install gcc libssl-dev libexpat1-dev libyaml-dev g++ zlib1g-dev
```


