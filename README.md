Foxplan
=======

Foxplan is a Erlang/OTP server designed to handle multiple [Planning Poker](https://en.wikipedia.org/wiki/Planning_poker) estimation sessions in Real Time. It uses WebSockets to enable users to connect, estimate and see each other estimations in a shared environment.

It currently supports the following features:

* Room creation
* Open, Finish and Cancel estimation
* User invite (using name and email)
* User status visibility (connected, ready to estimate, estimated, etc)
* User estimation

Events
------

### Room

#### Receives

* `room_created`
* `user_invited`
* `user_connected`
* `user_ready_to_vote`
* `user_estimate`

#### Sends

* `open_estimation`
* `finish_estimation`
* `cancel_estimation`

### User Session

#### Receives

* `open_estimation`
* `finish_estimation`
* `cancel_estimation`

#### Sends

* `join_room`


Samples
-----

Example of how to use it:

* Room (https://gist.github.com/elvio/93f6be34929941adac94)
* User (https://gist.github.com/elvio/80fe2ed9fa656f22fb05)
