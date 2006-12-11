/s_new create a new synth

string - synth definition name
int    - synth ID
int    - add action (0,1,2, 3 or 4 see below)
int    - add target ID
[
  int|string - a control index or name
  float      - a control value
] * N

Create a new synth from a synth definition, give it an ID, and add it
to the tree of nodes. There are four ways to add the node to the tree
as determined by the add action argument which is defined as follows:

0 - add the new node to the the head of the target group
1 - add the new node to the the tail of the target group
2 - add the new node just before the target node
3 - add the new node just after the target node
4 - the new node replaces the target node, which is freed

Controls may be set when creating the synth. The control arguments are
the same as for the n_set command.

If you send /s_new with a synth ID of -1, then the server will
generate an ID for you. The server reserves all negative IDs. Since
you don't know what the ID is, you cannot talk to this node directly
later. So this is useful for nodes that are of finite duration and
that get the control information they need from arguments and buses or
messages directed to their group. In addition no notifications are
sent when there are changes of state for this node, such as /go, /end,
/on, /off.

If you use a node ID of -1 for any other command, such as /n_map, then
it refers to the most recently created node by /s_new (auto generated
ID or not). This is how you can map the controls of a node with an
auto generated ID. In a multi-client situation, the only way you can
be sure what node -1 refers to is to put the messages in a bundle.
