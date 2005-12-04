/n_set set a node's control value(s)

int - node ID
[
int or string - a control index or name
float - a control value
] * N

Takes a list of pairs of control indices and values and sets the
controls to those values. If the node is a group, then it sets the
controls of every node in the group.

