#! /usr/bin/env python3

from random import randrange


MAXVALUE = 99


class Node:

    def __init__(self, value, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def __str__(self):
        return ('{0:'+str(len(str(MAXVALUE)))+'}').format(self.value)


def tree_depth(rootnode):
    if rootnode is None:
        return 0
    return 1 + max(tree_depth(rootnode.left),
                   tree_depth(rootnode.right))


def iter_nodes_at_depth(d, rootnode):
    if (not rootnode) or (d < 0):
        pass
    elif d == 0:
        yield rootnode
    elif d > 0:
        for x in iter_nodes_at_depth(d-1, rootnode.left):
            yield x
        for x in iter_nodes_at_depth(d-1, rootnode.right):
            yield x


def print_tree(rootnode):
    d = tree_depth(rootnode)
    for i, j in zip(range(d), reversed(range(d))):
        padding = 2**j - 1
        between = 2**(j+1) - 1
        #print((i, j, padding, between))
        print(' '*len(str(MAXVALUE))*padding, end='')
        for n, node in enumerate(iter_nodes_at_depth(i, rootnode)):
            print(node, end=' '*len(str(MAXVALUE))
                  * between if n < 2**i-1 else '')
        print()


def random_tree(depth):
    if depth <= 0:
        return None
    return Node(randrange(MAXVALUE+1),
                random_tree(depth-1),
                random_tree(depth-1))


print_tree(random_tree(6))
